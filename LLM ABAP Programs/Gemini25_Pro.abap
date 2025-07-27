REPORT Gemini25_Pro.

"!----------------------------------------------------------------------
"! Program Description
"!----------------------------------------------------------------------
"! This program processes a sales order to create a subsequent
"! delivery and invoice, and displays the document flow.
"!----------------------------------------------------------------------

PARAMETERS p_vbeln TYPE vbak-vbeln OBLIGATORY.

DATA: lv_vbeln_so TYPE vbeln_va,
      lv_vbeln_dn TYPE vbeln_vl,
      lv_vbeln_iv TYPE vbeln_vf.

CLASS lcl_main DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.
    METHODS read_sales_order
      IMPORTING
        iv_vbeln TYPE vbeln_va
      RETURNING
        VALUE(rs_order_status) TYPE bapisdstat
      RAISING
        cx_static_check.

    METHODS update_pricing_date
      IMPORTING
        iv_vbeln TYPE vbeln_va
        is_order_header TYPE bapisdhd
      RAISING
        cx_static_check.

    METHODS create_delivery
      IMPORTING
        iv_vbeln TYPE vbeln_va
      RETURNING
        VALUE(rv_delivery) TYPE vbeln_vl
      RAISING
        cx_static_check.

    METHODS create_invoice
      IMPORTING
        iv_delivery TYPE vbeln_vl
      RETURNING
        VALUE(rv_invoice) TYPE vbeln_vf
      RAISING
        cx_static_check.

    METHODS display_document_flow
      IMPORTING
        iv_vbeln TYPE vbeln_va
      RAISING
        cx_static_check.

    METHODS handle_bapi_return
      IMPORTING
        it_return TYPE bapirettab
      RAISING
        cx_static_check.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    DATA(lo_exception) = CAST cx_static_check( cx_root=>get_source_position( ) ).

    TRY.
        lv_vbeln_so = p_vbeln.

        " 1. Read sales order
        DATA(ls_order_status) = read_sales_order( lv_vbeln_so ).

        " 2. Update pricing date if necessary
        update_pricing_date(
          EXPORTING
            iv_vbeln        = lv_vbeln_so
            is_order_header = ls_order_status-header
        ).

        " 3. Check delivery block
        IF ls_order_status-header-dlv_block IS NOT INITIAL.
          RAISE EXCEPTION TYPE cx_static_check
            EXPORTING
              textid = cx_static_check=>co_textid_message
              msgv1  = |Sales order { lv_vbeln_so } is blocked for delivery.|
              msgty  = 'E'.
        ENDIF.

        " 4. Create delivery
        lv_vbeln_dn = create_delivery( lv_vbeln_so ).
        MESSAGE |Delivery { lv_vbeln_dn } created.| TYPE 'S'.

        " 5. Create invoice
        lv_vbeln_iv = create_invoice( lv_vbeln_dn ).
        MESSAGE |Invoice { lv_vbeln_iv } created.| TYPE 'S'.

        " 6. Display document flow
        display_document_flow( lv_vbeln_so ).

      CATCH cx_static_check INTO lo_exception.
        MESSAGE lo_exception->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD read_sales_order.
    DATA: lt_return TYPE bapirettab.

    CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
      EXPORTING
        salesdocument = iv_vbeln
      IMPORTING
        statusinfo    = rs_order_status
      TABLES
        return        = lt_return.

    handle_bapi_return( lt_return ).
  ENDMETHOD.

  METHOD update_pricing_date.
    IF is_order_header-pric_date IS INITIAL OR is_order_header-pric_date = is_order_header-creat_date.
      DATA(lv_new_pric_date) = is_order_header-creat_date + 7.

      DATA(ls_order_header_in) = VALUE bapisdhd1(
        pric_date = lv_new_pric_date
      ).
      DATA(ls_order_header_inx) = VALUE bapisdhd1x(
        updateflag = 'U'
        pric_date  = 'X'
      ).

      DATA(lt_return) = VALUE bapirettab( ).

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = iv_vbeln
          order_header_in  = ls_order_header_in
          order_header_inx = ls_order_header_inx
        TABLES
          return           = lt_return.

      handle_bapi_return( lt_return ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      MESSAGE |Pricing date for order { iv_vbeln } updated to { lv_new_pric_date }.| TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD create_delivery.
    DATA: ls_ship_point TYPE bapiaddr1-addr_no,
          ls_sales_org  TYPE bapiaddr1-addr_no,
          lt_item_data  TYPE STANDARD TABLE OF bapisditem,
          ls_delivery   TYPE bapidlvcreatehdr,
          lt_return     TYPE bapirettab.

    DATA(lv_req_date) = cl_abap_context_info=>get_system_date( ).

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
      EXPORTING
        ship_point      = ls_ship_point
        sales_organization = ls_sales_org
        due_date        = lv_req_date
        salesdocument   = iv_vbeln
      IMPORTING
        delivery        = rv_delivery
      TABLES
        item_data       = lt_item_data
        return          = lt_return.

    handle_bapi_return( lt_return ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.

  METHOD create_invoice.
    DATA: lt_return    TYPE bapirettab,
          lt_doc_items TYPE STANDARD TABLE OF bapi_incinv_create_item.

    APPEND VALUE #( ref_doc = iv_delivery ) TO lt_doc_items.

    CALL FUNCTION 'BAPI_BILLINGDOC_CREATEFROMDATA'
      EXPORTING
        reversal_specific_data = ' ' "TODO-CHECK: Check if needed
      TABLES
        billing_data_in        = lt_doc_items
        return                 = lt_return.

    handle_bapi_return( lt_return ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'S'.
      rv_invoice = ls_return-message_v1.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD display_document_flow.
    TYPES: BEGIN OF ty_doc_flow,
             from_vbeln TYPE vbeln_va,
             from_objcat TYPE char1,
             to_vbeln   TYPE vbeln_vf,
             to_objcat   TYPE char1,
             created_on  TYPE erdat,
           END OF ty_doc_flow.

    DATA: lt_doc_flow TYPE STANDARD TABLE OF ty_doc_flow.
    DATA: lt_vbfa     TYPE STANDARD TABLE OF vbfa.

    SELECT * FROM vbfa INTO TABLE lt_vbfa WHERE vbelv = iv_vbeln.

    LOOP AT lt_vbfa INTO DATA(ls_vbfa).
      APPEND INITIAL TO lt_doc_flow ASSIGNING FIELD-SYMBOL(<fs_doc_flow>).
      <fs_doc_flow>-from_vbeln = ls_vbfa-vbelv.
      <fs_doc_flow>-from_objcat = ls_vbfa-vbtyp_v.
      <fs_doc_flow>-to_vbeln = ls_vbfa-vbeln.
      <fs_doc_flow>-to_objcat = ls_vbfa-vbtyp_n.
      <fs_doc_flow>-created_on = ls_vbfa-erdat.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = lt_doc_flow ).

        lo_alv->display( ).
      CATCH cx_salv_msg.
        RAISE EXCEPTION TYPE cx_static_check
          EXPORTING
            textid = cx_static_check=>co_textid_message
            msgv1  = 'Error displaying ALV grid.'
            msgty  = 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_bapi_return.
    LOOP AT it_return INTO DATA(ls_return) WHERE type CA 'EAX'.
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = cx_static_check=>co_textid_message
          msgv1  = ls_return-message
          msgty  = ls_return-type.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->run( ).