REPORT Sonnet37.

PARAMETERS: p_vbeln TYPE vbak-vbeln OBLIGATORY.

CLASS lcx_process DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcx_process IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      "1. Read sales order via BAPI
      DATA: ls_order_header_out TYPE bapisdhd1,
            lt_order_items_out  TYPE TABLE OF bapisditm,
            lt_return           TYPE TABLE OF bapiret2.

      CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
        EXPORTING
          salesdocument    = p_vbeln
        IMPORTING
          order_header_out = ls_order_header_out
        TABLES
          order_items_out  = lt_order_items_out
          return           = lt_return.

      "Check for errors
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE lcx_process.
      ENDLOOP.

      "2. Check and update pricing date if needed
      IF ls_order_header_out-pricing_date IS INITIAL OR
         ls_order_header_out-pricing_date = ls_order_header_out-doc_date.

        DATA: lv_new_date TYPE sy-datum.
        lv_new_date = ls_order_header_out-doc_date + 7.

        DATA: ls_order_header_in  TYPE bapisdhd1,
              ls_order_header_inx TYPE bapisdhd1x.

        ls_order_header_in-pricing_date = lv_new_date.
        ls_order_header_inx-pricing_date = 'X'.
        ls_order_header_inx-updateflag = 'U'.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = p_vbeln
            order_header_in  = ls_order_header_in
            order_header_inx = ls_order_header_inx
          TABLES
            return           = lt_return.

        "Check for errors
        LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A'.
          RAISE EXCEPTION TYPE lcx_process.
        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.

      "3. Check delivery block
      IF ls_order_header_out-dlv_block IS NOT INITIAL.
        RAISE EXCEPTION TYPE lcx_process.
      ENDIF.

      "4. Create delivery
      DATA: lt_sales_orders     TYPE TABLE OF bapidlvreftosalesorder,
            lv_delivery         TYPE vbeln_vl.

      APPEND INITIAL LINE TO lt_sales_orders ASSIGNING FIELD-SYMBOL(<ls_order>).
      <ls_order>-ref_doc = p_vbeln.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
        EXPORTING
          ship_point        = '1000' "TODO-CHECK:shipping_point
        IMPORTING
          delivery          = lv_delivery
        TABLES
          sales_order_items = lt_sales_orders
          return            = lt_return.

      "Check for errors
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE lcx_process.
      ENDLOOP.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      "5. Create billing document
      DATA: lt_billing_items TYPE TABLE OF bapi_dlv_billing_item,
            lv_billing_doc   TYPE vbeln.

      APPEND INITIAL LINE TO lt_billing_items ASSIGNING FIELD-SYMBOL(<ls_bill>).
      <ls_bill>-deliv_numb = lv_delivery.

      CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
        EXPORTING
          billing_type     = 'F2'
        IMPORTING
          billingdocument  = lv_billing_doc
        TABLES
          billing_items    = lt_billing_items
          return           = lt_return.

      "Check for errors
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE lcx_process.
      ENDLOOP.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      "6. Display document flow
      DATA: lt_docflow  TYPE TABLE OF bapivbfa,
            lt_fieldcat TYPE slis_t_fieldcat_alv.

      CALL FUNCTION 'BAPI_DOCUMENT_FLOW2'
        EXPORTING
          doc_number  = p_vbeln
        TABLES
          flow_items  = lt_docflow
          return      = lt_return.

      "Check for errors
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE lcx_process.
      ENDLOOP.

      "Filter and prepare ALV display data
      TYPES: BEGIN OF ty_alv,
               from_vbeln  TYPE vbeln,
               from_objcat TYPE char4,
               to_vbeln    TYPE vbeln,
               to_objcat   TYPE char4,
               created_on  TYPE datum,
             END OF ty_alv.
      DATA: lt_alv TYPE TABLE OF ty_alv,
            ls_alv TYPE ty_alv.

      LOOP AT lt_docflow ASSIGNING FIELD-SYMBOL(<ls_flow>).
        CLEAR ls_alv.
        ls_alv-from_vbeln  = <ls_flow>-docnum_from.
        ls_alv-from_objcat = <ls_flow>-vbtyp_from.
        ls_alv-to_vbeln    = <ls_flow>-docnum_to.
        ls_alv-to_objcat   = <ls_flow>-vbtyp_to.
        ls_alv-created_on  = <ls_flow>-erdat.
        APPEND ls_alv TO lt_alv.
      ENDLOOP.

      "Prepare ALV fieldcatalog
      DATA: lt_fieldcat_src TYPE slis_t_fieldcat_alv.
      
      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          i_program_name     = sy-repid
          i_internal_tabname = 'LT_ALV'
          i_inclname         = sy-repid
        CHANGING
          ct_fieldcat        = lt_fieldcat
        EXCEPTIONS
          OTHERS             = 1.

      "Display ALV grid
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = sy-repid
          it_fieldcat        = lt_fieldcat
        TABLES
          t_outtab           = lt_alv
        EXCEPTIONS
          OTHERS             = 1.

    CATCH lcx_process.
      MESSAGE 'Process error occurred' TYPE 'E'.
  ENDTRY.