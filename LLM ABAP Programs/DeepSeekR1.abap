REPORT DeepSeekR1.

DATA: lv_vbeln TYPE vbak-vbeln.

PARAMETERS: p_vbeln TYPE vbak-vbeln.

CLASS lcx_order_blocked DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      " Step 1: Get order details
      DATA: lt_order_header TYPE TABLE OF bapisdhd1,
            lt_order_items  TYPE TABLE OF bapisditm.

      CALL FUNCTION 'BAPI_SALESORDER_GETDETAILEDLIST'
        EXPORTING
          salesdocument = p_vbeln
        TABLES
          order_header  = lt_order_header
          order_items   = lt_order_items
          return        = DATA(lt_return).

      READ TABLE lt_return TRANSPORTING NO FIELDS
        WHERE type CA 'AE'.
      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE cx_sy_dynamic_osql_error
          EXPORTING
            textid = VALUE scx_t100key(
              msgid = '00'
              msgno = '001'
              attr1 = 'Error reading order details').
      ENDIF.

      " Step 2: Check/update pricing date
      DATA(ls_header) = lt_header[ 1 ].
      IF ls_header-pricing_date IS INITIAL OR
         ls_header-pricing_date = ls_header-created_on.
        ls_header-pricing_date = ls_header-created_on + 7.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = p_vbeln
          TABLES
            return           = lt_return
            order_header_inx = VALUE bapisdh1x( updateflag = 'U' ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.

      " Step 3: Check delivery block
      IF lt_header[ 1 ]-delivery_block NE space.
        RAISE EXCEPTION TYPE lcx_order_blocked.
      ENDIF.

      " Step 4: Create delivery
      DATA: lv_delivery TYPE vbeln_vl.
      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
        EXPORTING
          shipment_type = 'LF'
        IMPORTING
          delivery      = lv_delivery
        TABLES
          return        = lt_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      " Step 5: Create invoice
      DATA: lv_invoice TYPE vbeln_vf.
      CALL FUNCTION 'BAPI_BILLINGDOC_CREATEFROMDATA'
        EXPORTING
          delivery_number = lv_delivery
        IMPORTING
          billingdocument = lv_invoice
        TABLES
          return          = lt_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      " Step 6: Show document flow
      DATA: lt_relations TYPE TABLE OF bapi_obj_rel.
      CALL FUNCTION 'BAPI_OBJREL_GETDETAILEDLIST'
        EXPORTING
          object_id    = p_vbeln
          object_type  = 'VBAK'
        TABLES
          objrel_list  = lt_relations
          return       = lt_return.

      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = DATA(lo_alv)
        CHANGING
          t_table      = lt_relations ).

      lo_alv->display().

    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.