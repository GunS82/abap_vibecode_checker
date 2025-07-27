REPORT Sonnet35.

PARAMETERS: p_vbeln TYPE vbak-vbeln OBLIGATORY.

CLASS lcx_process DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcx_process IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      "1. Read sales order via BAPI
      DATA: ls_order_header_in  TYPE bapisdh1,
            ls_order_header_out TYPE bapisdh1,
            lt_order_items_out  TYPE TABLE OF bapisditm,
            lt_return          TYPE TABLE OF bapiret2.

      CALL FUNCTION 'BAPI_SALESORDER_GETDETAILALL'
        EXPORTING
          salesdocument    = p_vbeln
        IMPORTING
          order_header_out = ls_order_header_out
        TABLES
          order_items_out  = lt_order_items_out
          return          = lt_return.

      "Check for errors
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE lcx_process.
      ENDLOOP.

      "2. Check and update pricing date if needed
      IF ls_order_header_out-pricing_date IS INITIAL OR
         ls_order_header_out-pricing_date = ls_order_header_out-doc_date.

        DATA: lv_new_date TYPE sy-datum.
        lv_new_date = ls_order_header_out-doc_date + 7.

        ls_order_header_in-pricing_date = lv_new_date.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = p_vbeln
            order_header_in  = ls_order_header_in
          TABLES
            return          = lt_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.

      "3. Check delivery block
      IF ls_order_header_out-dlv_block IS NOT INITIAL.
        RAISE EXCEPTION TYPE lcx_process.
      ENDIF.

      "4. Create delivery
      DATA: lt_sales_order_items TYPE TABLE OF bapidlvreftosalesorder,
            lt_deliveries       TYPE TABLE OF bapidlvheaderexp,
            lv_delivery        TYPE vbeln_vl.

      APPEND INITIAL LINE TO lt_sales_order_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-ref_doc = p_vbeln.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
        EXPORTING
          ship_point     = '1000' "TODO-CHECK:shipping_point
        IMPORTING
          delivery      = lv_delivery
        TABLES
          sales_order_items = lt_sales_order_items
          return           = lt_return.

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
          return          = lt_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      "6. Display document flow
      DATA: lt_docflow TYPE TABLE OF bapivbfa,
            lt_fieldcat TYPE slis_t_fieldcat_alv.

      CALL FUNCTION 'BAPI_DOCUMENT_FLOW2'
        EXPORTING
          doc_number  = p_vbeln
        TABLES
          flow_items  = lt_docflow
          return      = lt_return.

      "Prepare ALV display
      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = 'BAPIVBFA'
        CHANGING
          ct_fieldcat     = lt_fieldcat.

      "Display ALV grid
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = sy-repid
          it_fieldcat       = lt_fieldcat
        TABLES
          t_outtab         = lt_docflow.

    CATCH lcx_process.
      MESSAGE 'Process error occurred' TYPE 'E'.
ENDTRY.