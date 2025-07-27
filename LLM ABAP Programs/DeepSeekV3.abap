REPORT z_deepseekv3.

* Input parameters
PARAMETERS: p_vbeln TYPE vbak-vbeln OBLIGATORY.

* Data declarations
DATA: lv_order_data TYPE bapisdhd1,
      lt_order_items TYPE TABLE OF bapisditm,
      lt_return      TYPE TABLE OF bapiret2,
      lv_delivery    TYPE vbeln_vl,
      lv_invoice     TYPE vbeln_vf,
      lt_docflow     TYPE TABLE OF bapisdflow.

* Main processing
START-OF-SELECTION.
  TRY.
      * 1. Read sales order data
      CALL FUNCTION 'BAPI_SALESORDER_GETDETAIL'
        EXPORTING
          salesdocument = p_vbeln
        IMPORTING
          order_header  = lv_order_data
        TABLES
          order_items   = lt_order_items
          return        = lt_return.

      * Check for errors
      READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE cx_sales_order_error.
      ENDIF.

      * 2. Check and update pricing date if needed
      IF lv_order_data-pricing_date IS INITIAL OR
         lv_order_data-pricing_date = lv_order_data-created_on.
        lv_order_data-pricing_date = lv_order_data-created_on + 7.
        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument = p_vbeln
            order_header  = lv_order_data
          TABLES
            return        = lt_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.

      * 3. Check delivery block
      IF lv_order_data-dlv_block NE space.
        RAISE EXCEPTION TYPE cx_delivery_blocked.
      ENDIF.

      * 4. Create delivery
      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
        EXPORTING
          sales_document = p_vbeln
        IMPORTING
          delivery       = lv_delivery
        TABLES
          return         = lt_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      * 5. Create invoice
      CALL FUNCTION 'BAPI_BILLINGDOC_CREATEFROMDATA'
        EXPORTING
          salesdocument = p_vbeln
          delivery      = lv_delivery
        IMPORTING
          billingdoc     = lv_invoice
        TABLES
          return         = lt_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      * 6. Get document flow
      CALL FUNCTION 'BAPI_SALESORDER_GETDOCFLOW'
        EXPORTING
          salesdocument = p_vbeln
        TABLES
          docflow      = lt_docflow
          return       = lt_return.

      * Display ALV grid
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_structure_name = 'BAPISDFLOW'
        TABLES
          t_outtab         = lt_docflow.

    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.