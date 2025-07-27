REPORT grok4.

PARAMETERS: p_vbeln TYPE vbak-vbeln.

START-OF-SELECTION.

  DATA: ls_order_header_out TYPE bapisdhd1,
        lt_order_items_out  TYPE TABLE OF bapisditm,
        lt_return           TYPE TABLE OF bapiret2,
        ls_order_header_in  TYPE bapisdh1,
        ls_order_header_inx TYPE bapisdh1x,
        lv_new_prsdt        TYPE dats,
        lv_delivery         TYPE vbeln_vl,
        lt_billingdata_in   TYPE TABLE OF bapivbrk,
        lt_billing_success  TYPE TABLE OF bapivbrksuccess,
        lt_docflow          TYPE TABLE OF "TODO-CHECK:BAPI_STRUCTURE_FOR_SD_DOCFLOW".

  " Step 1: Read sales order header, items (status via header)
  CALL FUNCTION 'BAPI_SALESORDER_GETDETAIL'
    EXPORTING
      salesdocument = p_vbeln
    IMPORTING
      orderheader   = ls_order_header_out
    TABLES
      return        = lt_return
      order_item    = lt_order_items_out.

  " Check return for errors
  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc = 0.
    MESSAGE e000(00) WITH 'Error reading sales order'.
  ENDIF.

  " Step 2: Check and update pricing date
  lv_new_prsdt = ls_order_header_out-created_on + 7.
  IF ls_order_header_out-pric_date IS INITIAL OR ls_order_header_out-pric_date = ls_order_header_out-created_on.
    ls_order_header_in-pric_date = lv_new_prsdt.
    ls_order_header_inx-updateflag = 'U'.
    ls_order_header_inx-pric_date = 'X'.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = p_vbeln
        order_header_in  = ls_order_header_in
        order_header_inx = ls_order_header_inx
      TABLES
        return           = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc = 0.
      MESSAGE e000(00) WITH 'Error changing sales order'.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

  " Step 3: Ensure not blocked for delivery
  IF ls_order_header_out-dlv_block IS NOT INITIAL.
    " Raise checked exception - using MESSAGE as per constraints
    MESSAGE e000(00) WITH 'Order is blocked for delivery' RAISING blocked.
  ENDIF.

  " Step 4: Create outbound delivery
  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
    EXPORTING
      ship_point     = ls_order_header_out-ship_point " Assume from order or determine
      due_date       = sy-datum
    IMPORTING
      delivery       = lv_delivery
    TABLES
      sales_order_items = lt_order_items_out " Need to map
      return         = lt_return.

  " Note: BAPI_OUTB_DELIVERY_CREATE_SLS parameters may need adjustment; assumes order items

  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc = 0.
    MESSAGE e000(00) WITH 'Error creating delivery'.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  " Step 5: Create billing document
  APPEND INITIAL LINE TO lt_billingdata_in ASSIGNING FIELD-SYMBOL(<fs_bill>).
  <fs_bill>-ref_doc = lv_delivery.
  <fs_bill>-ref_doc_ca = 'J'. " Delivery

  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    TABLES
      billingdatain = lt_billingdata_in
      return        = lt_return
      success       = lt_billing_success.

  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc = 0.
    MESSAGE e000(00) WITH 'Error creating invoice'.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  " Step 6: Retrieve document flow
  " TODO-CHECK:BAPI_FOR_SD_DOCUMENT_FLOW
  " Assume call to BAPI_XXX exporting p_vbeln importing lt_docflow

  " Display ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = DATA(lo_alv)
        CHANGING
          t_table      = lt_docflow ).
      lo_alv->display( ).
    CATCH cx_salv_msg.
      MESSAGE e000(00) WITH 'Error displaying ALV'.
  ENDTRY.