"! Report GPT_o3
"! @brief  Process a sales order: update pricing date, create delivery & invoice, show document flow.

REPORT gpt_o3.

PARAMETERS p_vbeln TYPE vbak-vbeln OBLIGATORY.  "Sales order number

TYPES:
  BEGIN OF ty_s_display,
    from_vbeln   TYPE vbeln_vl,
    from_objcat  TYPE vbfa-vbtyp_v,     "TODO-CHECK: field name in VBFA
    to_vbeln     TYPE vbeln_vl,
    to_objcat    TYPE vbfa-vbtyp_n,     "TODO-CHECK: field name in VBFA
    created_on   TYPE erdat,
  END OF ty_s_display.

DATA:
  lt_return       TYPE TABLE OF bapiret2,
  ls_header_out   TYPE bapisdh1,
  lt_items_out    TYPE TABLE OF bapisditm,
  lt_status       TYPE TABLE OF bapisdsfh,      "TODO-CHECK:bapisdsfh
  lv_new_prsdt    TYPE vbak-prsdt,
  lv_delivery     TYPE likp-vbeln,
  lv_billing      TYPE vbrk-vbeln,
  lt_docflow      TYPE TABLE OF bapidocflow,    "TODO-CHECK:bapidocflow
  lt_display      TYPE TABLE OF ty_s_display.

START-OF-SELECTION.

  "1. Read sales order header, items and status
  CALL FUNCTION 'BAPI_SALESORDER_GETDETAIL'
    EXPORTING
      salesdocument     = p_vbeln
    IMPORTING
      order_header_out  = ls_header_out
    TABLES
      order_items_out   = lt_items_out
      return            = lt_return.

  IF line_exists( lt_return[ type = 'E' ] ).
    RAISE EXCEPTION TYPE cx_bapi_error.
  ENDIF.

  CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
    EXPORTING
      salesdocument = p_vbeln
    TABLES
      statusinfo    = lt_status
      return        = lt_return.

  IF line_exists( lt_return[ type = 'E' ] ).
    RAISE EXCEPTION TYPE cx_bapi_error.
  ENDIF.

  "2. Adjust pricing date if initial or equal to creation date
  IF ls_header_out-pricing_date IS INITIAL
     OR ls_header_out-pricing_date = ls_header_out-created_on.

    lv_new_prsdt = ls_header_out-created_on + 7.

    DATA(ls_header_in)  = VALUE bapisdh1( pricing_date = lv_new_prsdt ).
    DATA(ls_header_inx) = VALUE bapisdh1x( pricing_date = abap_true ).

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = p_vbeln
        order_header_in  = ls_header_in
        order_header_inx = ls_header_inx
      TABLES
        return           = lt_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    IF line_exists( lt_return[ type = 'E' ] ).
      RAISE EXCEPTION TYPE cx_bapi_error.
    ENDIF.
  ENDIF.

  "3. Ensure no delivery block
  SELECT SINGLE lifsk
    INTO @DATA(lv_lifsk)
    FROM vbak
    WHERE vbeln = @p_vbeln.

  IF lv_lifsk IS NOT INITIAL.
    RAISE EXCEPTION TYPE cx_bapi_error.
  ENDIF.

  "4. Create outbound delivery
  CLEAR lt_return.
  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
    EXPORTING
      sales_document = p_vbeln
    IMPORTING
      delivery       = lv_delivery
    TABLES
      return         = lt_return.

  IF line_exists( lt_return[ type = 'E' ] ).
    RAISE EXCEPTION TYPE cx_bapi_error.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

  "5. Create billing document
  CLEAR lt_return.
  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    TABLES
      input  = VALUE bapivbrkin( ( ref_doc = lv_delivery ) )   "TODO-CHECK:bapivbrkin
      return = lt_return.

  IF line_exists( lt_return[ type = 'E' ] ).
    RAISE EXCEPTION TYPE cx_bapi_error.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

  "6. Retrieve document flow and display via ALV
  CLEAR lt_return.
  CALL FUNCTION 'BAPI_SALESORDER_GETDOCFLOW'
    EXPORTING
      salesdocument = p_vbeln
    TABLES
      doc_flow      = lt_docflow
      return        = lt_return.

  IF line_exists( lt_return[ type = 'E' ] ).
    RAISE EXCEPTION TYPE cx_bapi_error.
  ENDIF.

  LOOP AT lt_docflow ASSIGNING FIELD-SYMBOL(<fs_flow>).
    APPEND VALUE ty_s_display(
             from_vbeln  = <fs_flow>-from_vbeln
             from_objcat = <fs_flow>-from_objcat
             to_vbeln    = <fs_flow>-to_vbeln
             to_objcat   = <fs_flow>-to_objcat
             created_on  = <fs_flow>-created_on ) TO lt_display.
  ENDLOOP.

  TRY.
      DATA(lo_alv) = cl_salv_table=>factory(
                       IMPORTING r_salv_table = DATA(lo_table)
                       CHANGING  t_table      = lt_display ).
      lo_table->display( ).
    CATCH cx_salv_error INTO DATA(lx_salv).
      RAISE EXCEPTION TYPE cx_bapi_error.
  ENDTRY.
