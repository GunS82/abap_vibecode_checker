REPORT kimik2.

TYPES:
  BEGIN OF ty_docflow,
    from_vbeln   TYPE vbeln,
    from_objcat  TYPE objcat,
    to_vbeln     TYPE vbeln,
    to_objcat    TYPE objcat,
    created_on   TYPE erdat,
  END OF ty_docflow.

DATA lt_docflow TYPE TABLE OF ty_docflow.

PARAMETERS p_vbeln TYPE vbak-vbeln OBLIGATORY.

CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      DATA(ls_order) = read_sales_order( p_vbeln ).
      IF ls_order-pricing_date IS INITIAL OR ls_order-pricing_date = ls_order-creation_date.
        ls_order-pricing_date = ls_order-creation_date + 7.
        update_sales_order( is_order = ls_order ).
      ENDIF.
      check_order_delivery_block( is_order = ls_order ).
      DATA(lv_delivery) = create_delivery( iv_vbeln = p_vbeln ).
      DATA(lv_invoice)  = create_invoice( iv_vbeln = lv_delivery ).
      retrieve_document_flow( iv_vbeln = p_vbeln ).
    CATCH lcx_exception INTO DATA(lx_exc).
      MESSAGE lx_exc->get_text( ) TYPE 'E'.
  ENDTRY.

CLASS lcx_exception IMPLEMENTATION.
ENDCLASS.

FORM read_sales_order USING uv_vbeln TYPE vbak-vbeln
                      RETURNING VALUE(rs_order) TYPE bapisdh1
                      RAISING   lcx_exception.
  DATA lt_return TYPE TABLE OF bapiret2.
  CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
    EXPORTING
      salesdocument = uv_vbeln
    TABLES
      return        = lt_return.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RAISE NEW lcx_exception( ).
  ENDIF.
  CALL FUNCTION 'BAPI_SALESORDER_GETDETAIL'
    EXPORTING
      salesdocument = uv_vbeln
    IMPORTING
      order_header_in = rs_order
    TABLES
      return        = lt_return.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RAISE NEW lcx_exception( ).
  ENDIF.
ENDFORM.

FORM update_sales_order USING is_order TYPE bapisdh1
                        RAISING   lcx_exception.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA ls_headerx TYPE bapisdh1x.
  ls_headerx-updateflag = 'U'.
  ls_headerx-pricing_date = abap_true.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument = is_order-doc_number
      order_header_in = is_order
      order_header_inx = ls_headerx
    TABLES
      return        = lt_return.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RAISE NEW lcx_exception( ).
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.
ENDFORM.

FORM check_order_delivery_block USING is_order TYPE bapisdh1
                                RAISING   lcx_exception.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA lt_status TYPE TABLE OF bapisdstat.

  CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
    EXPORTING
      salesdocument = is_order-doc_number
    TABLES
      return        = lt_return
      statusinfo    = lt_status.

  READ TABLE lt_status WITH KEY gbstk = 'B' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RAISE NEW lcx_exception( ).
  ENDIF.
ENDFORM.

FORM create_delivery USING iv_vbeln TYPE vbak-vbeln
                     RETURNING VALUE(rv_delivery) TYPE vbeln_vl
                     RAISING   lcx_exception.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA lt_header TYPE TABLE OF bapidelicious.
  DATA lt_headerx TYPE TABLE OF bapideliciousx.
  DATA lt_items TYPE TABLE OF bapidlvitem.
  DATA lt_itemsx TYPE TABLE OF bapidlvitemx.
  DATA lt_partners TYPE TABLE OF bapidlvpartner.

  CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXECUTE'
    EXPORTING
      salesdocument = iv_vbeln
    IMPORTING
      delivery = rv_delivery
    TABLES
      return   = lt_return.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RAISE NEW lcx_exception( ).
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.
ENDFORM.

FORM create_invoice USING iv_vbeln TYPE vbeln_vl
                    RETURNING VALUE(rv_invoice) TYPE vbeln_vf
                    RAISING   lcx_exception.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA lt_billingdatain TYPE TABLE OF bapibillingdatain.
  DATA lt_billings TYPE TABLE OF bapivbrksales.

  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    TABLES
      billingdatain = lt_billingdatain
      return        = lt_return
      billings      = lt_billings.

  READ TABLE lt_billings ASSIGNING FIELD-SYMBOL(<ls_bill>) INDEX 1.
  IF sy-subrc = 0.
    rv_invoice = <ls_bill>-billingdoc.
  ENDIF.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RAISE NEW lcx_exception( ).
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.
ENDFORM.

FORM retrieve_document_flow USING iv_vbeln TYPE vbak-vbeln.
  DATA lt_docflow TYPE TABLE OF veda.
  DATA lt_vbeln TYPE TABLE OF vbeln.
  APPEND iv_vbeln TO lt_vbeln.

  CALL FUNCTION 'RV_DOCUMENT_FLOW'
    TABLES
      i_vbeln   = lt_vbeln
      et_docflow = lt_docflow.

  LOOP AT lt_docflow ASSIGNING FIELD-SYMBOL(<ls_flow>).
    APPEND VALUE ty_docflow(
      from_vbeln  = <ls_flow>-vbelv
      from_objcat = <ls_flow>-objnr_v
      to_vbeln    = <ls_flow>-vbeln
      to_objcat   = <ls_flow>-objnr_n
      created_on  = <ls_flow>-erdat
    ) TO lt_docflow.
  ENDLOOP.

  DATA go_alv TYPE REF TO cl_gui_alv_grid.
  DATA lt_fieldcat TYPE lvc_t_fcat.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(lo_alv)
    CHANGING
      t_table      = lt_docflow
  ).

  lo_alv->display( ).
ENDFORM.