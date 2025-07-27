REPORT z_gpt4o.

PARAMETERS: p_vbeln TYPE vbak-vbeln.

CLASS cx_delivery_blocked DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS cx_delivery_blocked IMPLEMENTATION.
ENDCLASS.

DATA: ls_vbak TYPE vbak,
      lt_vbap TYPE TABLE OF vbap,
      ls_vbap TYPE vbap,
      lv_pricing_date TYPE vbak-pricdate,
      lv_creation_date TYPE vbak-erdat,
      lv_new_pricing_date TYPE vbak-pricdate,
      lt_bapiret2 TYPE TABLE OF bapiret2,
      ls_bapiret2 TYPE bapiret2.

DATA: lt_vbfa TYPE TABLE OF vbfa,
      ls_vbfa TYPE vbfa.

DATA: gr_alv TYPE REF TO cl_salv_table.

" Step 1: Read sales order header and items using released BAPI
CALL FUNCTION 'BAPI_SALESORDER_GETDETAIL'
  EXPORTING
    salesdocument = p_vbeln
  IMPORTING
    order_header_in = ls_vbak
  TABLES
    order_items_in = lt_vbap
    return = lt_bapiret2.

READ TABLE lt_bapiret2 INTO ls_bapiret2 WITH KEY type = 'E'.
IF sy-subrc = 0.
  RAISE EXCEPTION TYPE cx_static_check
    EXPORTING textid = cx_static_check=>textid.
ENDIF.

lv_pricing_date = ls_vbak-pricdate.
lv_creation_date = ls_vbak-erdat.

" Step 2: Check pricing date and update if needed
IF lv_pricing_date IS INITIAL OR lv_pricing_date = lv_creation_date.
  lv_new_pricing_date = lv_creation_date + 7.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument = p_vbeln
    TABLES
      return = lt_bapiret2
      order_header_inx = VALUE bapisdhd1x( pricdate = 'X' )
      order_header_in = VALUE bapisdhd1( pricdate = lv_new_pricing_date ).

  READ TABLE lt_bapiret2 INTO ls_bapiret2 WITH KEY type = 'E'.
  IF sy-subrc = 0.
    RAISE EXCEPTION TYPE cx_static_check
      EXPORTING textid = cx_static_check=>textid.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING wait = 'X'.
ENDIF.

" Step 3: Check if order is blocked for delivery
DATA: lv_blocked TYPE char1.
CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
  EXPORTING
    salesdocument = p_vbeln
  IMPORTING
    delivery_blocked = lv_blocked
  TABLES
    return = lt_bapiret2.

IF lv_blocked = 'X'.
  RAISE EXCEPTION TYPE cx_delivery_blocked
    EXPORTING textid = cx_static_check=>textid.
ENDIF.

" Step 4: Create outbound delivery
CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
  EXPORTING
    salesdocument = p_vbeln
  IMPORTING
    delivery = DATA(lv_delivery)
  TABLES
    return = lt_bapiret2.

READ TABLE lt_bapiret2 INTO ls_bapiret2 WITH KEY type = 'E'.
IF sy-subrc = 0.
  RAISE EXCEPTION TYPE cx_static_check
    EXPORTING textid = cx_static_check=>textid.
ENDIF.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING wait = 'X'.

" Step 5: Create billing document for the delivery
CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
  EXPORTING
    delivery = lv_delivery
  IMPORTING
    billingdocument = DATA(lv_billingdoc)
  TABLES
    return = lt_bapiret2.

READ TABLE lt_bapiret2 INTO ls_bapiret2 WITH KEY type = 'E'.
IF sy-subrc = 0.
  RAISE EXCEPTION TYPE cx_static_check
    EXPORTING textid = cx_static_check=>textid.
ENDIF.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING wait = 'X'.

" Step 6: Retrieve document flow
CALL FUNCTION 'BAPI_DOCUMENT_FLOW_GETDETAIL'
  EXPORTING
    salesdocument = p_vbeln
  TABLES
    document_flow = lt_vbfa
    return = lt_bapiret2.

READ TABLE lt_bapiret2 INTO ls_bapiret2 WITH KEY type = 'E'.
IF sy-subrc = 0.
  RAISE EXCEPTION TYPE cx_static_check
    EXPORTING textid = cx_static_check=>textid.
ENDIF.

" Display ALV grid
cl_salv_table=>factory(
  IMPORTING r_salv_table = gr_alv
  CHANGING t_table = lt_vbfa ).

DATA(lo_columns) = gr_alv->get_columns( ).
lo_columns->get_column( 'VBELN_VL' )->set_short_text( 'FROM_VBELN' ).
lo_columns->get_column( 'VBTYP_VL' )->set_short_text( 'FROM_OBJCAT' ).
lo_columns->get_column( 'VBELN_VF' )->set_short_text( 'TO_VBELN' ).
lo_columns->get_column( 'VBTYP_VF' )->set_short_text( 'TO_OBJCAT' ).
lo_columns->get_column( 'ERDAT' )->set_short_text( 'CREATED_ON' ).

gr_alv->display( ).