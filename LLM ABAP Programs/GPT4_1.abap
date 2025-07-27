REPORT GPT4_1.

PARAMETERS p_vbeln TYPE vbak-vbeln OBLIGATORY.

DATA: lv_pricing_date TYPE vbak-prsdt,
      lv_creation_date TYPE vbak-erdat,
      lv_new_pricing_date TYPE vbak-prsdt,
      lt_items TYPE TABLE OF vbap,
      ls_item TYPE vbap,
      lt_status TYPE TABLE OF vbak,
      lt_return TYPE TABLE OF bapiret2,
      ls_return TYPE bapiret2,
      lv_delivery TYPE likp-vbeln,
      lv_invoice TYPE vbrk-vbeln,
      lt_doc_flow TYPE TABLE OF sdit_vbfa,
      ls_doc_flow TYPE sdit_vbfa.

* Step 1: Read sales order header, items, and status
CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
  EXPORTING
    salesdocument = p_vbeln
  TABLES
    statusinfo   = lt_status
    return       = lt_return.

CALL FUNCTION 'BAPI_SALESORDER_GETDETAIL'
  EXPORTING
    salesdocument = p_vbeln
  TABLES
    order_items_out = lt_items
    return          = lt_return.

CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
  EXPORTING
    salesdocument = p_vbeln
  TABLES
    statusinfo   = lt_status
    return       = lt_return.

* Step 2: Check and update pricing date
SELECT SINGLE prsdt erdat FROM vbak INTO (lv_pricing_date, lv_creation_date) WHERE vbeln = p_vbeln.
IF lv_pricing_date IS INITIAL OR lv_pricing_date = lv_creation_date.
  lv_new_pricing_date = lv_creation_date + 7.
  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = p_vbeln
      order_header_inx = VALUE bapisdh1x( updateflag = 'U' )
      order_header_in  = VALUE bapisdh1( pricing_date = lv_new_pricing_date )
    TABLES
      return           = lt_return.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
ENDIF.

* Step 3: Check delivery block
DATA(lv_delivery_block) = ''.
SELECT SINGLE lfsta FROM vbak INTO lv_delivery_block WHERE vbeln = p_vbeln.
IF lv_delivery_block IS NOT INITIAL AND lv_delivery_block <> ' '.
  RAISE EXCEPTION TYPE cx_static_check
    EXPORTING
      textid = 'Order is blocked for delivery.'.
ENDIF.

* Step 4: Create outbound delivery
CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
  EXPORTING
    salesdocument = p_vbeln
  IMPORTING
    delivery      = lv_delivery
  TABLES
    return        = lt_return.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

* Step 5: Create billing document
CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
  TABLES
    salesdocumentin = VALUE TABLE OF bapivbrksalesdocumentin( ( vbeln = lv_delivery ) )
    return          = lt_return
    billingdocout   = VALUE TABLE OF bapivbrkbillingdocout( )
    billingdocin    = VALUE TABLE OF bapivbrkbillingdocin( )
    billingdoc      = VALUE TABLE OF bapivbrkbillingdoc( )
    billingdocitem  = VALUE TABLE OF bapivbrkbillingdocitem( ).
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

* Step 6: Retrieve document flow
CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
  EXPORTING
    i_vbeln = p_vbeln
  TABLES
    t_vbfa  = lt_doc_flow.

* Display ALV grid
DATA: lt_alv TYPE TABLE OF sdit_vbfa,
      ls_alv TYPE sdit_vbfa.
lt_alv = lt_doc_flow.
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_structure_name = 'SDIT_VBFA'
  TABLES
    t_outtab         = lt_alv.