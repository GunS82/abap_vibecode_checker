REPORT sonnet4.

"! SD Order to Delivery to Invoice Processing
"! This program processes a sales order through the complete order-to-cash cycle
"! @parameter p_vbeln | Sales order number to process

PARAMETERS: p_vbeln TYPE vbak-vbeln OBLIGATORY.

"! Custom exception for order processing errors
CLASS lcx_order_processing DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    METHODS constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL
        message  TYPE string OPTIONAL.
    DATA message TYPE string READ-ONLY.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF order_blocked,
        msgid TYPE symsgid VALUE 'ZSD',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MESSAGE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF order_blocked.
ENDCLASS.

CLASS lcx_order_processing IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    me->message = message.
  ENDMETHOD.
ENDCLASS.

"! Main processing class for order-to-cash flow
CLASS lcl_order_processor DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Process complete order-to-cash cycle
    "! @parameter iv_vbeln | Sales order number
    "! @raising lcx_order_processing | Processing error
    METHODS process_order
      IMPORTING
        iv_vbeln TYPE vbak-vbeln
      RAISING
        lcx_order_processing.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_doc_flow,
        from_vbeln  TYPE vbfa-vbelv,
        from_objcat TYPE vbfa-vbtyp_v,
        to_vbeln    TYPE vbfa-vbeln,
        to_objcat   TYPE vbfa-vbtyp_n,
        created_on  TYPE vbfa-erdat,
      END OF ty_doc_flow,
      tt_doc_flow TYPE TABLE OF ty_doc_flow.

    DATA:
      mv_order_number    TYPE vbak-vbeln,
      mv_delivery_number TYPE likp-vbeln,
      mv_invoice_number  TYPE vbrk-vbeln.

    "! Read sales order data
    "! @raising lcx_order_processing | Order read error
    METHODS read_sales_order
      RAISING
        lcx_order_processing.

    "! Update pricing date if needed
    "! @raising lcx_order_processing | Update error
    METHODS update_pricing_date
      RAISING
        lcx_order_processing.

    "! Check order delivery block status
    "! @raising lcx_order_processing | Order blocked
    METHODS check_delivery_block
      RAISING
        lcx_order_processing.

    "! Create outbound delivery
    "! @raising lcx_order_processing | Delivery creation error
    METHODS create_delivery
      RAISING
        lcx_order_processing.

    "! Create billing document
    "! @raising lcx_order_processing | Billing creation error
    METHODS create_billing
      RAISING
        lcx_order_processing.

    "! Display document flow in ALV
    "! @raising lcx_order_processing | Display error
    METHODS display_document_flow
      RAISING
        lcx_order_processing.

    "! Convert BAPI messages to exception
    "! @parameter it_return | BAPI return messages
    "! @raising lcx_order_processing | BAPI error
    METHODS handle_bapi_messages
      IMPORTING
        it_return TYPE bapiret2_t
      RAISING
        lcx_order_processing.
ENDCLASS.

CLASS lcl_order_processor IMPLEMENTATION.
  METHOD process_order.
    mv_order_number = iv_vbeln.

    read_sales_order( ).
    update_pricing_date( ).
    check_delivery_block( ).
    create_delivery( ).
    create_billing( ).
    display_document_flow( ).
  ENDMETHOD.

  METHOD read_sales_order.
    DATA:
      ls_order_header_in TYPE bapisdh1,
      lt_order_items_in  TYPE TABLE OF bapisditm,
      lt_return          TYPE TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
      EXPORTING
        salesdocument = mv_order_number
      IMPORTING
        order_header_in = ls_order_header_in
      TABLES
        order_items_in = lt_order_items_in
        return         = lt_return.

    handle_bapi_messages( lt_return ).
  ENDMETHOD.

  METHOD update_pricing_date.
    DATA:
      ls_order_header_in  TYPE bapisdh1,
      ls_order_header_inx TYPE bapisdh1x,
      lt_return           TYPE TABLE OF bapiret2,
      lv_creation_date    TYPE sy-datum,
      lv_pricing_date     TYPE sy-datum,
      lv_new_pricing_date TYPE sy-datum.

    " Get current order data
    CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
      EXPORTING
        salesdocument = mv_order_number
      IMPORTING
        order_header_in = ls_order_header_in
      TABLES
        return = lt_return.

    handle_bapi_messages( lt_return ).

    lv_creation_date = ls_order_header_in-doc_date.
    lv_pricing_date = ls_order_header_in-price_date.

    " Check if pricing date needs update
    IF lv_pricing_date IS INITIAL OR lv_pricing_date = lv_creation_date.
      lv_new_pricing_date = lv_creation_date + 7.

      " Prepare change data
      ls_order_header_in-price_date = lv_new_pricing_date.
      ls_order_header_inx-price_date = 'X'.
      ls_order_header_inx-updateflag = 'U'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = mv_order_number
          order_header_in  = ls_order_header_in
          order_header_inx = ls_order_header_inx
        TABLES
          return = lt_return.

      handle_bapi_messages( lt_return ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD check_delivery_block.
    DATA:
      ls_order_header_in TYPE bapisdh1,
      lt_return          TYPE TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
      EXPORTING
        salesdocument = mv_order_number
      IMPORTING
        order_header_in = ls_order_header_in
      TABLES
        return = lt_return.

    handle_bapi_messages( lt_return ).

    " Check delivery block
    IF ls_order_header_in-deliv_block IS NOT INITIAL.
      RAISE EXCEPTION TYPE lcx_order_processing
        EXPORTING
          message = |Order { mv_order_number } is blocked for delivery|.
    ENDIF.
  ENDMETHOD.

  METHOD create_delivery.
    DATA:
      lt_sales_orders TYPE TABLE OF bapidlvreftosalesorder,
      ls_sales_order  TYPE bapidlvreftosalesorder,
      lt_created_items TYPE TABLE OF bapidelivcreateditems,
      lt_return       TYPE TABLE OF bapiret2,
      lv_delivery     TYPE bapidlvhdrchg-deliv_numb.

    ls_sales_order-ref_doc = mv_order_number.
    ls_sales_order-ref_doc_ca = 'C'.  " Sales order category
    APPEND ls_sales_order TO lt_sales_orders.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
      IMPORTING
        delivery = lv_delivery
      TABLES
        sales_order_items = lt_sales_orders
        created_items = lt_created_items
        return = lt_return.

    handle_bapi_messages( lt_return ).

    mv_delivery_number = lv_delivery.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.

  METHOD create_billing.
    DATA:
      lt_billing_data TYPE TABLE OF bapivbrksimulateditems,
      ls_billing_data TYPE bapivbrksimulateditems,
      lt_return       TYPE TABLE OF bapiret2,
      lv_invoice      TYPE bapivbrkchg-bill_doc.

    ls_billing_data-ref_doc = mv_delivery_number.
    ls_billing_data-ref_doc_ca = 'J'.  " Delivery category
    APPEND ls_billing_data TO lt_billing_data.

    CALL FUNCTION 'BAPI_BILLINGDOC_CREATE1'
      IMPORTING
        billingdocument = lv_invoice
      TABLES
        billingdatain = lt_billing_data
        return = lt_return.

    handle_bapi_messages( lt_return ).

    mv_invoice_number = lv_invoice.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.

  METHOD display_document_flow.
    DATA:
      lt_doc_flow TYPE tt_doc_flow,
      ls_doc_flow TYPE ty_doc_flow,
      lo_alv      TYPE REF TO cl_salv_table.

    " Get document flow from VBFA table
    SELECT vbelv AS from_vbeln,
           vbtyp_v AS from_objcat,
           vbeln AS to_vbeln,
           vbtyp_n AS to_objcat,
           erdat AS created_on
      FROM vbfa
      INTO CORRESPONDING FIELDS OF TABLE lt_doc_flow
      WHERE vbelv = mv_order_number
         OR vbeln = mv_order_number
         OR vbelv = mv_delivery_number
         OR vbeln = mv_delivery_number
         OR vbelv = mv_invoice_number
         OR vbeln = mv_invoice_number.

    IF lt_doc_flow IS INITIAL.
      " Add at least the order itself
      ls_doc_flow-from_vbeln = mv_order_number.
      ls_doc_flow-from_objcat = 'C'.
      ls_doc_flow-to_vbeln = mv_delivery_number.
      ls_doc_flow-to_objcat = 'J'.
      ls_doc_flow-created_on = sy-datum.
      APPEND ls_doc_flow TO lt_doc_flow.

      CLEAR ls_doc_flow.
      ls_doc_flow-from_vbeln = mv_delivery_number.
      ls_doc_flow-from_objcat = 'J'.
      ls_doc_flow-to_vbeln = mv_invoice_number.
      ls_doc_flow-to_objcat = 'M'.
      ls_doc_flow-created_on = sy-datum.
      APPEND ls_doc_flow TO lt_doc_flow.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table = lt_doc_flow ).

        lo_alv->get_columns( )->set_optimize( ).
        lo_alv->get_display_settings( )->set_list_header( 'Document Flow: Order → Delivery → Invoice' ).
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        RAISE EXCEPTION TYPE lcx_order_processing
          EXPORTING
            message = |ALV display error: { lx_salv->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_bapi_messages.
    DATA: lv_message TYPE string.

    LOOP AT it_return INTO DATA(ls_return) WHERE type CA 'AEX'.
      lv_message = |{ ls_return-type }: { ls_return-message }|.
      RAISE EXCEPTION TYPE lcx_order_processing
        EXPORTING
          message = lv_message.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_processor TYPE REF TO lcl_order_processor.

  CREATE OBJECT lo_processor.

  TRY.
      lo_processor->process_order( p_vbeln ).
      MESSAGE 'Order processing completed successfully' TYPE 'S'.

    CATCH lcx_order_processing INTO DATA(lx_error).
      MESSAGE lx_error->message TYPE 'E'.
  ENDTRY.