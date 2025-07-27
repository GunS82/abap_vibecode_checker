*&---------------------------------------------------------------------*
*& Report Qwen3_Coder.abap
*&---------------------------------------------------------------------*
REPORT Qwen3_Coder.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_vbeln TYPE vbak-vbeln OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

CLASS lcx_sales_order_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA: mv_message TYPE string.
    METHODS: constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL
        message  TYPE string OPTIONAL.
ENDCLASS.

CLASS lcx_sales_order_exception IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid = textid previous = previous ).
    mv_message = message.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sales_order_processor DEFINITION.
  PUBLIC SECTION.
    "! Process the sales order: update pricing if needed, create delivery, create invoice, display flow.
    "! @parameter iv_vbeln | Sales Order Number
    "! @raising lcx_sales_order_exception | Raised for business errors
    METHODS process_sales_order
      IMPORTING
        iv_vbeln TYPE vbak-vbeln
      RAISING
        lcx_sales_order_exception.

  PRIVATE SECTION.
    METHODS:
      read_sales_order
        IMPORTING
          iv_vbeln TYPE vbak-vbeln
        EXPORTING
          ev_creation_date TYPE sy-datum
          ev_pricing_date  TYPE sy-datum
          ev_delivery_block TYPE vbak-lifsk
        RAISING
          lcx_sales_order_exception,
      update_pricing_date
        IMPORTING
          iv_vbeln         TYPE vbak-vbeln
          iv_pricing_date  TYPE sy-datum
        RAISING
          lcx_sales_order_exception,
      check_delivery_block
        IMPORTING
          iv_delivery_block TYPE vbak-lifsk
        RAISING
          lcx_sales_order_exception,
      create_delivery
        IMPORTING
          iv_vbeln         TYPE vbak-vbeln
        RETURNING VALUE(rv_delivery) TYPE vbeln
        RAISING
          lcx_sales_order_exception,
      create_invoice
        IMPORTING
          iv_delivery      TYPE vbeln
        RETURNING VALUE(rv_invoice) TYPE vbeln
        RAISING
          lcx_sales_order_exception,
      display_document_flow
        IMPORTING
          iv_vbeln         TYPE vbak-vbeln
          iv_delivery      TYPE vbeln
          iv_invoice       TYPE vbeln
        RAISING
          lcx_sales_order_exception.

    TYPES:
      BEGIN OF ty_doc_flow_line,
        from_vbeln  TYPE vbfa-vbelv,
        from_objcat TYPE vbfa-vbtypv,
        to_vbeln    TYPE vbfa-vbeln,
        to_objcat   TYPE vbfa-vbtypl,
        created_on  TYPE sy-datum,
      END OF ty_doc_flow_line,
      tt_doc_flow_line TYPE STANDARD TABLE OF ty_doc_flow_line WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_sales_order_processor IMPLEMENTATION.

  METHOD process_sales_order.
    DATA: lv_creation_date    TYPE sy-datum,
          lv_pricing_date     TYPE sy-datum,
          lv_delivery_block   TYPE vbak-lifsk,
          lv_new_pricing_date TYPE sy-datum,
          lv_delivery         TYPE vbeln,
          lv_invoice          TYPE vbeln.

    " Step 1: Read sales order
    read_sales_order(
      EXPORTING
        iv_vbeln           = iv_vbeln
      IMPORTING
        ev_creation_date   = lv_creation_date
        ev_pricing_date    = lv_pricing_date
        ev_delivery_block  = lv_delivery_block
    ).

    " Step 2: Update pricing date if necessary
    IF lv_pricing_date IS INITIAL OR lv_pricing_date = lv_creation_date.
      lv_new_pricing_date = lv_creation_date + 7.
      update_pricing_date(
        EXPORTING
          iv_vbeln        = iv_vbeln
          iv_pricing_date = lv_new_pricing_date
      ).
    ENDIF.

    " Step 3: Check delivery block
    check_delivery_block( iv_delivery_block = lv_delivery_block ).

    " Step 4: Create delivery
    lv_delivery = create_delivery( iv_vbeln = iv_vbeln ).

    " Step 5: Create invoice
    lv_invoice = create_invoice( iv_delivery = lv_delivery ).

    " Step 6: Display document flow
    display_document_flow(
      EXPORTING
        iv_vbeln   = iv_vbeln
        iv_delivery = lv_delivery
        iv_invoice  = lv_invoice
    ).

  ENDMETHOD.

  METHOD read_sales_order.
    DATA: lt_order_headers_out TYPE STANDARD TABLE OF bapisdhd1,
          lt_return            TYPE STANDARD TABLE OF bapiret2,
          ls_order_header_out  TYPE bapisdhd1,
          ls_return            TYPE bapiret2.

    CALL FUNCTION 'BAPI_SALESORDER_GETDETAIL'
      EXPORTING
        salesdocument    = iv_vbeln
      TABLES
        order_headers_out = lt_order_headers_out
        return           = lt_return.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = ls_return-message.
    ENDIF.

    READ TABLE lt_order_headers_out INTO ls_order_header_out INDEX 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = 'Sales order header data not found.'.
    ENDIF.

    ev_creation_date = ls_order_header_out-created_on.
    ev_pricing_date = ls_order_header_out-pricing_date.
    ev_delivery_block = ls_order_header_out-dlv_block.

  ENDMETHOD.

  METHOD update_pricing_date.
    DATA: lt_return        TYPE STANDARD TABLE OF bapiret2,
          ls_header_in     TYPE bapisdh1,
          ls_header_inx    TYPE bapisdh1x,
          ls_return        TYPE bapiret2.

    ls_header_in-doc_number = iv_vbeln.
    ls_header_in-pricing_date = iv_pricing_date.
    ls_header_inx-doc_number = iv_vbeln.
    ls_header_inx-updateflag = 'U'.
    ls_header_inx-pricing_date = 'X'.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = iv_vbeln
        order_header_in  = ls_header_in
        order_header_inx = ls_header_inx
      TABLES
        return          = lt_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = ls_return-message.
    ENDIF.

  ENDMETHOD.

  METHOD check_delivery_block.
    IF iv_delivery_block IS NOT INITIAL.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = 'Sales order is blocked for delivery.'.
    ENDIF.
  ENDMETHOD.

  METHOD create_delivery.
    DATA: lt_sales_order_items TYPE STANDARD TABLE OF bapidlvitemcreation,
          lt_created_deliveries TYPE STANDARD TABLE OF bapidlvcreatedheader,
          lt_return             TYPE STANDARD TABLE OF bapiret2,
          ls_sales_order_item   TYPE bapidlvitemcreation,
          ls_created_delivery   TYPE bapidlvcreatedheader,
          ls_return             TYPE bapiret2,
          lv_sales_order_item   TYPE posnr.

    " Get items for delivery creation
    SELECT posnr FROM vbap INTO TABLE @DATA(lt_vbap_items)
      WHERE vbeln = @iv_vbeln.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = 'No items found for sales order.'.
    ENDIF.

    LOOP AT lt_vbap_items INTO DATA(ls_vbap_item).
      CLEAR ls_sales_order_item.
      ls_sales_order_item-ref_doc = iv_vbeln.
      ls_sales_order_item-ref_item = ls_vbap_item-posnr.
      APPEND ls_sales_order_item TO lt_sales_order_items.
    ENDLOOP.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
      TABLES
        sales_order_items     = lt_sales_order_items
        created_deliveries    = lt_created_deliveries
        return                = lt_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = ls_return-message.
    ENDIF.

    READ TABLE lt_created_deliveries INTO ls_created_delivery INDEX 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = 'Delivery creation failed or no delivery created.'.
    ENDIF.

    rv_delivery = ls_created_delivery-delivery.

  ENDMETHOD.

  METHOD create_invoice.
    DATA: lt_billing_data_in  TYPE STANDARD TABLE OF bapibus2017,
          lt_billing_data_out TYPE STANDARD TABLE OF bapibus2018,
          lt_return           TYPE STANDARD TABLE OF bapiret2,
          ls_billing_data_in  TYPE bapibus2017,
          ls_billing_data_out TYPE bapibus2018,
          ls_return           TYPE bapiret2.

    ls_billing_data_in-doc_number = iv_delivery.
    ls_billing_data_in-obj_type = 'DLN'.
    APPEND ls_billing_data_in TO lt_billing_data_in.

    CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
      TABLES
        billingdatain  = lt_billing_data_in
        billingdataout = lt_billing_data_out
        return         = lt_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = ls_return-message.
    ENDIF.

    READ TABLE lt_billing_data_out INTO ls_billing_data_out INDEX 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = 'Invoice creation failed or no invoice created.'.
    ENDIF.

    rv_invoice = ls_billing_data_out-bill_doc.

  ENDMETHOD.

  METHOD display_document_flow.
    DATA: lt_vbfa        TYPE STANDARD TABLE OF vbfa,
          lt_doc_flow    TYPE tt_doc_flow_line,
          ls_doc_flow    TYPE ty_doc_flow_line,
          lo_alv         TYPE REF TO cl_salv_table,
          lx_salv_error  TYPE REF TO cx_salv_error,
          lx_exception   TYPE REF TO lcx_sales_order_exception.

    SELECT * FROM vbfa INTO TABLE @lt_vbfa
      WHERE vbelv = @iv_vbeln OR vbelv = @iv_delivery OR vbelv = @iv_invoice
         OR vbeln = @iv_vbeln OR vbeln = @iv_delivery OR vbeln = @iv_invoice.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_sales_order_exception
        EXPORTING
          message = 'No document flow data found.'.
    ENDIF.

    LOOP AT lt_vbfa INTO DATA(ls_vbfa).
      CLEAR ls_doc_flow.
      ls_doc_flow-from_vbeln = ls_vbfa-vbelv.
      ls_doc_flow-from_objcat = ls_vbfa-vbtypv.
      ls_doc_flow-to_vbeln = ls_vbfa-vbeln.
      ls_doc_flow-to_objcat = ls_vbfa-vbtypl.
      ls_doc_flow-created_on = ls_vbfa-erdat.
      APPEND ls_doc_flow TO lt_doc_flow.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_doc_flow
        ).

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_salv_error.
        RAISE EXCEPTION TYPE lcx_sales_order_exception
          EXPORTING
            message = lx_salv_error->get_text( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA: lo_processor TYPE REF TO lcl_sales_order_processor,
        lx_exception TYPE REF TO lcx_sales_order_exception.

  CREATE OBJECT lo_processor.

  TRY.
      lo_processor->process_sales_order( iv_vbeln = p_vbeln ).
    CATCH lcx_sales_order_exception INTO lx_exception.
      MESSAGE lx_exception->mv_message TYPE 'E'.
  ENDTRY.