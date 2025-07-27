REPORT Gemini25_Flash.

*! @parameter p_vbeln TYPE vbak-vbeln "Sales order number
PARAMETERS: p_vbeln TYPE vbak-vbeln.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: run.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    DATA: lv_vbeln TYPE vbak-vbeln VALUE p_vbeln.
    DATA: ls_vbak TYPE vbak.
    DATA: lt_vbap TYPE STANDARD TABLE OF vbap.
    DATA: lt_bapiret2 TYPE STANDARD TABLE OF bapiret2.
    DATA: lv_pricing_date TYPE vbak-prsdt.
    DATA: lv_creation_date TYPE vbak-erdat.
    DATA: lv_new_pricing_date TYPE vbak-prsdt.
    DATA: lv_delivery_document TYPE likp-vbeln.
    DATA: lv_billing_document TYPE vbrk-vbeln.
    DATA: lt_document_flow TYPE STANDARD TABLE OF sww_cont.

    "1. Read the sales order header, items and current status through a released SAP API.
    CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
      EXPORTING
        salesdocument = lv_vbeln
      IMPORTING
        salesorder_header_out = ls_vbak
      TABLES
        salesorder_item_out = lt_vbap
        return = lt_bapiret2.

    " Helper to convert bapiret2 messages to exceptions
    " TODO-CHECK:CL_BAPI_MESSAGES_TO_EXCEPTION

    IF ls_vbak IS INITIAL.
      MESSAGE 'Sales order not found' TYPE 'E'.
    ENDIF.

    lv_pricing_date = ls_vbak-prsdt.
    lv_creation_date = ls_vbak-erdat.

    "2. If the pricing date of the order is either empty or identical to the creation date,
    "   then set a new pricing date = creation_date + 7 calendar days, save the change and commit.
    IF lv_pricing_date IS INITIAL OR lv_pricing_date = lv_creation_date.
      lv_new_pricing_date = lv_creation_date + 7.

      DATA: ls_order_header_in TYPE bapisdhd1.
      DATA: ls_order_header_inx TYPE bapisdhd1x.

      ls_order_header_in-prsdt = lv_new_pricing_date.
      ls_order_header_inx-updateflag = 'U'.
      ls_order_header_inx-prsdt = 'X'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument = lv_vbeln
          order_header_in = ls_order_header_in
          order_header_inx = ls_order_header_inx
        TABLES
          return = lt_bapiret2.

      " Helper to convert bapiret2 messages to exceptions
      " TODO-CHECK:CL_BAPI_MESSAGES_TO_EXCEPTION

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    "3. Ensure the order is not blocked for delivery.
    "   If blocked, raise a checked exception and stop processing.
    IF ls_vbak-lfsak = 'X'. " Delivery block
      " TODO-CHECK:CX_SALESORDER_BLOCKED_FOR_DELIVERY
      MESSAGE 'Sales order is blocked for delivery' TYPE 'E'.
    ENDIF.

    "4. Create an outbound delivery for the order via a released SAP API and commit.
    DATA: lt_request TYPE STANDARD TABLE OF bapi_deliv_create_from_so_req.
    DATA: ls_request TYPE bapi_deliv_create_from_so_req.
    DATA: lt_created_deliveries TYPE STANDARD TABLE OF bapi_deliv_create_from_so_res.

    ls_request-salesdocument = lv_vbeln.
    APPEND ls_request TO lt_request.

    CALL FUNCTION 'BAPI_DELIVERY_CREATE_FROM_SALESORDER'
      EXPORTING
        sales_orders = lt_request
      IMPORTING
        created_deliveries = lt_created_deliveries
      TABLES
        return = lt_bapiret2.

    " Helper to convert bapiret2 messages to exceptions
    " TODO-CHECK:CL_BAPI_MESSAGES_TO_EXCEPTION

    READ TABLE lt_created_deliveries INTO DATA(ls_created_delivery) INDEX 1.
    IF sy-subrc = 0.
      lv_delivery_document = ls_created_delivery-delivery.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      MESSAGE 'Delivery creation failed' TYPE 'E'.
    ENDIF.

    "5. Create a billing document for that delivery via a released SAP API and commit.
    DATA: lt_billing_request TYPE STANDARD TABLE OF bapi_billdoc_create_from_deliv_req.
    DATA: ls_billing_request TYPE bapi_billdoc_create_from_deliv_req.
    DATA: lt_created_billdocs TYPE STANDARD TABLE OF bapi_billdoc_create_from_deliv_res.

    ls_billing_request-deliverydocument = lv_delivery_document.
    APPEND ls_billing_request TO lt_billing_request.

    CALL FUNCTION 'BAPI_BILLINGDOC_CREATE_FROM_DELIVERY'
      EXPORTING
        delivery_documents = lt_billing_request
      IMPORTING
        created_billdocs = lt_created_billdocs
      TABLES
        return = lt_bapiret2.

    " Helper to convert bapiret2 messages to exceptions
    " TODO-CHECK:CL_BAPI_MESSAGES_TO_EXCEPTION

    READ TABLE lt_created_billdocs INTO DATA(ls_created_billdoc) INDEX 1.
    IF sy-subrc = 0.
      lv_billing_document = ls_created_billdoc-billingdocument.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      MESSAGE 'Billing document creation failed' TYPE 'E'.
    ENDIF.

    "6. Retrieve the complete document flow (order -> delivery -> invoice) via a released SAP API
    "   and display it as an ALV grid with columns: FROM_VBELN | FROM_OBJCAT | TO_VBELN | TO_OBJCAT | CREATED_ON.
    DATA: lt_doc_flow TYPE STANDARD TABLE OF sww_cont.
    DATA: ls_doc_flow TYPE sww_cont.

    CALL FUNCTION 'BAPI_SALESORDER_GETDETAILEDLIST'
      EXPORTING
        i_vbeln = lv_vbeln
      TABLES
        e_document_flow = lt_doc_flow
        return = lt_bapiret2.

    " Helper to convert bapiret2 messages to exceptions
    " TODO-CHECK:CL_BAPI_MESSAGES_TO_EXCEPTION

    DATA: lt_alv_display TYPE STANDARD TABLE OF struct_alv_doc_flow.
    DATA: ls_alv_display TYPE struct_alv_doc_flow.

    TYPES: BEGIN OF struct_alv_doc_flow,
             from_vbeln  TYPE vbeln_vl,
             from_objcat TYPE obj_type,
             to_vbeln    TYPE vbeln_vl,
             to_objcat   TYPE obj_type,
             created_on  TYPE erdat,
           END OF struct_alv_doc_flow.

    LOOP AT lt_doc_flow INTO ls_doc_flow.
      ls_alv_display-from_vbeln = ls_doc_flow-doc_number_a.
      ls_alv_display-from_objcat = ls_doc_flow-obj_type_a.
      ls_alv_display-to_vbeln = ls_doc_flow-doc_number_b.
      ls_alv_display-to_objcat = ls_doc_flow-obj_type_b.
      ls_alv_display-created_on = ls_doc_flow-crea_date.
      APPEND ls_alv_display TO lt_alv_display.
    ENDLOOP.

    " Display ALV grid
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = DATA(lo_alv_table)
      CHANGING
        t_table      = lt_alv_display
    ).

    lo_alv_table->display( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_main=>run().