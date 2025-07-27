*&---------------------------------------------------------------------*
*& Include          ZSD_PRINT_MX_SD_F01
*&---------------------------------------------------------------------*
DATA:
  gt_rng_sorg_v1    TYPE RANGE OF vbak-vkorg,
  gt_rng_sorg_v2    TYPE RANGE OF vbak-vkorg,
  gv_mat_charact    TYPE cabn-atinn,
  gv_mh3_bp_primary TYPE abap_bool. " BASHUROV_PV 16.05.2022 ЗНИ 3100007241

*&---------------------------------------------------------------------*
*& Form PRINT_MX1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM print_mx1 USING pv_doknr TYPE draw-doknr.
  DATA: ls_data TYPE gty_mx_data.

  PERFORM select_mx1 USING    pv_doknr
                     CHANGING ls_data.
  PERFORM show_mx_form USING 'ZSD_TD_3R_MX1_FORM' ls_data.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form PRINT_MX3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM print_mx3 USING pv_doknr TYPE draw-doknr.
  DATA: ls_data TYPE gty_mx_data.

  PERFORM select_mx3 USING    pv_doknr
                     CHANGING ls_data.
  PERFORM show_mx_form USING 'ZSD_TD_3R_MX3_FORM' ls_data.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form SELECT_MX1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LS_DATA  text
*&---------------------------------------------------------------------*
FORM select_mx1 USING    pv_doknr TYPE draw-doknr
                CHANGING ps_data  TYPE gty_mx_data.
  DATA: lv_zzmh1     TYPE lips-zzmh1,
        lv_zzmh1date TYPE lips-zzmh1date,
        lv_salesorg   TYPE vbak-vkorg,
        lv_name      TYPE string,
        lv_inn       TYPE stcd1,
        lv_addr      TYPE string,
        lv_kpp       TYPE stcd3,
        lv_bank_str  TYPE char255,
        lt_del       TYPE zsd_hyb_t_group_deliv_del,
        lv_desc      TYPE char255,
        lt_rng_msehi TYPE RANGE OF msehi,
        lt_ausp      TYPE tt_ausp.

  PERFORM get_stvarv. "*{ [3100004538] 12.03.2021 Сендюрева А.В.

  zcl_sd_hyb_dms_mh1=>get_del_by_dms_number(
    EXPORTING
      iv_documentnumber = pv_doknr
    IMPORTING
      et_del            = lt_del ).

  IF lt_del IS NOT INITIAL.
    SELECT lips~vbeln, lips~posnr,
           lips~werks, lips~lgort, lips~ntgew,
           lips~meins, lips~matnr, lips~vgbel,
           vbap~netpr, vbap~netwr,
           likp~kunnr,
           likp~kunag,
           zzcrtf_nr, zzcrtf_item, traid   "[3100004538] 15.03.2021 Сендюрева А.В.
      FROM lips
      LEFT JOIN vbap
        ON vbap~vbeln = lips~vgbel AND vbap~posnr = lips~vgpos
      LEFT JOIN likp
        ON likp~vbeln = lips~vbeln
      INTO TABLE @DATA(lt_lips)
      FOR ALL ENTRIES IN @lt_del
      WHERE lips~vbeln = @lt_del-vbeln AND
            lips~posnr = @lt_del-posnr.
    IF sy-subrc = 0.
      DATA(lv_werks) = lt_lips[ 1 ]-werks.
      DATA(lv_lgort) = lt_lips[ 1 ]-lgort.
      DATA(lv_kunag) = lt_lips[ 1 ]-kunag.
      DATA(lv_vgbel) = lt_lips[ 1 ]-vgbel.
      DATA(lv_kunnr) = lt_lips[ 1 ]-kunnr.
    ENDIF.
  ENDIF.

  "{ [3100004538] 15.03.2021 Сендюрева А.В.
  PERFORM get_charact USING lt_lips CHANGING lt_ausp.
  "} [3100004538] 15.03.2021 Сендюрева А.В.
*формирование заголовка
  zcl_sd_hyb_dms_mh1=>get_mh_by_dms_number(
    EXPORTING
      iv_documentnumber = pv_doknr
    IMPORTING
      ev_zzmh1          = lv_zzmh1
      ev_zzmh1date      = lv_zzmh1date
      ev_salesorg       = lv_salesorg ).

  IF lv_salesorg IS NOT INITIAL.
    "4
    SELECT SINGLE paval
      FROM t001z
      INTO ps_data-header-okpo_code
      WHERE bukrs = lv_salesorg
        AND party = 'SAPR02'.

    "1
    CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
      EXPORTING
        i_bukrs     = lv_salesorg
        i_no_tel    = abap_true
        i_langu     = sy-langu
        valid_date  = sy-datum
      IMPORTING
        e_name_str  = lv_name
        e_inn       = lv_inn
        e_addr_str  = lv_addr
        e_kpp       = lv_kpp
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      ps_data-header-receiver_data = |{ lv_name }, { lv_addr }, ИНН { lv_inn }, КПП { lv_kpp }|.
      " Банковские данные
      CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_BANK'
        EXPORTING
          i_bukrs     = lv_salesorg
        CHANGING
          c_bank_str  = lv_bank_str
        EXCEPTIONS
          input_error = 1
          OTHERS      = 2.

      IF sy-subrc = 0 AND lv_bank_str IS NOT INITIAL.
        ps_data-header-receiver_data = |{ ps_data-header-receiver_data }, { lv_bank_str }|.
      ENDIF.
    ENDIF.

    "2
    IF lv_werks IS NOT INITIAL AND lv_lgort IS NOT INITIAL.
      SELECT SINGLE adrc~name1
        FROM adrc
        JOIN twlad ON twlad~adrnr = adrc~addrnumber
        INTO @ps_data-header-receiver_data_2
        WHERE twlad~werks = @lv_werks
          AND twlad~lgort = @lv_lgort.
    ENDIF.

    "3
    CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
      EXPORTING
*[5100003176] Grigoryev 22/03/21 <<
        i_kunnr     = lv_kunag
*       i_lifnr     = lv_kunag
*[5100003176] Grigoryev 22/03/21 >>
        i_no_tel    = abap_true
        i_langu     = sy-langu
        valid_date  = sy-datum
      IMPORTING
        e_name_str  = lv_name
        e_inn       = lv_inn
        e_addr_str  = lv_addr
        e_kpp       = lv_kpp
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      " Название, адрес, ИНН, КПП
      ps_data-header-supplier_data = |{ lv_name }, { lv_addr }, ИНН { lv_inn }, КПП { lv_kpp }|.

      " Банковские данные
      CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_BANK'
        EXPORTING
          i_kunnr     = lv_kunag
          i_vbeln_va  = lv_vgbel " [5100003199] 29.03.2021 AFANASYEV_AA5
        CHANGING
          c_bank_str  = lv_bank_str
        EXCEPTIONS
          input_error = 1
          OTHERS      = 2.

      IF sy-subrc = 0 AND lv_bank_str IS NOT INITIAL.
        ps_data-header-supplier_data = |{ ps_data-header-supplier_data }, { lv_bank_str }|.
      ENDIF.
    ENDIF.

    " Получение данных грузополучателя
    " [3100005231] 12.07.2021 SUBBOTIN_AM >>
    CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
      EXPORTING
        i_kunnr     = lv_kunnr
        i_parvw     = 'SH'
        i_no_tel    = abap_true
        i_langu     = sy-langu
        valid_date  = sy-datum
      IMPORTING
        e_name_str  = lv_name
        e_inn       = lv_inn
        e_addr_str  = lv_addr
        e_kpp       = lv_kpp
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      " Название, адрес, ИНН, КПП
      ps_data-header-suppl_data = |{ lv_name }, { lv_addr }, ИНН { lv_inn }, КПП { lv_kpp }|.

      " Банковские данные
      CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_BANK'
        EXPORTING
          i_kunnr     = lv_kunnr
          i_vbeln_va  = lv_vgbel
        CHANGING
          c_bank_str  = lv_bank_str
        EXCEPTIONS
          input_error = 1
          OTHERS      = 2.

      IF sy-subrc = 0 AND lv_bank_str NS ', p/c , к/c , БИК'.
        ps_data-header-suppl_data = |{ ps_data-header-suppl_data }, { lv_bank_str }|.
      ENDIF.
    ENDIF.
    " << Получение данных грузополучателя
    " [3100005231] 12.07.2021 SUBBOTIN_AM


    "8, 9
    ps_data-header-document_number = lv_zzmh1.
    ps_data-header-document_date = lv_zzmh1date.

    "5, 6
    SELECT SINGLE vbak~zzdog_num, draw~zzdat_zak
      FROM vbak
      JOIN draw
        ON vbak~zzdoknr = draw~doknr
      INTO ( @ps_data-header-contract_number, @ps_data-header-contract_date )
      WHERE vbak~vbeln = @lv_vgbel.
  ENDIF.

  "7
  ps_data-header-operation_type = 'ХРАНЕНИЕ'(001).



  DATA:
    lt_rng_sales     TYPE RANGE OF vbak-vkorg.
  DATA lv_old_schema TYPE char1.

  zcl_tvarvc=>get_range( EXPORTING iv_name = 'ZSD_PRINT_MX_SD_VKORG'
                           IMPORTING et_result = lt_rng_sales ).
  zcl_tvarvc=>get_const( EXPORTING iv_name = 'ZSD_SORT_OLD_OH_SCHEMA'
                           IMPORTING ev_result = lv_old_schema ).

  CLEAR ps_data-header-storage_location.
  IF lv_salesorg IN lt_rng_sales[]"5010 или 5020 или 5030 (т.е. сортовые площадки)
     AND lv_old_schema IS NOT INITIAL.
    CLEAR: lv_name,
         lv_inn,
         lv_addr,
         lv_kpp.
    CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
      EXPORTING
*[5100003176] Grigoryev 22/03/21 <<
        i_kunnr     = lv_kunnr
*       i_lifnr     = lv_kunag
        i_parvw     = 'SH'
*[5100003176] Grigoryev 22/03/21 >>
        i_no_tel    = abap_true
        i_langu     = sy-langu
        valid_date  = sy-datum
      IMPORTING
        e_name_str  = lv_name
        e_inn       = lv_inn
        e_addr_str  = lv_addr
        e_kpp       = lv_kpp
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      ps_data-header-storage_location = lv_addr.
    ENDIF.
  ELSE.

    IF lt_del[] IS NOT INITIAL.
      SELECT DISTINCT vbpa~kunnr FROM  vbpa INTO TABLE @DATA(lt_kunnr)
        FOR ALL ENTRIES IN @lt_del
          WHERE vbpa~vbeln = @lt_del-vbeln
          AND parvw = 'SB'.
    ENDIF.

    IF lt_kunnr[] IS NOT INITIAL.
      CLEAR: lv_name,
           lv_inn,
           lv_addr,
           lv_kpp.
      DATA(lv_kunnr_sf) = lt_kunnr[ 1 ]-kunnr.
      CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
        EXPORTING
*[5100003176] Grigoryev 22/03/21 <<
          i_kunnr     = lv_kunnr_sf
*         i_lifnr     = lv_kunag
          i_parvw     = 'SB'
*[5100003176] Grigoryev 22/03/21 >>
          i_no_tel    = abap_true
          i_langu     = sy-langu
          valid_date  = sy-datum
        IMPORTING
          e_name_str  = lv_name
          e_inn       = lv_inn
          e_addr_str  = lv_addr
          e_kpp       = lv_kpp
        EXCEPTIONS
          input_error = 1
          OTHERS      = 2.
      IF sy-subrc = 0.
        ps_data-header-storage_location = lv_addr.
      ENDIF.
    ENDIF.
  ENDIF.

* формирование позиций поставок
  IF lt_lips[] IS NOT INITIAL.
    SELECT * "msehi, mseht
      FROM t006a
      INTO TABLE @DATA(lt_meins)
      FOR ALL ENTRIES IN @lt_lips[]
      WHERE msehi = @lt_lips-meins
        AND spras = @sy-langu.
    SORT lt_meins[] BY msehi mseht.
    DELETE ADJACENT DUPLICATES FROM lt_meins[] COMPARING msehi mseht.

    LOOP AT lt_meins ASSIGNING FIELD-SYMBOL(<ls_meins>).
      INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_meins>-msehi ) INTO TABLE lt_rng_msehi. "это не опечатка, нужно именно это значение
    ENDLOOP.

    IF lt_rng_msehi IS NOT INITIAL.
      SELECT * "uom, okei
        FROM j_3rj_uom_conv
        INTO TABLE @DATA(lt_okei)
        WHERE uom IN @lt_rng_msehi.
      SORT lt_okei[] BY uom.
      DELETE ADJACENT DUPLICATES FROM lt_okei[] COMPARING uom.
    ENDIF.
  ENDIF.

  LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_item>).
    DATA(lv_index) = sy-tabix.
    APPEND INITIAL LINE TO ps_data-items ASSIGNING FIELD-SYMBOL(<ls_pos>).
    <ls_pos>-sequential_number = lv_index.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = <ls_item>-matnr
      IMPORTING
        output = <ls_pos>-material_number.

    CALL FUNCTION 'ZSD_GET_MAT_NAME'
      EXPORTING
        i_vbtyp = 'J'
        i_vbeln = <ls_item>-vbeln
        i_posnr = <ls_item>-posnr
      IMPORTING
        e_matnm = lv_desc.
    <ls_pos>-material_description = lv_desc.

    READ TABLE lt_meins ASSIGNING <ls_meins> WITH KEY msehi = <ls_item>-meins BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_pos>-unit_of_entry_descr = <ls_meins>-mseht.
    ENDIF.

    READ TABLE lt_okei ASSIGNING FIELD-SYMBOL(<ls_okei>) WITH KEY uom = <ls_item>-meins BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_pos>-unit_of_entry = <ls_okei>-okei.
    ENDIF.

    <ls_pos>-quantity_in_unit_of_entry = <ls_item>-ntgew.

    <ls_pos>-price_in_local_curr = <ls_item>-netpr.
    <ls_pos>-amount_in_local_curr = <ls_pos>-price_in_local_curr * <ls_pos>-quantity_in_unit_of_entry.

*{ [3100004538] 15.03.2021 Сендюрева А.В.
    READ TABLE lt_ausp ASSIGNING FIELD-SYMBOL(<ls_ausp>) WITH TABLE KEY objek = <ls_item>-matnr.
    IF sy-subrc = 0.
      <ls_pos>-characteristics = <ls_ausp>-atwrt.
    ELSE.
      <ls_pos>-characteristics = <ls_pos>-material_number.
    ENDIF.
    <ls_pos>-remark = <ls_item>-traid.
    <ls_pos>-material_number = <ls_item>-zzcrtf_nr && '/' && <ls_item>-zzcrtf_item.
*} [3100004538] 15.03.2021 Сендюрева А.В.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_MX1_FORM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LS_DATA  text
*&---------------------------------------------------------------------*
FORM show_mx_form  USING  pv_name  TYPE fpname
                          ps_data  TYPE gty_mx_data.
  DATA: ls_params     TYPE sfpoutputparams,
        ls_message    TYPE bapiret2,
        lv_funcname   TYPE funcname,
        ls_params_pdf TYPE sfpdocparams.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_params
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    PERFORM sy_to_bapiret CHANGING ls_message.
  ELSE.
    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = pv_name
          IMPORTING
            e_funcname = lv_funcname.

        ls_params_pdf-langu = sy-langu.

        CALL FUNCTION lv_funcname
          EXPORTING
            /1bcdwb/docparams = ls_params_pdf
            is_header         = ps_data-header
            it_items          = ps_data-items[].
*            iv_period         = lv_period
*            iv_barcode        = lv_barcode.

        CALL FUNCTION 'FP_JOB_CLOSE'
          EXCEPTIONS
            usage_error    = 1
            system_error   = 2
            internal_error = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
          PERFORM sy_to_bapiret CHANGING ls_message.
        ENDIF.

      CATCH cx_fp_api_repository cx_fp_api_usage cx_fp_api_internal cx_sy_dyn_call_illegal_type INTO DATA(lr_root).
        MESSAGE e000(zmm_td_print_mx) WITH lr_root->get_text( ) INTO DATA(lv_dummy).
        PERFORM sy_to_bapiret CHANGING ls_message.
    ENDTRY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SY_TO_BAPIRET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LS_MESSAGE  text
*&---------------------------------------------------------------------*
FORM sy_to_bapiret  CHANGING ps_message TYPE bapiret2.
  ps_message-type        = sy-msgty.
  ps_message-id          = sy-msgid.
  ps_message-number      = sy-msgno.
  ps_message-message_v1  = sy-msgv1.
  ps_message-message_v2  = sy-msgv2.
  ps_message-message_v3  = sy-msgv3.
  ps_message-message_v4  = sy-msgv4.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_MX3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_PV_DOKNR  text
*      <--P_LS_DATA  text
*&---------------------------------------------------------------------*
FORM select_mx3 USING    pv_doknr TYPE draw-doknr
                CHANGING ps_data  TYPE gty_mx_data.
  DATA: lv_salesorg     TYPE vbak-vkorg,
        lv_name        TYPE string,
        lv_inn         TYPE stcd1,
        lv_addr        TYPE string,
        lv_kpp         TYPE stcd3,
        lv_bank_str    TYPE char255,
        lt_del         TYPE zsd_hyb_t_group_deliv_del,

        lv_consignee1  TYPE kunnr,
        lv_sales_order TYPE vbak-vbeln,
        lv_zzmh3       TYPE zsd_mh3,
        lv_zzmh3date   TYPE datum,
        lv_desc        TYPE char255,
        BEGIN OF ls_key,
          matnr TYPE lips-matnr,
          charg TYPE lips-charg,
        END OF ls_key,
        lt_key LIKE STANDARD TABLE OF ls_key,
        BEGIN OF ls_mdoc,
          matnr TYPE matdoc-ummat,
          charg TYPE matdoc-charg,
          xblnr TYPE lips-vbeln,
        END OF ls_mdoc,
        lt_mdoc LIKE SORTED TABLE OF ls_mdoc WITH NON-UNIQUE KEY matnr charg,
        lt_ausp TYPE tt_ausp,
        lt_sales_orders TYPE zsd_hyb_dms_vbeln_t. "3100010402 Коробейников А. Н. 16.10.2023

  PERFORM get_stvarv. "*{ [3100004538] 12.03.2021 Сендюрева А.В.

  zcl_sd_hyb_dms_mh3=>get_mh_dat_by_dms_num(
    EXPORTING
      iv_documentnumber = pv_doknr
    IMPORTING
      ev_consignee1     = lv_consignee1
      ev_sales_order    = lv_sales_order
      ev_zzmh3          = lv_zzmh3
      ev_zzmh3date      = lv_zzmh3date
      ev_salesorg       = lv_salesorg
      et_sales_orders   = lt_sales_orders ).  "3100010402 Коробейников А. Н. 16.10.2023

  IF lv_sales_order IS NOT INITIAL.

    SELECT lips~werks,      lips~lgort,
           lips~meins,      lips~matnr,
           vbap~netpr,      vbap~netwr,
           vbak~vkorg,      vbak~kunnr,
           vbak~zzdog_num,  draw~zzdat_zak
      FROM vbak
      LEFT JOIN draw
        ON vbak~zzdoknr = draw~doknr
      LEFT JOIN lips
        ON lips~vbeln = vbak~vbeln
      LEFT JOIN vbap
        ON vbap~vbeln = lips~vgbel AND vbap~posnr = lips~vgpos
      INTO TABLE @DATA(lt_lips)
      WHERE vbak~vbeln = @lv_sales_order.
    IF sy-subrc = 0.
      DATA(lv_werks) = lt_lips[ 1 ]-werks.
      DATA(lv_lgort) = lt_lips[ 1 ]-lgort.
      lv_salesorg = lt_lips[ 1 ]-vkorg.
      DATA(lv_kunnr) = lt_lips[ 1 ]-kunnr.

      ps_data-header-contract_number = lt_lips[ 1 ]-zzdog_num.
      ps_data-header-contract_date = lt_lips[ 1 ]-zzdat_zak .
    ENDIF.
  ENDIF.

*формирование заголовка
  SELECT SINGLE * FROM draw
    INTO @DATA(ls_draw)
    WHERE doknr = @pv_doknr AND dokar = 'ZM3'.

  IF lv_salesorg IS NOT INITIAL.
    "1
    CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
      EXPORTING
        i_bukrs     = lv_salesorg
        i_no_tel    = abap_true
        i_langu     = sy-langu
        valid_date  = sy-datum
      IMPORTING
        e_name_str  = lv_name
        e_inn       = lv_inn
        e_addr_str  = lv_addr
        e_kpp       = lv_kpp
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      ps_data-header-receiver_data = |{ lv_name }, { lv_addr }, ИНН { lv_inn }, КПП { lv_kpp }|.
      " Банковские данные
      CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_BANK'
        EXPORTING
          i_bukrs     = lv_salesorg
        CHANGING
          c_bank_str  = lv_bank_str
        EXCEPTIONS
          input_error = 1
          OTHERS      = 2.
      IF sy-subrc = 0 AND lv_bank_str IS NOT INITIAL.
        ps_data-header-receiver_data = |{ ps_data-header-receiver_data }, { lv_bank_str }|.
      ENDIF.
    ENDIF.

    "4
    SELECT SINGLE paval
      FROM t001z
      INTO ps_data-header-okpo_code
      WHERE bukrs = lv_salesorg
        AND party = 'SAPR02'.
  ENDIF.

  "2
  SELECT SINGLE adrc~name1
    FROM adrc
    JOIN twlad ON twlad~adrnr = adrc~addrnumber
    INTO @ps_data-header-receiver_data_2
    WHERE twlad~werks = @lv_werks
      AND twlad~lgort = @lv_lgort
      AND twlad~lfdnr = '001'.

*{ BASHUROV_PV 16.05.2022 ЗНИ 3100007241
  PERFORM get_details_of_customer USING lv_kunnr lv_sales_order
                                  CHANGING ps_data-header-supplier_data
                                           ps_data-header-suppl_data.

*} BASHUROV_PV 16.05.2022 ЗНИ 3100007241

  ps_data-header-document_number = lv_zzmh3.
  ps_data-header-document_date = lv_zzmh3date.

  "7
  ps_data-header-operation_type = 'ХРАНЕНИЕ'(001).

* формирование позиций поставок
  IF lv_sales_order IS NOT INITIAL.
    IF lt_sales_orders IS INITIAL.  "3100010402 Коробейников А. Н. 16.10.2023
      SELECT vbeln, posnr, meins, matnr, kwmeng, charg, netpr
        FROM vbap
        INTO TABLE @DATA(lt_vbap)
        WHERE vbeln = @lv_sales_order.
*{ 3100010402 Коробейников А. Н. 16.10.2023
    ELSE.
      SORT lt_sales_orders BY vbeln.
      DELETE ADJACENT DUPLICATES FROM lt_sales_orders COMPARING vbeln.

      SELECT vbeln, posnr, meins, matnr, kwmeng, charg, netpr
        FROM vbap
        INTO TABLE @lt_vbap
        FOR ALL ENTRIES IN @lt_sales_orders
        WHERE vbeln = @lt_sales_orders-vbeln.
    ENDIF.
*} 3100010402 Коробейников А. Н. 16.10.2023

    IF lt_vbap IS NOT INITIAL.
      SELECT msehi, mseht
        FROM t006a
        INTO TABLE @DATA(lt_meins)
        FOR ALL ENTRIES IN @lt_vbap
        WHERE msehi = @lt_vbap-meins
          AND spras = @sy-langu.
      SORT lt_meins[] BY msehi.
      DELETE ADJACENT DUPLICATES FROM lt_meins[].

      SELECT uom, okei
        FROM j_3rj_uom_conv
        INTO TABLE @DATA(lt_okei)
        FOR ALL ENTRIES IN @lt_vbap
        WHERE uom = @lt_vbap-meins.
      SORT lt_okei[] BY uom.
      DELETE ADJACENT DUPLICATES FROM lt_okei[].
    ENDIF.
*{ [3100004538] 12.03.2021 Сендюрева А.В.
    IF lv_salesorg IN gt_rng_sorg_v1 OR
       lv_salesorg IN gt_rng_sorg_v2.
      LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<ls_vbap>).
        ls_key-matnr = <ls_vbap>-matnr.
        ls_key-charg = <ls_vbap>-charg.
        COLLECT ls_key INTO lt_key.
      ENDLOOP.
    ENDIF.
    IF lv_salesorg IN gt_rng_sorg_v1.
      IF lt_key IS NOT INITIAL.
        SELECT lips~matnr, lips~charg, zzcrtf_nr, zzcrtf_item, traid, vbap~netpr,
           lips~vgbel "Terletskaya_ay 08.12.21 [3100006151, изменение вида договора в актах мх3 на Х]
          INTO TABLE @DATA(lt_tab1)
          FROM lips INNER JOIN likp ON lips~vbeln = likp~vbeln
                    INNER JOIN vbap ON vbap~vbeln = lips~vgbel AND
                                       vbap~posnr = lips~vgpos
          FOR ALL ENTRIES IN @lt_key
          WHERE lips~matnr = @lt_key-matnr
            AND lips~charg = @lt_key-charg
            AND zzmh1 <> @space.
        SORT lt_tab1 BY matnr charg.
* << Terletskaya_ay 08.12.21 [3100006151, изменение вида договора в актах мх3 на Х]
        IF lt_tab1 IS NOT INITIAL.  "3100010402 Коробейников А. Н. 16.10.2023 - проверка АТС
          SELECT zzdog_num, zzbegin, vbeln
                ,kunnr                        " BASHUROV_PV 16.05.2022 ЗНИ 3100007241
            FROM vbak
           INTO TABLE @DATA(lt_tab_vbak)
            FOR ALL ENTRIES IN @lt_tab1
            WHERE vbeln = @lt_tab1-vgbel.
        ENDIF.                      "3100010402 Коробейников А. Н. 16.10.2023 - проверка АТС

        READ TABLE lt_tab_vbak ASSIGNING FIELD-SYMBOL(<lt_tab_vbak>) INDEX 1.
        IF sy-subrc = 0.
          ps_data-header-contract_number = <lt_tab_vbak>-zzdog_num.
          ps_data-header-contract_date = <lt_tab_vbak>-zzbegin.

*{ BASHUROV_PV 16.05.2022 ЗНИ 3100007241
          IF gv_mh3_bp_primary IS NOT INITIAL.
            PERFORM get_details_of_customer USING <lt_tab_vbak>-kunnr <lt_tab_vbak>-vbeln
                                            CHANGING ps_data-header-supplier_data
                                                     ps_data-header-suppl_data.
          ENDIF.
*} BASHUROV_PV 16.05.2022 ЗНИ 3100007241

        ENDIF.
* >> Terletskaya_ay 08.12.21 [3100006151, изменение вида договора в актах мх3 на Х]
      ENDIF.
    ELSEIF lv_salesorg IN gt_rng_sorg_v2.
      IF lt_key IS NOT INITIAL.
        SELECT ummat AS matnr, charg, xblnr
          INTO TABLE @lt_mdoc
          FROM matdoc
          FOR ALL ENTRIES IN @lt_key
          WHERE ummat = @lt_key-matnr
            AND charg = @lt_key-charg.

        IF lt_mdoc IS NOT INITIAL.
          SELECT lips~vbeln, lips~matnr, zzcrtf_nr, zzcrtf_item, traid, vbap~netpr,
                 vbak~zzdog_num, vbak~zzbegin
            INTO TABLE @DATA(lt_tab2)
            FROM  lips INNER JOIN likp ON lips~vbeln = likp~vbeln
                    INNER JOIN vbap ON vbap~vbeln = lips~vgbel AND
                                       vbap~posnr = lips~vgpos
                    INNER JOIN vbak ON vbak~vbeln = lips~vgbel "  LAISHTSEV_SA 09.02.2022 S 3100006556: изменение номера договора в актах мх3 на
          FOR ALL ENTRIES IN @lt_mdoc
          WHERE lips~vbeln = @lt_mdoc-xblnr
            AND lips~matnr = @lt_mdoc-matnr
             AND zzmh1 <> @space.
          SORT lt_tab2 BY vbeln matnr.
        ENDIF.
      ENDIF.
    ENDIF.
    PERFORM get_charact USING lt_vbap CHANGING lt_ausp.

*} [3100004538] 12.03.2021 Сендюрева А.В.
    LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<ls_item>).
      DATA(lv_index) = sy-tabix.
      APPEND INITIAL LINE TO ps_data-items ASSIGNING FIELD-SYMBOL(<ls_pos>).
      <ls_pos>-sequential_number = lv_index.

      CALL FUNCTION 'ZSD_GET_MAT_NAME'
        EXPORTING
          i_vbtyp = 'C' " 5100002685 Lapygin_VA 17.11.20
          i_vbeln = <ls_item>-vbeln
          i_posnr = <ls_item>-posnr
        IMPORTING
          e_matnm = lv_desc.
      <ls_pos>-material_description = lv_desc.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = <ls_item>-matnr
        IMPORTING
          output = <ls_pos>-material_number.

* << LAISHTSEV_SA 21.04.2021 S 5100003279: Коррект. формы МХ-3, вывод единиц измер.
      READ TABLE lt_meins ASSIGNING FIELD-SYMBOL(<ls_meins>) WITH KEY msehi = <ls_item>-meins BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_pos>-unit_of_entry_descr = <ls_meins>-mseht.
      ENDIF.
* >> LAISHTSEV_SA 21.04.2021 S 5100003279: Коррект. формы МХ-3, вывод единиц измер.

      READ TABLE lt_okei ASSIGNING FIELD-SYMBOL(<ls_okei>) WITH KEY uom = <ls_item>-meins BINARY SEARCH.
      IF sy-subrc = 0.
*        <ls_pos>-unit_of_entry_descr = <ls_okei>-okei. " LAISHTSEV_SA 21.04.2021 S 5100003279: Коррект. формы МХ-3, вывод единиц измер.
        <ls_pos>-unit_of_entry = <ls_okei>-okei.        " LAISHTSEV_SA 21.04.2021 S 5100003279: Коррект. формы МХ-3, вывод единиц измер.
      ENDIF.

      <ls_pos>-quantity_in_unit_of_entry = <ls_item>-kwmeng.
*      <ls_pos>-unit_of_entry = <ls_item>-meins. " LAISHTSEV_SA 21.04.2021 S 5100003279: Коррект. формы МХ-3, вывод единиц измер.

      <ls_pos>-price_in_local_curr = <ls_item>-netpr.
*{ [3100004538] 12.03.2021 Сендюрева А.В.
*     <ls_pos>-amount_in_local_curr = <ls_item>-netwr.

      READ TABLE lt_ausp ASSIGNING FIELD-SYMBOL(<ls_ausp>) WITH TABLE KEY objek = <ls_item>-matnr.
      IF sy-subrc = 0.
        <ls_pos>-characteristics = <ls_ausp>-atwrt.
      ELSE.
        <ls_pos>-characteristics = <ls_pos>-material_number.
      ENDIF.

      IF lv_salesorg IN gt_rng_sorg_v1.
        READ TABLE lt_tab1 ASSIGNING FIELD-SYMBOL(<ls_tab1>)
                                  WITH KEY matnr = <ls_item>-matnr
                                           charg = <ls_item>-charg
                                           BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_pos>-price_in_local_curr = <ls_tab1>-netpr.
          <ls_pos>-remark = <ls_tab1>-traid.
          <ls_pos>-material_number = <ls_tab1>-zzcrtf_nr && '/' && <ls_tab1>-zzcrtf_item.
        ENDIF.
      ELSEIF lv_salesorg IN gt_rng_sorg_v2.
        LOOP AT lt_mdoc ASSIGNING FIELD-SYMBOL(<ls_mdoc>) WHERE matnr = <ls_item>-matnr
                                                            AND charg = <ls_item>-charg.
          READ TABLE lt_tab2 ASSIGNING FIELD-SYMBOL(<ls_tab2>)
                                                         WITH KEY vbeln = <ls_mdoc>-xblnr
                                                                  matnr = <ls_mdoc>-matnr
                                                                  BINARY SEARCH.
          IF sy-subrc = 0.
            <ls_pos>-price_in_local_curr = <ls_tab2>-netpr.
            <ls_pos>-remark = <ls_tab2>-traid.
            <ls_pos>-material_number = <ls_tab2>-zzcrtf_nr && '/' && <ls_tab2>-zzcrtf_item.

*            << LAISHTSEV_SA 09.02.2022 S 3100006556: изменение номера договора в актах мх3 на
            ps_data-header-contract_number = <ls_tab2>-zzdog_num.
            ps_data-header-contract_date = <ls_tab2>-zzbegin.
*            >> LAISHTSEV_SA 09.02.2022 S 3100006556: изменение номера договора в актах мх3 на
          ENDIF.
        ENDLOOP.
      ENDIF.
      <ls_pos>-amount_in_local_curr = <ls_pos>-price_in_local_curr *  <ls_pos>-quantity_in_unit_of_entry.

*} [3100004538] 12.03.2021 Сендюрева А.В.
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM get_stvarv.

  SELECT sign opti low high
    INTO TABLE gt_rng_sorg_v1
    FROM tvarvc
    WHERE name = 'ZSD_PRINT_MX_SD_SALESORG_V1'.

  SELECT sign opti low high
    INTO TABLE gt_rng_sorg_v2
    FROM tvarvc
    WHERE name = 'ZSD_PRINT_MX_SD_SALESORG_V2'.

  SELECT SINGLE atinn
    INTO gv_mat_charact
    FROM cabn
    WHERE atnam = 'STNDRT_PROD'.

*{ BASHUROV_PV 16.05.2022 ЗНИ 3100007241
  SELECT SINGLE low
  INTO gv_mh3_bp_primary
  FROM tvarvc
  WHERE name = 'ZSD_HYBRIS_MH3_BP_PRIMARY'
    AND type = 'P'.
*} BASHUROV_PV 16.05.2022 ЗНИ 3100007241

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHARACT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LT_AUSP  text
*&---------------------------------------------------------------------*
FORM get_charact USING pt_tab TYPE INDEX TABLE
                 CHANGING ct_ausp TYPE tt_ausp.
  DATA: lt_matnr TYPE STANDARD TABLE OF ausp-objek.

  LOOP AT pt_tab ASSIGNING FIELD-SYMBOL(<ls_tab>).
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_matnr>).
    CHECK sy-subrc = 0.
    APPEND <lv_matnr> TO lt_matnr.
  ENDLOOP.
  SORT lt_matnr BY table_line.
  DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING ALL FIELDS.
  IF lt_matnr IS NOT INITIAL.
    SELECT objek, atwrt
      INTO TABLE @ct_ausp
      FROM ausp
      FOR ALL ENTRIES IN @lt_matnr
      WHERE objek = @lt_matnr-table_line
        AND atinn = @gv_mat_charact.
  ENDIF.
ENDFORM.

*{ BASHUROV_PV 16.05.2022 ЗНИ 3100007241
*&---------------------------------------------------------------------*
*& Form DETAILS_OF_CUSTOMER
*&---------------------------------------------------------------------*
FORM get_details_of_customer USING uv_kunnr       TYPE vbak-kunnr
                                   uv_sales_order TYPE vbak-vbeln
                             CHANGING cv_supplier_data TYPE gty_mx_data-header-supplier_data
                                      cv_suppl_data    TYPE gty_mx_data-header-suppl_data.

  DATA:
    lv_name     TYPE string,
    lv_inn      TYPE stcd1,
    lv_addr     TYPE string,
    lv_kpp      TYPE stcd3,
    lv_bank_str TYPE char255.

  "3
  CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
    EXPORTING
*[5100003176] Grigoryev 22/03/21 <<
      i_kunnr     = uv_kunnr
*     i_lifnr     = uv_kunnr
*[5100003176] Grigoryev 22/03/21 >>
      i_no_tel    = abap_true
      i_langu     = sy-langu
      valid_date  = sy-datum
    IMPORTING
      e_name_str  = lv_name
      e_inn       = lv_inn
      e_addr_str  = lv_addr
      e_kpp       = lv_kpp
    EXCEPTIONS
      input_error = 1
      OTHERS      = 2.
  IF sy-subrc = 0.

    " Название, адрес, ИНН, КПП
    cv_supplier_data = |{ lv_name }, { lv_addr }, ИНН { lv_inn }, КПП { lv_kpp }|.

    " Банковские данные
    CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_BANK'
      EXPORTING
* >>>  [5100003199] 29.03.2021 AFANASYEV_AA5
        i_kunnr     = uv_kunnr
*       i_kunnr     = ls_draw-zzkunnr
        i_vbeln_va  = uv_sales_order
* <<<  [5100003199] 29.03.2021 AFANASYEV_AA5
      CHANGING
        c_bank_str  = lv_bank_str
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.

    IF sy-subrc = 0 AND lv_bank_str IS NOT INITIAL.
      cv_supplier_data = |{ cv_supplier_data }, { lv_bank_str }|.
    ENDIF.
  ENDIF.

  " Получение данных грузополучателя
  " [3100005231] 12.07.2021 SUBBOTIN_AM >>
  SELECT SINGLE kunnr
    FROM vbpa
    INTO @DATA(lv_kunnr_gp)
    WHERE vbeln = @uv_sales_order
      AND posnr = ''
      AND parvw = 'WE'.

  CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_NAME_ADR'
    EXPORTING
      i_kunnr     = lv_kunnr_gp
      i_parvw     = 'SH'
      i_no_tel    = abap_true
      i_langu     = sy-langu
      valid_date  = sy-datum
    IMPORTING
      e_name_str  = lv_name
      e_inn       = lv_inn
      e_addr_str  = lv_addr
      e_kpp       = lv_kpp
    EXCEPTIONS
      input_error = 1
      OTHERS      = 2.
  IF sy-subrc = 0.
    " Название, адрес, ИНН, КПП
    cv_suppl_data = |{ lv_name }, { lv_addr }, ИНН { lv_inn }, КПП { lv_kpp }|.

    " Банковские данные
    CALL FUNCTION 'Z_UNI_SD_GET_KUNNR_BANK'
      EXPORTING
        i_kunnr     = lv_kunnr_gp
        i_vbeln_va  = uv_sales_order
      CHANGING
        c_bank_str  = lv_bank_str
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.

    IF sy-subrc = 0 AND lv_bank_str NS ', p/c , к/c , БИК'.
      cv_suppl_data = |{ cv_suppl_data }, { lv_bank_str }|.
    ENDIF.
  ENDIF.
  " << Получение данных грузополучателя
  " [3100005231] 12.07.2021 SUBBOTIN_AM

ENDFORM.
*} BASHUROV_PV 16.05.2022 ЗНИ 3100007241
