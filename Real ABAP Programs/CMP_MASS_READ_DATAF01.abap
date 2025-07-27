*----------------------------------------------------------------------*
***INCLUDE CMP_MASS_READ_DATAF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data .
  DATA: it_vbrk  TYPE TABLE OF vbrk,
        wa_vbrk  TYPE vbrk,
        it_vbrp  TYPE TABLE OF vbrp,
        wa_vbrp  TYPE vbrp,
        wa_konv  TYPE konv,
        l_kschl  TYPE cmp_mapmasskschl-kschl,
        wa_vbfa  TYPE vbfa,
        currdec  TYPE tcurx-currdec,
        exponent TYPE int2,
        l_kon    TYPE cmp_mapmasskschl-cmp_kond_kuer.
  DATA: it_nametab TYPE TABLE OF x031l,
        wa_nametab TYPE x031l.

  FIELD-SYMBOLS: <l_fieldname> TYPE any,
                 <l_value>     TYPE any,
                 <l_wa>        TYPE cmp_cust_fields_s.

  DATA: l_add_string(50) TYPE c.
  DATA: std_sel_krit  TYPE TABLE OF string,
        cust_sel_krit TYPE TABLE OF string,
        sel_krit      TYPE TABLE OF string,
        l_wert        TYPE string,
        l_wert_str    TYPE string.
  DATA: l_waers       TYPE waers.      "note 1569229

  CLEAR gwa_result_tab.
  CLEAR gt_result_tab[].
  CLEAR: it_vbrp, it_vbrk.
  CLEAR gs_struc_fb_mass.

  PERFORM read_complaint_reason .


* ist eine Referenz zum kundeneigenen Selektionscreen vorhanden?
  IF g_ref_exit IS NOT INITIAL.
    gs_struc_fb_mass-cmp_price = p_pr_neu.

* alle kundeneigenen Feldnamen einlesen
    CALL FUNCTION 'DD_GET_NAMETAB'
      EXPORTING
*       STATUS    = 'A'
        tabname   = 'CMP_CUST_FIELDS_S'
*       GET_ALL   = ' '
* IMPORTING
*       F_STATUS  =
*       R_MODEFLAG       =
*       R_STATUS  =
*       X030L_WA  =
      TABLES
        x031l_tab = it_nametab
      EXCEPTIONS
        not_found = 1
        no_fields = 2
        OTHERS    = 3.
    IF sy-subrc <> 0.
      MESSAGE s160(cmp_new).
    ENDIF.



    ASSIGN gs_cmp_cust_fields TO <l_wa>.

    LOOP AT it_nametab INTO wa_nametab.
      ASSIGN wa_nametab-fieldname TO <l_fieldname>.

      ASSIGN COMPONENT sy-tabix OF STRUCTURE <l_wa> TO <l_value>.

      IF <l_value> IS NOT INITIAL.
        l_wert = <l_value>.
        CONDENSE l_wert.
        IF l_wert NE ''.
          CONCATENATE '''' l_wert '''' INTO l_wert_str.
          CONDENSE l_wert_str.
          CONCATENATE 'and' <l_fieldname> '=' l_wert_str INTO l_add_string SEPARATED BY space.
          APPEND l_add_string TO cust_sel_krit.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

* Standard-Selektionskriterien
  APPEND 'vbeln IN r_vbeln AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'fkdat IN r_fkdat AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'kunag IN r_kunag AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'kunrg IN r_kunrg AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'fkart IN r_fkart AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'vkorg IN r_vkorg AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'rfbsk = ''C''    AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'fksto = ''''     AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'vbtyp NE IF_SD_DOC_CATEGORY=>RESERV_LITTLE_N   AND' TO std_sel_krit.          "#EC NOTEXT )
  APPEND 'vbtyp NE IF_SD_DOC_CATEGORY=>LOAD_CONF_REPOSTING_IS_OIL     ' TO std_sel_krit.          "#EC NOTEXT )

  APPEND LINES OF std_sel_krit[] TO sel_krit.

* kundeneigene Selektionskriterien dazu kopieren
  IF cust_sel_krit[] IS NOT INITIAL.
    APPEND LINES OF cust_sel_krit[] TO sel_krit.
  ENDIF.


  SELECT * FROM vbrk
     INTO TABLE it_vbrk
     WHERE (sel_krit).

  IF NOT it_vbrk IS INITIAL.
    SORT it_vbrk BY vbeln.
  ELSE.
* keine Fakturen ==> keine weitere Verarbeitung
    RETURN.
  ENDIF.


****************************************************************************
****************************************************************************
****************************************************************************
  IF mytabstrip-activetab = 'TAB_KUN' .
* Änderung Kundennummer
    gs_struc_fb_mass-compreas = p_rekla.
    gs_struc_fb_mass-cmp_kunag = p_kunag.

    LOOP AT it_vbrk INTO wa_vbrk.
      gwa_result_tab-vbeln = wa_vbrk-vbeln.
      gwa_result_tab-fkart = wa_vbrk-fkart.
      gwa_result_tab-fkdat = wa_vbrk-fkdat.
      gwa_result_tab-netwr = wa_vbrk-netwr.
      gwa_result_tab-waerk = wa_vbrk-waerk.
      gwa_result_tab-kunag = wa_vbrk-kunag.
      gwa_result_tab-kunrg = wa_vbrk-kunrg.
      gwa_result_tab-light = 3. " Vorbelegung Exception = GRÜN

      APPEND gwa_result_tab TO gt_result_tab.
    ENDLOOP.

    SORT gt_result_tab BY vbeln.
  ENDIF. " op_kun = 'x'


****************************************************************************
****************************************************************************
****************************************************************************
  IF mytabstrip-activetab = 'TAB_ME'.
* Änderung Mengeneinheit
    gs_struc_fb_mass-compreas = p_rekla.
    gs_struc_fb_mass-cmp_vrkme = p_me_neu.
    gs_struc_fb_mass-cmp_matnr = p_matnr.

* Alle Positionen mit gewünschter Materialnummer einlesen
    IF NOT it_vbrk IS INITIAL.
      SELECT vbeln posnr fkimg vrkme matnr aktnr werks FROM vbrp
             INTO CORRESPONDING FIELDS OF TABLE it_vbrp
*            WHERE matnr = p_matnr.                     "#EC CI_NOFIELD
             FOR ALL ENTRIES IN it_vbrk              "note 1113831
             WHERE vbeln = it_vbrk-vbeln             "note 1113831
             AND   matnr = p_matnr.
    ENDIF.

* die Artikel den Fakturen zuordnen
    IF NOT it_vbrp IS INITIAL.
      LOOP AT it_vbrp INTO wa_vbrp.
        CLEAR: gwa_result_tab.
* nur die am Selektionsbildschirm als 'alte Mengeneinheit' eingegebene Einheit
* interessiert an dieser Stelle
        IF NOT p_me_alt IS INITIAL AND p_me_alt EQ wa_vbrp-vrkme.
          READ TABLE it_vbrk INTO wa_vbrk WITH KEY
              vbeln = wa_vbrp-vbeln BINARY SEARCH.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          MOVE-CORRESPONDING wa_vbrk TO gwa_result_tab.     "#EC ENHOK
          MOVE wa_vbrp-posnr TO gwa_result_tab-posnr.
          MOVE wa_vbrp-matnr TO gwa_result_tab-matnr.
          MOVE wa_vbrp-fkimg TO gwa_result_tab-fkimg.
          MOVE wa_vbrp-vrkme TO gwa_result_tab-vrkme.
          gwa_result_tab-light = 3. " Vorbelegung Exception = GRÜN

          APPEND gwa_result_tab TO gt_result_tab.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


****************************************************************************
****************************************************************************
****************************************************************************
  IF mytabstrip-activetab = 'TAB_PREIS'.
* Änderung  Preis
    gs_struc_fb_mass-compreas = p_rekla.
    gs_struc_fb_mass-cmp_matnr =  p_matnr.

* Alle Positionen mit gewünschter Materialnummer einlesen
    IF NOT it_vbrk IS INITIAL.
      SELECT vbeln posnr fkimg vrkme matnr aktnr prsdt werks FROM vbrp
             INTO CORRESPONDING FIELDS OF TABLE it_vbrp
*            WHERE vbeln IN r_vbeln AND              "note 1113831
*                  matnr = p_matnr.                     "#EC CI_NOFIELD
             FOR ALL ENTRIES IN it_vbrk              "note 1113831
             WHERE vbeln = it_vbrk-vbeln             "note 1113831
             AND   matnr = p_matnr.
    ENDIF.

* Nachkommastellen anpassen
* z.B. hat EUR4 4 Dezimalstellen, EUR nur 2
    IF number = 2300.              "note 1569229
      IF p_waeh NE '%'.
        SELECT SINGLE currdec FROM tcurx INTO currdec
               WHERE currkey = p_waeh.
        IF sy-subrc = 0.
          exponent = currdec.
          g_pr_alt = ( p_pr_alt * ( 10 ** ( exponent - 2 ) ) ).
          g_pr_neu = ( p_pr_neu * ( 10 ** ( exponent - 2 ) ) ).
        ELSE.
          g_pr_alt = p_pr_alt .
          g_pr_neu = p_pr_neu .
        ENDIF.
      ELSE.
        g_pr_alt = p_pr_alt * 10.
        g_pr_neu = p_pr_neu * 10.
      ENDIF.
*  note 1569229 {
    ELSE.
      g_pr_alt = p_pr_alt.
      g_pr_neu = p_pr_neu.
    ENDIF.
    IF p_waeh = '%'.
      CLEAR l_waers.
    ELSE.
      l_waers = p_waeh.
    ENDIF.
*  note 1569229 }

    gs_struc_fb_mass-cmp_price = g_pr_neu.

* die Artikel den Fakturen zuordnen
    IF NOT it_vbrp IS INITIAL.
      LOOP AT it_vbrp INTO wa_vbrp.
        CLEAR: gwa_result_tab.

* nur die am Selektionsbildschirm eingegebene Konditionsart und der alte Preis
* interessieren an dieser Stelle
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY
            vbeln = wa_vbrp-vbeln BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        MOVE-CORRESPONDING wa_vbrk TO gwa_result_tab.       "#EC ENHOK
        MOVE wa_vbrp-posnr TO gwa_result_tab-posnr.
        MOVE wa_vbrp-matnr TO gwa_result_tab-matnr.
        MOVE wa_vbrp-fkimg TO gwa_result_tab-fkimg.
        MOVE wa_vbrp-vrkme TO gwa_result_tab-vrkme.
* Zusatz für die Änderung des Preises
* das kundeneigene Konditionskürzel steht für welche Kondition?
        l_kon = p_kon.
        PERFORM check_kondart USING l_kon
                              CHANGING l_kschl.
        gs_struc_fb_mass-cmp_kschl = l_kschl.

        IF l_kschl IS NOT INITIAL.
          SELECT SINGLE * FROM v_konv_cds INTO wa_konv WHERE      "#EC * )
                 knumv EQ wa_vbrk-knumv AND
                 kposn EQ wa_vbrp-posnr AND
                 kschl EQ l_kschl AND
                 waers EQ l_waers AND            "note 1569229
                 kbetr EQ g_pr_alt AND
                 kdatu EQ wa_vbrp-prsdt.

          IF sy-subrc = 0.
            IF p_waeh = '%'.
              wa_konv-kbetr = wa_konv-kbetr / 10.
            ENDIF.
            MOVE-CORRESPONDING wa_konv TO gwa_result_tab.   "#EC ENHOK
            MOVE wa_vbrk-waerk TO gwa_result_tab-waerk.
* % oder Währung
            MOVE p_waeh TO gwa_result_tab-waers.
            gwa_result_tab-light = 3. " Vorbelegung Exception = GRÜN
            APPEND gwa_result_tab TO gt_result_tab.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


****************************************************************************
****************************************************************************
****************************************************************************
  IF mytabstrip-activetab = 'TAB_RET'.
* Retoure
    gs_struc_fb_mass-compreas = p_rekla.
    gs_struc_fb_mass-cmp_matnr =  p_matnr.

* Alle Positionen mit gewünschter Materialnummer einlesen
    IF NOT it_vbrk IS INITIAL.
      SELECT vbeln posnr fkimg vrkme matnr aktnr werks FROM vbrp
             INTO CORRESPONDING FIELDS OF TABLE it_vbrp
*            WHERE matnr = p_matnr.                     "#EC CI_NOFIELD
             FOR ALL ENTRIES IN it_vbrk              "note 1113831
             WHERE vbeln = it_vbrk-vbeln             "note 1113831
             AND   matnr = p_matnr.
    ENDIF.

* die Artikel den Fakturen zuordnen
    IF NOT it_vbrp IS INITIAL.
      LOOP AT it_vbrp INTO wa_vbrp.
        CLEAR: gwa_result_tab.

* nur der am Selektionsbildschirm eingegebene Artikel
* interessiert an dieser Stelle
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY
            vbeln = wa_vbrp-vbeln BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        MOVE-CORRESPONDING wa_vbrk TO gwa_result_tab.       "#EC ENHOK
        MOVE wa_vbrp-posnr TO gwa_result_tab-posnr.
        MOVE wa_vbrp-matnr TO gwa_result_tab-matnr.
        MOVE wa_vbrp-fkimg TO gwa_result_tab-fkimg.
        MOVE wa_vbrp-vrkme TO gwa_result_tab-vrkme.
        MOVE wa_vbrk-waerk TO gwa_result_tab-waerk.
        gwa_result_tab-light = 3. " Vorbelegung Exception = GRÜN
        APPEND gwa_result_tab TO gt_result_tab.

      ENDLOOP.
    ENDIF.
  ENDIF.


****************************************************************************
****************************************************************************
****************************************************************************
* überprüfen, ob schon Reklamationen zu Positionen vorhanden sind
* determine whether there are already complaint documents
  LOOP AT gt_result_tab INTO gwa_result_tab.
* bei Änderung Kunde gibt es keine Positionen
    IF gwa_result_tab-posnr IS INITIAL.
* Folgebelege lesen, d.h. die Belege, die den jetzigen als Vorgänger haben
      SELECT * FROM vbfa INTO wa_vbfa
               WHERE vbelv EQ gwa_result_tab-vbeln "#EC CI_NOFIELD. "HKLMNOP
                 AND vbtyp_n IN (if_sd_doc_category=>returns,
                                 if_sd_doc_category=>credit_memo_req,
                                 if_sd_doc_category=>debit_memo_req,
                                 if_sd_doc_category=>invoice,
                                 if_sd_doc_category=>invoice_cancel,
                                 if_sd_doc_category=>credit_memo,
                                 if_sd_doc_category=>debit_memo ).
* H-Retoure
* K-Gutschriftsanforderung
* L-Lastschriftanforderung
* M-Rechnung
* N-Storno Rechnung
* O-Gutschrift
* P-Lastschrift
        gwa_result_tab-prev_compl_flg = 'X'.
        gwa_result_tab-light = 2.
        MODIFY gt_result_tab FROM gwa_result_tab
               INDEX sy-tabix TRANSPORTING prev_compl_flg light.
        EXIT.
      ENDSELECT.
    ELSE.
      SELECT * FROM vbfa INTO wa_vbfa
               WHERE vbelv EQ gwa_result_tab-vbeln AND "#EC CI_NOFIELD.
                     posnv EQ gwa_result_tab-posnr AND "#EC CI_NOFIELD.
                     vbtyp_n IN (if_sd_doc_category=>returns,
                                 if_sd_doc_category=>credit_memo_req,
                                 if_sd_doc_category=>debit_memo_req,
                                 if_sd_doc_category=>invoice,
                                 if_sd_doc_category=>invoice_cancel,
                                 if_sd_doc_category=>credit_memo,
                                 if_sd_doc_category=>debit_memo ).
        gwa_result_tab-prev_compl_flg = 'X'.
        gwa_result_tab-light = 2.
        MODIFY gt_result_tab FROM gwa_result_tab
         INDEX sy-tabix TRANSPORTING prev_compl_flg light.
        EXIT.
      ENDSELECT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  read_complaint_reason
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_complaint_reason .
  DATA:  s_cmp_reason TYPE cmp_reason.

  IF s_cmp_reason IS INITIAL.
    SELECT SINGLE * FROM cmp_reason INTO s_cmp_reason
    WHERE compreas = p_rekla.
  ENDIF.

  gs_struc_fb_mass-auart1 = s_cmp_reason-auart1.
  gs_struc_fb_mass-auart2 = s_cmp_reason-auart2.
  gs_struc_fb_mass-fkart1 = s_cmp_reason-fkart1.
  gs_struc_fb_mass-fkart2 = s_cmp_reason-fkart2.

ENDFORM.                    " read_complaint_reason