REPORT rbdapp01 NO STANDARD PAGE HEADING LINE-SIZE 152.
* xlp 100398 Serialization

* Include constants.
INCLUDE mbdconst.

* table declarations needed for the selection screens
TABLES: edidc, tbd55, serial, bdfields.

* selection screen
SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
  SELECT-OPTIONS docnum FOR edidc-docnum.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS: credat FOR edidc-credat,
                  cretim FOR edidc-cretim DEFAULT '000000' TO '235959'.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS status FOR edidc-status.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS: mestyp FOR bdfields-mestyp,
                  mescod FOR edidc-mescod,
                  mesfct FOR edidc-mesfct.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS: sndprt FOR edidc-sndprt,
                  sndprn FOR edidc-sndprn,
                  sndpfc FOR edidc-sndpfc.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_pcksiz LIKE edp13-pcksiz DEFAULT 5.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS test FOR edidc-test NO-EXTENSION.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-ti2.
    SELECT-OPTIONS obj_type FOR tbd55-obj_type.
  SELECTION-SCREEN END   OF BLOCK block2.
  SELECTION-SCREEN BEGIN OF BLOCK blockas WITH FRAME TITLE text-tas.
  parameters: p_output TYPE ALELIST DEFAULT c_true.
  SELECTION-SCREEN END   OF BLOCK blockas.
  SELECTION-SCREEN END OF SCREEN 1100.

SELECTION-SCREEN BEGIN OF SCREEN 1200 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE text-tit.
    PARAMETERS: p_parall LIKE bdfields-parallel DEFAULT c_false,
                p_rfcgr  LIKE bdfields-rfcgr DEFAULT ' '.
*    CONSTANTS p_bnd TYPE parto_ale VALUE 1000.
    parameters: p_bnd TYPE parto_ale default '1000'.
*   parameters: p_bnd TYPE p default '1000'.
    PARAMETERS p_wait TYPE xfeld DEFAULT c_false.
  SELECTION-SCREEN END   OF BLOCK block.
SELECTION-SCREEN END OF SCREEN 1200.

*  definition of tab strip areas
SELECTION-SCREEN BEGIN OF TABBED BLOCK selscr FOR 25 LINES.
  SELECTION-SCREEN TAB (15) st_stand USER-COMMAND ucomm_stand
                  DEFAULT SCREEN 1100.
  SELECTION-SCREEN TAB (15) st_par   USER-COMMAND ucomm_add
                  DEFAULT SCREEN 1200.
SELECTION-SCREEN END OF BLOCK selscr.

************************************************************************

* type declarations
TYPES: gy_counter TYPE n LENGTH 7,
       BEGIN OF gy_update,
         docnum TYPE edi_docnum,
         upddat TYPE edi_upddat,
         updtim TYPE edi_updtim,
       END OF gy_update,
       BEGIN of gy_task,
         task_name  TYPE char32,
         task_count TYPE gy_counter,
         idoc_count TYPE sytabix,
       END OF gy_task.

* data declarations - internal tables
DATA: t_idoc_control_r TYPE STANDARD TABLE OF edidc WITH HEADER LINE,
      t_idoc_control_n TYPE STANDARD TABLE OF edidc WITH HEADER LINE,
      BEGIN OF t_idoc_control_s OCCURS 0,
        bo_cnum(14),
        control TYPE edidc,
        serial  TYPE serial,
      END OF t_idoc_control_s,
     BEGIN OF t_idoc_control_eoio OCCURS 0,
        queue_name TYPE bdeoio_queue_name,
        queue_counter TYPE bdeoio_queue_counter,
        control TYPE edidc,
      END OF t_idoc_control_eoio,
      t_idoc_control_tmp
               TYPE STANDARD TABLE OF edidc WITH KEY docnum
                   WITH HEADER LINE,
      t_packet TYPE STANDARD TABLE OF edp21 WITH HEADER LINE,
      t_teds2  TYPE SORTED TABLE OF teds2 WITH UNIQUE KEY status
                   WITH HEADER LINE,
      t_update TYPE STANDARD TABLE OF gy_update WITH KEY docnum
                   WITH HEADER LINE,
      i_tbd55  TYPE STANDARD TABLE OF tbd55 WITH HEADER LINE,
      gt_task  TYPE STANDARD TABLE OF gy_task WITH KEY task_name.
* Only needed for interface to APPLICATION_IDOC_POST
DATA: t_idoc_data TYPE STANDARD TABLE OF edidd.

* data declarations - fields
DATA: output_counter TYPE gy_counter,
      output_total   TYPE gy_counter,
      output_trigger TYPE sytabix,
      initialisation_ok(1) TYPE c VALUE ' ',
      help_counter   TYPE sytabix,
      end_of_packet  TYPE xfeld,
      lastt          TYPE p,
      gd_task        TYPE gy_task,
      finish_counter TYPE gy_counter,
      status_text    TYPE edi_text60,
      select_all_use TYPE xfeld,
      err_msgs       TYPE xfeld,
      edi_global     TYPE edi_glo.

AT SELECTION-SCREEN.
  IF p_parall = c_true AND p_rfcgr IS INITIAL.
      MESSAGE w122(b1).
*   Bei Parallelverarbeitung muß eine Servergruppe angegeben werden!
  ENDIF.

INITIALIZATION.
  st_stand = 'IDoc Selektion'(sei).
  st_par = 'Parallelverarb.'(sep).


START-OF-SELECTION.
* Read IDOC control records with status 64 or 66
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = 'Daten werden gerade selektiert'(o02).
  SELECT_ALL_USE = c_true.
  LOOP AT DOCNUM.
    IF docnum-sign NE 'I' OR docnum-option NE 'EQ'.
      SELECT_ALL_USE = c_false.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF syst-subrc IS NOT INITIAL.
    SELECT_ALL_USE = c_false.
  ENDIF.
  IF SELECT_ALL_USE = c_false.
    SELECT * FROM edidc INTO TABLE t_idoc_control_r
      WHERE ( status = c_status_in_ready_post OR
           status = c_idoc_status_postponed )
      AND status IN status
      AND docnum IN docnum
      AND mestyp IN mestyp
      AND mescod IN mescod
      AND mesfct IN mesfct
      AND sndprt IN sndprt
      AND sndprn IN sndprn
      AND sndpfc IN sndpfc
      AND credat IN credat
      AND cretim IN cretim
      AND test   IN test.
  ELSE.
    SELECT * FROM edidc INTO TABLE t_idoc_control_r
      FOR ALL ENTRIES IN DOCNUM
      WHERE ( status = c_status_in_ready_post OR
           status = c_idoc_status_postponed )
      AND status IN status
      AND docnum = docnum-low
      AND mestyp IN mestyp
      AND mescod IN mescod
      AND mesfct IN mesfct
      AND sndprt IN sndprt
      AND sndprn IN sndprn
      AND sndpfc IN sndpfc
      AND credat IN credat
      AND cretim IN cretim
      AND test   IN test.
  ENDIF.

  IF syst-subrc IS NOT INITIAL.            "keine Daten selektiert
    IF sy-batch IS NOT INITIAL.
      CLEAR edi_global.
      CALL FUNCTION 'IDOC_READ_GLOBAL'
      IMPORTING
        GLOBAL_DATA          = edi_global
      EXCEPTIONS
        INTERNAL_ERROR       = 0
        OTHERS               = 0.
      IF edi_global-AVMSG <> 'X'.
        MESSAGE i083(b1).
      ENDIF.
    ELSE.
      MESSAGE i083(b1).
    ENDIF.
*   Es konnten keine Daten selektiert werden.
    EXIT.
  ENDIF.
* all authorization ??
  AUTHORITY-CHECK OBJECT 'S_IDOCMONI'
           ID 'ACTVT' FIELD c_display
           ID 'EDI_DIR' FIELD '*'
           ID 'EDI_MES' FIELD '*'
           ID 'EDI_PRN' FIELD '*'
           ID 'EDI_PRT' FIELD '*'
           ID 'EDI_TCD' DUMMY.
  IF syst-subrc IS NOT INITIAL.            "no auth. for all test each
    LOOP AT t_idoc_control_r.
      AUTHORITY-CHECK OBJECT 'S_IDOCMONI'
               ID 'ACTVT' FIELD c_display
               ID 'EDI_DIR' FIELD t_idoc_control_r-direct
               ID 'EDI_MES' FIELD t_idoc_control_r-mestyp
               ID 'EDI_PRN' FIELD t_idoc_control_r-sndprn
               ID 'EDI_PRT' FIELD t_idoc_control_r-sndprt
               ID 'EDI_TCD' DUMMY.
      IF syst-subrc IS NOT INITIAL.                         "no auth.
        MESSAGE i285(b1) WITH t_idoc_control_r-docnum.
        DELETE t_idoc_control_r.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT t_idoc_control_r
       BY sndprn sndprt sndpfc mestyp mescod mesfct test serial.
* Find out, if IDocs are to serialize via object channel
  PERFORM divi_idocs.

  LOOP AT t_idoc_control_r.
    t_update-docnum = t_idoc_control_r-docnum.
    t_update-upddat = t_idoc_control_r-upddat.
    t_update-updtim = t_idoc_control_r-updtim.
    APPEND t_update.
  ENDLOOP.
* Copy packet-fields to t_packet.
* A packet will only be processed as such if table EDP21's key fields
* are identical for all IDOCs in the packet.
  LOOP AT t_idoc_control_n.
    MOVE-CORRESPONDING t_idoc_control_n TO t_packet.
    APPEND t_packet.
  ENDLOOP.

  IF p_parall = c_true AND p_rfcgr <> space.
    CALL FUNCTION 'SPBT_INITIALIZE'
         EXPORTING
              group_name                   = p_rfcgr
         EXCEPTIONS
              invalid_group_name           = 1
              internal_error               = 2
              pbt_env_already_initialized  = 3
              currently_no_resources_avail = 4
              no_pbt_resources_found       = 5
              OTHERS                       = 6.

    IF sy-subrc = 4.
      initialisation_ok = ' '.
    ENDIF.
    IF sy-subrc = 0 OR sy-subrc = 3.
      initialisation_ok = 'X'.
    ENDIF.
    IF sy-subrc = 1 OR sy-subrc = 2 OR sy-subrc = 5
        OR sy-subrc = 6.
      MESSAGE a286(b1) WITH p_rfcgr.
*   Servergruppe & konnte nicht initialisiert werden
      EXIT.
    ENDIF.
  ENDIF.

* Loop through IDOCs and call APPLICATION_IDOC_POST once per packet.
  DESCRIBE TABLE t_idoc_control_r LINES output_total.
  clear: output_counter,
         help_counter,
         output_trigger,
         end_of_packet.
  REFRESH t_idoc_control_tmp.

  LOOP AT t_packet.

    AT END OF test.
      end_of_packet = c_true.
    ENDAT.

    READ TABLE t_idoc_control_n INDEX sy-tabix.

    ADD 1 TO: help_counter,
              output_counter,
              output_trigger.
    IF ( output_trigger >= 10 ) OR ( output_total <= 100 )."#EC PORTABLE
      output_trigger = 0.
    ENDIF.

    PERFORM out_progress USING '' ''.

    INSERT t_idoc_control_n INTO TABLE t_idoc_control_tmp.

    IF   help_counter >= p_pcksiz
      OR end_of_packet = c_true.

*   Need to refresh t_idoc_data because APPLICATION_IDOC_POST fills it.
      REFRESH t_idoc_data.
      PERFORM idoc_data_read TABLES t_idoc_control_tmp
                                    t_idoc_data.

      IF p_parall = c_true.

        PERFORM fill_task_table USING help_counter CHANGING gd_task.

        IF p_rfcgr IS NOT INITIAL.

          DO.
          IF initialisation_ok EQ 'X'.
            CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
            exceptions others = 04.
            IF p_wait IS INITIAL.
              CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
                  STARTING NEW TASK gd_task-task_name
                  DESTINATION IN GROUP p_rfcgr
                TABLES
                  idoc_control              = t_idoc_control_tmp
                  idoc_data                 = t_idoc_data
                EXCEPTIONS
                  resource_failure          = 1
                  system_failure            = 2
                  communication_failure     = 3
                  others                    = 4.
            ELSE.
              CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
                  STARTING NEW TASK gd_task-task_name
                  DESTINATION IN GROUP p_rfcgr
                  PERFORMING end_of_idoc_posting ON END OF TASK
*               exporting
*                 post_immediately = c_true
                TABLES
                  idoc_control              = t_idoc_control_tmp
                  idoc_data                 = t_idoc_data
                EXCEPTIONS
                  resource_failure          = 1
                  system_failure            = 2
                  communication_failure     = 3
                  others                    = 4.
            ENDIF.
            IF syst-subrc IS NOT INITIAL.
              PERFORM check_self.
            ELSE.
              PERFORM out_protocol
                  TABLES t_idoc_control_tmp  "extended protocol
                  USING  c_true.             "parallel flag
              EXIT.
            ENDIF.
          ELSE.
            PERFORM check_self.
          ENDIF.
          ENDDO.
          PERFORM reset_self.
        ELSE.

          DO.
            IF p_wait IS INITIAL.
              CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
                  STARTING NEW TASK gd_task-task_name
                  DESTINATION IN GROUP DEFAULT
                TABLES
                  idoc_control              = t_idoc_control_tmp
                  idoc_data                 = t_idoc_data
                EXCEPTIONS
                  resource_failure          = 1
                  system_failure            = 2
                  communication_failure     = 3
                  others                    = 4.
            ELSE.
              CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
                  STARTING NEW TASK gd_task-task_name
                  DESTINATION IN GROUP DEFAULT
                  PERFORMING end_of_idoc_posting ON END OF TASK
                TABLES
                  idoc_control     = t_idoc_control_tmp
                  idoc_data        = t_idoc_data
                EXCEPTIONS
                  resource_failure          = 1
                  system_failure            = 2
                  communication_failure     = 3
                  others                    = 4.
            ENDIF.
            IF syst-subrc IS NOT INITIAL.
              PERFORM check_self.
            ELSE.
              PERFORM out_protocol
                  TABLES t_idoc_control_tmp  "extended protocol
                  USING  c_true.             "parallel flag
              EXIT.
            ENDIF.
          ENDDO.
          PERFORM reset_self.
        ENDIF.
      ELSE.
        CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
*          exporting
*               post_immediately = c_true
             TABLES
                  idoc_control     = t_idoc_control_tmp
                  idoc_data        = t_idoc_data
             EXCEPTIONS
                  error_message    = 1.
        IF syst-subrc IS NOT INITIAL.
          err_msgs = c_true.
        ENDIF.
        PERFORM out_protocol
            TABLES t_idoc_control_tmp  "extended protocol
            USING  c_false.            "parallel flag
      ENDIF.

      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.
      CLEAR: help_counter,
             end_of_packet.
      REFRESH t_idoc_control_tmp.
    ENDIF.
  ENDLOOP.                             "End of LOOP AT T_PACKET.

  IF help_counter <> 0.
    REFRESH t_idoc_data.
    PERFORM idoc_data_read TABLES t_idoc_control_tmp
                                  t_idoc_data.

    CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
*      exporting
*           post_immediately = c_true
         TABLES
              idoc_control     = t_idoc_control_tmp
              idoc_data        = t_idoc_data
         EXCEPTIONS
              error_message    = 1.
    IF syst-subrc IS NOT INITIAL.
      err_msgs = c_true.
    ENDIF.
    ADD help_counter TO finish_counter.
    PERFORM out_protocol
        TABLES t_idoc_control_tmp  "extended protocol
        USING  c_false.            "parallel flag
  ENDIF.

  PERFORM eoio_proc_bundle.

  PERFORM obj_serial_proc_bundle.
*PERFORM obj_serial_proc.

* Wait until processing is finished if that is required.
  IF p_parall = c_true AND p_wait = c_true.
**    WAIT UNTIL finish_counter >= output_total.
    WHILE finish_counter < output_total.
      WAIT UP TO 1 SECONDS.
    ENDWHILE.
  ENDIF.

* Output result
  IF p_output ne 'X'.
    exit.
  ENDIF.

*Exit here if there were no IDocs processed (this is because select
*with 'for all entries' takes ages when the entries table is empty).
CHECK t_update[] IS NOT INITIAL.

CALL FUNCTION 'IDOC_OUTPUT_LIST'
  EXPORTING
    headertxt       = TEXT-001
  tables
    output          = T_UPDATE

          .


*---------------------------------------------------------------------*
*       FORM IDOC_DATA_READ                                           *
*---------------------------------------------------------------------*
*   This routine reads the IDOC's data records from the database.     *
*---------------------------------------------------------------------*
*  -->  T_IDOC_CONTROL    IDOC control records                        *
*  <--  T_IDOC_DATA       IDOC data records                           *
*---------------------------------------------------------------------*
FORM idoc_data_read TABLES t_idoc_control STRUCTURE edidc   "Import
                           t_idoc_data    STRUCTURE edidd.  "Export

  CALL FUNCTION 'ALE_FTCH_DATA_SEGMENTS_OF_IDOC'
       EXPORTING
            hot_only       = 'X'
       TABLES
            t_idoc_control = t_idoc_control
            t_idoc_data    = t_idoc_data.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM OUT_PROGRESS                                             *
*---------------------------------------------------------------------*
FORM out_progress USING p_text TYPE itex132
                        p_proz TYPE char04.
* status output (if <=100, show all; otherwise show only all tenth)
  DATA output_text TYPE char80.

  CHECK output_trigger = 0 OR p_text NE ''.
  output_text = '(& von &) IDoc & wird gerade abgearbeitet'(o01).
  REPLACE '&' WITH: output_counter INTO output_text,
                    output_total   INTO output_text,
                    t_idoc_control_n-docnum INTO output_text.
  CONDENSE output_text.
  CONCATENATE p_text output_text INTO output_text SEPARATED BY space.
  CONDENSE output_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = p_proz
            text       = output_text.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DIVI_IDOCS
*&---------------------------------------------------------------------*
* Make 3 bundels of IDocs (EOIO, standard and to serialize ones)
*   EOIO-prcessing depends on EDIDC-ARCKEY
*   SERIALIZATION  depends on EDIDC-SERIAL
*   EOIO is preferred: Ignore SERIAL if EOIO is required
*----------------------------------------------------------------------*
FORM divi_idocs.
  DATA lines TYPE syindex.
  DATA l_eoio_active TYPE bdeoio_active.

  DESCRIBE TABLE obj_type LINES lines.
* Fetch Customizing table
  SELECT * FROM tbd55 INTO TABLE i_tbd55.

  SORT i_tbd55.

  LOOP AT t_idoc_control_r.
    IF t_idoc_control_r-arckey(16) EQ sidht_co_soap_seq_prefix.
* Check against tbd55_eoio
      CALL FUNCTION 'BDEOIO_CHECK_ACTIVE_IN'
        EXPORTING
          control       = t_idoc_control_r
       IMPORTING
          active        = l_eoio_active.
       IF l_eoio_active IS NOT INITIAL.
* EOIO-processing is required => SERIAL is ignored
          CALL FUNCTION 'BDEOIO_GET_ID_DETAILS'
            EXPORTING
              id                 = t_idoc_control_r-arckey
           IMPORTING
             queue_name          = t_idoc_control_eoio-queue_name
             queue_counter       = t_idoc_control_eoio-queue_counter
           EXCEPTIONS
             OTHERS              = 0.
          MOVE t_idoc_control_r TO t_idoc_control_eoio-control.
          APPEND t_idoc_control_eoio.
          CONTINUE.
        ENDIF.
    ENDIF.

    serial = t_idoc_control_r-serial.
    READ TABLE i_tbd55 WITH KEY mandt = sy-mandt
                                sndprn = t_idoc_control_r-sndprn
                                sndprt = t_idoc_control_r-sndprt
                                sndpfc = t_idoc_control_r-sndpfc
                        obj_type = serial-obj_type binary search.

    IF syst-subrc ne 0.                 "nicht da -> standard
      MOVE t_idoc_control_r TO t_idoc_control_n.
      IF lines EQ 0.
        "bei ges. obj_type wird KEIN anderes IDoc verarb.
        APPEND t_idoc_control_n.
      ELSE.
        DELETE t_idoc_control_r.
      ENDIF.
    ELSE.                              "da -> serial
      MOVE t_idoc_control_r-serial TO serial.
      MOVE-CORRESPONDING: t_idoc_control_r TO t_idoc_control_s-control,
                          serial TO t_idoc_control_s-serial.
      t_idoc_control_s-bo_cnum = serial.
      IF serial-obj_type IN obj_type.  "filter acc. to sel-options
        APPEND t_idoc_control_s.
      ELSE.
        DELETE t_idoc_control_r.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " DIVI_IDOCS

*&---------------------------------------------------------------------*
*&      Form  OBJ_SERIAL_PROC
*&---------------------------------------------------------------------*
*       Make bundels by Object channels
*----------------------------------------------------------------------*
FORM obj_serial_proc.
  DATA: act_okey TYPE bdserinstr,
        old_okey TYPE bdserinstr,
        count_id TYPE sytabix,
        t_idoc_proc TYPE STANDARD TABLE OF edidc WITH HEADER LINE,
        t_idoc_bundle TYPE STANDARD TABLE OF edidc WITH HEADER LINE.

  CHECK LINES( t_idoc_control_s ) IS NOT INITIAL.
* Serial packet process: Sort
  SORT t_idoc_control_s
      BY control-sndprt control-sndprn
          serial-obj_type serial-chnum serial-chcou.
* Serial packet process: Make bundles by objectchannels

  CLEAR: act_okey, old_okey, count_id.
  REFRESH t_idoc_proc.

  LOOP AT t_idoc_control_s.
    t_idoc_control_n-docnum = t_idoc_control_s-control-docnum.
    ADD 1 TO output_counter.
    PERFORM out_progress USING '' ''.        " show current IDoc
    MOVE-CORRESPONDING: t_idoc_control_s-control TO act_okey,
                        t_idoc_control_s-serial  TO act_okey.
    IF act_okey NE old_okey AND old_okey-obj_type NE ''.

* Process all data in bundle now
* Parallel ?
      IF p_parall = c_true AND p_rfcgr IS NOT INITIAL.
        IF count_id >= p_pcksiz.       "Counter must reach packet size
          PERFORM fill_task_table USING count_id CHANGING gd_task.
          DO.
            "collect more (than one Object per packet)
            "start new task
            IF p_wait IS INITIAL.
              CALL FUNCTION 'ALE_SERIAL_PROCESS'
                  STARTING NEW TASK gd_task-task_name
                  DESTINATION IN GROUP p_rfcgr
                EXPORTING
                  ext_prot              = c_false
                TABLES
                  t_idoc_cntrl          = t_idoc_proc
                EXCEPTIONS
                  resource_failure      = 1
                  system_failure        = 2
                  communication_failure = 3
                  others                = 4.
            ELSE.
              CALL FUNCTION 'ALE_SERIAL_PROCESS'
                  STARTING NEW TASK gd_task-task_name
                  DESTINATION IN GROUP p_rfcgr
                  PERFORMING end_of_serial_posting ON END OF TASK
                EXPORTING
                  ext_prot              = c_false
                TABLES
                  t_idoc_cntrl          = t_idoc_proc
                EXCEPTIONS
                  resource_failure      = 1
                  system_failure        = 2
                  communication_failure = 3
                  others                = 4.
            ENDIF.
            IF syst-subrc IS NOT INITIAL.
              PERFORM check_self.
            ELSE.
              PERFORM out_protocol
                  TABLES t_idoc_proc  "extended protocol
                  USING  c_true.      "parallel flag
              EXIT.
            ENDIF.
          ENDDO.

          REFRESH t_idoc_proc.
          CLEAR count_id.
          PERFORM reset_self.
          REFRESH t_idoc_proc.
          CLEAR count_id.
        ENDIF.
      ELSE.                            "take my workprocess
        CALL FUNCTION 'ALE_SERIAL_PROCESS'
             EXPORTING
                  ext_prot     = c_false
             TABLES
                  t_idoc_cntrl = t_idoc_proc.
        ADD count_id TO finish_counter.
        REFRESH t_idoc_proc.
        CLEAR count_id.
      ENDIF.
*     WRITE: / ' --> end of object bundle', old_okey(20).
    ENDIF.
    old_okey = act_okey.
*   WRITE: / t_idoc_cntrl-docnum, t_idoc_cntrl-mestyp,
*           t_idoc_cntrl-serial.
    MOVE t_idoc_control_s-control TO t_idoc_proc.
    APPEND t_idoc_proc.
    ADD 1 TO count_id.
  ENDLOOP.
* Process last bundle
* Parallel ?
  IF p_parall = c_true AND p_rfcgr IS NOT INITIAL.  " start new task
    PERFORM fill_task_table USING count_id CHANGING gd_task.
    DO.
      IF p_wait IS INITIAL.
        CALL FUNCTION 'ALE_SERIAL_PROCESS'
            STARTING NEW TASK gd_task-task_name
            DESTINATION IN GROUP p_rfcgr
          EXPORTING
            ext_prot              = c_false
          TABLES
            t_idoc_cntrl          = t_idoc_proc
          EXCEPTIONS
            resource_failure      = 1
            system_failure        = 2
            communication_failure = 3
            others                = 4.
      ELSE.
        CALL FUNCTION 'ALE_SERIAL_PROCESS'
            STARTING NEW TASK gd_task-task_name
            DESTINATION IN GROUP p_rfcgr
            PERFORMING end_of_serial_posting ON END OF TASK
          EXPORTING
            ext_prot              = c_false
          TABLES
            t_idoc_cntrl          = t_idoc_proc
          EXCEPTIONS
            resource_failure      = 1
            system_failure        = 2
            communication_failure = 3
            others                = 4.
      ENDIF.
      IF syst-subrc IS NOT INITIAL.
        PERFORM check_self.
      ELSE.
        PERFORM out_protocol
            TABLES t_idoc_proc  "extended protocol
            USING  c_true.      "parallel flag
        EXIT.
      ENDIF.
    ENDDO.
    PERFORM reset_self.
  ELSE.                                "take my workprocess
* serial mode all objects of one channel
    DATA: t_idoc_pack TYPE STANDARD TABLE OF edidc WITH HEADER LINE.
    CLEAR help_counter.

    LOOP at t_idoc_proc.
      ADD 1 TO help_counter.
      INSERT t_idoc_proc INTO TABLE t_idoc_pack.
      IF help_counter GE p_pcksiz.
        CALL FUNCTION 'ALE_SERIAL_PROCESS'
         EXPORTING
              ext_prot     = c_false
         TABLES
                t_idoc_cntrl = t_idoc_pack.
        CLEAR t_idoc_pack.
        REFRESH t_idoc_pack.
        CLEAR help_counter.
      ENDIF.
    ENDLOOP.
    IF not help_counter is initial.
      CALL FUNCTION 'ALE_SERIAL_PROCESS'
           EXPORTING
                ext_prot     = c_false
           TABLES
                t_idoc_cntrl = t_idoc_pack.
        CLEAR t_idoc_pack.
        REFRESH t_idoc_pack.
        CLEAR help_counter.
    ENDIF.
  ENDIF.

* WRITE: / ' --> end of object bundle', old_okey(20).
ENDFORM.                               " OBJ_SERIAL_PROC

*&---------------------------------------------------------------------*
*&      Form  OUT_PROTOCOL
*&---------------------------------------------------------------------*
*       Display IDocs and how they are processed.
*----------------------------------------------------------------------*
*      -->P_T_IDOC_CONTROL  Table
*      -->P_PARFL   Parallel
*----------------------------------------------------------------------*
FORM out_protocol TABLES p_t_idoc_control STRUCTURE edidc
                  USING  p_parfl          TYPE xfeld.

  CALL FUNCTION 'ALE_SER_PROTOCOLL'
       EXPORTING
            p_parfl          = p_parfl
            p_ext_prot       = c_false
       TABLES
            p_t_idoc_control = p_t_idoc_control.
  CLEAR sy-subrc.
ENDFORM.                               " OUT_PROTOCOL

*&---------------------------------------------------------------------*
*&      Form  check_self
*&---------------------------------------------------------------------*
FORM check_self.
  DATA: ppct TYPE p,
        pct  TYPE c LENGTH 4.

  IF lastt > p_bnd.
    MESSAGE i288(b1) WITH output_counter output_total.
*   & von & IDocs konnten parallel zur Verarbeitung übergeben werden.
    MESSAGE a287(b1).
*   Zahl maximaler Versuche beim Start paralleler Prozesse überschritten
  ELSE.
    IF initialisation_ok = ' '.
      CALL FUNCTION 'SPBT_INITIALIZE'
           EXPORTING
                group_name                   = p_rfcgr
           EXCEPTIONS
                invalid_group_name           = 1
                internal_error               = 2
                pbt_env_already_initialized  = 3
                currently_no_resources_avail = 4
                no_pbt_resources_found       = 5
                OTHERS                       = 6.
      CASE sy-subrc.
        WHEN 0 OR 3.
          initialisation_ok = 'X'.
        WHEN 1 OR 2 OR 5 OR 6.
          MESSAGE a286(b1) WITH p_rfcgr.
*    Servergruppe & konnte nicht initialisiert werden
        WHEN 4.
          initialisation_ok = ' '.
* Initialisierung wegen fehlender Ressourcen gescheitert
      ENDCASE.
    ENDIF.
* count tryouts
    ADD 1 TO lastt.
    COMPUTE ppct = ( lastt * 100 ) / p_bnd.
    WRITE ppct TO pct LEFT-JUSTIFIED.
    PERFORM out_progress USING 'Versuche Paketstart '(pa1) pct.
    WAIT UP TO 1 SECONDS.
  ENDIF.
ENDFORM.                               " check_self

*&---------------------------------------------------------------------*
*&      Form  reset_self
*&---------------------------------------------------------------------*
FORM reset_self.
  lastt = 0.
ENDFORM.                               " reset_self

*&---------------------------------------------------------------------*
*&      Form  fill_task_table
*&---------------------------------------------------------------------*
*       Save information for asynchronous processing.
*----------------------------------------------------------------------*
*      -->LP_COUNT   counter
*      <--LP_TASK    work area with task information
*----------------------------------------------------------------------*
FORM fill_task_table USING    lp_count TYPE sytabix
                     CHANGING lp_task  TYPE gy_task.
  CONSTANTS lc_prefix TYPE char32 VALUE 'ALE_'.

  ADD 1 TO lp_task-task_count.
  CONCATENATE lc_prefix lp_task-task_count INTO lp_task-task_name.
  MOVE lp_count TO lp_task-idoc_count.
  INSERT lp_task INTO TABLE gt_task.
ENDFORM.                    " fill_task_table

*&---------------------------------------------------------------------*
*&      Form  END_OF_IDOC_POSTING
*&---------------------------------------------------------------------*
*       Register the end of one parallel IDoc posting process.
*----------------------------------------------------------------------*
*      -->lp_task   name of task
*----------------------------------------------------------------------*
FORM end_of_idoc_posting USING lp_task TYPE char32.             "#EC *
  DATA: lt_edidc TYPE STANDARD TABLE OF edidc,
        ld_count TYPE sytabix,
        ld_task  TYPE gy_task.

  RECEIVE RESULTS FROM FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
    TABLES
      idoc_control          = lt_edidc
    EXCEPTIONS
      resource_failure      = 1
      system_failure        = 2
      communication_failure = 3
      others                = 4.
  IF syst-subrc IS INITIAL.
    DESCRIBE TABLE lt_edidc LINES ld_count.
  ELSE.
    READ TABLE gt_task INTO ld_task WITH TABLE KEY task_name = lp_task.
    IF syst-subrc IS INITIAL.
      MOVE ld_task-idoc_count TO ld_count.
    ELSE.
* best we can do in this situation to ensure that waiting ends
      MOVE p_pcksiz TO ld_count.
    ENDIF.
  ENDIF.
  ADD ld_count TO finish_counter.
ENDFORM.                               " end_of_idoc_posting

*&---------------------------------------------------------------------*
*&      Form  END_OF_SERIAL_POSTING
*&---------------------------------------------------------------------*
*       Register the end of one parallel IDoc posting process.
*----------------------------------------------------------------------*
*      -->lp_task   name of task
*----------------------------------------------------------------------*
FORM end_of_serial_posting USING lp_task TYPE char32.           "#EC *
  DATA: lt_edidc TYPE STANDARD TABLE OF edidc,
        ld_count TYPE sytabix,
        ld_task  TYPE gy_task.

  RECEIVE RESULTS FROM FUNCTION 'ALE_SERIAL_PROCESS'
    TABLES
      t_idoc_cntrl          = lt_edidc
    EXCEPTIONS
      resource_failure      = 1
      system_failure        = 2
      communication_failure = 3
      others                = 4.
  IF syst-subrc IS INITIAL.
    DESCRIBE TABLE lt_edidc LINES ld_count.
  ELSE.
    READ TABLE gt_task INTO ld_task WITH TABLE KEY task_name = lp_task.
    IF syst-subrc IS INITIAL.
      MOVE ld_task-idoc_count TO ld_count.
    ELSE.
* best we can do in this situation to ensure that waiting ends
      MOVE LINES( t_idoc_control_s ) TO ld_count.
    ENDIF.
  ENDIF.
  ADD ld_count TO finish_counter.
ENDFORM.                               " end_of_serial_posting
*&---------------------------------------------------------------------*
*&      Form  OBJ_SERIAL_PROC_BUNDLE
*&---------------------------------------------------------------------*

form OBJ_SERIAL_PROC_BUNDLE .
 DATA:  act_okey TYPE bdserinstr,
        old_okey TYPE bdserinstr,
        count_id TYPE sytabix,
        t_idoc_proc TYPE STANDARD TABLE OF edidc WITH HEADER LINE,
        t_idoc_bundle TYPE STANDARD TABLE OF edidc WITH HEADER LINE.

  CHECK LINES( t_idoc_control_s ) IS NOT INITIAL.
* Serial packet process: Sort
  SORT t_idoc_control_s
      BY control-sndprt control-sndprn
          serial-obj_type serial-chnum serial-chcou.
* Serial packet process: Make bundles by objectchannels

  CLEAR: act_okey, old_okey, count_id.
  REFRESH t_idoc_proc.

  LOOP AT t_idoc_control_s.
    AT END OF BO_CNUM.
      end_of_packet = c_true.
    ENDAT.
    ADD 1 TO: output_counter,
              help_counter.
    PERFORM out_progress USING '' ''.        " show current IDoc

    MOVE t_idoc_control_s-control TO t_idoc_bundle.
    APPEND t_idoc_bundle.


    IF   help_counter >= p_pcksiz
      OR end_of_packet = c_true.
* start of processing
* parallel
      IF p_parall = c_true AND p_rfcgr IS NOT INITIAL.
        PERFORM fill_task_table USING count_id CHANGING gd_task.
* parallel mode using servergroups
          DO.
            IF p_wait IS INITIAL.
              CALL FUNCTION 'ALE_SERIAL_PROCESS'
                STARTING NEW TASK gd_task-task_name
                DESTINATION IN GROUP p_rfcgr
                  EXPORTING
                    ext_prot              = c_false
                  TABLES
                    t_idoc_cntrl          = t_idoc_bundle
                  EXCEPTIONS
                    resource_failure      = 1
                    system_failure        = 2
                    communication_failure = 3
                    others                = 4.
            ELSE.
              CALL FUNCTION 'ALE_SERIAL_PROCESS'
                STARTING NEW TASK gd_task-task_name
                DESTINATION IN GROUP p_rfcgr
                PERFORMING end_of_serial_posting ON END OF TASK
                  EXPORTING
                    ext_prot              = c_false
                  TABLES
                    t_idoc_cntrl          = t_idoc_bundle
                  EXCEPTIONS
                    resource_failure      = 1
                    system_failure        = 2
                    communication_failure = 3
                    others                = 4.
             ENDIF.
             IF syst-subrc IS NOT INITIAL.
               PERFORM check_self.
             ELSE.
               EXIT.
             ENDIF.
          ENDDO.
          PERFORM reset_self.
          CLEAR t_idoc_bundle.
          REFRESH t_idoc_bundle.
          CLEAR help_counter.
       ELSE.
* start of processing
* not parallel
      CALL FUNCTION 'ALE_SERIAL_PROCESS'
               EXPORTING
                 ext_prot     = c_false
               TABLES
                 t_idoc_cntrl = t_idoc_bundle.
          CLEAR t_idoc_bundle.
          REFRESH t_idoc_bundle.
          CLEAR help_counter.
      ENDIF.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.
      CLEAR: help_counter,
             end_of_packet.
      REFRESH t_idoc_bundle.
    ENDIF.
  ENDLOOP.

* process of last bundle
  IF help_counter <> 0.
    CALL FUNCTION 'ALE_SERIAL_PROCESS'
            EXPORTING
              ext_prot     = c_false
            TABLES
              t_idoc_cntrl = t_idoc_bundle.
            CLEAR t_idoc_bundle.
            REFRESH t_idoc_bundle.
            CLEAR help_counter.
  ENDIF.

endform.                    " OBJ_SERIAL_PROC_BUNDLE


*&---------------------------------------------------------------------*
*&      Form  EOIO_PROC_BUNDLE
*&---------------------------------------------------------------------*
FORM eoio_proc_bundle .
  DATA:  l_parall TYPE c.

  CHECK lines( t_idoc_control_eoio ) IS NOT INITIAL.
  REFRESH t_idoc_control_tmp.

* Sort by PARTNER and QUEUE
  SORT t_idoc_control_eoio
      BY control-sndprt control-sndprn
         queue_name queue_counter.

* Build packages for partner/queue

  LOOP AT t_idoc_control_eoio.
    AT END OF queue_name.
      end_of_packet = c_true.
    ENDAT.
    ADD 1 TO: output_counter,
              help_counter.

    MOVE t_idoc_control_eoio-control TO t_idoc_control_tmp.
    APPEND t_idoc_control_tmp.

    IF   help_counter >= p_pcksiz
      OR end_of_packet = c_true.
* start of processing

* parallel processing not possible for more than one package per queue
      IF help_counter >= p_pcksiz.
        l_parall = c_false. "successor package assumed => no parallel processing
      ELSE.
        l_parall = p_parall.
      ENDIF.

* read IDoc data
      REFRESH t_idoc_data.
      PERFORM idoc_data_read TABLES t_idoc_control_tmp
                                    t_idoc_data.

      IF l_parall = c_true AND p_rfcgr IS NOT INITIAL.
        PERFORM fill_task_table USING help_counter CHANGING gd_task.
* parallel mode using servergroups
        DO.
          IF p_wait IS INITIAL.
            CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
              STARTING NEW TASK gd_task-task_name
              DESTINATION IN GROUP p_rfcgr
              TABLES
                idoc_control          = t_idoc_control_tmp
                idoc_data             = t_idoc_data
              EXCEPTIONS
                resource_failure      = 1
                system_failure        = 2
                communication_failure = 3
                OTHERS                = 4.
          ELSE.
            CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
              STARTING NEW TASK gd_task-task_name
              DESTINATION IN GROUP p_rfcgr
              PERFORMING end_of_idoc_posting ON END OF TASK
              TABLES
                idoc_control          = t_idoc_control_tmp
                idoc_data             = t_idoc_data
              EXCEPTIONS
                resource_failure      = 1
                system_failure        = 2
                communication_failure = 3
                OTHERS                = 4.
          ENDIF.
          IF syst-subrc IS NOT INITIAL.
            PERFORM check_self.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
        PERFORM reset_self.
      ELSE.
* start of processing
* not parallel
        CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
          TABLES
            idoc_control  = t_idoc_control_tmp
            idoc_data     = t_idoc_data
          EXCEPTIONS
            error_message = 1.
        IF syst-subrc IS NOT INITIAL.
          err_msgs = c_true.
        ENDIF.
        ADD help_counter TO finish_counter.
      ENDIF.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.
      CLEAR: help_counter,
             end_of_packet,
             t_idoc_control_tmp.
      REFRESH t_idoc_control_tmp.
    ENDIF.
  ENDLOOP.

* process of last bundle
  IF help_counter <> 0.
        CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
          TABLES
            idoc_control  = t_idoc_control_tmp
            idoc_data     = t_idoc_data
          EXCEPTIONS
            error_message = 1.
        IF syst-subrc IS NOT INITIAL.
          err_msgs = c_true.
        ENDIF.
        ADD help_counter TO finish_counter.
  ENDIF.


ENDFORM.                    " EOIO_PROC_BUNDLE
