*&---------------------------------------------------------------------*
*& Report  RC1_IDOC_SET_STATUS                                         *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  RC1_IDOC_SET_STATUS           .

TABLES: EDIDC.

SELECTION-SCREEN BEGIN OF BLOCK 1.
SELECT-OPTIONS: P_IDOC   FOR EDIDC-DOCNUM NO INTERVALS.
PARAMETERS:     P_MESTYP LIKE EDIDC-MESTYP.
SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK 2.
PARAMETERS : P_STATUS LIKE EDIDS-STATUS DEFAULT 51 OBLIGATORY,
             P_STANEU LIKE EDIDS-STATUS DEFAULT 68 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK 2.

PARAMETERS P_TEST AS CHECKBOX DEFAULT 'X'.


DATA: L_EDIDC_TAB       LIKE EDIDC      OCCURS 1 WITH HEADER LINE,
      L_IDOC_STATUS_TAB LIKE BDIDOCSTAT OCCURS 1 WITH HEADER LINE,
      L_LINES           LIKE SY-TABIX,
      L_IDOC_CONTROL    LIKE EDIDC,
      L_ZAEHLER         LIKE SY-TABIX.

IF P_MESTYP <> SPACE.
  SELECT * INTO TABLE L_EDIDC_TAB FROM EDIDC
                                 WHERE MESTYP = P_MESTYP
* Begin Correction 23.06.10 1481960 **********************************
                                   AND STATUS = P_STATUS
                                   AND DOCNUM IN P_IDOC.
ELSEIF NOT P_IDOC[] IS INITIAL.
  SELECT * INTO TABLE L_EDIDC_TAB FROM EDIDC
                                 WHERE STATUS = P_STATUS
                                   AND DOCNUM IN P_IDOC.
* End Correction 23.06.10 1481960 ************************************
ELSE.
  EXIT.
ENDIF.

  DESCRIBE TABLE L_EDIDC_TAB LINES L_LINES.

IF P_TEST = 'X'.
  WRITE: L_LINES, ' IDOCs werden umgesetzt.'(001).
  EXIT.
ELSE.
  LOOP AT L_EDIDC_TAB.
    CLEAR L_IDOC_STATUS_TAB. REFRESH L_IDOC_STATUS_TAB.

    L_IDOC_STATUS_TAB-DOCNUM = L_EDIDC_TAB-DOCNUM.
    L_IDOC_STATUS_TAB-STATUS = P_STANEU.
    APPEND L_IDOC_STATUS_TAB.


    CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
         EXPORTING
              IDOC_NUMBER               = L_EDIDC_TAB-DOCNUM
         IMPORTING
              IDOC_CONTROL              = L_IDOC_CONTROL
         TABLES
              IDOC_STATUS               = L_IDOC_STATUS_TAB
         EXCEPTIONS
              IDOC_FOREIGN_LOCK         = 1
              IDOC_NOT_FOUND            = 2
              IDOC_STATUS_RECORDS_EMPTY = 3
              IDOC_STATUS_INVALID       = 4
              DB_ERROR                  = 5
              OTHERS                    = 6.

    ADD 1 TO L_ZAEHLER.

*   Um Überlauf der Sperrtabelle zu verhindern, bei großer Anzahl IDOCs
    IF L_ZAEHLER = 1000.
      CALL FUNCTION 'DEQUEUE_ALL'.
      CLEAR L_ZAEHLER.
    ENDIF.

    IF SY-SUBRC <> 0.
      WRITE : / 'Fehler:'(002), SY-SUBRC.
    ENDIF.
  ENDLOOP.

  WRITE: L_LINES, ' IDOCs wurden umgesetzt.'(003).

ENDIF.
