       IDENTIFICATION DIVISION.
       PROGRAM-ID.       WBCI0090.
      ******************************************************************
      *                                                                *
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  1. GETS CALLED BY WBCI1206.                                   *
      *  2. CALLS WBCI0095 THREE TIMES.                                *
      *  3. RETURNS.                                                   *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE   ASSIGN TO EMPSTAT.
       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(80).
       WORKING-STORAGE SECTION.
       01  PGM-NAME.
           05  WBCI0095    PIC X(8)      VALUE 'WBCI0095'.
       LINKAGE SECTION.
       01  PARMINFO.
           03  RECORD-COUNT       PIC 99.
       PROCEDURE DIVISION USING PARMINFO.
       0000-MAINLINE.
           CALL WBCI0095 USING RECORD-COUNT.
           CALL WBCI0095 USING RECORD-COUNT.
           CALL WBCI0095 USING RECORD-COUNT.
           GOBACK.
*********
*********