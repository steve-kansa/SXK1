       IDENTIFICATION DIVISION.
       PROGRAM-ID.       WBCI0040.
      ******************************************************************
      *                                                                *
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  1. GETS CALLED BY WBCI1206.                                   *
      *  2. CALLS CWBWCOB1.                                            *
      *                                                                *
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
*********
*********
       WORKING-STORAGE SECTION.
       01  WS-SYSUT1-STATUS           PIC XX       VALUE '  '.
       01  WS-SALES-RECORD            PIC X(60).
       01  PGM-NAME.
           05  P94COMM    PIC X(8)      VALUE 'P94COMM'.
*********
*********
       LINKAGE SECTION.
       01  RECORD-DATA        PIC X(80).
       01  SALES-RECORD       PIC X(60).
*********
*********
       PROCEDURE DIVISION USING RECORD-DATA,
                                SALES-RECORD.
       0000-MAINLINE.
           MOVE SALES-RECORD TO WS-SALES-RECORD.
           CALL P94COMM USING RECORD-DATA
                               WS-SALES-RECORD.
           GOBACK.