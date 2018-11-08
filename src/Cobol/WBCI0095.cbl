       IDENTIFICATION DIVISION.
       PROGRAM-ID.       WBCI0095.
      ******************************************************************
      *                                                                *
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  1. RETURNS.                                                   *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE   ASSIGN TO EMPOUT.
       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(80).
       WORKING-STORAGE SECTION.
       01  PGM-NAME.
           05  RVII00XX    PIC X(8)      VALUE 'RVII00XX'.
       01  SAMPLE-RECORD   PIC X(80)     VALUE 'SAMPLE'.
       01  LOOP-COUNT      PIC 99        VALUE 0.
       LINKAGE SECTION.
       01  PARMINFO.
           03  RECORD-COUNT       PIC 99.
       PROCEDURE DIVISION USING PARMINFO.
       0000-MAINLINE.
           OPEN OUTPUT REPORT-FILE.
           PERFORM 200-WRITE
             VARYING LOOP-COUNT FROM 1 BY 1
             UNTIL LOOP-COUNT > RECORD-COUNT.
           CLOSE REPORT-FILE.
           GOBACK.
*********
*********
       200-WRITE.
           WRITE REPORT-RECORD FROM SAMPLE-RECORD.
