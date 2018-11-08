       IDENTIFICATION DIVISION.
       PROGRAM-ID.       WBCI0080.
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
           05  RVWKSORT    PIC X(8)      VALUE 'RVWKSORT'.
       LINKAGE SECTION.
       01  PARMINFO.
           03  RECORD-DATA        PIC X(80).
       PROCEDURE DIVISION USING PARMINFO.
       0000-MAINLINE.
           CALL RVWKSORT.
           CALL RVWKSORT.
           GOBACK.
*********
*********