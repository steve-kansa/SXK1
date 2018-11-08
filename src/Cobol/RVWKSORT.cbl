       IDENTIFICATION DIVISION.
       PROGRAM-ID.       RVWKSORT.
      ******************************************************************
      *                                                                *
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  1. GETS CALLED BY VARIOUS.                                    *
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
       01  PGM-NAME.
           05  RVWKSOR     PIC X(8)      VALUE 'RVWKSORT'.
*********
*********
       LINKAGE SECTION.
       01  PARMINFO.
           03  RECORD-DATA        PIC X(80).
*********
*********
       PROCEDURE DIVISION USING PARMINFO.
       0000-MAINLINE.
           GOBACK.