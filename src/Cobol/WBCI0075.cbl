       IDENTIFICATION DIVISION.
       PROGRAM-ID.       WBCI0075.
      ******************************************************************
      *                                                                *
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  1. GETS CALLED BY CWBWCOB1.                                   *
      *  2. OPENS EMPCHECK, READS A RECORD, CLOSES EMPCHECK            *
      *                                                                *
      *  USED TO GENERATE CALL DEPTH AND I/O FOR RUNTIME VISUALIZER.   *
      *                                                                *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHECK-FILE       ASSIGN TO EMPCHECK.
       DATA DIVISION.
       FILE SECTION.
       FD  CHECK-FILE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  CHECK-RECORD               PIC X(80).
       WORKING-STORAGE SECTION.
       01  CHECK-SAMPLE PIC X(80) VALUE 'SAMPLE RECORD'.
       01  RVWKEVAL     PIC X(8)  VALUE 'RVWKEVAL'.
       LINKAGE SECTION.
       01  PARMINFO.
           03  PARM-LTH           PIC S9(4) COMP.
           03  PARM-DATA          PIC X(5).
       PROCEDURE DIVISION USING PARMINFO.
       0000-MAINLINE.
           PERFORM 9000-OPEN.
           PERFORM 9100-READ-RECORD.
           CALL RVWKEVAL.
           PERFORM 9200-CLOSE.
           GOBACK.
*********
*********
       9000-OPEN.
           OPEN INPUT CHECK-FILE.
*********
*********
       9100-READ-RECORD.
           READ CHECK-FILE INTO CHECK-SAMPLE.
*********
*********
       9200-CLOSE.
           CLOSE CHECK-FILE.