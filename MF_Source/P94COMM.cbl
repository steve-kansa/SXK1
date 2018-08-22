
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       P94COMM.

      *****************************************************************
      *                 PROCESS NIGHTLY SALES DATA                    *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   P94COMM                                           *
      *                                                               *
      * FUNCTION:   PASSED A WORK ORDER LIST, IT DETERMINES THE       *
      *             OPTIMAL PROCESSING PATH AND THEN WRITES THE       *
      *             RESULTS TO SALES FILE.  CUMULATIVE DATA           *
      *             IS WRITTEN TO EMPSTAT.                            *
      *                                                               *
      *             EMPSTAT IS PROCESSED LATER TO DETERMINE THE       *
      *             WINNER OF THE WORK ORDER LIST - THE WORK ORDER    *
      *             PATH THAT REQUIRES THE LEAST RESOURCES TO FULFILL *
      *                                                               *
      *                                                               *
      * FILES   :   EMPIRICAL STATUS FILE (VSAM)      (OUTPUT)        *
      *             SALES COMP FILE (VSAM)            (I-O)           *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      * DATE        UPDATED BY            CHANGE DESCRIPTION          *
      * ----------  --------------------  --------------------------  *
      * 12/14/2005  PAUL BARON            ELIMINATE USE OF THE FIELDS *
      *                                   CUSTOMER-TOTAL-DOLLAR-AMT-R *
      *                                   PDAS01-ORDER-DOLLAR-AMT-R   *
      *                                                               *
      *                                   CHANGE SCENARIOS #2 AND #22 *
      *                                   TO REFERENCE AS "STORAGE    *
      *                                   OVERLAY" NOT "STORAGE       *
      *                                   VIOLATION", CHANGE WS-16 AND*
      *                                   LS-16 ARRAYS TO BE MORE     *
      *                                   DESCRIPTIVE, CHANGE THE 1   *
      *                                   BYTE ARRAY ENTRIES TO BE    *
      *                                   MORE REALISTIC I.E. ITEM    *
      *                                   STATUS INDICATORS           *
      *                                                               *
      * MM/DD/YYYY  XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO SALES
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS SALES-ID
              FILE STATUS IS WS-SALES-STATUS.
           SELECT REPORT-FILE   ASSIGN TO EMPSTAT.
       DATA DIVISION.
       FILE SECTION.
       FD  SALES-FILE.
       01  SALES-REC.
           05 SALES-ID  PIC X(8).
           05 FILLER    PIC X(52).
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(80).
       WORKING-STORAGE SECTION.
      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB                      PIC S9(04)  COMP   VALUE +0.
       77  WS-SUB1                     PIC S9(04)  COMP   VALUE +0.
       77  WS-SUB2                     PIC S9(04)  COMP   VALUE +0.
       77  LS-SUB                      PIC S9(04)  COMP   VALUE +0.
       77  WS-MAX-PARAMETERS           PIC S9(04)  COMP   VALUE +500.
       77  WS-USERID-PARM-COUNT        PIC S9(04)  COMP   VALUE +0.
       77  WS-RETURN-CODE              PIC  9(04)  COMP   VALUE  0.
       77  WS-PARAMETER-RECORDS-IN     PIC S9(05)  COMP-3 VALUE +0.
       77  WS-COUNT                    PIC S9(04)  COMP   VALUE +0.
       77  WS-SUPPLIER-COUNT           PIC S9(07)  COMP-3 VALUE +0.
       77  WS-CAT-SUB                  PIC S9(04)  COMP   VALUE +0.
       77  WS-SUBCAT-SUB               PIC S9(04)  COMP   VALUE +0.
       77  WS-STATUS-ARRAY-MAX         PIC S9(04)  COMP   VALUE +17.
       77  WS-COUNTER                  PIC S9(04)  COMP-3 VALUE 0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'Y'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-END-OF-PARM-FILE-SW  PIC X(01)             VALUE 'Y'.
               88  END-OF-PARM-FILE                          VALUE 'Y'.
               88  NOT-END-OF-PARM-FILE                      VALUE 'N'.

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-PARM-ERROR-FOUND-SW  PIC X(01)             VALUE 'N'.
               88  PARM-ERROR-FOUND                          VALUE 'Y'.
               88  NOT-PARM-ERROR-FOUND                      VALUE 'N'.


       01  WS-SYSUT1-STATUS           PIC XX       VALUE '  '.
       01  WS-SALES-STATUS            PIC XX       VALUE '  '.
           88 NOT-FOUND                           VALUE '23'.
       01  WS-RECORD                  PIC X(60).
       01  WS-SALES-POINT-COMMISSION PIC 9(6)V99 VALUE ZEROES.
       01  SWITCHES.
           05  JUMP-SW                PIC X        VALUE 'N'.
               88  JUMPING                         VALUE 'Y'.
           05  REWRITE-SW             PIC X        VALUE 'R'.
               88 REWRITING                        VALUE 'R'.
               88 WRITING                          VALUE 'W'.
           05  EOF-SW                 PIC X        VALUE 'N'.
               88  END-OF-FILE                     VALUE 'Y'.
           05  EOF-SW2                PIC X        VALUE 'N'.
               88  END-OF-FILE2                    VALUE 'Y'.
           05  REGION-ERROR-SW        PIC X        VALUE 'N'.
               88  INVALID-REGION                  VALUE 'Y'.
               88  VALID-REGION                    VALUE 'N'.
           05  PARM-ERROR-SW          PIC X        VALUE 'N'.
               88  BAD-PARM                        VALUE 'Y'.
               88  GOOD-PARM                       VALUE 'N'.
           05  END-OF-MONTH-SW        PIC X        VALUE 'N'.
               88  END-OF-MONTH                    VALUE 'Y'.
           05  ANNIVERSARY-IND        PIC X        VALUE 'N'.
               88 ANNIVERSARY                      VALUE 'Y'.
       01  COUNTERS.
           05  PAGE-COUNT             PIC 9(3)     VALUE 1.
           05  EMP-LINE-COUNT         PIC S99      VALUE +56.
           05  REG-LINE-COUNT         PIC S99      VALUE +56.
           05  START-NUMBER           PIC 999.
           05  RECORDS-READ           PIC 999      VALUE 0.
           05  NORTH-COUNT            PIC 9(2)     VALUE 0.
           05  SOUTH-COUNT            PIC 9(2)     VALUE 0.
           05  EAST-COUNT             PIC 9(2)     VALUE 0.
           05  WEST-COUNT             PIC 9(2)     VALUE 0.
       01  RETCODE                    PIC 9        VALUE 0.
       01  RC-STATUS                  PIC XX       VALUE SPACES.
       01  REGION-SUB                 PIC 9        VALUE 0.
       01  TODAYS-DATE                PIC X(6).
       01  DATE-FIELDS REDEFINES TODAYS-DATE.
           05  DATE-YEAR              PIC 9(2).
           05  DATE-MONTH             PIC 9(2).
           05  DATE-DAY               PIC 9(2).
       01  HIGH-VALUE-SW              PIC X        VALUE HIGH-VALUE.
           88 LOW-VALUE-IND                        VALUE LOW-VALUE.
********
********  HOLD EMPLOYEE DETAIL PRINT LINES UNTIL READY TO PRINT
********  EMPLOYEE COMPENSATION REPORT.  THE DATA IS STORED BY
********  REGION AND THEN BY SEQUENCE IN EMPLOYEE FILE.
********
       01  HOLD-TABLE.
           05  HOLD-AREA        OCCURS 4 TIMES
                                INDEXED BY REG-IX.
               10  HOLD-LINE    OCCURS 20 TIMES
                                INDEXED BY HOLD-IX.
                   15  HOLD-ANNIV              PIC X.
                   15  HOLD-REGION             PIC X(5).
                   15  HOLD-TYPE               PIC X.
                   15  HOLD-NAME               PIC X(15).
                   15  HOLD-WAGES              PIC 9(5)V99.
                   15  HOLD-OT                 PIC 9(5)V99.
                   15  HOLD-COMM               PIC 9(5)V99.
                   15  HOLD-TOTAL              PIC 9(5)V99.
********
********  STORES THE NAME OF EACH REGION
********
       01  REGION-NAME-TABLE.
           05  FILLER            PIC X(5)    VALUE 'NORTH'.
           05  FILLER            PIC X(5)    VALUE 'SOUTH'.
           05  FILLER            PIC X(5)    VALUE 'EAST '.
           05  FILLER            PIC X(5)    VALUE 'WEST '.
       01  REGION-TABLE     REDEFINES REGION-NAME-TABLE.
           05  REGION-ID         PIC X(5)  OCCURS 4 TIMES.
********
********  STORES REGIONAL INFORMATION THAT IS USED TO PRINT THE
********  REGIONAL SALES REPORT.  REGION SALES IS A SUM OF ALL SALES
********  FOR THE REGION AND IS USED TO CALCULATE MANAGER COMMISSION
********  THE COMMENT FIELD IS USED TO FLAG A REGION IF AN EMPLOYEE IN
********  THE REGION HAS 0 SALES.
********
       01  REGION-SALES-TABLE.
           05  REGION-DATA         OCCURS 4 TIMES.
               10  REGION-NAME       PIC X(5).
               10  REGION-MANAGER    PIC X(15).
               10  REGION-SALARY     PIC 9(4)V99.
               10  REGION-SALES      PIC 9(6)V99.
               10  REGION-COMMENT    PIC X(5).
********
********  FIELDS USED BY CALLED PROGRAM CWAASUBC TO CALCULATE
********  COMMISSION BASED ON SALES AMOUNT
********
       01  CALC-COMMISSION-FIELDS.
           05  EMP-TYPE              PIC X.
           05  CALC-SALES            PIC 9(6)V99           VALUE 0.
           05  CALC-COMMISSION       PIC 9(5)V99  COMP-3   VALUE 0.
********
********  ACCUMULATORS USED FOR CALCULATING HOURLY EMPLOYEE WAGES,
********  TOTAL EMPLOYEE COMPENSATION (SALARY PLUS COMMISSION OR
********  HOURLY EMPLOYEE WAGES PLUS OVERTIME), AND TOTAL MANAGEMENT
********  COMPENSATION (SALARY PLUS COMMISSION BASED ON TOTAL SALES
********  FOR THE REGION)
********
       01  TOTAL-FIELDS.
           05  EMP-WAGES             PIC 9(5)V99    COMP-3.
           05  EMP-COMPENSATION      PIC 9(5)V99    COMP-3.
           05  MGMT-COMPENSATION     PIC 9(5)V99    COMP-3.
********
********  TOTAL COMPENSATION GIVEN TO ALL EMPLOYEES (HOURLY AND SALES)
********  OR MANAGEMENT.  EACH SUM IS PRINTED AT THE END OF THEIR
********  RESPECTIVE REPORTS.
********
       01  GRAND-TOTAL-FIELDS.
           05  GRAND-TOTAL-EMP       PIC 9(7)V99   COMP-3  VALUE 0.
           05  GRAND-TOTAL-MGMT      PIC 9(7)V99   COMP-3  VALUE 0.
********
********  USED FOR CALCULATING OVERTIME FOR ANY HOURLY EMPLOYEE
********  WHOSE HOURS EXCEEDS 40
********
       01  OVERTIME-FIELDS.
           05  OT-AMOUNT             PIC 9(5)V99    COMP-3.
           05  OT-HOURS              PIC 9(2).
********
********  EMPLOYEE RECORD WORK-AREA.
********
       01  EMPLOYEE-WORK-AREA.
           05  WA-EMP-NUM            PIC 9(5).
           05  WA-EMP-TYPE           PIC X.
               88  HOURLY            VALUE 'H'.
               88  SALES             VALUE 'S'.
               88  MANAGEMENT        VALUE 'M'.
           05  WA-EMP-REGION         PIC 9.
               88  NORTH             VALUE 1.
               88  SOUTH             VALUE 2.
               88  EAST              VALUE 3.
               88  WEST              VALUE 4.
           05  WA-EMP-NAME           PIC X(15).
           05  WA-EMP-ADDRESS.
               10  WA-EMP-STREET     PIC X(15).
               10  WA-EMP-CITY       PIC X(8).
               10  WA-EMP-STATE      PIC XX.
               10  WA-EMP-ZIP        PIC X(9).
           05  FILLER                PIC X(13).
           05  WA-EMP-HIRE-DATE.
               10  WA-EMP-HIRE-MONTH PIC 9(2) VALUE 0.
               10  WA-EMP-HIRE-DAY   PIC 9(2) VALUE 0.
               10  WA-EMP-HIRE-YEAR  PIC 9(2) VALUE 0.
           05  WA-EMP-BONUS          PIC X(2).
           05  FILLER                PIC X(3).
********
********  EMPLOYEE SALARY AREA. EMPLOYEE DATA IS REDEFINED
********  BASED ON ONE OF THE 3 EMPLOYEE TYPES, HOURLY, SALES OR
********  MANAGEMENT.
********
       01  EMPLOYEE-SALARY-AREA.
           05  SA-EMP-NUM            PIC 9(5).
           05  SA-HOURLY-EMPLOYEE-DATA.
               10  SA-EMP-HOURS      PIC 9(2).
               10  SA-EMP-RATE       PIC 9(3)V99     COMP-3.
               10  FILLER            PIC X(70).
           05  SA-SALES-EMPLOYEE-DATA   REDEFINES
                                        SA-HOURLY-EMPLOYEE-DATA.
               10  SA-SALES-SALARY   PIC 9(5)V99     COMP-3.
               10  SA-SALES-AMOUNT   PIC 9(5)V99.
               10  FILLER            PIC X(55).
           05  SA-MGMT-EMPLOYEE-DATA   REDEFINES
                                        SA-SALES-EMPLOYEE-DATA.
               10  SA-MGMT-SALARY    PIC 9(5)V99     COMP-3.
               10  FILLER            PIC X(71).
*********
*********  EMPLOYEE COMPENSATION REPORT
*********
       01  EMPLOYEE-HDR1.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(10)
                              VALUE  'RUN DATE  '.
           05  EMP-RUN-MONTH
                           PIC Z9.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-DAY
                           PIC 99.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-YEAR
                           PIC 99.
           05  FILLER      PIC X(6)      VALUE SPACES.
           05  FILLER      PIC X(28)
                              VALUE  'EMPLOYEE COMPENSATION REPORT'.
           05  FILLER      PIC X(18)     VALUE SPACES.
           05  FILLER      PIC X(05)     VALUE 'PAGE '.
           05  EMP-PAGE    PIC ZZ9.
       01  EMPLOYEE-HDR2.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(13)     VALUE 'EMPLOYEE NAME'.
           05  FILLER      PIC X(4)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'REGION'.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'TYPE'.
           05  FILLER      PIC X(6)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'SALARY'.
           05  FILLER      PIC X(6)      VALUE SPACES.
           05  FILLER      PIC X(8)      VALUE 'OVERTIME'.
           05  FILLER      PIC X(4)      VALUE SPACES.
           05  FILLER      PIC X(10)     VALUE 'COMMISSION'.
           05  FILLER      PIC X(5)      VALUE SPACES.
           05  FILLER      PIC X(5)      VALUE 'TOTAL'.
       01  EMPLOYEE-DTL.
           05  FILLER          PIC X         VALUE SPACES.
           05  EMP-DTL-ANNIV-IND
                               PIC X.
           05  EMP-DTL-NAME    PIC X(15).
           05  FILLER          PIC X(2)      VALUE SPACES.
           05  EMP-DTL-REGION  PIC X(5).
           05  FILLER          PIC X(4)      VALUE SPACES.
           05  EMP-DTL-TYPE    PIC X.
           05  FILLER          PIC X(5)      VALUE SPACES.
           05  EMP-DTL-WAGES   PIC ZZZZ9.99.
           05  FILLER          PIC X(4)      VALUE SPACES.
           05  EMP-DTL-OT      PIC ZZZZ9.99.
           05  FILLER          PIC X(6)      VALUE SPACES.
           05  EMP-DTL-COMM    PIC ZZZZ9.99.
           05  FILLER          PIC X(4)      VALUE SPACES.
           05  EMP-DTL-TOTAL   PIC ZZZZ9.99.
       01  EMP-TOTAL-DTL.
           05  FILLER            PIC X(4)      VALUE SPACES.
           05  FILLER            PIC X(5)      VALUE 'TOTAL'.
           05  FILLER            PIC X(61)     VALUE SPACES.
           05  EMP-GRAND-TOTAL   PIC ZZZZZZ9.99.
*********
*********  REGIONAL SALES REPORT
*********
       01  REGION-HDR1.
           05  FILLER      PIC X(2)   VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'RUN DATE  '.
           05  REG-RUN-MONTH
                           PIC Z9.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-DAY PIC 99.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-YEAR
                           PIC 99.
           05  FILLER      PIC X(10)  VALUE SPACES.
           05  FILLER      PIC X(21)  VALUE  'REGIONAL SALES REPORT'.
           05  FILLER      PIC X(21)  VALUE SPACES.
           05  FILLER      PIC X(05)  VALUE 'PAGE '.
           05  REG-PAGE    PIC ZZ9.
       01  REGION-HDR2.
           05  FILLER      PIC XX     VALUE SPACES.
           05  FILLER      PIC X(7)   VALUE 'MANAGER'.
           05  FILLER      PIC X(9)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'REGION'.
           05  FILLER      PIC X(2)   VALUE SPACES.
           05  FILLER      PIC X(11)  VALUE 'TOTAL SALES'.
           05  FILLER      PIC X(6)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'SALARY'.
           05  FILLER      PIC X(5)   VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'COMMISSION'.
           05  FILLER      PIC X(3)   VALUE SPACES.
           05  FILLER      PIC X(5)   VALUE 'TOTAL'.
       01  REGION-DETAIL.
           05  FILLER             PIC X(2)      VALUE SPACES.
           05  REG-DTL-MANAGER    PIC X(15).
           05  FILLER             PIC X(1)      VALUE SPACES.
           05  REG-DTL-REGION     PIC X(5).
           05  FILLER             PIC X(4)      VALUE SPACES.
           05  REG-DTL-SALES      PIC ZZZZZ9.99.
           05  FILLER             PIC X(6)      VALUE SPACES.
           05  REG-DTL-SALARY     PIC ZZZ9.99.
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-COMM       PIC ZZZZ9.99.
           05  FILLER             PIC X(3)      VALUE SPACES.
           05  REG-DTL-TOTAL      PIC ZZZZ9.99.
           05  FILLER             PIC X(1)      VALUE SPACES.
           05  REG-DTL-COMMENT    PIC X(5).
       01  MGMT-TOTAL-DTL.
           05  FILLER             PIC X(4)      VALUE SPACES.
           05  FILLER             PIC X(5)      VALUE 'TOTAL'.
           05  FILLER             PIC X(54)     VALUE SPACES.
           05  MGMT-GRAND-TOTAL   PIC ZZZZZZ9.99.
*********
*********  ERROR MESSAGE LINE
*********
       01  ERROR-LINE             PIC X(80).
*********
*********  BLANK LINE TO CONTROL SPACING OF REPORTS
*********
       01  BLANK-LINE             PIC X(80)   VALUE SPACES.
       01  PGM-NAME.
           05  CWAADATE    PIC X(8)      VALUE 'CWAADATE'.
           05  CWAAHOUR    PIC X(8)      VALUE 'CWAAHOUR'.
           05  CWAADATA    PIC X(8)      VALUE 'CWAADATA'.
           05  CWAASUBC    PIC X(8)      VALUE 'CWAASUBC'.
           05  ABENDIT     PIC X(8)      VALUE 'ABENDIT'.
           05  WBCI0075    PIC X(8)      VALUE 'WBCI0075'.
           05  RVWKEVAL    PIC X(8)      VALUE 'RVWKEVAL'.
*********
*********  PARM IS AN OPTIONAL FIELD USED TO START PROCESSING
*********  AT A PARTICULAR RECORD IN THE EMPLOYEE FILE.  VALID
*********  VALUES FOR PARM-DATA ARE:
*********
*********        VALUE           FUNCTION
*********     - SPACES OR 00001  BEGIN PROCESSING FROM FIRST RECORD.
*********                          CAUSES S0C7 ABEND BECAUSE FIRST
*********                          RECORD CONTAINS INVALID DATA.
*********     - 00002            BEGIN PROCESSING FROM SECOND RECORD.
*********                          CAUSES S0C7 ABEND BECAUSE SECOND
*********                          RECORD CONTAINS INVALID REGION
*********                          CREATING TABLE OVERFLOW CONDITION.
*********     - 00003 THRU 00019 BEGIN PROCESSING AT SPECIFIED RECORD
*********                          UNTIL END-OF-FILE.  BYPASSES S0C7
*********                          CAUSED BY RECORDS 00001 AND 00002.
*********     - 00016 THRU 00019 PROCESS MANAGEMENT RECORDS AND PRINT
*********                          REGIONAL SALES REPORT ONLY.  IF
*********                          PARM > 16, ONE OR MORE MANAGER NAMES
*********                          WILL NOT APPEAR ON THE REPORT.
*********     - 00020 OR ABOVE   PRINTS MANAGEMENT REPORT WITHOUT
*********                          MANAGER NAMES AND ZERO VALUES.
*********     - NON NUMERIC OR   PRINTS ERROR MESSAGE AND SKIPS ALL
*********         < 5 CHARACTERS   PROCESSING.
*********

      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-CATEGORY            PIC X(32)   VALUE 'BOLTS'.
           05  WMF-SUB-CATEGORY        PIC X(32)   VALUE 'ANCHOR'.
           05  WMF-CUSTOMER-ID         PIC X(32)   VALUE 'ARROW'.
           05  WMF-ITEM                PIC X(32)   VALUE '1000'.
           05  WMF-USERID              PIC X(08)   VALUE 'USERIDXX'.
           05  WMF-PO-NUMBER           PIC  9(13)  VALUE 998123 COMP-3.

           05  WMF-CUSTOMR-STATUS      PIC X(02)   VALUE '00'.
           05  WMF-PENDORD-STATUS      PIC X(02)   VALUE '00'.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
PWB305     05  WMF-MAX-DAYS            PIC S9(03)  VALUE +366.
PWB305     05  WMF-MAX-DAYS-PER-MTH    PIC  9(03)  VALUE 31.

           05  WMF-DATE-YYMMDD.
               10 WMF-DATE-YY          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-MM          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-DD          PIC 9(02)   VALUE ZEROES.

           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(80)   VALUE SPACES.

           05  WMF-ITEM-NUMBER         PIC S9(08)  VALUE +0  COMP-3.
           05  WMF-USERID-NUMBER       PIC S9(09)  VALUE +0  COMP.
           05  WMF-NULL-IND            PIC S9(04)  VALUE +0  COMP.
           05  WMF-ITEM-SEQ            PIC S9(04)  VALUE +0  COMP.
           05  WMF-ORDER-TOTAL-AMOUNT  PIC S9(11)V99
                                                   VALUE +0  COMP-3.
           05  WMF-EXTENDED-PRICE      PIC S9(11)V99
                                                   VALUE +0  COMP-3.

           05  WMF-ACTIVE-SCENARIOS    PIC X(250)  VALUE SPACES.
           05  WMF-ACTIVE-SCENARIOS-R  REDEFINES WMF-ACTIVE-SCENARIOS
                                       OCCURS 250 TIMES
                                       PIC X(01).

      *****************************************************************
      *  THIS AREA CONTAINS THE DATA FROM THE FUNCTION CURRENT-DATE   *
      *****************************************************************

       01  WS-CURRENT-DATE-TIME.
           03  WS-CDT-DATE.
               05  WS-CDT-D-YEAR       PIC 9(4)  VALUE ZEROES.
               05  WS-CDT-D-MONTH      PIC 99    VALUE ZEROES.
               05  WS-CDT-D-DAY        PIC 99    VALUE ZEROES.
           03  WS-CDT-TIME.
               05  WS-CDT-T-HOURS      PIC 99    VALUE ZEROES.
               05  WS-CDT-T-MINUTES    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-SECONDS    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-HUNDRETHS  PIC 99    VALUE ZEROES.
           03  WS-CDT-GMT-INDICATOR    PIC X     VALUE SPACES.
               88  AHEAD-OF-GMT                  VALUE '+'.
               88  BEHIND-GMT                    VALUE '-'.
               88  GMT-NOT-AVAILABLE             VALUE '0'.
           03  WS-CDT-GMT-TIME-DIFFERENTIAL.
               05  WS-CDT-GMT-HOURS    PIC 99    VALUE ZEROES.
               05  WS-CDT-GMT-MINUTES  PIC 99    VALUE ZEROES.

PWB305 01  WS-CURRENT-DATE-TIME-R      REDEFINES WS-CURRENT-DATE-TIME.
PWB305     05  WS-CDT-DATE-R           PIC X(08).
PWB305     05  WS-CDT-TIME-R           PIC X(08).
PWB305     05  FILLER                  PIC X(01).
PWB305     05  FILLER                  PIC X(04).

           COPY SALESCMP.

      *****************************************************************
      *  SUBROUTINE PARAMETER AREAS                                   *
      *****************************************************************

       01  WS-PDAS02                   PIC X(8)    VALUE 'PDAS02'.

           COPY PDAS01CY.
       01  PDAS01                      PIC X(8)    VALUE 'PDAS01'.

       01  PDAS03                      PIC X(8)    VALUE 'PDAS03'.
       01  PDAS03-PARMS.
           03  PDAS03-AGE-DAYS         PIC 9(5)    VALUE ZEROES.
           03  PDAS03-MESSAGE          PIC X(15)   VALUE SPACES.


      *****************************************************************
      *    PARAMETER RECORD LAYOUTS                                   *
      *****************************************************************

       01  WS-PARAMETER-RECORD.
           05  WPR-RECORD-TYPE         PIC X(01).
               88  WPR-ADD-ORDER       VALUE 'A'.
               88  WPR-CHANGE-ORDER    VALUE 'C'.
               88  WPR-DELETE-ORDER    VALUE 'D'.
               88  WPR-SCENARIO        VALUE 'S'.
               88  WPR-USERID          VALUE 'U'.
           05  FILLER                  PIC X(01).
           05  WPR-RECORD-DATA         PIC X(78).
           05  WPR-RECORD-DATA-ORDER   REDEFINES WPR-RECORD-DATA.
               10  WPR-ORDER-NUMBER    PIC X(10).
               10  WPR-ORDER-NUMBER-R  REDEFINES WPR-ORDER-NUMBER
                                       PIC 9(10).
               10  FILLER              PIC X(68).
           05  WPR-RECORD-DATA-SCENARIO
                                       REDEFINES WPR-RECORD-DATA.
               10  WPR-SCENARIO-NUMBER PIC X(03).
               10  WPR-SCENARIO-NUMBER-R
                                       REDEFINES WPR-SCENARIO-NUMBER
                                       PIC 9(03).
               10  FILLER              PIC X(75).
           05  WPR-RECORD-DATA-USERID  REDEFINES WPR-RECORD-DATA.
               10  WPR-USERID-VALUE    PIC X(08).
               10  FILLER              PIC X(70).


      *****************************************************************
      *    PARAMETER RECORD ARRAY                                     *
      *****************************************************************
       01  WS-PARAMETER-RECORD-ARRAY.
           05  WPRA-RECORD             OCCURS 500 TIMES
                                       PIC X(80).


      *****************************************************************
      *    VSAM FILE DEFINITIONS                                      *
      *****************************************************************

KCS305**** COPY VPENDORD.


KCS305**** COPY VCUSTOMR.



      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

      *****************************************************************
      *    IMS FUNCTION DEFINITIONS                                   *
      *****************************************************************

       01  IMS-CALL-FUNCTIONS.
           05 ICF-GHU                  PIC X(04)   VALUE 'GHU'.
           05 ICF-GN                   PIC X(04)   VALUE 'GN'.
           05 ICF-ISRT                 PIC X(04)   VALUE 'ISRT'.
           05 ICF-REPL                 PIC X(04)   VALUE 'REPL'.
           05 ICF-DLET                 PIC X(04)   VALUE 'DLET'.

      *****************************************************************
      *    IMS SEGMENT SEARCH ARGUMENTS (SSA)                         *
      *****************************************************************

       01  ORDER-SSA-QUAL.
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.
           03  FILLER                  PIC X     VALUE '('.
           03  FILLER                  PIC X(8)  VALUE 'ORDKEY'.
           03  FILLER                  PIC XX    VALUE ' ='.
           03  OSQ-ORDER-KEY.
               05  OSQ-ORDER-PREFIX    PIC 9(5)  VALUE ZEROES.
               05  OSQ-ORDER-NUMBER    PIC 9(10) VALUE ZEROES.
           03  FILLER                  PIC X     VALUE ')'.

       01  ORDER-SSA-UNQUAL.
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.
           03  FILLER                  PIC X     VALUE SPACES.

       01  ORDER-ITEM-SSA-UNQUAL.
           03  FILLER                  PIC X(8)  VALUE 'ORDITEM'.
           03  FILLER                  PIC X     VALUE SPACES.


      *****************************************************************
      *    IMS SEGMENT I/O AREAS                                      *
      *****************************************************************

           COPY IORDER.


           COPY IORDITEM.


      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************

      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************



      *****************************************************************
      *         USER IDENTIFICATION TABLE        -- DCLGEN DUSERID    *
      *****************************************************************



      *****************************************************************
      *         ITEM TABLE                       -- DCLGEN DITEM      *
      *****************************************************************



      *****************************************************************
      *         SUPPLIER TABLE                   -- DCLGEN DSUPPLR    *
      *****************************************************************



      *****************************************************************
      *         PURCHASE TYPE TABLE              -- DCLGEN DPURTYP    *
      *****************************************************************



      *****************************************************************
      *         ITEM SUPPLIER TABLE              -- DCLGEN DITMSUP    *
      *****************************************************************



      *****************************************************************
      *         DB2 CURSORS                                           *
      *****************************************************************

      *****************************************************************
      *         ITEM TABLE CURSOR                                     *
      *****************************************************************


      *****************************************************************
      *         DB2 STORED PROCEDURE PARAMETER / WORK AREAS           *
      *****************************************************************

       01  PDASP1-PARAMETERS.
           05  PDASP1-PREFIX           PIC X(05)          VALUE ZEROES.
           05  PDASP1-PREFIX-R         REDEFINES PDASP1-PREFIX
                                       PIC 9(05).
           05  PDASP1-TOTAL-COST       PIC S9(15)V99 COMP-3 VALUE +0.
           05  PDASP1-STATUS           PIC X(04)          VALUE SPACES.


       01  PDASP2-PARAMETERS.
           05  PDASP2-USERID           PIC X(08)          VALUE SPACES.
           05  PDASP2-USERID-NUMBER    PIC S9(09)  COMP   VALUE +0.
           05  PDASP2-ACTIVE-SCENARIOS PIC X(250)         VALUE SPACES.
           05  PDASP2-STATUS           PIC X(04)          VALUE SPACES.

      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS                        *
      *****************************************************************
      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      *                                                                *
      * ERROR WORK AREA DEFINITIONS FOR: CICS, IMS-DLI, DB2, MQSERIES  *
      *                                                                *
      ******************************************************************

       01  WS-PDA-ERROR-GENERAL.

           05  WS-PDA-ERROR-TYPE       PIC X(04)       VALUE SPACES.
               88  PDA-GENERAL-ERROR                   VALUE 'GEN'.
               88  PDA-DB2-ERROR                       VALUE 'DB2'.
               88  PDA-IMS-ERROR                       VALUE 'IMS'.
               88  PDA-MQSERIES-ERROR                  VALUE 'MQS'.


      ******************************************************************
      *    PDA FORMATTED ERROR LINES                                   *
      ******************************************************************

       01  WS-PDA-ERROR-AREA.
           05  WPEA-ERROR-01           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-02.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-03.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE
               '   PRODUCT DEMONSTRATION APPLICATION (PDA) ERROR '.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-04.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-05           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-06.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-07.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-07-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-08.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-08-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-09.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-10           PIC X(80)       VALUE ALL '*'.


      ******************************************************************
      *    PDA GENERAL ERROR LINES                                     *
      ******************************************************************

       01  WS-PDA-GEN-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(07)       VALUE
               'ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPGE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPGE-PARAGRAPH          PIC X(06).
           05  FILLER                  PIC X(32)       VALUE SPACES.

       01  WS-PDA-GEN-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  WPGE-DESCRIPTION        PIC X(78)       VALUE SPACES.


      ******************************************************************
      *    PDA IMS-DLI ERROR LINES                                     *
      ******************************************************************

       01  WS-PDA-IMS-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE
               'IMS-DLI ERROR: '.
           05  FILLER                  PIC X(08)       VALUE
               'PROGRAM='.
           05  WPIE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', PARAGRAPH='.
           05  WPIE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE
               ', STATUS='.
           05  WPIE-STATUS-CODE        PIC X(2)        VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', FUNCTION='.
           05  WPIE-FUNCTION-CODE      PIC X(4)        VALUE SPACES.

       01  WS-PDA-IMS-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(08)       VALUE
               'SEGMENT='.
           05  WPIE-SEGMENT-NAME       PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               ', DATABASE='.
           05  WPIE-DATABASE-NAME      PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE
               ', COMMAND='.
           05  WPIE-COMMAND            PIC X(32)       VALUE SPACES.


      ******************************************************************
      *    PDA DB2 ERROR LINES                                         *
      ******************************************************************

       01  WS-PDA-DB2-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'DB2 ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPDE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', RETCODE = '.
           05  WPDE-DB2-RETCODE        PIC ZZZZZZ9-.
           05  FILLER                  PIC X(28)       VALUE SPACES.

       01  WS-PDA-DB2-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPDE-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPDE-FUNCTION-R         REDEFINES WPDE-FUNCTION.
               10  WPDE-FUNCTION-1     PIC X(15).
               10  WPDE-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPDE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE SPACES.


      ******************************************************************
      *    PDA MQSERIES ERROR LINES                                    *
      ******************************************************************

       01  WS-PDA-MQSERIES-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               'MQSERIES ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPME-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               ', REASON CODE = '.
           05  WPME-REASON-CODE        PIC ZZZZZZZZ9.
           05  FILLER                  PIC X(18)       VALUE SPACES.

       01  WS-PDA-MQSERIES-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPME-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPME-FUNCTION-R         REDEFINES WPME-FUNCTION.
               10  WPME-FUNCTION-1     PIC X(15).
               10  WPME-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPME-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(17)       VALUE SPACES.

      *****************************************************************
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       *
      *****************************************************************

       01  WS-PDAB06-MESSAGES.

           05  WPM-BLANK               PIC X(01)       VALUE     ' '.
           05  WPM-ALL-ASTERISK        PIC X(80)       VALUE ALL '*'.

           05  WPM-BEGIN-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN PROGRAM PDAB06 *****'.

           05  WPM-END-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** END PROGRAM PDAB06 *****'.

           05  WPM-VSAM-ERROR.
               10 FILLER               PIC X(21)   VALUE
                  'VSAM ERROR ON FILE - '.
               10 WPM-VSAM-ERROR-FILE  PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',FILE STATUS = '.
               10 WPM-VSAM-ERROR-STATUS
                                       PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(12)   VALUE
                  ', COMMAND = '.
               10 WPM-VSAM-ERROR-COMMAND
                                       PIC X(19)   VALUE SPACES.

           05  WPM-PARAMETER-FILE-EMPTY.
               10 FILLER               PIC X(78)   VALUE
                  'INPUT PARAMETER FILE (IPARAMS) IS EMPTY - PARAMETERS
      -           'ARE REQUIRED'.

           05  WPM-MAX-PARAMETERS-EXCEEDED.
               10 FILLER               PIC X(48)   VALUE
                  'MAX NUMBER OF INPUT PARAMETER RECORDS EXCEEDED, '.
               10 FILLER               PIC X(14)   VALUE
                  'MAX ALLOWED = '.
               10 WPM-MAX-PARAMETERS   PIC ZZZZ9.
               10 FILLER               PIC X(11)   VALUE SPACES.

           05  WPM-PARM-INVALID-RECORD-TYPE.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 1 - RECORD TYPE MUST BE A,C,D,S OR U '.

           05  WPM-RECORD-NUMBER-MSG.
               10 FILLER               PIC X(16)   VALUE
                  'RECORD NUMBER = '.
               10 WPM-RECORD-NUMBER    PIC 9(05)   VALUE ZEROES.
               10 FILLER               PIC X(59)   VALUE SPACES.

           05  WPM-INVALID-ORDER-NUMBER.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 12, ORDER NUMBER MUST BE NUMERIC '.

           05  WPM-INVALID-SCENARIO-NUMBER.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 5, SCENARIO NUMBER MUST BE NUMERIC, VALU
      -           'E 1 THRU 250'.

           05  WPM-ORDER-NOT-FOUND-CHANGE.
               10 FILLER               PIC X(06)   VALUE
                  'ORDER '.
               10 WPM-ORDER-NUMBER-CHG PIC X(10)   VALUE ZEROES.
               10 FILLER               PIC X(64)   VALUE
                  ' NOT ON FILE, CHANGE UNSUCCESSFUL'.

           05  WPM-ORDER-NOT-FOUND-DELETE.
               10 FILLER               PIC X(06)   VALUE
                  'ORDER '.
               10 WPM-ORDER-NUMBER-DEL PIC X(10)   VALUE ZEROES.
               10 FILLER               PIC X(64)   VALUE
                  ' NOT ON FILE, DELETE UNSUCCESSFUL'.

           05  WPM-TABLE-OVERFLOW.
               10 FILLER               PIC X(25)   VALUE
                  'TABLE/ARRAY OVERFLOW ON: '.
               10 WPM-TABLE-NAME       PIC X(30)   VALUE SPACES.
               10 FILLER               PIC X(23)   VALUE SPACES.

           05  WPM-USERID-PARM-REQUIRED.
               10 FILLER               PIC X(78)   VALUE
                  'USER ID INPUT PARAMETER RECORD IS REQUIRED '.

           05  WPM-USERID-PARM-TOO-MANY.
               10 FILLER               PIC X(78)   VALUE
                  'ONLY 1 USER ID INPUT PARAMETER RECORD IS ALLOWED '.

           05  WPM-INVALID-USERID.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 10, USER ID IS REQUIRED '.

           05  WPM-USERID-NOT-FOUND.
               10 FILLER               PIC X(08)   VALUE
                  'USER ID '.
               10 WPM-USERID-VALUE     PIC X(08)   VALUE SPACES.
               10 FILLER               PIC X(62)   VALUE
                  ' NOT FOUND IN THE PDA APP., ADD THE ID USING THE PDA
      -           'CICS APP.'.

           05  WPM-PROGRAM-ERROR.
               10 FILLER               PIC X(29)   VALUE
                  'ERROR RETURNED FROM PROGRAM: '.
               10 WPM-PROGRAM-NAME     PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',RETURN CODE = '.
               10 WPM-RETURN-CODE      PIC X(10)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE SPACES.



      *****************************************************************
      *    PDA STANDARD CATEGORY / SUB-CATEGORY FOR THE APPLICATION   *
      *****************************************************************

           COPY PDACATGY.


      *****************************************************************
      *    PROGRAM INTERNAL USE ARRAYS CATEGORY, SUB-CATEGORY         *
      *****************************************************************

       01  WS-PDA-CATEGORY-ARRAY.
           05  WPCA-CATEGORY-MAX       PIC S9(05)   COMP-3  VALUE +100.
           05  WPCA-CATEGORY-COUNT     PIC S9(05)   COMP-3.
           05  WPCA-CATEGORY-GRP       OCCURS 1 TO 100 TIMES
                                       DEPENDING ON
                                         WPCA-CATEGORY-COUNT
                                           INDEXED BY WPCA-CAT-IX.
               10  WPCA-CATEGORY       PIC X(32).
               10  WPCA-SUB-CATEGORY   PIC X(32).



      *****************************************************************
      *    D E M O N S T R A T I O N    P U R P O S E S   O N L Y     *
      *    ARRAY NOT USED IN APPLICATION                              *
      *    PROGRAM INTERNAL USE ARRAY  SUB-CATEGORY                   *
      *****************************************************************

       01  WS-SUB-CATEGORY-ARRAY.
           05  WSCA-MAX-ENTRIES        PIC S9(05)   COMP-3  VALUE +100.
           05  WSCA-SUB-CATEGORY-COUNT PIC S9(05)   COMP-3.
           05  WSCA-SUB-CATEGORY-GRP   OCCURS 1 TO 100 TIMES
                                       DEPENDING ON
                                         WSCA-SUB-CATEGORY-COUNT
                                           INDEXED BY WSCA-SUBCAT-IX.
               10  WSCA-SUB-CATEGORY   PIC X(32).


      *****************************************************************
      *         DB2 TABLE ITEM STATUS CODES FROM VENDOR               *
      *         BLANK  = ACTIVE                                       *
      *         S      = SUSPENDED                                    *
      *         C      = CANCELLED                                    *
      *         N      = NO STOCK                                     *
      *                                                               *
      *         (DEMO PURPOSES ONLY NOT REALLY IN DB2 TABLE)          *
      *****************************************************************

       01  WS-VENDOR-ITEM-STATUS.
           05  VENDOR-ITEM-STATUS-CODE-GRP
                                       PIC X(12).
           05  VENDOR-ITEM-STATUS-CODE REDEFINES
                                       VENDOR-ITEM-STATUS-CODE-GRP
                                       OCCURS 12
                                       PIC X.


      *****************************************************************
      *    WORKING STORAGE ITEM STATUS ARRAY                          *
      *                                                               *
      *    VALID ITEM STATUS CODES (CORPORATE MASTER)                 *
      *    A = ACTIVE                                                 *
      *    B = BACK ORDER                                             *
      *    D = DISCONTINUED                                           *
      *    I = INACTIVE                                               *
      *****************************************************************

       01  WS-ITEM-STATUS-ARRAY.
           03  WISA-ITEM-STATUS-GRP    PIC X(12).
           03  WISA-ITEM-STATUS        REDEFINES WISA-ITEM-STATUS-GRP
                                       PIC X(01)
                                       OCCURS 12 TIMES
                                       INDEXED BY STATUS-INDEX.

       77  WS-SAVE-NUMBER-OF-ENTRIES   PIC S9(3) COMP-3 VALUE +0.
       77  WS-NUMBER-OF-ENTRIES        PIC S9(3) COMP-3 VALUE +0.


      *****************************************************************
      *    LINKAGE ITEM STATUS ARRAY                                  *
      *                                                               *
      *    VALID ITEM STATUS CODES (CORPORATE MASTER)                 *
      *    A = ACTIVE                                                 *
      *    B = BACK ORDER                                             *
      *    D = DISCONTINUED                                           *
      *    I = INACTIVE                                               *
      *****************************************************************

       01  LS-ITEM-STATUS-ARRAY.
           03  LISA-ITEM-STATUS-GRP    PIC X(12).
           03  LISA-ITEM-STATUS        REDEFINES LISA-ITEM-STATUS-GRP
                                       PIC X(01)
                                       OCCURS 12 TIMES.


       01  WS-END-OF-WS.
           05  FILLER                  PIC X(05)   VALUE '#####'.

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.
       01  RECORD-DATA        PIC X(80).
       01  SALES-RECORD       PIC X(60).



      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION USING RECORD-DATA,
                                SALES-RECORD.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION      *
      *                BATCH PROCESS                                  *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           INITIALIZE WS-SALES-STATUS.
           MOVE SALES-RECORD TO SALES-COMMISSION.

           CALL WBCI0075.
           PERFORM  P00050-INITIALIZE
               THRU P00050-INITIALIZE-EXIT.


           IF NO-ERROR-FOUND
               PERFORM  P00500-MAIN-PROCESS
                   THRU P00500-MAIN-PROCESS-EXIT.

           PERFORM  P00100-END-OF-JOB
               THRU P00100-END-OF-JOB-EXIT.

           CLOSE SALES-FILE.



           GOBACK.

       P00000-MAINLINE-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00050-INITIALIZE                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE RELEVANT WORK FIELDS     *
      *                AND VARIABLES, PERFORM ONE TIME TASKS          *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00050-INITIALIZE.

           MOVE 'N'                    TO WS-ERROR-FOUND-SW
                                          WS-END-OF-PARM-FILE-SW
                                          WS-PROCESS-COMPLETE-SW.


      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME                               *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.



           IF WMF-CUSTOMR-STATUS = '00'
               PERFORM 9000-OPEN
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           PERFORM P00800-CALC-COMMISSION
                   THRU P00800-CALC-COMMISSION-EXIT.


           IF WMF-PENDORD-STATUS = '00'
               GO TO P00050-INITIALIZE-EXIT
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PERFORM 1ST READ ON PARAMETER FILE -- EOF IS AN ERROR      *
      *****************************************************************

           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.


           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           IF END-OF-PARM-FILE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE WPM-PARAMETER-FILE-EMPTY
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P00050-INITIALIZE-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00100-END-OF-JOB                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM NORMAL END OF PROGRAM       *
      *                OPERATIONS, I.E. CLOSE FILES, ETC.             *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00100-END-OF-JOB.

      *****************************************************************
      *    CLOSE FILES, VERIFY SUCCESSFUL VSAM FILE CLOSURES          *
      *****************************************************************

           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.




           IF WMF-CUSTOMR-STATUS = '00'
               PERFORM 9900-CLOSE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00100'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           IF WMF-PENDORD-STATUS = '00'
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00100'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P00100-END-OF-JOB-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  CONTROL HIGH LEVEL PROCESSING FOR BOTH         *
      *                PARAMETER AND ORDER PROCESSES                  *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00500-MAIN-PROCESS.

      *****************************************************************
      *    PERFORM INPUT PARAMETER PROCESS -- IF ERROR FOUND, EXIT    *
      *****************************************************************
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.

           PERFORM  P00600-PRIMARY-WORK-LIST
               THRU P00600-PRIMARY-WORK-LIST-EXIT.

           IF ERROR-FOUND
               GO TO P00500-MAIN-PROCESS-EXIT.

       P00500-MAIN-PROCESS-EXIT.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PARAMETER INPUT RECORDS,   *
      *                STORE PARAMETERS IN AN ARRAY, EDIT THE         *
      *                PARAMETER CONTENT                               *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00600-PRIMARY-WORK-LIST.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.

      *****************************************************************
      *    PROCESS PARAMETERS UNTIL END OF FILE                       *
      *****************************************************************

           MOVE ZEROES                 TO WS-SUB1
                                          WS-USERID-PARM-COUNT.
           PERFORM 9800-WRITE-OUTPUT
           IF ERROR-FOUND
               GO TO P00600-PRIMARY-WORK-LIST-EXIT.
           PERFORM  P00630-LOAD-PARM-ARRAY
               THRU P00630-LOAD-PARM-ARRAY-EXIT
                   UNTIL END-OF-PARM-FILE.


      *****************************************************************
      *    PERFORM PARAMETER RECORD EDITS                             *
      *****************************************************************

           MOVE SPACES                 TO WMF-ACTIVE-SCENARIOS.

           PERFORM  P00660-EDIT-PARMS
               THRU P00660-EDIT-PARMS-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-PARAMETER-RECORDS-IN.

           IF ERROR-FOUND
               GO TO P00600-PRIMARY-WORK-LIST-EXIT.


      *****************************************************************
      *    IF NO USER ID SPECIFICATION RECORD, ERROR - TERMINATE      *
      *****************************************************************

           IF WS-USERID-PARM-COUNT     > ZEROES
               NEXT SENTENCE
           ELSE
               MOVE WPM-USERID-PARM-REQUIRED
                                       TO WMF-MESSAGE-AREA
               PERFORM  P99400-ERROR-ROUTINE
                   THRU P99400-ERROR-ROUTINE-EXIT.

       P00600-PRIMARY-WORK-LIST-EXIT.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ PARAMETER RECORDS AND STORE THE*
      *                PARAMETERS IN AN ARRAY FOR LATER PROCESSING    *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00630-LOAD-PARM-ARRAY.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.

      *****************************************************************
      *    CHECK FOR MAXIMUM PARAMETER RECORDS ALLOWED                *
      *****************************************************************

           ADD +1                      TO WS-SUB1.
           MOVE 'Y' TO WS-END-OF-PARM-FILE-SW.

           IF WS-SUB1                  >  WS-MAX-PARAMETERS
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00630'           TO WPGE-PARAGRAPH
               MOVE WS-MAX-PARAMETERS  TO WPM-MAX-PARAMETERS
               MOVE WPM-MAX-PARAMETERS-EXCEEDED
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           MOVE WS-PARAMETER-RECORD    TO WPRA-RECORD (WS-SUB1).


      *****************************************************************
      *    READ NEXT PARAMETER RECORD                                 *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

       P00630-LOAD-PARM-ARRAY-EXIT.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00660-EDIT-PARMS                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE PARAMETER RECORD SYNTAX     *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00660-EDIT-PARMS.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.

           MOVE 'N'                    TO WS-PARM-ERROR-FOUND-SW.
           MOVE WPRA-RECORD (WS-SUB1)  TO WS-PARAMETER-RECORD.

      *****************************************************************
      *    EDIT THE RECORD TYPE -  A = ADD ORDER, C = CHANGE ORDER,   *
      *    D = DELETE ORDER, S = SCENARIO NUMBER SPECIFICATION,       *
      *    U = USERID SPECIFICATION                                   *
      *****************************************************************

           IF WPR-ADD-ORDER            OR
              WPR-CHANGE-ORDER         OR
              WPR-DELETE-ORDER         OR
              WPR-SCENARIO             OR
              WPR-USERID
               NEXT SENTENCE
           ELSE
               MOVE WPM-PARM-INVALID-RECORD-TYPE
                                       TO WMF-MESSAGE-AREA
               PERFORM  P00700-PARM-ERROR
                   THRU P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    FOR ACTION A= ADD ORDER, C= CHANGE ORDER, D= DELETE ORDER  *
      *    A 10 POSITION NUMERIC ORDER NUMBER IS REQUIRED             *
      *****************************************************************

           IF WPR-ADD-ORDER            OR
              WPR-CHANGE-ORDER         OR
              WPR-DELETE-ORDER
               IF  WPR-ORDER-NUMBER NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE WPM-INVALID-ORDER-NUMBER
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.

      *****************************************************************
      *    FOR ACTION S= SCENARIO,                                    *
      *    A 3 POSITION NUMERIC SCENARIO NUMBER IS REQUIRED           *
      *****************************************************************

           IF WPR-SCENARIO
               IF (WPR-SCENARIO-NUMBER NUMERIC)    AND
                  (WPR-SCENARIO-NUMBER-R > 0)      AND
                  (WPR-SCENARIO-NUMBER-R < 251)
                   MOVE 'Y'            TO WMF-ACTIVE-SCENARIOS-R
                                             (WPR-SCENARIO-NUMBER-R)
               ELSE
                   MOVE WPM-INVALID-SCENARIO-NUMBER
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    FOR ACTION U= USER ID SPECIFICATION, ONLY 1 USER ID PARM   *
      *    RECORD IS ALLOWED, USERID MUST BE NON-BLANK                *
      *****************************************************************

           IF WPR-USERID
               ADD +1                  TO WS-USERID-PARM-COUNT
               IF  WS-USERID-PARM-COUNT > +1
                   MOVE WPM-USERID-PARM-TOO-MANY
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
               ELSE
               IF  WPR-USERID-VALUE     > SPACES
                   MOVE WPR-USERID-VALUE
                                       TO WMF-USERID
               ELSE
                   MOVE WPM-INVALID-USERID
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.

       P00660-EDIT-PARMS-EXIT.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           EXIT.

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00700-PARM-ERROR                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / DISPLAY PARM RECORD ERRORS*
      *                                                               *
      *    CALLED BY:  P00660-EDIT-PARMS                              *
      *                                                               *
      *****************************************************************

       P00700-PARM-ERROR.

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

      *****************************************************************
      *    IF ERROR ALREADY ENCOUNTERED FOR THIS RECORD, JUST ADD THE *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           IF PARM-ERROR-FOUND
               GO TO P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    IF 1ST ERROR FOR THIS RECORD, DISPLAY THE ALL ASTERISK     *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           MOVE 'Y'                    TO WS-PARM-ERROR-FOUND-SW.
           MOVE WS-SUB1                TO WPM-RECORD-NUMBER.

       P00700-PARM-ERROR-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00800-CALCULATE-COMMISSION                    *
      *                                                               *
      *    FUNCTION :  CALCULATE SALES COMMISSION                     *
      *                                                               *
      *    CALLED BY:  MAINLINE                                       *
      *                                                               *
      *****************************************************************

       P00800-CALC-COMMISSION.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.

           IF SALES-TYPE = "ONL"
               MOVE 25.00 TO SALES-POINT-COMMISSION
           ELSE
               COMPUTE SALES-POINT-COMMISSION = SALES-AMT * .10
               IF SALES-POINT-COMMISSION > +100.00
                  MOVE 100.00 TO SALES-POINT-COMMISSION
               END-IF
           END-IF.

      * CWE-121638
           IF (SALES-POINT-STATE = "CA" AND SALES-TYPE = "POS")
              COMPUTE WS-SALES-POINT-COMMISSION = SALES-POINT-COMMISSION
                          + (SALES-AMT * .01)
               IF WS-SALES-POINT-COMMISSION > +100.00
                   MOVE 100.00 TO WS-SALES-POINT-COMMISSION
               END-IF
               MOVE WS-SALES-POINT-COMMISSION TO SALES-POINT-COMMISSION
           END-IF.
      * CWE-121638

           PERFORM P00850-SALES-COMP THROUGH P00850-SALES-COMP-EXIT.


       P00800-CALC-COMMISSION-EXIT.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00850-UPDATE-SALES                            *
      *                                                               *
      *    FUNCTION :  WRITE RECORDS TO SALES FILE                    *
      *                                                               *
      *    CALLED BY:  MAINLINE                                       *
      *                                                               *
      *****************************************************************

       P00850-SALES-COMP.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.

           MOVE 'R' TO REWRITE-SW.
           MOVE SALES-COMMISSION TO SALES-REC.
           READ SALES-FILE
           KEY IS SALES-ID
           IF NOT-FOUND
                DISPLAY 'RECORD NOT FOUND: ' SALES-ID
                MOVE 'W' TO REWRITE-SW
           END-IF.

           IF REWRITING
               REWRITE SALES-REC FROM SALES-COMMISSION
           ELSE
               WRITE SALES-REC FROM SALES-COMMISSION
           END-IF.


       P00850-SALES-COMP-EXIT.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           EXIT.











      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-READ-PARAMETERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE INPUT PARAMETER FILE       *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *****************************************************************

       P80000-READ-PARAMETERS.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.
           ADD 1 TO WS-COUNTER.


           ADD +1                      TO WS-PARAMETER-RECORDS-IN.

           MOVE WS-PARAMETER-RECORDS-IN TO WPM-RECORD-NUMBER.

       P80000-READ-PARAMETERS-EXIT.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.
           EXIT.




      *****************************************************************
      *                                                               *
      *    P R O D U C T    D E M O N S T R A T I O N     A P P L     *
      *                                                               *
      *             E R R O R    R O U T I N E S                      *
      *                                                               *
      *                                                               *
      *****************************************************************


KCS305*****************************************************************
KCS305*                                                               *
KCS305*    PARAGRAPH:  P99400-ERROR-ROUTINE                           *
KCS305*                                                               *
KCS305*    FUNCTION :  ROUTINE TO FORMAT AND DISPLAY NON-FATAL ERRORS *
KCS305*                                                               *
KCS305*                ERROR TEXT IS DISPLAYED                        *
KCS305*                TO THE USER INDICATING THE NATURE OF THE ERROR *
KCS305*                                                               *
KCS305*                CONTROL IS RETURNED TO CALLING ROUTINE         *
KCS305*                                                               *
KCS305*    CALLED BY:  GLOBAL                                         *
KCS305*                                                               *
KCS305*****************************************************************

KCS305 P99400-ERROR-ROUTINE.

KCS305     MOVE 'Y'                    TO WS-ERROR-FOUND-SW.


KCS305     MOVE WMF-MESSAGE-AREA       TO WPEA-ERROR-07-TEXT.


KCS305 P99400-ERROR-ROUTINE-EXIT.
KCS305     EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-PDA-ERROR                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING GENERAL, *
      *                DB2, IMS-DLI, MQSERIES ERRORS                  *
      *                                                               *
      *                ERROR TEXT IS DISPLAYED                        *
      *                TO THE USER INDICATING THE NATURE OF THE ERROR *
      *                                                               *
      *                PROGRAM IS ABNORMALLY TERMINATED               *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-PDA-ERROR.

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.


      *****************************************************************
      *      FORMAT AND SEND ERROR TEXT                               *
      *****************************************************************

           IF PDA-DB2-ERROR
               MOVE WS-PDA-DB2-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-DB2-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
           IF PDA-IMS-ERROR
               MOVE WS-PDA-IMS-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-IMS-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
           IF PDA-MQSERIES-ERROR
               MOVE WS-PDA-MQSERIES-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-MQSERIES-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
               MOVE WS-PDA-GEN-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-GEN-ERROR-02
                                       TO WPEA-ERROR-08-TEXT.


           DISPLAY WPGE-DESCRIPTION.
           MOVE 99                     TO WS-RETURN-CODE.
           CALL 'ILBOABN0'          USING WS-RETURN-CODE.
           MOVE WS-RETURN-CODE         TO RETURN-CODE.

           GOBACK.

       P99500-PDA-ERROR-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99999-ERROR                                   *
      *                                                               *
      *    FUNCTION :  GENERIC ERROR ROUTINE TO HANDLE GENERAL ERRORS *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

RTN    P99999-ERROR.

USED       MOVE 'GEN'                  TO WS-PDA-ERROR-TYPE.
AS OF      MOVE 'PDAB06'               TO WPGE-PROGRAM-ID.
JAN        MOVE 99                     TO WS-RETURN-CODE.
2001       MOVE 'ERROR'                TO WPGE-DESCRIPTION.
           MOVE 'P99999'               TO WPGE-PARAGRAPH.

           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.

*********
*********
       9000-OPEN.
           OPEN EXTEND REPORT-FILE.
           OPEN I-O SALES-FILE.
*********
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P9800-WRITE-OUTPUT                             *
      *                                                               *
      *    FUNCTION :  A POTENTIAL WORK-ORDER-LIST HAS BEEN ISOLOATED *
      *                WRITE IT TO EMPSTAT FOR LATER PROCESSING.      *
      *                                                               *
      *    CALLED BY:  VARIOUS                                        *
      *                                                               *
      *****************************************************************
*********
       9800-WRITE-OUTPUT.
           CALL RVWKEVAL USING RECORD-DATA.
           WRITE REPORT-RECORD FROM RECORD-DATA.
*********
*********
       9900-CLOSE.
           CLOSE REPORT-FILE.
           CLOSE SALES-FILE.