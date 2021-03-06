//PFHSXKX JOB (EUUK),'RV II',CLASS=J,
//         MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//         REGION=8M
/*JOBPARM  S=CWCC
//****************************************************************
//*
//* RUN WBCI1206 EXAMPLE.
//*
//****************************************************************
//STEP1   EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=SYS2.CW.&CWGACX..SLCXLOAD,DISP=SHR
//        DD DISP=SHR,DSN=SALESSUP.SXK1.DEV2.LOAD
//        DD  DISP=SHR,
//             DSN=DSNB10.SDSNLOAD
//*         DD  DISP=SHR,
//*             DSN=SALESSUP.SXK1.QA2.LOAD
//         DD  DISP=SHR,
//             DSN=SALESSUP.SXK1.PRD.LOAD
//         DD  DISP=SHR,
//             DSN=CEE.SCEERUN
//*         DD  DISP=SHR,
//*             DSN=HSTJXL0.RVII.WEBCAST.LOAD
//*STEPLIB  DD  DISP=SHR,
//*             DSN=DSNB10.SDSNLOAD
//*        DD  DISP=SHR,
//*             DSN=HSTJXL0.RVII.WEBCAST.LOAD
//*         DD  DISP=SHR,
//*             DSN=CEE.SCEERUN
//*         DD  DISP=SHR,
//*             DSN=SALESSUP.BAW1.PRD.LOAD
//SYSTSPRT  DD SYSOUT=*
//EMPFILE   DD DISP=SHR,DSN=SALESSUP.SXK1.EMPFILE
//*EMPFILE   DD  DISP=SHR,DSN=HSTJXL0.RVIIDEMO.EMPFILE
//EMPSTAT   DD  DSN=&&STAT,DISP=(MOD,PASS),UNIT=SYSDA,
//            SPACE=(800,(500,500))
//RPTFILE   DD SYSOUT=*
//SYSPRINT  DD  SYSOUT=*,TERM=TS
//SYSOUT    DD  SYSOUT=*,TERM=TS
//SYSUDUMP  DD  SYSOUT=*
//EMPFILE2  DD  DSN=SYS2.CW.XT.R1605.MLXT160.SLXTSAMP(CWXTDATA),
//            DISP=SHR
//EMPCHECK  DD  DSN=SYS2.CW.XT.R1605.MLXT160.SLXTSAMP(CWXTDATA),
//            DISP=SHR
//EMPINP    DD  DSN=HSTJXL0.WBIC.SALES.COMM,DISP=SHR
//SALES     DD  DSN=PFHSXK0.FASAMP.SALES,DISP=SHR
//RPTFILE2  DD  SYSOUT=X
//EMPOUT    DD  SYSOUT=*
//SYSHELP   DD  DISP=SHR,
//            DSN=DSNB10.SDSNEXIT
//SYSIN     DD  DUMMY
//SYSTSIN   DD  *
 DSN SYSTEM(DBCC)
 RUN PROGRAM(WBCI1206) PLAN(HSTJXL02) PARMS('/04')
/*
//