//TJOB06 JOB (BENCHMARK),'ISPW TRAINING',
//         CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),TIME=(0,09)
//*
//TREXX06  EXEC PGM=IKJEFT01,DYNAMNBR=20,REGION=2048K
//SYSPROC  DD DISP=SHR,DSN=SALESSUP.$MOD.DEV1.CLST
//         DD DISP=SHR,DSN=SALESSUP.$MOD.STG1.CLST
//         DD DISP=SHR,DSN=SALESSUP.$MOD.QA.CLST
//         DD DISP=SHR,DSN=SALESSUP.$MOD.PRD.CLST
//ISPPROF  DD DSN=&&ISPPROF,DISP=(NEW,PASS),
//            UNIT=SYSDA,
//            SPACE=(TRK,(1,1,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=9040)
//ISPTLIB  DD DSN=&&ISPPROF,DISP=(OLD,DELETE),
//            UNIT=AFF=ISPPROF,VOL=REF=*.ISPPROF
//         DD DISP=SHR,DSN=SYS1.BASE.ISPTLIB
//ISPPLIB  DD DISP=SHR,DSN=SYS1.BASE.ISPPLIB
//ISPSLIB  DD DISP=SHR,DSN=SYS1.BASE.ISPSLIB
//ISPMLIB  DD DISP=SHR,DSN=SYS1.BASE.ISPMLIB
//ISPLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=120,DSORG=PS)
//SYSTSPRT DD DSN=&&TEMP,
//            DCB=(RECFM=FB,LRECL=80,DSORG=PS,BLKSIZE=6000),
//            DISP=(NEW,KEEP),SPACE=(TRK,(1,1),RLSE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSTSIN  DD *
  PROFILE MSGID
  ISPSTART  CMD(%TREXX06)
/*
//*==================================================
//* EXECUTE THE PROGRAM TPROG06 TO PRODUCE THE REPORT
//*==================================================
//PROCESS  EXEC PGM=TPROG06
//STEPLIB   DD DISP=SHR,DSN=SALESSUP.$MOD.DEV1.LOAD
//          DD DISP=SHR,DSN=SALESSUP.$MOD.STG1.LOAD
//          DD DISP=SHR,DSN=SALESSUP.$MOD.QA.LOAD
//          DD DISP=SHR,DSN=SALESSUP.$MOD.PRD.LOAD
//INPUT     DD DISP=(OLD,PASS),DSN=&&TEMP
//OUTPUT    DD SYSOUT=*
//SYSOUT    DD SYSOUT=*