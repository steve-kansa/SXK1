//SNCBAW0C JOB ,,CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//    NOTIFY=SNCBAW0,TIME=5
//*
//*
//*
//* CLONE APPLICATION
//* ==================
//*
//* FROM CONFIG=ISPW APPL=$MOD STREAM=FTSDEMO
//* TO   CONFIG=ISPW APPL=BAW1 STREAM=FTSDEMO
//*
//******************************************************************
//* STEP 1:  DELETE TEMP CLONE REPORT AND ECI COMMAND FILES
//******************************************************************
//STEP1   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
   DELETE SNCBAW0.TEMP.$MOD.CLONE
   DELETE SNCBAW0.TEMP.MODL.ECITASK
   DELETE SNCBAW0.TEMP.MODL.ECIASGN
   DELETE SNCBAW0.TEMP.MODL.CPYCMDS
   SET MAXCC = 0
//******************************************************************
//* STEP 2:  ALLOCATE DATASETS
//******************************************************************
//STEP1   EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//CLONEDD  DD DSN=SNCBAW0.TEMP.$MOD.CLONE,
//         DISP=(NEW,CATLG),UNIT=SYSDA,
//         SPACE=(CYL,(15,5),RLSE),RECFM=FB,
//         LRECL=150,BLKSIZE=0,DSORG=PS
//ECITASK  DD DSN=SNCBAW0.TEMP.MODL.ECITASK,
//         DISP=(NEW,CATLG),UNIT=SYSDA,
//         SPACE=(CYL,(50,5),RLSE),RECFM=FB,
//         LRECL=256,BLKSIZE=0,DSORG=PS
//ECIASGN  DD DSN=SNCBAW0.TEMP.MODL.ECIASGN,
//         DISP=(NEW,CATLG),UNIT=SYSDA,
//         SPACE=(CYL,(15,5),RLSE),RECFM=FB,
//         LRECL=256,BLKSIZE=0,DSORG=PS
//CPYCMDS  DD DSN=SNCBAW0.TEMP.MODL.CPYCMDS,
//         DISP=(NEW,CATLG),UNIT=SYSDA,
//         SPACE=(CYL,(15,5),RLSE),RECFM=FB,
//         LRECL=256,BLKSIZE=0,DSORG=PS
//******************************************************************
//* STEP 3:  CREATE CLONE REPORT
//******************************************************************
//STEP3    EXEC  PGM=WZZRCJOB,PARM='ISPW/WZZCLONE',
//         COND=((4,LT))
//CLONEDD  DD DISP=SHR,DSN=SNCBAW0.TEMP.$MOD.CLONE
//WORKIN   DD    *
$DEFINE_WPLI
  SORTSEQ=T
  WPFLAG=D
  WZVFGRUP=$MOD
  WZVFSTRM=FTSDEMO
  WZVFLVL=PRD
//WZZTRXXX DD *
WZZDBGA=SYSOUT=K
DEBUGM=DSA
TRACEM=DSA
TRACE=OP
DEBUG=OP
//******************************************************************
//* STEP 4:  CREATE THE ECI COMMANDS AND IEBCOPY COMMANDS
//******************************************************************
//STEP4    EXEC PGM=WZZRCBAT,PARM='ISPW/IKJEFT01',
//         DYNAMNBR=64,
//         REGION=6M,
//         COND=((4,LT))
//ISPLOG   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=121,BLKSIZE=1210)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//CLONEDD  DD DISP=SHR,DSN=SNCBAW0.TEMP.$MOD.CLONE
//ECITASK  DD DISP=SHR,DSN=SNCBAW0.TEMP.MODL.ECITASK
//ECIASGN  DD DISP=SHR,DSN=SNCBAW0.TEMP.MODL.ECIASGN
//CPYCMDS  DD DISP=SHR,DSN=SNCBAW0.TEMP.MODL.CPYCMDS
//SYSTSIN  DD  *
 ISPSTART CMD(%WZZCLONE MODL FTSDEMO DUMMY PRD DEV1 +
              ISPWCT ISPWCT)
//******************************************************************
//* STEP 5: CREATE ASSIGNMENT VIA ECI
//******************************************************************
//STEP5    EXEC  PGM=WZZRCJOB,PARM='ISPW/WZZECIJ',
//         COND=((4,LT))
//WORKIN   DD DISP=SHR,DSN=SNCBAW0.TEMP.MODL.ECIASGN
//******************************************************************
//* STEP 6:  ADD TASKS VIA ECI
//******************************************************************
//STEP6    EXEC  PGM=WZZRCJOB,PARM='ISPW/WZZECIJ',
//         COND=((4,LT))
//WORKIN   DD DISP=SHR,DSN=SNCBAW0.TEMP.MODL.ECITASK
//******************************************************************
//* STEP 7:  RUN THE COPY COMMANDS
//******************************************************************
//STEP7    EXEC  PGM=WZZRCJOB,PARM='ISPW/WZZBP',
//         COND=((4,LT))
//WZZBPOUT DD SYSOUT=*
//WZZBPIN  DD DISP=SHR,DSN=SNCBAW0.TEMP.MODL.CPYCMDS