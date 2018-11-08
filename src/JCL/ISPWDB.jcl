//USERIDXX JOB  ('OAABAS9.0.4QC',822),'90 JCLXXXXX 09/16',
//             CLASS=L,MSGCLASS=X,NOTIFY=&USERID
/*JOBPARM  L=9999,S=CWCC
//*
//*********************************************************************
//*                                                                   *
//*  MEMBER = ISPWDB                                                  *
//*  YOU MUST RUN JOB ISPWSHDR PRIOR TO RUNNING ISPWDB                *
//*                                                                   *
//*  THIS IS A SAMPLE JOB TO CREATE COMPUWARE SOURCE LISTING          *
//*  DATABASES FOR THE END-TO-END DEMO SCENARIO.                      *
//*  THIS JOB WILL CREATE 8 SOURCE LISTING DATABASES                  *
//*  FOR THE FOLLOWING LEVELS AND ATTACH THEM TO THEIR ASSOCIATED     *
//*  SHARED DIRECTORIES:                                              *
//*                                                                   *
//*  DEV1, DEV2, DEV3, QA, STG, EMR, FIX, PRD                         *
//*                                                                   *
//*  BEFORE SUBMITTING THIS JOB, PLEASE MAKE THE FOLLOWING CHANGES:   *
//*                                                                   *
//*  1. CHANGE THE JOBNAME TO YOUR TSOID+1 CHARACTER                  *
//*                                                                   *
//*  2. CHANGE &APP TO YOUR INITIALS FOLLOWED BY THE NUMBER 1         *
//*     I.E. BRIAN WESTERHOLT = BAW1  DAVE KARTZMAN - DGK1            *
//*     IF YOU DO NOT HAVE A MIDDLE INITIAL, USE AN X, I.E. BXW1      *
//*     THIS SHOULD MATCH THE APP NAME YOU USED IN ISPWSHDR           *
//*                                                                   *
//*********************************************************************
//CXUTIL   EXEC PGM=CWDDALLU
//STEPLIB  DD DISP=SHR,DSN=SYS2.CW.&CWGACX..SLCXLOAD
//ABNLREPT DD SYSOUT=*
//CWUT0001 DD DISP=SHR,DSN=SALESSUP.&APP.DEV1.SSD
//CWUT0002 DD DISP=SHR,DSN=SALESSUP.&APP.DEV2.SSD
//CWUT0003 DD DISP=SHR,DSN=SALESSUP.&APP.DEV3.SSD
//CWUT0004 DD DISP=SHR,DSN=SALESSUP.&APP.STG.SSD
//CWUT0005 DD DISP=SHR,DSN=SALESSUP.&APP.QA1.SSD
//CWUT0006 DD DISP=SHR,DSN=SALESSUP.&APP.QA2.SSD
//CWUT0007 DD DISP=SHR,DSN=SALESSUP.&APP.QA3.SSD
//CWUT0008 DD DISP=SHR,DSN=SALESSUP.&APP.PRD.SSD
//CWUT0009 DD DISP=SHR,DSN=SALESSUP.&APP.EMR.SSD
//CWUT0010 DD DISP=SHR,DSN=SALESSUP.&APP.FIX.SSD
//ABNLPARM DD *
*
*    CREATE/FORMAT SD/DB:  SALESSUP.&APP.DEV1.DB
*
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.DEV1.DB,
       DIRDD=CWUT0001
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.DEV2.DB,
       DIRDD=CWUT0002
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.DEV3.DB,
       DIRDD=CWUT0003
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.STG.DB,
       DIRDD=CWUT0004
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.QA1.DB,
       DIRDD=CWUT0005
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTR
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.QA2.DB,
       DIRDD=CWUT0006
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTR
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.QA3.DB,
       DIRDD=CWUT0007
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.PRD.DB,
       DIRDD=CWUT0008
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.EMR.DB,
       DIRDD=CWUT0009
CREATE OBJECT=DATABASE,
       TYPE=SOURCE,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00200,
       REALLOCATE=NO,
       AD=DUPS,
       RC=00100,                                  MAXIMUM NUMBER OF ENTRIES
       GC=0003,                                   ALLOCATION GROUP SIZE
       DSN=SALESSUP.&APP.FIX.DB,
       DIRDD=CWUT0010
//