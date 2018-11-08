//USERIDXX JOB  ('OAABAS9.0.4QC',822),'90 JCLXXXXX 09/16',
//             CLASS=L,MSGCLASS=X,NOTIFY=&USERID
/*JOBPARM  L=9999,S=CWCC
//*
//*********************************************************************
//*                                                                   *
//*  MEMBER = ISPWSHDR                                                *
//*  THIS JOB MUST BE EXECUTED PRIOR TO RUNNING ISPWDB                *
//*                                                                   *
//*  THIS IS A SAMPLE JOB TO CREATE COMPUWARE SHARED DIRECTORIES      *
//*  FOR THE END-TO-END DEMO SCENARIO. THIS JOB WILL CREATE 8         *
//*  SHARED DIRECTORIES FOR THE FOLLOWING LEVELS:                     *
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
//*                                                                   *
//*********************************************************************
//CXUTIL   EXEC PGM=CWDDALLU
//STEPLIB  DD DISP=SHR,DSN=SYS2.CW.&CWGACX..SLCXLOAD
//ABNLREPT DD SYSOUT=*
//ABNLPARM DD *
*
*    CREATE/FORMAT SD/DB:  SALESSUP.APP.LEVL.SSD
*
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.DEV1.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.DEV2.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.DEV3.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.QA1.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.QA2.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.QA3.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.STG.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.FIX.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.EMR.SSD
CREATE OBJECT=SRCEDIR,
       AM=VSAM,
       CISIZE=18432,
       TRACKS=00070,
       REALLOCATE=NO,
       DIRDSN=SALESSUP.&APP.PRD.SSD
//