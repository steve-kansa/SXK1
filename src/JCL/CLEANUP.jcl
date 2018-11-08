//SNCBAW0C  JOB ,,CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//    NOTIFY=SNCBAW0,TIME=5
//* --------------------------------------------------
//* SQL TO DELETE TASKS TO CLEAN UP A TEST APPLICATION
//* CORRECT JCL ERROR BEFORE SUBMIT
//* --------------------------------------------------
//STEP1    EXEC PGM=IKJEFT01,DYNAMNBR=20
//SYSTSPRT  DD  SYSOUT=*
//SYSPRINT  DD  SYSOUT=*
//SYSTSIN   DD  *
 DSN SYSTEM(DB01)
 RUN  PROGRAM(DSNTEP2) PLAN(DSNTEP2) -
      LIB('SPP.DB2.PRODUCTS.LOAD')
 END
//SYSIN     DD  *
 SET CURRENT SQLID = '<WZAUTH>';
 INSERT INTO WZT_CT_REQ
   (REQ_ID, REQ_TYPE, SRC_STORAGE_TYPE,
    SRC_SRVR_NAME, SRC_STORAGE_KEY, SRC_STORAGE_NAME)
      SELECT X'5C'||SUBSTR(T.TASK_ID, 2, 5),
             'DELE', 'WH',
             W.TRNS_SRVR_NAME, V.VERS_STORAGE_KEY, W.WH_ID
      FROM WZT_COMPONENT C, WZT_CMPNT_VERSION V, WZT_TASK T,
           WZT_WAREHOUSE W
      WHERE C.APPL_ID          = 'BAW1'
      AND   V.VERS_STORAGE_KEY <> ''
      AND   V.CMPNT_ID         = C.CMPNT_ID
      AND   V.CMPNT_ID         = T.CMPNT_ID
      AND   V.CMPNT_VID        = T.CMPNT_VID
      AND   SUBSTR(V.VERS_STORAGE_KEY, 1, 8) = W.WH_ID;
 INSERT INTO WZT_CT_REQ
   (REQ_ID, REQ_TYPE, SRC_STORAGE_TYPE,
    SRC_SRVR_NAME, SRC_STORAGE_KEY, SRC_STORAGE_NAME)
      SELECT X'5C'||SUBSTR(V.PART_ID, 2, 5),
             'DELE', 'WH',
             W.TRNS_SRVR_NAME, V.VERS_STORAGE_KEY, W.WH_ID
      FROM WZT_COMPONENT C, WZT_GPART V, WZT_WAREHOUSE W
      WHERE C.APPL_ID          = 'BAW1'
      AND   V.VERS_STORAGE_KEY <> ''
      AND   V.CMPNT_ID         = C.CMPNT_ID
      AND   SUBSTR(V.VERS_STORAGE_KEY, 1, 8) = W.WH_ID;
 DELETE FROM WZT_OPERATION
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPNT_BUILD
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_BUILD_MAP
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPNT_GPRTCOLL
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_GPART_COLL
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_GPART
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_PART_INSTANCE
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_EXTS_DATA
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_EXTN_DATA
   WHERE BASE_CLASS = 'CMPNVERS'
   AND   BASE_ID IN
     (SELECT V.BASE_ID
      FROM   WZT_CMPNT_VERSION V, WZT_COMPONENT C
      WHERE  V.CMPNT_ID = C.CMPNT_ID
      AND    C.APPL_ID  = 'BAW1');
 DELETE FROM WZT_IMPACT_MSGS
   WHERE BASE_CLASS = 'CMPNVERS'
   AND   BASE_ID IN
     (SELECT V.BASE_ID
      FROM   WZT_CMPNT_VERSION V, WZT_COMPONENT C
      WHERE  V.CMPNT_ID = C.CMPNT_ID
      AND    C.APPL_ID  = 'BAW1');
 DELETE FROM WZT_CMPNT_VERSION
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPV_EXT_NAME
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPV_REFERENCE
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPV_REFERENCE
   WHERE REF_CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPV_SUBCMPNT
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPV_SUBCMPNT
   WHERE OWN_CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_CMPNT_DOMAIN
   WHERE CMPNT_ID IN
     (SELECT CMPNT_ID
      FROM   WZT_COMPONENT
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_COMPONENT
   WHERE APPL_ID = 'BAW1';
 DELETE FROM WZT_CONTAINR_ENTRY
   WHERE TASK_ID IN
     (SELECT TASK_ID
      FROM   WZT_TASK
      WHERE  APPL_ID = 'BAW1');
 DELETE FROM WZT_EXTN_DATA
   WHERE BASE_CLASS = 'TASK'
   AND   BASE_ID IN
     (SELECT BASE_ID
      FROM   WZT_TASK
      WHERE  APPL_ID  = 'BAW1');
 DELETE FROM WZT_IMPACT_MSGS
   WHERE BASE_CLASS = 'TASK'
   AND   BASE_ID IN
     (SELECT BASE_ID
      FROM   WZT_TASK
      WHERE  APPL_ID  = 'BAW1');
 DELETE FROM WZT_TASK
   WHERE APPL_ID = 'BAW1';
 DELETE FROM WZT_SET_EXEC_CTL X
 WHERE EXISTS
   (SELECT 1
    FROM WZT_CONTAINER C
    WHERE APPL_ID = 'BAW1'
    AND   C.CNTR_ID   = X.SET_ID
    AND   C.CNTR_TYPE = 'S'
    AND NOT EXISTS
      (SELECT 1
       FROM WZT_CONTAINR_ENTRY E, WZT_TASK T
       WHERE E.TASK_ID   =  T.TASK_ID
       AND   T.APPL_ID   <> 'BAW1'
       AND   E.CNTR_ID   =  C.CNTR_ID
       AND   E.CNTR_TYPE =  C.CNTR_TYPE));
 DELETE FROM WZT_SET S
 WHERE EXISTS
   (SELECT 1
    FROM WZT_CONTAINER C
    WHERE APPL_ID = 'BAW1'
    AND   C.CNTR_ID   = S.SET_ID
    AND   C.CNTR_TYPE = 'S'
    AND NOT EXISTS
      (SELECT 1
       FROM WZT_CONTAINR_ENTRY E, WZT_TASK T
       WHERE E.TASK_ID   =  T.TASK_ID
       AND   T.APPL_ID   <> 'BAW1'
       AND   E.CNTR_ID   =  C.CNTR_ID
       AND   E.CNTR_TYPE =  C.CNTR_TYPE));
 DELETE FROM WZT_CONTAINER_USER U
 WHERE EXISTS
   (SELECT 1
    FROM WZT_CONTAINER C
    WHERE APPL_ID = 'BAW1'
    AND   C.CNTR_ID   = U.CNTR_ID
    AND   C.CNTR_TYPE = U.CNTR_TYPE
    AND NOT EXISTS
      (SELECT 1
       FROM WZT_CONTAINR_ENTRY E, WZT_TASK T
       WHERE E.TASK_ID   =  T.TASK_ID
       AND   T.APPL_ID   <> 'BAW1'
       AND   E.CNTR_ID   =  C.CNTR_ID
       AND   E.CNTR_TYPE =  C.CNTR_TYPE));
 DELETE FROM WZT_CONTAINER C
 WHERE APPL_ID = 'BAW1'
 AND NOT EXISTS
   (SELECT 1
    FROM WZT_CONTAINR_ENTRY E, WZT_TASK T
    WHERE E.TASK_ID   =  T.TASK_ID
    AND   T.APPL_ID   <> 'BAW1'
    AND   E.CNTR_ID   =  C.CNTR_ID
    AND   E.CNTR_TYPE =  C.CNTR_TYPE);
/*