       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ELIGINQ1.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-03-08.
      *================================================================*
      * PROGRAM:  ELIGINQ1                                             *
      * PURPOSE:  REAL-TIME MEMBER ELIGIBILITY INQUIRY                 *
      *           CICS ONLINE TRANSACTION THAT ACCEPTS A MEMBER ID     *
      *           AND DATE OF SERVICE, QUERIES THE DB2 MEMBER TABLE,   *
      *           AND RETURNS ELIGIBILITY STATUS AND BENEFIT SUMMARY   *
      *           TO THE CALLING PROGRAM OR BMS MAP.                   *
      * TRANSACTION: ELIG                                              *
      * FREQUENCY: ONLINE / REAL-TIME                                  *
      *================================================================*
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY MBRCOPY.
       COPY PLNCOPY.

       01  WS-COMMAREA.
           05  WS-CA-FUNCTION          PIC X(02).
               88  WS-CA-INQUIRY       VALUE 'IQ'.
               88  WS-CA-RESPONSE      VALUE 'RS'.
           05  WS-CA-MEMBER-ID         PIC X(12).
           05  WS-CA-DATE-OF-SERVICE   PIC 9(08).
           05  WS-CA-RETURN-CODE       PIC X(02).
           05  WS-CA-RETURN-MSG        PIC X(80).

       01  WS-COMMAREA-LENGTH          PIC S9(04) COMP VALUE 104.

       01  WS-RESP                     PIC S9(08) COMP.
       01  WS-RESP2                    PIC S9(08) COMP.

       01  WS-DB2-MEMBER-ID            PIC X(12).
       01  WS-DB2-DOS                  PIC X(08).

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  DCLMEMBER-ELIG.
           05  DB-MEMBER-ID            PIC X(12).
           05  DB-LAST-NAME            PIC X(35).
           05  DB-FIRST-NAME           PIC X(25).
           05  DB-DOB                  PIC X(08).
           05  DB-SEX                  PIC X(01).
           05  DB-GROUP-ID             PIC X(10).
           05  DB-PLAN-CODE            PIC X(08).
           05  DB-PRODUCT-TYPE         PIC X(03).
           05  DB-COVERAGE-TYPE        PIC X(02).
           05  DB-EFF-DATE             PIC X(08).
           05  DB-TERM-DATE            PIC X(08).
           05  DB-ELIG-STATUS          PIC X(01).
           05  DB-DEDUCT-YTD           PIC S9(07)V99 COMP-3.
           05  DB-DEDUCT-LIMIT         PIC S9(07)V99 COMP-3.
           05  DB-OOP-YTD              PIC S9(07)V99 COMP-3.
           05  DB-OOP-LIMIT            PIC S9(07)V99 COMP-3.
           05  DB-COPAY-PCP            PIC S9(05)V99 COMP-3.
           05  DB-PCP-NPI              PIC X(10).
           05  DB-PCP-NAME             PIC X(35).

       01  WS-BMS-MAP-NAME             PIC X(07) VALUE 'ELIGMP1'.
       01  WS-BMS-MAPSET               PIC X(07) VALUE 'ELIGSET'.

       01  ELIGMP1I.
           05  EM-MEMBER-IDL           PIC S9(04) COMP.
           05  EM-MEMBER-IDF           PIC X(01).
           05  EM-MEMBER-IDI           PIC X(12).
           05  EM-DOS-DATEL            PIC S9(04) COMP.
           05  EM-DOS-DATEF            PIC X(01).
           05  EM-DOS-DATEI            PIC X(08).
           05  EM-MSG-TEXTL            PIC S9(04) COMP.
           05  EM-MSG-TEXTF            PIC X(01).
           05  EM-MSG-TEXTI            PIC X(79).

       01  ELIGMP1O.
           05  EM-NAME-OUT             PIC X(60).
           05  EM-DOB-OUT              PIC X(10).
           05  EM-GROUP-OUT            PIC X(10).
           05  EM-PLAN-OUT             PIC X(08).
           05  EM-PRODUCT-OUT          PIC X(03).
           05  EM-STATUS-OUT           PIC X(15).
           05  EM-EFF-DATE-OUT         PIC X(10).
           05  EM-TERM-DATE-OUT        PIC X(10).
           05  EM-DEDUCT-REM-OUT       PIC X(12).
           05  EM-OOP-REM-OUT          PIC X(12).
           05  EM-COPAY-OUT            PIC X(10).
           05  EM-PCP-OUT              PIC X(45).

       01  WS-WORK-FIELDS.
           05  WS-DEDUCT-REMAINING     PIC S9(07)V99 COMP-3.
           05  WS-OOP-REMAINING        PIC S9(07)V99 COMP-3.
           05  WS-DISPLAY-AMOUNT       PIC $$$,$$9.99.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(104).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   PERFORM 1000-FIRST-TIME
               WHEN OTHER
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-PROCESS-INPUT
           END-EVALUATE

           EXEC CICS RETURN
               TRANSID('ELIG')
               COMMAREA(WS-COMMAREA)
               LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       1000-FIRST-TIME.
           INITIALIZE WS-COMMAREA
           INITIALIZE ELIGMP1O
           MOVE 'ENTER MEMBER ID AND DATE OF SERVICE'
               TO EM-MSG-TEXTI
           EXEC CICS SEND
               MAP(WS-BMS-MAP-NAME)
               MAPSET(WS-BMS-MAPSET)
               ERASE
               CURSOR
           END-EXEC.

       2000-PROCESS-INPUT.
           EXEC CICS RECEIVE
               MAP(WS-BMS-MAP-NAME)
               MAPSET(WS-BMS-MAPSET)
               INTO(ELIGMP1I)
               RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ER' TO WS-CA-RETURN-CODE
               MOVE 'ERROR RECEIVING MAP DATA'
                   TO WS-CA-RETURN-MSG
               PERFORM 9000-SEND-ERROR
           ELSE
               PERFORM 2100-VALIDATE-INPUT
               IF WS-CA-RETURN-CODE = SPACES
                   PERFORM 3000-QUERY-ELIGIBILITY
               END-IF
           END-IF.

       2100-VALIDATE-INPUT.
           MOVE SPACES TO WS-CA-RETURN-CODE
           IF EM-MEMBER-IDI = SPACES OR LOW-VALUES
               MOVE 'ER' TO WS-CA-RETURN-CODE
               MOVE 'MEMBER ID IS REQUIRED' TO WS-CA-RETURN-MSG
               PERFORM 9000-SEND-ERROR
           END-IF
           IF WS-CA-RETURN-CODE = SPACES
               IF EM-DOS-DATEI = SPACES OR LOW-VALUES
                   MOVE FUNCTION CURRENT-DATE(1:8)
                       TO EM-DOS-DATEI
               END-IF
               IF EM-DOS-DATEI IS NOT NUMERIC
                   MOVE 'ER' TO WS-CA-RETURN-CODE
                   MOVE 'DATE OF SERVICE MUST BE CCYYMMDD'
                       TO WS-CA-RETURN-MSG
                   PERFORM 9000-SEND-ERROR
               END-IF
           END-IF.

       3000-QUERY-ELIGIBILITY.
           MOVE EM-MEMBER-IDI TO WS-DB2-MEMBER-ID
           MOVE EM-DOS-DATEI  TO WS-DB2-DOS

           EXEC SQL
               SELECT M.MEMBER_ID,
                      M.LAST_NAME,
                      M.FIRST_NAME,
                      M.DATE_OF_BIRTH,
                      M.SEX_CODE,
                      E.GROUP_ID,
                      E.PLAN_CODE,
                      E.PRODUCT_TYPE,
                      E.COVERAGE_TYPE,
                      E.EFF_DATE,
                      E.TERM_DATE,
                      E.ELIG_STATUS,
                      A.DEDUCTIBLE_YTD,
                      A.DEDUCTIBLE_LIMIT,
                      A.OOP_YTD,
                      A.OOP_LIMIT,
                      P.COPAY_PCP,
                      C.PCP_NPI,
                      C.PCP_NAME
               INTO :DB-MEMBER-ID,
                    :DB-LAST-NAME,
                    :DB-FIRST-NAME,
                    :DB-DOB,
                    :DB-SEX,
                    :DB-GROUP-ID,
                    :DB-PLAN-CODE,
                    :DB-PRODUCT-TYPE,
                    :DB-COVERAGE-TYPE,
                    :DB-EFF-DATE,
                    :DB-TERM-DATE,
                    :DB-ELIG-STATUS,
                    :DB-DEDUCT-YTD,
                    :DB-DEDUCT-LIMIT,
                    :DB-OOP-YTD,
                    :DB-OOP-LIMIT,
                    :DB-COPAY-PCP,
                    :DB-PCP-NPI,
                    :DB-PCP-NAME
               FROM  HCAS.MEMBER M
               INNER JOIN HCAS.ENROLLMENT E
                   ON M.MEMBER_ID = E.MEMBER_ID
               INNER JOIN HCAS.ACCUMULATORS A
                   ON M.MEMBER_ID = A.MEMBER_ID
                  AND E.PLAN_CODE = A.PLAN_CODE
               INNER JOIN HCAS.BENEFIT_PLAN P
                   ON E.PLAN_CODE = P.PLAN_CODE
               LEFT  JOIN HCAS.PCP_ASSIGNMENT C
                   ON M.MEMBER_ID = C.MEMBER_ID
               WHERE M.MEMBER_ID = :WS-DB2-MEMBER-ID
                 AND E.EFF_DATE  <= :WS-DB2-DOS
                 AND (E.TERM_DATE >= :WS-DB2-DOS
                      OR E.TERM_DATE = '00000000')
                 AND E.ELIG_STATUS = 'A'
               FETCH FIRST 1 ROW ONLY
           END-EXEC

           EVALUATE SQLCODE
               WHEN ZERO
                   PERFORM 3100-FORMAT-RESPONSE
               WHEN +100
                   MOVE 'NF' TO WS-CA-RETURN-CODE
                   MOVE 'NO ACTIVE ELIGIBILITY FOUND FOR MEMBER'
                       TO WS-CA-RETURN-MSG
                   PERFORM 9000-SEND-ERROR
               WHEN OTHER
                   MOVE 'ER' TO WS-CA-RETURN-CODE
                   STRING 'DB2 ERROR SQLCODE=' DELIMITED SIZE
                          SQLCODE              DELIMITED SIZE
                          INTO WS-CA-RETURN-MSG
                   END-STRING
                   PERFORM 9000-SEND-ERROR
           END-EVALUATE.

       3100-FORMAT-RESPONSE.
           INITIALIZE ELIGMP1O
           STRING DB-FIRST-NAME DELIMITED SPACES
                  ' '           DELIMITED SIZE
                  DB-LAST-NAME  DELIMITED SPACES
                  INTO EM-NAME-OUT
           END-STRING
           STRING DB-DOB(1:4)  DELIMITED SIZE
                  '/'          DELIMITED SIZE
                  DB-DOB(5:2)  DELIMITED SIZE
                  '/'          DELIMITED SIZE
                  DB-DOB(7:2)  DELIMITED SIZE
                  INTO EM-DOB-OUT
           END-STRING
           MOVE DB-GROUP-ID     TO EM-GROUP-OUT
           MOVE DB-PLAN-CODE    TO EM-PLAN-OUT
           MOVE DB-PRODUCT-TYPE TO EM-PRODUCT-OUT
           EVALUATE DB-ELIG-STATUS
               WHEN 'A'
                   MOVE 'ACTIVE'    TO EM-STATUS-OUT
               WHEN 'T'
                   MOVE 'TERMINATED' TO EM-STATUS-OUT
               WHEN 'P'
                   MOVE 'PENDED'    TO EM-STATUS-OUT
               WHEN 'C'
                   MOVE 'COBRA'     TO EM-STATUS-OUT
               WHEN OTHER
                   MOVE 'UNKNOWN'   TO EM-STATUS-OUT
           END-EVALUATE
           COMPUTE WS-DEDUCT-REMAINING =
               DB-DEDUCT-LIMIT - DB-DEDUCT-YTD
           COMPUTE WS-OOP-REMAINING =
               DB-OOP-LIMIT - DB-OOP-YTD
           MOVE WS-DEDUCT-REMAINING TO WS-DISPLAY-AMOUNT
           MOVE WS-DISPLAY-AMOUNT   TO EM-DEDUCT-REM-OUT
           MOVE WS-OOP-REMAINING    TO WS-DISPLAY-AMOUNT
           MOVE WS-DISPLAY-AMOUNT   TO EM-OOP-REM-OUT
           MOVE DB-COPAY-PCP         TO WS-DISPLAY-AMOUNT
           MOVE WS-DISPLAY-AMOUNT   TO EM-COPAY-OUT
           IF DB-PCP-NPI NOT = SPACES
               STRING DB-PCP-NAME  DELIMITED SPACES
                      ' (NPI:'     DELIMITED SIZE
                      DB-PCP-NPI   DELIMITED SIZE
                      ')'          DELIMITED SIZE
                      INTO EM-PCP-OUT
               END-STRING
           ELSE
               MOVE 'NO PCP ASSIGNED' TO EM-PCP-OUT
           END-IF
           MOVE 'MEMBER ELIGIBILITY VERIFIED'
               TO EM-MSG-TEXTI
           EXEC CICS SEND
               MAP(WS-BMS-MAP-NAME)
               MAPSET(WS-BMS-MAPSET)
               DATAONLY
               CURSOR
           END-EXEC.

       9000-SEND-ERROR.
           MOVE WS-CA-RETURN-MSG TO EM-MSG-TEXTI
           EXEC CICS SEND
               MAP(WS-BMS-MAP-NAME)
               MAPSET(WS-BMS-MAPSET)
               DATAONLY
               CURSOR
           END-EXEC.
