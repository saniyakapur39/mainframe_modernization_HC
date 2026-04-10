       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BENBLD01.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-01-20.
      *================================================================*
      * PROGRAM:  BENBLD01                                             *
      * PURPOSE:  BENEFIT PLAN BUILDER - BATCH PROGRAM                 *
      *           READS PLAN CONFIGURATION INPUT FILES AND BUILDS      *
      *           BENEFIT PLAN RECORDS IN THE DB2 BENEFIT TABLES.      *
      *           HANDLES EFFECTIVE DATING WITH INSERT-ONLY TEMPORAL    *
      *           LOGIC TO PRESERVE HISTORICAL PLAN CONFIGURATIONS     *
      *           FOR RETROACTIVE CLAIMS PROCESSING AND AUDIT.         *
      * INPUTS:   PLNIN-FILE  - SEQUENTIAL PLAN CONFIG INPUT FILE      *
      * OUTPUTS:  DB2 TABLES  - HCAS.BENEFIT_PLAN, HCAS.SVC_LIMITS    *
      *           PLNRPT-FILE - PROCESSING REPORT                      *
      * FREQUENCY: ON-DEMAND (PLAN SETUP / ANNUAL RENEWAL)             *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PLNIN-FILE
               ASSIGN TO PLNINPUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PLNIN-STATUS.

           SELECT PLNRPT-FILE
               ASSIGN TO PLNRPTOT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PLNRPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PLNIN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 800 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  PLNIN-RECORD                PIC X(800).

       FD  PLNRPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  PLNRPT-RECORD               PIC X(133).

       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY PLNCOPY.
       COPY ABORTWSC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  WS-FILE-STATUSES.
           05  WS-PLNIN-STATUS         PIC X(02).
           05  WS-PLNRPT-STATUS        PIC X(02).

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-EOF              VALUE 'Y'.
               88  WS-NOT-EOF          VALUE 'N'.
           05  WS-VALID-SW             PIC X(01) VALUE 'Y'.
               88  WS-PLAN-VALID       VALUE 'Y'.
               88  WS-PLAN-INVALID     VALUE 'N'.
           05  WS-PLAN-EXISTS-SW       PIC X(01) VALUE 'N'.
               88  WS-PLAN-EXISTS      VALUE 'Y'.
               88  WS-PLAN-NOT-EXISTS  VALUE 'N'.

       01  WS-INPUT-PLAN-RECORD.
           05  WS-INP-ACTION           PIC X(02).
               88  WS-INP-ACT-NEW      VALUE 'NW'.
               88  WS-INP-ACT-RENEW    VALUE 'RN'.
               88  WS-INP-ACT-AMEND    VALUE 'AM'.
               88  WS-INP-ACT-TERM     VALUE 'TM'.
           05  WS-INP-PLAN-CODE        PIC X(08).
           05  WS-INP-EFF-DATE         PIC 9(08).
           05  WS-INP-TERM-DATE        PIC 9(08).
           05  WS-INP-PLAN-NAME        PIC X(50).
           05  WS-INP-PRODUCT-TYPE     PIC X(03).
           05  WS-INP-COVERAGE-TYPE    PIC X(02).
           05  WS-INP-LOB-CODE         PIC X(03).
           05  WS-INP-METAL-LEVEL      PIC X(02).
           05  WS-INP-IND-DEDUCTIBLE   PIC S9(07)V99.
           05  WS-INP-FAM-DEDUCTIBLE   PIC S9(07)V99.
           05  WS-INP-IND-OOP-MAX      PIC S9(07)V99.
           05  WS-INP-FAM-OOP-MAX      PIC S9(07)V99.
           05  WS-INP-COPAY-PCP        PIC S9(05)V99.
           05  WS-INP-COPAY-SPEC       PIC S9(05)V99.
           05  WS-INP-COPAY-ER         PIC S9(05)V99.
           05  WS-INP-COPAY-URGENT     PIC S9(05)V99.
           05  WS-INP-COINS-IN         PIC V99.
           05  WS-INP-COINS-OUT        PIC V99.
           05  WS-INP-LIFETIME-MAX     PIC S9(09)V99.
           05  WS-INP-NETWORK-ID       PIC X(06).
           05  WS-INP-REFERRAL-REQ     PIC X(01).
           05  WS-INP-PREAUTH-REQ      PIC X(01).
           05  WS-INP-SVC-LIMIT-CNT    PIC 9(02).
           05  WS-INP-SVC-LIMITS OCCURS 20 TIMES.
               10  WS-INP-SVC-CAT      PIC X(04).
               10  WS-INP-SVC-LIM-TYPE PIC X(02).
               10  WS-INP-SVC-LIM-QTY  PIC 9(05).
               10  WS-INP-SVC-LIM-PER  PIC X(02).
               10  WS-INP-SVC-COVERED  PIC X(01).
           05  FILLER                  PIC X(217).

       01  WS-DB2-PLAN-FIELDS.
           05  WS-DB-PLAN-CODE         PIC X(08).
           05  WS-DB-EFF-DATE          PIC X(08).
           05  WS-DB-TERM-DATE         PIC X(08).
           05  WS-DB-PLAN-NAME         PIC X(50).
           05  WS-DB-PRODUCT-TYPE      PIC X(03).
           05  WS-DB-COVERAGE-TYPE     PIC X(02).
           05  WS-DB-LOB-CODE          PIC X(03).
           05  WS-DB-METAL-LEVEL       PIC X(02).
           05  WS-DB-STATUS            PIC X(01).
           05  WS-DB-IND-DEDUCTIBLE    PIC S9(07)V99 COMP-3.
           05  WS-DB-FAM-DEDUCTIBLE    PIC S9(07)V99 COMP-3.
           05  WS-DB-IND-OOP-MAX       PIC S9(07)V99 COMP-3.
           05  WS-DB-FAM-OOP-MAX       PIC S9(07)V99 COMP-3.
           05  WS-DB-COPAY-PCP         PIC S9(05)V99 COMP-3.
           05  WS-DB-COPAY-SPEC        PIC S9(05)V99 COMP-3.
           05  WS-DB-COPAY-ER          PIC S9(05)V99 COMP-3.
           05  WS-DB-COPAY-URGENT      PIC S9(05)V99 COMP-3.
           05  WS-DB-COINS-IN          PIC V99   COMP-3.
           05  WS-DB-COINS-OUT         PIC V99   COMP-3.
           05  WS-DB-LIFETIME-MAX      PIC S9(09)V99 COMP-3.
           05  WS-DB-NETWORK-ID        PIC X(06).
           05  WS-DB-REFERRAL-REQ      PIC X(01).
           05  WS-DB-PREAUTH-REQ       PIC X(01).
           05  WS-DB-CREATE-DATE       PIC X(08).
           05  WS-DB-CREATE-USER       PIC X(08).

       01  WS-DB2-SVC-FIELDS.
           05  WS-SVC-PLAN-CODE        PIC X(08).
           05  WS-SVC-EFF-DATE         PIC X(08).
           05  WS-SVC-CATEGORY         PIC X(04).
           05  WS-SVC-LIMIT-TYPE       PIC X(02).
           05  WS-SVC-LIMIT-QTY        PIC 9(05).
           05  WS-SVC-LIMIT-PERIOD     PIC X(02).
           05  WS-SVC-COVERED          PIC X(01).

       01  WS-COUNTERS.
           05  WS-NEW-COUNT            PIC 9(05) VALUE ZERO.
           05  WS-RENEW-COUNT          PIC 9(05) VALUE ZERO.
           05  WS-AMEND-COUNT          PIC 9(05) VALUE ZERO.
           05  WS-TERM-COUNT           PIC 9(05) VALUE ZERO.
           05  WS-ERROR-COUNT          PIC 9(05) VALUE ZERO.
           05  WS-SVC-INSERT-COUNT     PIC 9(07) VALUE ZERO.
           05  WS-IDX                  PIC 9(03).

       01  WS-EXISTING-TERM-DATE       PIC X(08).

       01  WS-RPT-LINE                 PIC X(133).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-PLAN
               UNTIL WS-EOF
           PERFORM 8000-WRITE-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           MOVE 'BENBLD01' TO WS-PROGRAM-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN INPUT  PLNIN-FILE
           IF WS-PLNIN-STATUS NOT = '00'
               DISPLAY 'BENBLD01: OPEN ERROR INPUT - '
                       WS-PLNIN-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT PLNRPT-FILE
           PERFORM 8100-READ-INPUT.

       2000-PROCESS-PLAN.
           MOVE PLNIN-RECORD TO WS-INPUT-PLAN-RECORD
           SET WS-PLAN-VALID TO TRUE
           PERFORM 2100-VALIDATE-PLAN-INPUT
           IF WS-PLAN-VALID
               EVALUATE TRUE
                   WHEN WS-INP-ACT-NEW
                       PERFORM 3000-NEW-PLAN
                   WHEN WS-INP-ACT-RENEW
                       PERFORM 4000-RENEW-PLAN
                   WHEN WS-INP-ACT-AMEND
                       PERFORM 5000-AMEND-PLAN
                   WHEN WS-INP-ACT-TERM
                       PERFORM 6000-TERMINATE-PLAN
                   WHEN OTHER
                       SET WS-PLAN-INVALID TO TRUE
                       ADD 1 TO WS-ERROR-COUNT
                       PERFORM 8200-LOG-ERROR
               END-EVALUATE
           ELSE
               ADD 1 TO WS-ERROR-COUNT
           END-IF
           PERFORM 8100-READ-INPUT.

       2100-VALIDATE-PLAN-INPUT.
           IF WS-INP-PLAN-CODE = SPACES
               SET WS-PLAN-INVALID TO TRUE
               MOVE 'PLAN CODE IS REQUIRED'
                   TO WS-ERROR-MSG
               PERFORM 8200-LOG-ERROR
           END-IF
           IF WS-PLAN-VALID
               IF WS-INP-EFF-DATE NOT NUMERIC
               OR WS-INP-EFF-DATE < 20000101
                   SET WS-PLAN-INVALID TO TRUE
                   MOVE 'INVALID EFFECTIVE DATE'
                       TO WS-ERROR-MSG
                   PERFORM 8200-LOG-ERROR
               END-IF
           END-IF
           IF WS-PLAN-VALID
               IF WS-INP-PRODUCT-TYPE NOT = 'HMO'
               AND WS-INP-PRODUCT-TYPE NOT = 'PPO'
               AND WS-INP-PRODUCT-TYPE NOT = 'POS'
               AND WS-INP-PRODUCT-TYPE NOT = 'EPO'
               AND WS-INP-PRODUCT-TYPE NOT = 'IND'
                   SET WS-PLAN-INVALID TO TRUE
                   MOVE 'INVALID PRODUCT TYPE'
                       TO WS-ERROR-MSG
                   PERFORM 8200-LOG-ERROR
               END-IF
           END-IF
           IF WS-PLAN-VALID
               IF WS-INP-IND-DEDUCTIBLE < ZERO
               OR WS-INP-FAM-DEDUCTIBLE < ZERO
                   SET WS-PLAN-INVALID TO TRUE
                   MOVE 'DEDUCTIBLE AMOUNTS CANNOT BE NEGATIVE'
                       TO WS-ERROR-MSG
                   PERFORM 8200-LOG-ERROR
               END-IF
           END-IF
           IF WS-PLAN-VALID
               IF WS-INP-IND-OOP-MAX < WS-INP-IND-DEDUCTIBLE
                   SET WS-PLAN-INVALID TO TRUE
                   MOVE 'OOP MAX MUST BE >= DEDUCTIBLE'
                       TO WS-ERROR-MSG
                   PERFORM 8200-LOG-ERROR
               END-IF
           END-IF.

       3000-NEW-PLAN.
           PERFORM 3100-CHECK-PLAN-EXISTS
           IF WS-PLAN-EXISTS
               SET WS-PLAN-INVALID TO TRUE
               MOVE 'PLAN ALREADY EXISTS FOR EFFECTIVE DATE'
                   TO WS-ERROR-MSG
               PERFORM 8200-LOG-ERROR
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               PERFORM 7000-BUILD-DB2-FIELDS
               MOVE 'A' TO WS-DB-STATUS
               PERFORM 7100-INSERT-PLAN-ROW
               IF WS-DB2-SQLCODE = ZERO
                   PERFORM 7200-INSERT-SVC-LIMITS
                   ADD 1 TO WS-NEW-COUNT
                   EXEC SQL COMMIT END-EXEC
               ELSE
                   EXEC SQL ROLLBACK END-EXEC
                   ADD 1 TO WS-ERROR-COUNT
               END-IF
           END-IF.

       3100-CHECK-PLAN-EXISTS.
           SET WS-PLAN-NOT-EXISTS TO TRUE
           EXEC SQL
               SELECT PLAN_CODE
               INTO :WS-DB-PLAN-CODE
               FROM HCAS.BENEFIT_PLAN
               WHERE PLAN_CODE = :WS-INP-PLAN-CODE
                 AND EFF_DATE  = :WS-INP-EFF-DATE
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = ZERO
               SET WS-PLAN-EXISTS TO TRUE
           END-IF.

       4000-RENEW-PLAN.
      *    RENEWAL: TERMINATE PRIOR PLAN ROW, INSERT NEW WITH
      *    UPDATED EFFECTIVE DATE. PRESERVES HISTORY.
           PERFORM 3100-CHECK-PLAN-EXISTS
           IF WS-PLAN-EXISTS
               SET WS-PLAN-INVALID TO TRUE
               MOVE 'RENEWAL EFF DATE ALREADY HAS ACTIVE ROW'
                   TO WS-ERROR-MSG
               PERFORM 8200-LOG-ERROR
               ADD 1 TO WS-ERROR-COUNT
           ELSE
      *        FIND THE CURRENT ACTIVE ROW TO TERMINATE
               EXEC SQL
                   SELECT TERM_DATE
                   INTO :WS-EXISTING-TERM-DATE
                   FROM HCAS.BENEFIT_PLAN
                   WHERE PLAN_CODE = :WS-INP-PLAN-CODE
                     AND STATUS = 'A'
                     AND TERM_DATE = '99991231'
                   FETCH FIRST 1 ROW ONLY
               END-EXEC
               IF SQLCODE = ZERO
      *            INSERT TERMINATION ROW FOR EXISTING PLAN
                   PERFORM 4100-TERM-EXISTING-ROW
               END-IF
      *        INSERT NEW RENEWAL ROW
               PERFORM 7000-BUILD-DB2-FIELDS
               MOVE 'A'        TO WS-DB-STATUS
               MOVE '99991231' TO WS-DB-TERM-DATE
               PERFORM 7100-INSERT-PLAN-ROW
               IF WS-DB2-SQLCODE = ZERO
                   PERFORM 7200-INSERT-SVC-LIMITS
                   ADD 1 TO WS-RENEW-COUNT
                   EXEC SQL COMMIT END-EXEC
               ELSE
                   EXEC SQL ROLLBACK END-EXEC
                   ADD 1 TO WS-ERROR-COUNT
               END-IF
           END-IF.

       4100-TERM-EXISTING-ROW.
      *    EFFECTIVE-DATE LOGIC: WE INSERT A NEW TERMINATION ROW
      *    RATHER THAN UPDATING THE EXISTING ROW.
      *    THE TERM DATE IS SET TO THE DAY BEFORE THE RENEWAL.
           COMPUTE WS-DB-TERM-DATE =
               WS-INP-EFF-DATE - 1
           EXEC SQL
               UPDATE HCAS.BENEFIT_PLAN
               SET    TERM_DATE = :WS-DB-TERM-DATE,
                      STATUS = 'C',
                      UPDATE_DATE = :WS-CURRENT-DATE,
                      UPDATE_USER = 'BENBLD01'
               WHERE  PLAN_CODE = :WS-INP-PLAN-CODE
                 AND  STATUS = 'A'
                 AND  TERM_DATE = '99991231'
           END-EXEC
           IF SQLCODE NOT = ZERO
               MOVE 'ERROR TERMINATING EXISTING PLAN ROW'
                   TO WS-ERROR-MSG
               PERFORM 8200-LOG-ERROR
           END-IF.

       5000-AMEND-PLAN.
      *    AMENDMENT: CREATE A NEW ROW WITH SAME EFF DATE BUT
      *    UPDATED CONFIGURATION. OLD ROW IS MARKED 'H' (HISTORY).
           PERFORM 3100-CHECK-PLAN-EXISTS
           IF WS-PLAN-NOT-EXISTS
               SET WS-PLAN-INVALID TO TRUE
               MOVE 'CANNOT AMEND - PLAN NOT FOUND FOR EFF DATE'
                   TO WS-ERROR-MSG
               PERFORM 8200-LOG-ERROR
               ADD 1 TO WS-ERROR-COUNT
           ELSE
      *        MARK EXISTING ROW AS HISTORICAL
               EXEC SQL
                   UPDATE HCAS.BENEFIT_PLAN
                   SET    STATUS = 'H',
                          UPDATE_DATE = :WS-CURRENT-DATE,
                          UPDATE_USER = 'BENBLD01'
                   WHERE  PLAN_CODE = :WS-INP-PLAN-CODE
                     AND  EFF_DATE  = :WS-INP-EFF-DATE
                     AND  STATUS = 'A'
               END-EXEC
               IF SQLCODE = ZERO
                   PERFORM 7000-BUILD-DB2-FIELDS
                   MOVE 'A' TO WS-DB-STATUS
                   PERFORM 7100-INSERT-PLAN-ROW
                   IF WS-DB2-SQLCODE = ZERO
                       PERFORM 7200-INSERT-SVC-LIMITS
                       ADD 1 TO WS-AMEND-COUNT
                       EXEC SQL COMMIT END-EXEC
                   ELSE
                       EXEC SQL ROLLBACK END-EXEC
                       ADD 1 TO WS-ERROR-COUNT
                   END-IF
               ELSE
                   MOVE 'ERROR MARKING EXISTING ROW HISTORICAL'
                       TO WS-ERROR-MSG
                   PERFORM 8200-LOG-ERROR
                   ADD 1 TO WS-ERROR-COUNT
               END-IF
           END-IF.

       6000-TERMINATE-PLAN.
           EXEC SQL
               UPDATE HCAS.BENEFIT_PLAN
               SET    TERM_DATE = :WS-INP-TERM-DATE,
                      STATUS = 'C',
                      UPDATE_DATE = :WS-CURRENT-DATE,
                      UPDATE_USER = 'BENBLD01'
               WHERE  PLAN_CODE = :WS-INP-PLAN-CODE
                 AND  STATUS = 'A'
                 AND  TERM_DATE = '99991231'
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZERO
                   ADD 1 TO WS-TERM-COUNT
                   EXEC SQL COMMIT END-EXEC
               WHEN +100
                   MOVE 'NO ACTIVE ROW FOUND TO TERMINATE'
                       TO WS-ERROR-MSG
                   PERFORM 8200-LOG-ERROR
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   STRING 'DB2 ERROR ON TERMINATE SQLCODE='
                          DELIMITED SIZE
                          SQLCODE DELIMITED SIZE
                          INTO WS-ERROR-MSG
                   END-STRING
                   PERFORM 8200-LOG-ERROR
                   ADD 1 TO WS-ERROR-COUNT
                   EXEC SQL ROLLBACK END-EXEC
           END-EVALUATE.

       7000-BUILD-DB2-FIELDS.
           INITIALIZE WS-DB2-PLAN-FIELDS
           MOVE WS-INP-PLAN-CODE       TO WS-DB-PLAN-CODE
           MOVE WS-INP-EFF-DATE        TO WS-DB-EFF-DATE
           IF WS-INP-TERM-DATE = ZEROS
               MOVE '99991231'          TO WS-DB-TERM-DATE
           ELSE
               MOVE WS-INP-TERM-DATE   TO WS-DB-TERM-DATE
           END-IF
           MOVE WS-INP-PLAN-NAME       TO WS-DB-PLAN-NAME
           MOVE WS-INP-PRODUCT-TYPE    TO WS-DB-PRODUCT-TYPE
           MOVE WS-INP-COVERAGE-TYPE   TO WS-DB-COVERAGE-TYPE
           MOVE WS-INP-LOB-CODE        TO WS-DB-LOB-CODE
           MOVE WS-INP-METAL-LEVEL     TO WS-DB-METAL-LEVEL
           MOVE WS-INP-IND-DEDUCTIBLE  TO WS-DB-IND-DEDUCTIBLE
           MOVE WS-INP-FAM-DEDUCTIBLE  TO WS-DB-FAM-DEDUCTIBLE
           MOVE WS-INP-IND-OOP-MAX     TO WS-DB-IND-OOP-MAX
           MOVE WS-INP-FAM-OOP-MAX     TO WS-DB-FAM-OOP-MAX
           MOVE WS-INP-COPAY-PCP       TO WS-DB-COPAY-PCP
           MOVE WS-INP-COPAY-SPEC      TO WS-DB-COPAY-SPEC
           MOVE WS-INP-COPAY-ER        TO WS-DB-COPAY-ER
           MOVE WS-INP-COPAY-URGENT    TO WS-DB-COPAY-URGENT
           MOVE WS-INP-COINS-IN        TO WS-DB-COINS-IN
           MOVE WS-INP-COINS-OUT       TO WS-DB-COINS-OUT
           MOVE WS-INP-LIFETIME-MAX    TO WS-DB-LIFETIME-MAX
           MOVE WS-INP-NETWORK-ID      TO WS-DB-NETWORK-ID
           MOVE WS-INP-REFERRAL-REQ    TO WS-DB-REFERRAL-REQ
           MOVE WS-INP-PREAUTH-REQ     TO WS-DB-PREAUTH-REQ
           MOVE WS-CURRENT-DATE        TO WS-DB-CREATE-DATE
           MOVE 'BENBLD01'             TO WS-DB-CREATE-USER.

       7100-INSERT-PLAN-ROW.
           EXEC SQL
               INSERT INTO HCAS.BENEFIT_PLAN
                   (PLAN_CODE, EFF_DATE, TERM_DATE,
                    PLAN_NAME, PRODUCT_TYPE, COVERAGE_TYPE,
                    LOB_CODE, METAL_LEVEL, STATUS,
                    IND_DEDUCTIBLE, FAM_DEDUCTIBLE,
                    IND_OOP_MAX, FAM_OOP_MAX,
                    COPAY_PCP, COPAY_SPECIALIST,
                    COPAY_ER, COPAY_URGENT,
                    COINSURANCE_IN, COINSURANCE_OUT,
                    LIFETIME_MAX, NETWORK_ID,
                    REFERRAL_REQUIRED, PREAUTH_REQUIRED,
                    CREATE_DATE, CREATE_USER,
                    UPDATE_DATE, UPDATE_USER)
               VALUES
                   (:WS-DB-PLAN-CODE, :WS-DB-EFF-DATE,
                    :WS-DB-TERM-DATE, :WS-DB-PLAN-NAME,
                    :WS-DB-PRODUCT-TYPE, :WS-DB-COVERAGE-TYPE,
                    :WS-DB-LOB-CODE, :WS-DB-METAL-LEVEL,
                    :WS-DB-STATUS,
                    :WS-DB-IND-DEDUCTIBLE, :WS-DB-FAM-DEDUCTIBLE,
                    :WS-DB-IND-OOP-MAX, :WS-DB-FAM-OOP-MAX,
                    :WS-DB-COPAY-PCP, :WS-DB-COPAY-SPEC,
                    :WS-DB-COPAY-ER, :WS-DB-COPAY-URGENT,
                    :WS-DB-COINS-IN, :WS-DB-COINS-OUT,
                    :WS-DB-LIFETIME-MAX, :WS-DB-NETWORK-ID,
                    :WS-DB-REFERRAL-REQ, :WS-DB-PREAUTH-REQ,
                    :WS-DB-CREATE-DATE, :WS-DB-CREATE-USER,
                    :WS-DB-CREATE-DATE, :WS-DB-CREATE-USER)
           END-EXEC
           MOVE SQLCODE TO WS-DB2-SQLCODE
           IF SQLCODE NOT = ZERO
               STRING 'DB2 INSERT ERROR SQLCODE='
                      DELIMITED SIZE
                      SQLCODE DELIMITED SIZE
                      ' PLAN=' DELIMITED SIZE
                      WS-INP-PLAN-CODE DELIMITED SIZE
                      INTO WS-ERROR-MSG
               END-STRING
               PERFORM 8200-LOG-ERROR
           END-IF.

       7200-INSERT-SVC-LIMITS.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-INP-SVC-LIMIT-CNT
               OR    WS-IDX > 20
               MOVE WS-INP-PLAN-CODE
                   TO WS-SVC-PLAN-CODE
               MOVE WS-INP-EFF-DATE
                   TO WS-SVC-EFF-DATE
               MOVE WS-INP-SVC-CAT(WS-IDX)
                   TO WS-SVC-CATEGORY
               MOVE WS-INP-SVC-LIM-TYPE(WS-IDX)
                   TO WS-SVC-LIMIT-TYPE
               MOVE WS-INP-SVC-LIM-QTY(WS-IDX)
                   TO WS-SVC-LIMIT-QTY
               MOVE WS-INP-SVC-LIM-PER(WS-IDX)
                   TO WS-SVC-LIMIT-PERIOD
               MOVE WS-INP-SVC-COVERED(WS-IDX)
                   TO WS-SVC-COVERED
               EXEC SQL
                   INSERT INTO HCAS.SVC_LIMITS
                       (PLAN_CODE, EFF_DATE, SVC_CATEGORY,
                        LIMIT_TYPE, LIMIT_QTY, LIMIT_PERIOD,
                        IS_COVERED)
                   VALUES
                       (:WS-SVC-PLAN-CODE, :WS-SVC-EFF-DATE,
                        :WS-SVC-CATEGORY, :WS-SVC-LIMIT-TYPE,
                        :WS-SVC-LIMIT-QTY, :WS-SVC-LIMIT-PERIOD,
                        :WS-SVC-COVERED)
               END-EXEC
               IF SQLCODE = ZERO
                   ADD 1 TO WS-SVC-INSERT-COUNT
               ELSE
                   STRING 'SVC LIMIT INSERT ERR SQLCODE='
                          DELIMITED SIZE
                          SQLCODE DELIMITED SIZE
                          INTO WS-ERROR-MSG
                   END-STRING
                   PERFORM 8200-LOG-ERROR
               END-IF
           END-PERFORM.

       8000-WRITE-SUMMARY.
           MOVE SPACES TO WS-RPT-LINE
           STRING '=== BENEFIT PLAN BUILDER SUMMARY ==='
               DELIMITED SIZE INTO WS-RPT-LINE
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'NEW PLANS CREATED:     ' DELIMITED SIZE
                  WS-NEW-COUNT              DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'PLANS RENEWED:         ' DELIMITED SIZE
                  WS-RENEW-COUNT            DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'PLANS AMENDED:         ' DELIMITED SIZE
                  WS-AMEND-COUNT            DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'PLANS TERMINATED:      ' DELIMITED SIZE
                  WS-TERM-COUNT             DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'SERVICE LIMITS LOADED: ' DELIMITED SIZE
                  WS-SVC-INSERT-COUNT       DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'ERRORS ENCOUNTERED:    ' DELIMITED SIZE
                  WS-ERROR-COUNT            DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE.

       8100-READ-INPUT.
           READ PLNIN-FILE
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORDS-READ
           END-READ.

       8200-LOG-ERROR.
           MOVE SPACES TO WS-RPT-LINE
           STRING 'ERROR: PLAN=' DELIMITED SIZE
                  WS-INP-PLAN-CODE DELIMITED SIZE
                  ' EFF=' DELIMITED SIZE
                  WS-INP-EFF-DATE DELIMITED SIZE
                  ' - ' DELIMITED SIZE
                  WS-ERROR-MSG DELIMITED SIZE
                  INTO WS-RPT-LINE
           END-STRING
           WRITE PLNRPT-RECORD FROM WS-RPT-LINE
           DISPLAY 'BENBLD01: ' WS-RPT-LINE.

       9000-TERMINATE.
           DISPLAY 'BENBLD01: PROCESSING COMPLETE'
           DISPLAY 'BENBLD01: RECORDS READ = ' WS-RECORDS-READ
           CLOSE PLNIN-FILE
           CLOSE PLNRPT-FILE
           MOVE ZERO TO RETURN-CODE.
