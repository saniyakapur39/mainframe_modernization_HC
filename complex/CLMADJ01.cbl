       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CLMADJ01.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-05-15.
      *================================================================*
      * PROGRAM:  CLMADJ01                                             *
      * PURPOSE:  CLAIMS ADJUDICATION ENGINE - MAIN DRIVER             *
      *           MASTER BATCH PROGRAM THAT ORCHESTRATES THE COMPLETE  *
      *           CLAIMS ADJUDICATION PIPELINE. READS VALIDATED CLAIMS *
      *           FROM THE INTAKE STAGING TABLE, PERFORMS MULTI-PHASE   *
      *           ADJUDICATION INCLUDING ELIGIBILITY VERIFICATION,     *
      *           PROVIDER VALIDATION, BENEFIT APPLICATION, PRICING,   *
      *           DUPLICATE CHECKING, AND COB PROCESSING.              *
      *           CALLS SUBROUTINES VIA CALL VERB FOR PROVIDER         *
      *           VALIDATION (PRVVAL02) AND PRICING (CLMPRC01).        *
      * INPUTS:   DB2 TABLES - HCAS.CLAIM_STAGING                     *
      *           DB2 TABLES - HCAS.MEMBER, HCAS.ENROLLMENT            *
      * OUTPUTS:  DB2 TABLES - HCAS.CLAIM_MASTER, HCAS.CLAIM_LINE     *
      *           CLMRPT-FILE - ADJUDICATION PROCESSING REPORT         *
      * FREQUENCY: DAILY BATCH / MULTIPLE RUNS PER DAY                 *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLMRPT-FILE
               ASSIGN TO CLMRPTOT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLMRPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  CLMRPT-RECORD               PIC X(133).

       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY CLMCOPY.
       COPY MBRCOPY.
       COPY PRVCOPY.
       COPY PLNCOPY.
       COPY ABORTWSC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  WS-RPT-STATUS               PIC X(02).

       01  WS-SWITCHES.
           05  WS-MORE-CLAIMS-SW       PIC X(01) VALUE 'Y'.
               88  WS-MORE-CLAIMS      VALUE 'Y'.
               88  WS-NO-MORE-CLAIMS   VALUE 'N'.
           05  WS-CLAIM-VALID-SW       PIC X(01) VALUE 'Y'.
               88  WS-CLAIM-VALID      VALUE 'Y'.
               88  WS-CLAIM-INVALID    VALUE 'N'.
           05  WS-ELIG-VERIFIED-SW     PIC X(01) VALUE 'N'.
               88  WS-ELIG-VERIFIED    VALUE 'Y'.
               88  WS-ELIG-NOT-VERIFIED VALUE 'N'.
           05  WS-PRV-VALIDATED-SW     PIC X(01) VALUE 'N'.
               88  WS-PRV-VALIDATED    VALUE 'Y'.
               88  WS-PRV-NOT-VALID    VALUE 'N'.
           05  WS-DUPLICATE-SW         PIC X(01) VALUE 'N'.
               88  WS-IS-DUPLICATE     VALUE 'Y'.
               88  WS-NOT-DUPLICATE    VALUE 'N'.

      *--- DB2 CURSOR FOR STAGED CLAIMS ---
           EXEC SQL DECLARE CLAIM-CURSOR CURSOR FOR
               SELECT CLAIM_ID,
                      CLAIM_TYPE,
                      MEMBER_ID,
                      SUBSCRIBER_ID,
                      PATIENT_ACCT_NO,
                      MEMBER_DOB,
                      MEMBER_SEX,
                      BILLING_NPI,
                      RENDERING_NPI,
                      FACILITY_NPI,
                      PLACE_OF_SERVICE,
                      PROVIDER_TAX_ID,
                      PRINCIPAL_DIAG,
                      ADMIT_DIAG,
                      TOTAL_CHARGES,
                      RECEIPT_DATE,
                      SOURCE_CODE
               FROM HCAS.CLAIM_STAGING
               WHERE PROCESS_STATUS = 'R'
               ORDER BY RECEIPT_DATE, CLAIM_ID
               FOR UPDATE OF PROCESS_STATUS
           END-EXEC

      *--- DB2 HOST VARIABLES FOR STAGED CLAIM ---
       01  WS-STG-CLAIM.
           05  WS-STG-CLAIM-ID         PIC X(15).
           05  WS-STG-CLAIM-TYPE       PIC X(02).
           05  WS-STG-MEMBER-ID        PIC X(12).
           05  WS-STG-SUBSCRIBER-ID    PIC X(12).
           05  WS-STG-PATIENT-ACCT     PIC X(20).
           05  WS-STG-MEMBER-DOB       PIC X(08).
           05  WS-STG-MEMBER-SEX       PIC X(01).
           05  WS-STG-BILLING-NPI      PIC X(10).
           05  WS-STG-RENDERING-NPI    PIC X(10).
           05  WS-STG-FACILITY-NPI     PIC X(10).
           05  WS-STG-PLACE-OF-SVC     PIC X(02).
           05  WS-STG-PROVIDER-TIN     PIC X(09).
           05  WS-STG-PRINC-DIAG       PIC X(08).
           05  WS-STG-ADMIT-DIAG       PIC X(08).
           05  WS-STG-TOTAL-CHARGES    PIC S9(09)V99 COMP-3.
           05  WS-STG-RECEIPT-DATE     PIC X(08).
           05  WS-STG-SOURCE-CODE      PIC X(02).

      *--- CLAIM LINE ITEMS FROM DB2 ---
           EXEC SQL DECLARE LINE-CURSOR CURSOR FOR
               SELECT LINE_SEQ,
                      PROC_CODE,
                      MODIFIER_1,
                      MODIFIER_2,
                      MODIFIER_3,
                      MODIFIER_4,
                      REV_CODE,
                      DOS_FROM,
                      DOS_THRU,
                      UNITS,
                      LINE_CHARGE,
                      NDC_CODE,
                      DRG_CODE
               FROM HCAS.CLAIM_LINE_STAGING
               WHERE CLAIM_ID = :WS-STG-CLAIM-ID
               ORDER BY LINE_SEQ
           END-EXEC

       01  WS-STG-LINE.
           05  WS-STG-LN-SEQ           PIC 9(03).
           05  WS-STG-LN-PROC          PIC X(05).
           05  WS-STG-LN-MOD1          PIC X(02).
           05  WS-STG-LN-MOD2          PIC X(02).
           05  WS-STG-LN-MOD3          PIC X(02).
           05  WS-STG-LN-MOD4          PIC X(02).
           05  WS-STG-LN-REV           PIC X(04).
           05  WS-STG-LN-DOS-FROM      PIC X(08).
           05  WS-STG-LN-DOS-THRU      PIC X(08).
           05  WS-STG-LN-UNITS         PIC S9(05)V99 COMP-3.
           05  WS-STG-LN-CHARGE        PIC S9(07)V99 COMP-3.
           05  WS-STG-LN-NDC           PIC X(11).
           05  WS-STG-LN-DRG           PIC X(04).

      *--- PROVIDER VALIDATION CALL AREAS ---
       01  WS-PRV-REQUEST.
           05  WS-PRV-REQ-FUNC         PIC X(02).
           05  WS-PRV-REQ-NPI          PIC X(10).
           05  WS-PRV-REQ-TAXONOMY     PIC X(10).
           05  WS-PRV-REQ-DOS          PIC X(08).
           05  WS-PRV-REQ-PROC         PIC X(05).
           05  WS-PRV-REQ-MOD          PIC X(02).
           05  WS-PRV-REQ-NETWORK      PIC X(06).
           05  WS-PRV-REQ-STATE        PIC X(02).

       01  WS-PRV-RESPONSE.
           05  WS-PRV-RSP-RC           PIC X(02).
           05  WS-PRV-RSP-EDITS.
               10  WS-PRV-RSP-EDIT OCCURS 10 TIMES PIC X(05).
           05  WS-PRV-RSP-NAME         PIC X(60).
           05  WS-PRV-RSP-PAR          PIC X(01).
           05  WS-PRV-RSP-NETWORK      PIC X(06).
           05  WS-PRV-RSP-CONTR-TYPE   PIC X(02).
           05  WS-PRV-RSP-FEE-SCHED    PIC X(08).
           05  WS-PRV-RSP-FEE-AMT      PIC S9(07)V99 COMP-3.
           05  WS-PRV-RSP-CRED         PIC X(02).
           05  WS-PRV-RSP-MSG          PIC X(80).

      *--- PRICING CALL AREAS ---
       01  WS-PRICING-REQUEST.
           05  WS-PRC-CLAIM-TYPE       PIC X(02).
           05  WS-PRC-PLAN-CODE        PIC X(08).
           05  WS-PRC-NETWORK-ID       PIC X(06).
           05  WS-PRC-CONTRACT-TYPE    PIC X(02).
           05  WS-PRC-FEE-SCHED-ID    PIC X(08).
           05  WS-PRC-LINE-COUNT       PIC 9(03).
           05  WS-PRC-LINES OCCURS 50 TIMES.
               10  WS-PRC-LN-SEQ       PIC 9(03).
               10  WS-PRC-LN-PROC      PIC X(05).
               10  WS-PRC-LN-MOD       PIC X(02).
               10  WS-PRC-LN-REV       PIC X(04).
               10  WS-PRC-LN-DOS       PIC X(08).
               10  WS-PRC-LN-UNITS     PIC S9(05)V99 COMP-3.
               10  WS-PRC-LN-CHARGE    PIC S9(07)V99 COMP-3.
               10  WS-PRC-LN-DRG       PIC X(04).

       01  WS-PRICING-RESPONSE.
           05  WS-PRC-RSP-RC           PIC X(02).
           05  WS-PRC-RSP-MSG          PIC X(80).
           05  WS-PRC-RSP-TOTAL-ALLOWED
                                       PIC S9(09)V99 COMP-3.
           05  WS-PRC-RSP-TOTAL-PAID   PIC S9(09)V99 COMP-3.
           05  WS-PRC-RSP-LINE-COUNT   PIC 9(03).
           05  WS-PRC-RSP-LINES OCCURS 50 TIMES.
               10  WS-PRC-RSP-LN-SEQ   PIC 9(03).
               10  WS-PRC-RSP-LN-ALLOW PIC S9(07)V99 COMP-3.
               10  WS-PRC-RSP-LN-PAID  PIC S9(07)V99 COMP-3.
               10  WS-PRC-RSP-LN-DENY  PIC X(05).
               10  WS-PRC-RSP-LN-ADJ   PIC X(05)
                                        OCCURS 5 TIMES.

      *--- ELIGIBILITY QUERY RESULTS ---
       01  WS-ELIG-DATA.
           05  WS-ELIG-STATUS          PIC X(01).
           05  WS-ELIG-PLAN-CODE       PIC X(08).
           05  WS-ELIG-GROUP-ID        PIC X(10).
           05  WS-ELIG-PRODUCT-TYPE    PIC X(03).
           05  WS-ELIG-COVERAGE-TYPE   PIC X(02).
           05  WS-ELIG-EFF-DATE        PIC X(08).
           05  WS-ELIG-TERM-DATE       PIC X(08).
           05  WS-ELIG-NETWORK-ID      PIC X(06).
           05  WS-ELIG-DEDUCT-YTD      PIC S9(07)V99 COMP-3.
           05  WS-ELIG-DEDUCT-LIMIT    PIC S9(07)V99 COMP-3.
           05  WS-ELIG-OOP-YTD         PIC S9(07)V99 COMP-3.
           05  WS-ELIG-OOP-LIMIT       PIC S9(07)V99 COMP-3.
           05  WS-ELIG-COPAY-AMT       PIC S9(05)V99 COMP-3.
           05  WS-ELIG-COINS-PCT       PIC V99 COMP-3.

      *--- BENEFIT APPLICATION WORK AREAS ---
       01  WS-BENEFIT-WORK.
           05  WS-BEN-DEDUCTIBLE-APPLY PIC S9(07)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BEN-COPAY-APPLY      PIC S9(05)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BEN-COINS-APPLY      PIC S9(07)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BEN-PLAN-PAYS        PIC S9(09)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BEN-MEMBER-RESP      PIC S9(09)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BEN-DEDUCT-REMAINING PIC S9(07)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BEN-OOP-REMAINING    PIC S9(07)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BEN-LN-ALLOWED       PIC S9(07)V99 COMP-3.
           05  WS-BEN-LN-DEDUCT-AMT   PIC S9(07)V99 COMP-3.
           05  WS-BEN-LN-COINS-AMT    PIC S9(07)V99 COMP-3.
           05  WS-BEN-LN-PLAN-PAID    PIC S9(07)V99 COMP-3.

      *--- DUPLICATE CHECK AREA ---
       01  WS-DUP-CHECK.
           05  WS-DUP-COUNT            PIC 9(05).
           05  WS-DUP-CLAIM-ID         PIC X(15).

      *--- PROCESSING COUNTERS ---
       01  WS-ADJ-COUNTERS.
           05  WS-CLAIMS-PROCESSED     PIC 9(07) VALUE ZERO.
           05  WS-CLAIMS-PAID          PIC 9(07) VALUE ZERO.
           05  WS-CLAIMS-DENIED        PIC 9(07) VALUE ZERO.
           05  WS-CLAIMS-PENDED        PIC 9(07) VALUE ZERO.
           05  WS-TOTAL-CHARGES-SUM    PIC S9(13)V99 COMP-3
                                       VALUE ZERO.
           05  WS-TOTAL-PAID-SUM       PIC S9(13)V99 COMP-3
                                       VALUE ZERO.
           05  WS-LINE-COUNT           PIC 9(03).
           05  WS-LN-IDX               PIC 9(03).

       01  WS-FINAL-STATUS             PIC X(02).
       01  WS-EDIT-CODE-TABLE.
           05  WS-EDIT-CODE OCCURS 20 TIMES PIC X(05).
       01  WS-EDIT-IDX                 PIC 9(03) VALUE 1.

       01  WS-RPT-LINE                 PIC X(133).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CLAIMS
               UNTIL WS-NO-MORE-CLAIMS
           PERFORM 8000-WRITE-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           MOVE 'CLMADJ01' TO WS-PROGRAM-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN OUTPUT CLMRPT-FILE
           EXEC SQL OPEN CLAIM-CURSOR END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'CLMADJ01: ERROR OPENING CLAIM CURSOR - '
                       'SQLCODE=' SQLCODE
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 8100-FETCH-NEXT-CLAIM.

       2000-PROCESS-CLAIMS.
           ADD 1 TO WS-CLAIMS-PROCESSED
           SET WS-CLAIM-VALID TO TRUE
           SET WS-ELIG-NOT-VERIFIED TO TRUE
           SET WS-PRV-NOT-VALID TO TRUE
           SET WS-NOT-DUPLICATE TO TRUE
           MOVE 1 TO WS-EDIT-IDX
           INITIALIZE WS-EDIT-CODE-TABLE
           INITIALIZE WS-BENEFIT-WORK
      *--- PHASE 1: DATA VALIDATION ---
           PERFORM 3000-VALIDATE-CLAIM-DATA
      *--- PHASE 2: ELIGIBILITY VERIFICATION ---
           IF WS-CLAIM-VALID
               PERFORM 4000-VERIFY-ELIGIBILITY
           END-IF
      *--- PHASE 3: PROVIDER VALIDATION ---
           IF WS-CLAIM-VALID AND WS-ELIG-VERIFIED
               PERFORM 5000-VALIDATE-PROVIDER
           END-IF
      *--- PHASE 4: DUPLICATE CHECKING ---
           IF WS-CLAIM-VALID AND WS-PRV-VALIDATED
               PERFORM 5500-CHECK-DUPLICATES
           END-IF
      *--- PHASE 5: PRICING ---
           IF WS-CLAIM-VALID AND WS-NOT-DUPLICATE
               PERFORM 6000-PRICE-CLAIM
           END-IF
      *--- PHASE 6: BENEFIT APPLICATION ---
           IF WS-CLAIM-VALID
               PERFORM 6500-APPLY-BENEFITS
           END-IF
      *--- PHASE 7: FINALIZE AND STORE ---
           PERFORM 7000-FINALIZE-CLAIM
      *--- COMMIT AFTER EACH CLAIM ---
           EXEC SQL COMMIT END-EXEC
           PERFORM 8100-FETCH-NEXT-CLAIM.

       3000-VALIDATE-CLAIM-DATA.
      *--- VALIDATE REQUIRED HEADER FIELDS ---
           IF WS-STG-CLAIM-ID = SPACES
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED001' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF
           IF WS-STG-CLAIM-TYPE NOT = 'IN'
           AND WS-STG-CLAIM-TYPE NOT = 'PR'
           AND WS-STG-CLAIM-TYPE NOT = 'DN'
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED002' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF
           IF WS-STG-MEMBER-ID = SPACES
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED003' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF
           IF WS-STG-BILLING-NPI = SPACES
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED004' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF
           IF WS-STG-TOTAL-CHARGES <= ZERO
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED005' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF
           IF WS-STG-PRINC-DIAG = SPACES
           AND WS-STG-CLAIM-TYPE NOT = 'DN'
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED006' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF
      *--- VALIDATE CLAIM LINE ITEMS ---
           PERFORM 3100-VALIDATE-LINE-ITEMS.

       3100-VALIDATE-LINE-ITEMS.
           MOVE ZERO TO WS-LINE-COUNT
           EXEC SQL OPEN LINE-CURSOR END-EXEC
           IF SQLCODE NOT = ZERO
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED010' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           ELSE
               PERFORM 3110-FETCH-VALIDATE-LINE
                   UNTIL SQLCODE = +100
                   OR WS-LINE-COUNT > 50
               EXEC SQL CLOSE LINE-CURSOR END-EXEC
           END-IF
           IF WS-LINE-COUNT = ZERO
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'ED011' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF.

       3110-FETCH-VALIDATE-LINE.
           EXEC SQL
               FETCH LINE-CURSOR
               INTO :WS-STG-LN-SEQ,
                    :WS-STG-LN-PROC,
                    :WS-STG-LN-MOD1,
                    :WS-STG-LN-MOD2,
                    :WS-STG-LN-MOD3,
                    :WS-STG-LN-MOD4,
                    :WS-STG-LN-REV,
                    :WS-STG-LN-DOS-FROM,
                    :WS-STG-LN-DOS-THRU,
                    :WS-STG-LN-UNITS,
                    :WS-STG-LN-CHARGE,
                    :WS-STG-LN-NDC,
                    :WS-STG-LN-DRG
           END-EXEC
           IF SQLCODE = ZERO
               ADD 1 TO WS-LINE-COUNT
      *        VALIDATE DOS FORMAT
               IF WS-STG-LN-DOS-FROM IS NOT NUMERIC
                   MOVE 'ED020' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               END-IF
      *        VALIDATE DOS-FROM <= DOS-THRU
               IF WS-STG-LN-DOS-FROM > WS-STG-LN-DOS-THRU
               AND WS-STG-LN-DOS-THRU NOT = SPACES
               AND WS-STG-LN-DOS-THRU NOT = '00000000'
                   MOVE 'ED021' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               END-IF
      *        VALIDATE LINE CHARGE
               IF WS-STG-LN-CHARGE <= ZERO
                   MOVE 'ED022' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               END-IF
      *        PROF CLAIMS REQUIRE PROC CODE
               IF WS-STG-CLAIM-TYPE = 'PR'
               AND WS-STG-LN-PROC = SPACES
                   MOVE 'ED023' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               END-IF
      *        INSTITUTIONAL CLAIMS REQUIRE REV CODE
               IF WS-STG-CLAIM-TYPE = 'IN'
               AND WS-STG-LN-REV = SPACES
                   MOVE 'ED024' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               END-IF
      *        STORE LINE FOR PRICING
               MOVE WS-STG-LN-SEQ    TO
                   WS-PRC-LN-SEQ(WS-LINE-COUNT)
               MOVE WS-STG-LN-PROC   TO
                   WS-PRC-LN-PROC(WS-LINE-COUNT)
               MOVE WS-STG-LN-MOD1   TO
                   WS-PRC-LN-MOD(WS-LINE-COUNT)
               MOVE WS-STG-LN-REV    TO
                   WS-PRC-LN-REV(WS-LINE-COUNT)
               MOVE WS-STG-LN-DOS-FROM TO
                   WS-PRC-LN-DOS(WS-LINE-COUNT)
               MOVE WS-STG-LN-UNITS  TO
                   WS-PRC-LN-UNITS(WS-LINE-COUNT)
               MOVE WS-STG-LN-CHARGE TO
                   WS-PRC-LN-CHARGE(WS-LINE-COUNT)
               MOVE WS-STG-LN-DRG    TO
                   WS-PRC-LN-DRG(WS-LINE-COUNT)
           END-IF.

       4000-VERIFY-ELIGIBILITY.
           EXEC SQL
               SELECT E.ELIG_STATUS,
                      E.PLAN_CODE,
                      E.GROUP_ID,
                      E.PRODUCT_TYPE,
                      E.COVERAGE_TYPE,
                      E.EFF_DATE,
                      E.TERM_DATE,
                      B.NETWORK_ID,
                      A.DEDUCTIBLE_YTD,
                      A.DEDUCTIBLE_LIMIT,
                      A.OOP_YTD,
                      A.OOP_LIMIT,
                      B.COPAY_PCP,
                      B.COINSURANCE_IN
               INTO :WS-ELIG-STATUS,
                    :WS-ELIG-PLAN-CODE,
                    :WS-ELIG-GROUP-ID,
                    :WS-ELIG-PRODUCT-TYPE,
                    :WS-ELIG-COVERAGE-TYPE,
                    :WS-ELIG-EFF-DATE,
                    :WS-ELIG-TERM-DATE,
                    :WS-ELIG-NETWORK-ID,
                    :WS-ELIG-DEDUCT-YTD,
                    :WS-ELIG-DEDUCT-LIMIT,
                    :WS-ELIG-OOP-YTD,
                    :WS-ELIG-OOP-LIMIT,
                    :WS-ELIG-COPAY-AMT,
                    :WS-ELIG-COINS-PCT
               FROM HCAS.ENROLLMENT E
               INNER JOIN HCAS.BENEFIT_PLAN B
                   ON E.PLAN_CODE = B.PLAN_CODE
                  AND B.STATUS = 'A'
               INNER JOIN HCAS.ACCUMULATORS A
                   ON E.MEMBER_ID = A.MEMBER_ID
                  AND E.PLAN_CODE = A.PLAN_CODE
               WHERE E.MEMBER_ID = :WS-STG-MEMBER-ID
                 AND E.ELIG_STATUS = 'A'
                 AND E.EFF_DATE <= :WS-STG-RECEIPT-DATE
                 AND (E.TERM_DATE >= :WS-STG-RECEIPT-DATE
                      OR E.TERM_DATE = '99991231')
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZERO
                   SET WS-ELIG-VERIFIED TO TRUE
                   COMPUTE WS-BEN-DEDUCT-REMAINING =
                       WS-ELIG-DEDUCT-LIMIT -
                       WS-ELIG-DEDUCT-YTD
                   COMPUTE WS-BEN-OOP-REMAINING =
                       WS-ELIG-OOP-LIMIT -
                       WS-ELIG-OOP-YTD
               WHEN +100
                   SET WS-CLAIM-INVALID TO TRUE
                   MOVE 'EL001' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               WHEN OTHER
                   SET WS-CLAIM-INVALID TO TRUE
                   MOVE 'EL099' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
           END-EVALUATE.

       5000-VALIDATE-PROVIDER.
      *--- CALL PRVVAL02 FOR PROVIDER VALIDATION ---
           INITIALIZE WS-PRV-REQUEST
           INITIALIZE WS-PRV-RESPONSE
           MOVE 'BT'                TO WS-PRV-REQ-FUNC
           MOVE WS-STG-BILLING-NPI  TO WS-PRV-REQ-NPI
           MOVE SPACES              TO WS-PRV-REQ-TAXONOMY
           MOVE WS-STG-RECEIPT-DATE TO WS-PRV-REQ-DOS
           MOVE WS-PRC-LN-PROC(1)  TO WS-PRV-REQ-PROC
           MOVE WS-PRC-LN-MOD(1)   TO WS-PRV-REQ-MOD
           MOVE WS-ELIG-NETWORK-ID  TO WS-PRV-REQ-NETWORK
           MOVE SPACES              TO WS-PRV-REQ-STATE
           CALL 'PRVVAL02' USING WS-PRV-REQUEST
                                  WS-PRV-RESPONSE
           EVALUATE WS-PRV-RSP-RC
               WHEN '00'
                   SET WS-PRV-VALIDATED TO TRUE
               WHEN '01'
                   SET WS-CLAIM-INVALID TO TRUE
                   MOVE 'PV001' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               WHEN '02'
                   SET WS-CLAIM-INVALID TO TRUE
                   MOVE 'PV002' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
               WHEN OTHER
                   SET WS-CLAIM-INVALID TO TRUE
                   MOVE 'PV099' TO WS-EDIT-CODE(WS-EDIT-IDX)
                   ADD 1 TO WS-EDIT-IDX
           END-EVALUATE.

       5500-CHECK-DUPLICATES.
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-DUP-COUNT
               FROM HCAS.CLAIM_MASTER
               WHERE MEMBER_ID = :WS-STG-MEMBER-ID
                 AND BILLING_NPI = :WS-STG-BILLING-NPI
                 AND TOTAL_CHARGES = :WS-STG-TOTAL-CHARGES
                 AND CLAIM_STATUS <> '99'
                 AND EXISTS (
                     SELECT 1
                     FROM HCAS.CLAIM_LINE CL1
                     INNER JOIN HCAS.CLAIM_LINE_STAGING CL2
                         ON CL1.PROC_CODE = CL2.PROC_CODE
                        AND CL1.DOS_FROM = CL2.DOS_FROM
                     WHERE CL1.CLAIM_ID = CLAIM_MASTER.CLAIM_ID
                       AND CL2.CLAIM_ID = :WS-STG-CLAIM-ID
                 )
           END-EXEC
           IF WS-DUP-COUNT > 0
               SET WS-IS-DUPLICATE TO TRUE
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'DP001' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF.

       6000-PRICE-CLAIM.
      *--- CALL CLMPRC01 FOR CLAIM PRICING ---
           INITIALIZE WS-PRICING-RESPONSE
           MOVE WS-STG-CLAIM-TYPE    TO WS-PRC-CLAIM-TYPE
           MOVE WS-ELIG-PLAN-CODE    TO WS-PRC-PLAN-CODE
           MOVE WS-ELIG-NETWORK-ID   TO WS-PRC-NETWORK-ID
           MOVE WS-PRV-RSP-CONTR-TYPE TO WS-PRC-CONTRACT-TYPE
           MOVE WS-PRV-RSP-FEE-SCHED TO WS-PRC-FEE-SCHED-ID
           MOVE WS-LINE-COUNT        TO WS-PRC-LINE-COUNT
           CALL 'CLMPRC01' USING WS-PRICING-REQUEST
                                  WS-PRICING-RESPONSE
           IF WS-PRC-RSP-RC NOT = '00'
               SET WS-CLAIM-INVALID TO TRUE
               MOVE 'PC001' TO WS-EDIT-CODE(WS-EDIT-IDX)
               ADD 1 TO WS-EDIT-IDX
           END-IF.

       6500-APPLY-BENEFITS.
      *--- APPLY DEDUCTIBLE, COINSURANCE, COPAY ---
           IF WS-PRC-RSP-RC = '00'
               PERFORM VARYING WS-LN-IDX FROM 1 BY 1
                   UNTIL WS-LN-IDX > WS-PRC-RSP-LINE-COUNT
      *            GET ALLOWED AMOUNT FROM PRICING
                   MOVE WS-PRC-RSP-LN-ALLOW(WS-LN-IDX)
                       TO WS-BEN-LN-ALLOWED
                   IF WS-BEN-LN-ALLOWED > ZERO
      *                APPLY DEDUCTIBLE
                       IF WS-BEN-DEDUCT-REMAINING > ZERO
                           IF WS-BEN-LN-ALLOWED <=
                              WS-BEN-DEDUCT-REMAINING
                               MOVE WS-BEN-LN-ALLOWED
                                   TO WS-BEN-LN-DEDUCT-AMT
                           ELSE
                               MOVE WS-BEN-DEDUCT-REMAINING
                                   TO WS-BEN-LN-DEDUCT-AMT
                           END-IF
                           SUBTRACT WS-BEN-LN-DEDUCT-AMT
                               FROM WS-BEN-DEDUCT-REMAINING
                           ADD WS-BEN-LN-DEDUCT-AMT
                               TO WS-BEN-DEDUCTIBLE-APPLY
                       ELSE
                           MOVE ZERO TO WS-BEN-LN-DEDUCT-AMT
                       END-IF
      *                APPLY COINSURANCE ON REMAINING
                       COMPUTE WS-BEN-LN-COINS-AMT =
                           (WS-BEN-LN-ALLOWED -
                            WS-BEN-LN-DEDUCT-AMT) *
                           WS-ELIG-COINS-PCT
                       ADD WS-BEN-LN-COINS-AMT
                           TO WS-BEN-COINS-APPLY
      *                CALCULATE PLAN PAYMENT
                       COMPUTE WS-BEN-LN-PLAN-PAID =
                           WS-BEN-LN-ALLOWED -
                           WS-BEN-LN-DEDUCT-AMT -
                           WS-BEN-LN-COINS-AMT
                       IF WS-BEN-LN-PLAN-PAID < ZERO
                           MOVE ZERO TO WS-BEN-LN-PLAN-PAID
                       END-IF
                       ADD WS-BEN-LN-PLAN-PAID
                           TO WS-BEN-PLAN-PAYS
      *                UPDATE PRICING RESPONSE
                       MOVE WS-BEN-LN-PLAN-PAID
                           TO WS-PRC-RSP-LN-PAID(WS-LN-IDX)
                   END-IF
               END-PERFORM
      *        CALCULATE TOTAL MEMBER RESPONSIBILITY
               COMPUTE WS-BEN-MEMBER-RESP =
                   WS-BEN-DEDUCTIBLE-APPLY +
                   WS-BEN-COPAY-APPLY +
                   WS-BEN-COINS-APPLY
      *        CHECK OOP MAX
               IF WS-BEN-MEMBER-RESP > WS-BEN-OOP-REMAINING
                   COMPUTE WS-BEN-PLAN-PAYS =
                       WS-BEN-PLAN-PAYS +
                       (WS-BEN-MEMBER-RESP -
                        WS-BEN-OOP-REMAINING)
                   MOVE WS-BEN-OOP-REMAINING
                       TO WS-BEN-MEMBER-RESP
               END-IF
           END-IF.

       7000-FINALIZE-CLAIM.
      *--- DETERMINE FINAL CLAIM STATUS ---
           IF WS-CLAIM-INVALID
               IF WS-IS-DUPLICATE
                   MOVE '40' TO WS-FINAL-STATUS
                   ADD 1 TO WS-CLAIMS-DENIED
               ELSE
                   MOVE '40' TO WS-FINAL-STATUS
                   ADD 1 TO WS-CLAIMS-DENIED
               END-IF
           ELSE
               IF WS-BEN-PLAN-PAYS > ZERO
                   MOVE '50' TO WS-FINAL-STATUS
                   ADD 1 TO WS-CLAIMS-PAID
               ELSE
                   MOVE '40' TO WS-FINAL-STATUS
                   ADD 1 TO WS-CLAIMS-DENIED
               END-IF
           END-IF
           ADD WS-STG-TOTAL-CHARGES TO WS-TOTAL-CHARGES-SUM
           ADD WS-BEN-PLAN-PAYS    TO WS-TOTAL-PAID-SUM
      *--- INSERT INTO CLAIM MASTER ---
           PERFORM 7100-INSERT-CLAIM-MASTER
      *--- INSERT CLAIM LINES ---
           IF WS-PRC-RSP-RC = '00'
               PERFORM 7200-INSERT-CLAIM-LINES
           END-IF
      *--- UPDATE STAGING STATUS ---
           PERFORM 7300-UPDATE-STAGING-STATUS
      *--- UPDATE ACCUMULATORS IF PAID ---
           IF WS-FINAL-STATUS = '50'
               PERFORM 7400-UPDATE-ACCUMULATORS
           END-IF.

       7100-INSERT-CLAIM-MASTER.
           EXEC SQL
               INSERT INTO HCAS.CLAIM_MASTER
                   (CLAIM_ID, CLAIM_TYPE, CLAIM_STATUS,
                    SOURCE_CODE, RECEIPT_DATE, PROCESS_DATE,
                    ADJUD_DATE, MEMBER_ID, SUBSCRIBER_ID,
                    BILLING_NPI, RENDERING_NPI, FACILITY_NPI,
                    PLACE_OF_SERVICE, PRINCIPAL_DIAG,
                    TOTAL_CHARGES, ALLOWED_AMT, PAID_AMT,
                    COPAY_AMT, DEDUCTIBLE_AMT, COINSURANCE_AMT)
               VALUES
                   (:WS-STG-CLAIM-ID, :WS-STG-CLAIM-TYPE,
                    :WS-FINAL-STATUS, :WS-STG-SOURCE-CODE,
                    :WS-STG-RECEIPT-DATE, :WS-CURRENT-DATE,
                    :WS-CURRENT-DATE, :WS-STG-MEMBER-ID,
                    :WS-STG-SUBSCRIBER-ID,
                    :WS-STG-BILLING-NPI, :WS-STG-RENDERING-NPI,
                    :WS-STG-FACILITY-NPI, :WS-STG-PLACE-OF-SVC,
                    :WS-STG-PRINC-DIAG,
                    :WS-STG-TOTAL-CHARGES,
                    :WS-PRC-RSP-TOTAL-ALLOWED,
                    :WS-BEN-PLAN-PAYS,
                    :WS-BEN-COPAY-APPLY,
                    :WS-BEN-DEDUCTIBLE-APPLY,
                    :WS-BEN-COINS-APPLY)
           END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'CLMADJ01: INSERT CLAIM ERROR SQLCODE='
                       SQLCODE ' CLM=' WS-STG-CLAIM-ID
           END-IF.

       7200-INSERT-CLAIM-LINES.
           PERFORM VARYING WS-LN-IDX FROM 1 BY 1
               UNTIL WS-LN-IDX > WS-PRC-RSP-LINE-COUNT
               EXEC SQL
                   INSERT INTO HCAS.CLAIM_LINE
                       (CLAIM_ID, LINE_SEQ, PROC_CODE,
                        LINE_CHARGE, ALLOWED_AMT, PAID_AMT,
                        DENY_CODE, ADJ_REASON_1)
                   VALUES
                       (:WS-STG-CLAIM-ID,
                        :WS-PRC-RSP-LN-SEQ(WS-LN-IDX),
                        :WS-PRC-LN-PROC(WS-LN-IDX),
                        :WS-PRC-LN-CHARGE(WS-LN-IDX),
                        :WS-PRC-RSP-LN-ALLOW(WS-LN-IDX),
                        :WS-PRC-RSP-LN-PAID(WS-LN-IDX),
                        :WS-PRC-RSP-LN-DENY(WS-LN-IDX),
                        :WS-PRC-RSP-LN-ADJ(WS-LN-IDX, 1))
               END-EXEC
               IF SQLCODE NOT = ZERO
                   DISPLAY 'CLMADJ01: INSERT LINE ERROR SQLCODE='
                           SQLCODE ' CLM=' WS-STG-CLAIM-ID
                           ' LN=' WS-LN-IDX
               END-IF
           END-PERFORM.

       7300-UPDATE-STAGING-STATUS.
           EXEC SQL
               UPDATE HCAS.CLAIM_STAGING
               SET    PROCESS_STATUS = 'P',
                      PROCESS_DATE = :WS-CURRENT-DATE
               WHERE CURRENT OF CLAIM-CURSOR
           END-EXEC.

       7400-UPDATE-ACCUMULATORS.
           EXEC SQL
               UPDATE HCAS.ACCUMULATORS
               SET    DEDUCTIBLE_YTD = DEDUCTIBLE_YTD +
                          :WS-BEN-DEDUCTIBLE-APPLY,
                      OOP_YTD = OOP_YTD +
                          :WS-BEN-MEMBER-RESP,
                      LAST_UPDATE_DATE = :WS-CURRENT-DATE
               WHERE  MEMBER_ID = :WS-STG-MEMBER-ID
                 AND  PLAN_CODE = :WS-ELIG-PLAN-CODE
           END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'CLMADJ01: ACCUM UPDATE ERROR SQLCODE='
                       SQLCODE ' MBR=' WS-STG-MEMBER-ID
           END-IF.

       8000-WRITE-SUMMARY.
           MOVE SPACES TO WS-RPT-LINE
           STRING '=============================================='
               DELIMITED SIZE INTO WS-RPT-LINE
           WRITE CLMRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'CLMADJ01 - CLAIMS ADJUDICATION SUMMARY'
               DELIMITED SIZE INTO WS-RPT-LINE
           WRITE CLMRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING '=============================================='
               DELIMITED SIZE INTO WS-RPT-LINE
           WRITE CLMRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'CLAIMS PROCESSED: ' DELIMITED SIZE
                  WS-CLAIMS-PROCESSED   DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE CLMRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'CLAIMS PAID:      ' DELIMITED SIZE
                  WS-CLAIMS-PAID        DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE CLMRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING 'CLAIMS DENIED:    ' DELIMITED SIZE
                  WS-CLAIMS-DENIED      DELIMITED SIZE
                  INTO WS-RPT-LINE
           WRITE CLMRPT-RECORD FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE
           STRING '=============================================='
               DELIMITED SIZE INTO WS-RPT-LINE
           WRITE CLMRPT-RECORD FROM WS-RPT-LINE.

       8100-FETCH-NEXT-CLAIM.
           EXEC SQL
               FETCH CLAIM-CURSOR
               INTO :WS-STG-CLAIM-ID,
                    :WS-STG-CLAIM-TYPE,
                    :WS-STG-MEMBER-ID,
                    :WS-STG-SUBSCRIBER-ID,
                    :WS-STG-PATIENT-ACCT,
                    :WS-STG-MEMBER-DOB,
                    :WS-STG-MEMBER-SEX,
                    :WS-STG-BILLING-NPI,
                    :WS-STG-RENDERING-NPI,
                    :WS-STG-FACILITY-NPI,
                    :WS-STG-PLACE-OF-SVC,
                    :WS-STG-PROVIDER-TIN,
                    :WS-STG-PRINC-DIAG,
                    :WS-STG-ADMIT-DIAG,
                    :WS-STG-TOTAL-CHARGES,
                    :WS-STG-RECEIPT-DATE,
                    :WS-STG-SOURCE-CODE
           END-EXEC
           IF SQLCODE = +100
               SET WS-NO-MORE-CLAIMS TO TRUE
           ELSE IF SQLCODE NOT = ZERO
               DISPLAY 'CLMADJ01: FETCH ERROR SQLCODE=' SQLCODE
               SET WS-NO-MORE-CLAIMS TO TRUE
           END-IF.

       9000-TERMINATE.
           EXEC SQL CLOSE CLAIM-CURSOR END-EXEC
           EXEC SQL COMMIT END-EXEC
           DISPLAY '=============================================='
           DISPLAY 'CLMADJ01: ADJUDICATION COMPLETE'
           DISPLAY '  CLAIMS PROCESSED: ' WS-CLAIMS-PROCESSED
           DISPLAY '  CLAIMS PAID:      ' WS-CLAIMS-PAID
           DISPLAY '  CLAIMS DENIED:    ' WS-CLAIMS-DENIED
           DISPLAY '=============================================='
           CLOSE CLMRPT-FILE
           MOVE ZERO TO RETURN-CODE.
