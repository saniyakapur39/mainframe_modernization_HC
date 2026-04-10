       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CLMPRC01.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-05-20.
      *================================================================*
      * PROGRAM:  CLMPRC01                                             *
      * PURPOSE:  CLAIMS PRICING ENGINE - SUBROUTINE                   *
      *           CALLED BY CLMADJ01 TO CALCULATE ALLOWED AMOUNTS      *
      *           FOR EACH CLAIM SERVICE LINE BASED ON PROVIDER         *
      *           CONTRACT TYPE, FEE SCHEDULE RATES, AND PLAN           *
      *           CONFIGURATION. SUPPORTS MULTIPLE PRICING              *
      *           METHODOLOGIES: FEE-FOR-SERVICE (FFS), PERCENT OF     *
      *           CHARGE, CAPITATION, AND DRG-BASED INSTITUTIONAL      *
      *           PRICING WITH WAGE INDEX ADJUSTMENTS.                  *
      *           HANDLES MODIFIER-BASED REDUCTIONS, MULTI-PROCEDURE   *
      *           DISCOUNTING, AND OUTLIER THRESHOLD CALCULATIONS.     *
      * CALLED BY: CLMADJ01 (CLAIMS ADJUDICATION ENGINE)              *
      * TABLES:   HCAS.FEE_SCHEDULE, HCAS.DRG_WEIGHTS,               *
      *           HCAS.WAGE_INDEX, HCAS.CONTRACT_RATES,               *
      *           HCAS.MODIFIER_RULES, HCAS.OUTLIER_THRESHOLD         *
      * FREQUENCY: PER-CLAIM (CALLED DURING ADJUDICATION)              *
      *================================================================*
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY ABORTWSC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  WS-PROGRAM-NAME             PIC X(08) VALUE 'CLMPRC01'.

      *--- FEE SCHEDULE LOOKUP ---
       01  WS-FEE-LOOKUP.
           05  WS-FEE-SCHED-ID         PIC X(08).
           05  WS-FEE-PROC-CODE        PIC X(05).
           05  WS-FEE-MODIFIER         PIC X(02).
           05  WS-FEE-DOS              PIC X(08).
           05  WS-FEE-BASE-AMOUNT      PIC S9(07)V99 COMP-3.
           05  WS-FEE-RVU-WORK         PIC S9(03)V9999 COMP-3.
           05  WS-FEE-RVU-PE           PIC S9(03)V9999 COMP-3.
           05  WS-FEE-RVU-MP           PIC S9(03)V9999 COMP-3.
           05  WS-FEE-CONV-FACTOR      PIC S9(05)V9999 COMP-3.
           05  WS-FEE-FOUND-SW         PIC X(01).
               88  WS-FEE-FOUND        VALUE 'Y'.
               88  WS-FEE-NOT-FOUND    VALUE 'N'.

      *--- DRG PRICING FIELDS ---
       01  WS-DRG-PRICING.
           05  WS-DRG-CODE             PIC X(04).
           05  WS-DRG-WEIGHT           PIC S9(03)V9999 COMP-3.
           05  WS-DRG-GMLOS            PIC S9(03)V99 COMP-3.
           05  WS-DRG-AMLOS            PIC S9(03)V99 COMP-3.
           05  WS-DRG-BASE-RATE        PIC S9(09)V99 COMP-3.
           05  WS-DRG-OPER-RATE        PIC S9(09)V99 COMP-3.
           05  WS-DRG-CAPITAL-RATE     PIC S9(09)V99 COMP-3.
           05  WS-DRG-FOUND-SW         PIC X(01).
               88  WS-DRG-FOUND        VALUE 'Y'.
               88  WS-DRG-NOT-FOUND    VALUE 'N'.

      *--- WAGE INDEX FIELDS ---
       01  WS-WAGE-INDEX.
           05  WS-WI-CBSA-CODE         PIC X(05).
           05  WS-WI-FACTOR            PIC S9(01)V9999 COMP-3.
           05  WS-WI-FOUND-SW          PIC X(01).
               88  WS-WI-FOUND         VALUE 'Y'.
               88  WS-WI-NOT-FOUND     VALUE 'N'.

      *--- CONTRACT RATE FIELDS ---
       01  WS-CONTRACT-RATE.
           05  WS-CR-NETWORK-ID        PIC X(06).
           05  WS-CR-CONTRACT-TYPE     PIC X(02).
           05  WS-CR-PERCENT-CHARGE    PIC V99   COMP-3.
           05  WS-CR-PERCENT-FEE       PIC S9(01)V99 COMP-3.
           05  WS-CR-CAP-RATE          PIC S9(07)V99 COMP-3.
           05  WS-CR-FOUND-SW          PIC X(01).
               88  WS-CR-FOUND         VALUE 'Y'.
               88  WS-CR-NOT-FOUND     VALUE 'N'.

      *--- MODIFIER RULES ---
       01  WS-MOD-RULES.
           05  WS-MOD-PROC-CODE        PIC X(05).
           05  WS-MOD-CODE             PIC X(02).
           05  WS-MOD-PERCENT          PIC V99   COMP-3.
           05  WS-MOD-ACTION           PIC X(02).
               88  WS-MOD-ACT-REDUCE   VALUE 'RD'.
               88  WS-MOD-ACT-INCREASE VALUE 'IN'.
               88  WS-MOD-ACT-REPLACE  VALUE 'RP'.
           05  WS-MOD-FOUND-SW         PIC X(01).
               88  WS-MOD-FOUND        VALUE 'Y'.
               88  WS-MOD-NOT-FOUND    VALUE 'N'.

      *--- MULTI-PROCEDURE DISCOUNT FIELDS ---
       01  WS-MPD-FIELDS.
           05  WS-MPD-HIGHEST-FEE      PIC S9(07)V99 COMP-3
                                       VALUE ZERO.
           05  WS-MPD-HIGHEST-IDX      PIC 9(03) VALUE ZERO.
           05  WS-MPD-DISCOUNT-PCT     PIC V99   COMP-3 VALUE .50.
           05  WS-MPD-SURGICAL-COUNT   PIC 9(03) VALUE ZERO.

      *--- OUTLIER THRESHOLD FIELDS ---
       01  WS-OUTLIER.
           05  WS-OTL-FIXED-LOSS       PIC S9(09)V99 COMP-3.
           05  WS-OTL-MARGINAL-PCT     PIC V99   COMP-3.
           05  WS-OTL-THRESHOLD        PIC S9(09)V99 COMP-3.
           05  WS-OTL-PAYMENT          PIC S9(09)V99 COMP-3.
           05  WS-OTL-FOUND-SW         PIC X(01).
               88  WS-OTL-FOUND        VALUE 'Y'.
               88  WS-OTL-NOT-FOUND    VALUE 'N'.

      *--- CALCULATION WORK AREAS ---
       01  WS-CALC-WORK.
           05  WS-CALC-ALLOWED         PIC S9(09)V99 COMP-3.
           05  WS-CALC-FEE-AMT         PIC S9(07)V99 COMP-3.
           05  WS-CALC-LINE-ALLOW      PIC S9(07)V99 COMP-3.
           05  WS-CALC-ADJUSTED        PIC S9(09)V99 COMP-3.
           05  WS-CALC-OPER-AMT        PIC S9(09)V99 COMP-3.
           05  WS-CALC-CAPITAL-AMT     PIC S9(09)V99 COMP-3.
           05  WS-CALC-TOTAL-DRG       PIC S9(09)V99 COMP-3.
           05  WS-CALC-CHARGE-PCT      PIC S9(09)V99 COMP-3.
           05  WS-CALC-LESSER-OF       PIC S9(09)V99 COMP-3.
           05  WS-CALC-LN-CHARGE       PIC S9(07)V99 COMP-3.
           05  WS-CALC-RVU-TOTAL       PIC S9(05)V9999 COMP-3.
           05  WS-CALC-RVU-AMT         PIC S9(07)V99 COMP-3.

       01  WS-LINE-IDX                 PIC 9(03).
       01  WS-SORT-IDX                 PIC 9(03).
       01  WS-TEMP-AMOUNT              PIC S9(07)V99 COMP-3.

      *--- LINE-LEVEL WORKING STORAGE ---
       01  WS-LINE-WORK-TABLE.
           05  WS-LW-ENTRY OCCURS 50 TIMES.
               10  WS-LW-SEQ           PIC 9(03).
               10  WS-LW-PROC-CODE     PIC X(05).
               10  WS-LW-MODIFIER      PIC X(02).
               10  WS-LW-REV-CODE      PIC X(04).
               10  WS-LW-DOS           PIC X(08).
               10  WS-LW-UNITS         PIC S9(05)V99 COMP-3.
               10  WS-LW-CHARGE        PIC S9(07)V99 COMP-3.
               10  WS-LW-DRG           PIC X(04).
               10  WS-LW-FEE-AMT       PIC S9(07)V99 COMP-3.
               10  WS-LW-ALLOWED       PIC S9(07)V99 COMP-3.
               10  WS-LW-PAID          PIC S9(07)V99 COMP-3.
               10  WS-LW-DENY-CODE     PIC X(05).
               10  WS-LW-ADJ-REASON    PIC X(05) OCCURS 5 TIMES.
               10  WS-LW-IS-SURGICAL   PIC X(01).
                   88  WS-LW-SURGICAL  VALUE 'Y'.
                   88  WS-LW-NOT-SURG  VALUE 'N'.

       LINKAGE SECTION.

       01  LS-PRICING-REQUEST.
           05  LS-PRC-CLAIM-TYPE       PIC X(02).
           05  LS-PRC-PLAN-CODE        PIC X(08).
           05  LS-PRC-NETWORK-ID       PIC X(06).
           05  LS-PRC-CONTRACT-TYPE    PIC X(02).
           05  LS-PRC-FEE-SCHED-ID    PIC X(08).
           05  LS-PRC-LINE-COUNT       PIC 9(03).
           05  LS-PRC-LINES OCCURS 50 TIMES.
               10  LS-PRC-LN-SEQ       PIC 9(03).
               10  LS-PRC-LN-PROC      PIC X(05).
               10  LS-PRC-LN-MOD       PIC X(02).
               10  LS-PRC-LN-REV       PIC X(04).
               10  LS-PRC-LN-DOS       PIC X(08).
               10  LS-PRC-LN-UNITS     PIC S9(05)V99 COMP-3.
               10  LS-PRC-LN-CHARGE    PIC S9(07)V99 COMP-3.
               10  LS-PRC-LN-DRG       PIC X(04).

       01  LS-PRICING-RESPONSE.
           05  LS-PRC-RSP-RC           PIC X(02).
           05  LS-PRC-RSP-MSG          PIC X(80).
           05  LS-PRC-RSP-TOTAL-ALLOWED
                                       PIC S9(09)V99 COMP-3.
           05  LS-PRC-RSP-TOTAL-PAID   PIC S9(09)V99 COMP-3.
           05  LS-PRC-RSP-LINE-COUNT   PIC 9(03).
           05  LS-PRC-RSP-LINES OCCURS 50 TIMES.
               10  LS-PRC-RSP-LN-SEQ   PIC 9(03).
               10  LS-PRC-RSP-LN-ALLOW PIC S9(07)V99 COMP-3.
               10  LS-PRC-RSP-LN-PAID  PIC S9(07)V99 COMP-3.
               10  LS-PRC-RSP-LN-DENY  PIC X(05).
               10  LS-PRC-RSP-LN-ADJ   PIC X(05)
                                        OCCURS 5 TIMES.

       PROCEDURE DIVISION USING LS-PRICING-REQUEST
                                LS-PRICING-RESPONSE.

       0000-MAIN-PROCESS.
           INITIALIZE LS-PRICING-RESPONSE
           MOVE '00' TO LS-PRC-RSP-RC
           MOVE ZERO TO LS-PRC-RSP-TOTAL-ALLOWED
           MOVE ZERO TO LS-PRC-RSP-TOTAL-PAID
           MOVE LS-PRC-LINE-COUNT TO LS-PRC-RSP-LINE-COUNT
      *--- LOAD LINE ITEMS INTO WORK TABLE ---
           PERFORM 1000-LOAD-LINE-ITEMS
      *--- LOOKUP CONTRACT RATES ---
           PERFORM 1500-LOOKUP-CONTRACT-RATES
      *--- DETERMINE PRICING METHOD ---
           EVALUATE LS-PRC-CLAIM-TYPE
               WHEN 'PR'
                   PERFORM 2000-PRICE-PROFESSIONAL
               WHEN 'IN'
                   PERFORM 3000-PRICE-INSTITUTIONAL
               WHEN 'DN'
                   PERFORM 4000-PRICE-DENTAL
               WHEN OTHER
                   MOVE '99' TO LS-PRC-RSP-RC
                   MOVE 'UNKNOWN CLAIM TYPE FOR PRICING'
                       TO LS-PRC-RSP-MSG
           END-EVALUATE
      *--- APPLY LESSER-OF LOGIC ---
           IF LS-PRC-RSP-RC = '00'
               PERFORM 5000-APPLY-LESSER-OF-LOGIC
           END-IF
      *--- BUILD RESPONSE ---
           IF LS-PRC-RSP-RC = '00'
               PERFORM 6000-BUILD-RESPONSE
           END-IF
           GOBACK.

       1000-LOAD-LINE-ITEMS.
           INITIALIZE WS-LINE-WORK-TABLE
           MOVE ZERO TO WS-MPD-SURGICAL-COUNT
           MOVE ZERO TO WS-MPD-HIGHEST-FEE
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               OR WS-LINE-IDX > 50
               MOVE LS-PRC-LN-SEQ(WS-LINE-IDX)
                   TO WS-LW-SEQ(WS-LINE-IDX)
               MOVE LS-PRC-LN-PROC(WS-LINE-IDX)
                   TO WS-LW-PROC-CODE(WS-LINE-IDX)
               MOVE LS-PRC-LN-MOD(WS-LINE-IDX)
                   TO WS-LW-MODIFIER(WS-LINE-IDX)
               MOVE LS-PRC-LN-REV(WS-LINE-IDX)
                   TO WS-LW-REV-CODE(WS-LINE-IDX)
               MOVE LS-PRC-LN-DOS(WS-LINE-IDX)
                   TO WS-LW-DOS(WS-LINE-IDX)
               MOVE LS-PRC-LN-UNITS(WS-LINE-IDX)
                   TO WS-LW-UNITS(WS-LINE-IDX)
               MOVE LS-PRC-LN-CHARGE(WS-LINE-IDX)
                   TO WS-LW-CHARGE(WS-LINE-IDX)
               MOVE LS-PRC-LN-DRG(WS-LINE-IDX)
                   TO WS-LW-DRG(WS-LINE-IDX)
               SET WS-LW-NOT-SURG(WS-LINE-IDX) TO TRUE
      *        CHECK IF SURGICAL PROCEDURE (10000-69999)
               IF WS-LW-PROC-CODE(WS-LINE-IDX) >= '10000'
               AND WS-LW-PROC-CODE(WS-LINE-IDX) <= '69999'
                   SET WS-LW-SURGICAL(WS-LINE-IDX) TO TRUE
                   ADD 1 TO WS-MPD-SURGICAL-COUNT
               END-IF
           END-PERFORM.

       1500-LOOKUP-CONTRACT-RATES.
           SET WS-CR-NOT-FOUND TO TRUE
           EXEC SQL
               SELECT PERCENT_OF_CHARGE,
                      PERCENT_OF_FEE,
                      CAPITATION_RATE
               INTO :WS-CR-PERCENT-CHARGE,
                    :WS-CR-PERCENT-FEE,
                    :WS-CR-CAP-RATE
               FROM HCAS.CONTRACT_RATES
               WHERE NETWORK_ID = :LS-PRC-NETWORK-ID
                 AND CONTRACT_TYPE = :LS-PRC-CONTRACT-TYPE
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = ZERO
               SET WS-CR-FOUND TO TRUE
           END-IF.

       2000-PRICE-PROFESSIONAL.
      *--- PROFESSIONAL CLAIM: FEE SCHEDULE / RBRVS BASED ---
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               OR WS-LINE-IDX > 50
      *        LOOKUP FEE SCHEDULE RATE
               PERFORM 2100-LOOKUP-FEE-SCHEDULE
               IF WS-FEE-FOUND
      *            CHECK FOR RVU-BASED PRICING
                   IF WS-FEE-RVU-WORK > ZERO
                       PERFORM 2200-CALCULATE-RVU-PRICE
                   ELSE
      *                STANDARD FEE SCHEDULE RATE
                       MOVE WS-FEE-BASE-AMOUNT
                           TO WS-CALC-FEE-AMT
                   END-IF
      *            APPLY MODIFIER ADJUSTMENTS
                   PERFORM 2300-APPLY-MODIFIER-RULES
      *            CALCULATE LINE ALLOWED = FEE * UNITS
                   COMPUTE WS-LW-FEE-AMT(WS-LINE-IDX) =
                       WS-CALC-FEE-AMT
                   COMPUTE WS-LW-ALLOWED(WS-LINE-IDX) =
                       WS-CALC-FEE-AMT *
                       WS-LW-UNITS(WS-LINE-IDX)
      *            APPLY CONTRACT RATE ADJUSTMENT
                   IF WS-CR-FOUND
                       EVALUATE LS-PRC-CONTRACT-TYPE
                           WHEN 'FF'
                               CONTINUE
                           WHEN 'PC'
                               COMPUTE WS-LW-ALLOWED(WS-LINE-IDX)
                                   = WS-LW-CHARGE(WS-LINE-IDX)
                                   * WS-CR-PERCENT-CHARGE
                           WHEN 'CP'
                               MOVE ZERO
                                   TO WS-LW-ALLOWED(WS-LINE-IDX)
                       END-EVALUATE
                   END-IF
               ELSE
      *            NO FEE SCHEDULE ENTRY - DENY LINE
                   MOVE ZERO TO WS-LW-ALLOWED(WS-LINE-IDX)
                   MOVE '45'  TO WS-LW-DENY-CODE(WS-LINE-IDX)
                   MOVE '16'  TO WS-LW-ADJ-REASON
                                  (WS-LINE-IDX, 1)
               END-IF
           END-PERFORM
      *--- APPLY MULTI-PROCEDURE DISCOUNT ---
           IF WS-MPD-SURGICAL-COUNT > 1
               PERFORM 2500-APPLY-MULTI-PROC-DISCOUNT
           END-IF.

       2100-LOOKUP-FEE-SCHEDULE.
           SET WS-FEE-NOT-FOUND TO TRUE
           MOVE LS-PRC-FEE-SCHED-ID TO WS-FEE-SCHED-ID
           MOVE WS-LW-PROC-CODE(WS-LINE-IDX) TO WS-FEE-PROC-CODE
           MOVE WS-LW-MODIFIER(WS-LINE-IDX)  TO WS-FEE-MODIFIER
           MOVE WS-LW-DOS(WS-LINE-IDX)       TO WS-FEE-DOS
           EXEC SQL
               SELECT BASE_AMOUNT,
                      RVU_WORK,
                      RVU_PE,
                      RVU_MP,
                      CONVERSION_FACTOR
               INTO :WS-FEE-BASE-AMOUNT,
                    :WS-FEE-RVU-WORK,
                    :WS-FEE-RVU-PE,
                    :WS-FEE-RVU-MP,
                    :WS-FEE-CONV-FACTOR
               FROM HCAS.FEE_SCHEDULE
               WHERE FEE_SCHED_ID = :WS-FEE-SCHED-ID
                 AND PROC_CODE = :WS-FEE-PROC-CODE
                 AND (MODIFIER = :WS-FEE-MODIFIER
                      OR MODIFIER = '  ')
                 AND EFF_DATE <= :WS-FEE-DOS
                 AND (TERM_DATE >= :WS-FEE-DOS
                      OR TERM_DATE = '99991231')
               ORDER BY MODIFIER DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = ZERO
               SET WS-FEE-FOUND TO TRUE
           END-IF.

       2200-CALCULATE-RVU-PRICE.
      *--- RBRVS CALCULATION:
      *    TOTAL RVU = WORK RVU + PE RVU + MP RVU
      *    ALLOWED = TOTAL RVU * GEOGRAPHIC ADJ * CONV FACTOR
           COMPUTE WS-CALC-RVU-TOTAL =
               WS-FEE-RVU-WORK +
               WS-FEE-RVU-PE +
               WS-FEE-RVU-MP
      *    LOOKUP GEOGRAPHIC WAGE INDEX
           PERFORM 2210-LOOKUP-WAGE-INDEX
           IF WS-WI-FOUND
               COMPUTE WS-CALC-RVU-AMT =
                   (WS-FEE-RVU-WORK * WS-WI-FACTOR) +
                   (WS-FEE-RVU-PE * WS-WI-FACTOR) +
                   WS-FEE-RVU-MP
               COMPUTE WS-CALC-FEE-AMT =
                   WS-CALC-RVU-AMT * WS-FEE-CONV-FACTOR
           ELSE
               COMPUTE WS-CALC-FEE-AMT =
                   WS-CALC-RVU-TOTAL * WS-FEE-CONV-FACTOR
           END-IF
      *    ROUND TO NEAREST CENT
           COMPUTE WS-CALC-FEE-AMT ROUNDED =
               WS-CALC-FEE-AMT.

       2210-LOOKUP-WAGE-INDEX.
           SET WS-WI-NOT-FOUND TO TRUE
           EXEC SQL
               SELECT WAGE_FACTOR
               INTO :WS-WI-FACTOR
               FROM HCAS.WAGE_INDEX
               WHERE NETWORK_ID = :LS-PRC-NETWORK-ID
                 AND EFF_DATE <= :WS-FEE-DOS
                 AND (TERM_DATE >= :WS-FEE-DOS
                      OR TERM_DATE = '99991231')
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = ZERO
               SET WS-WI-FOUND TO TRUE
           END-IF.

       2300-APPLY-MODIFIER-RULES.
           IF WS-LW-MODIFIER(WS-LINE-IDX) NOT = SPACES
           AND WS-LW-MODIFIER(WS-LINE-IDX) NOT = '  '
               SET WS-MOD-NOT-FOUND TO TRUE
               EXEC SQL
                   SELECT ADJUSTMENT_PERCENT,
                          ACTION_CODE
                   INTO :WS-MOD-PERCENT,
                        :WS-MOD-ACTION
                   FROM HCAS.MODIFIER_RULES
                   WHERE PROC_CODE = :WS-FEE-PROC-CODE
                     AND MODIFIER = :WS-FEE-MODIFIER
                   FETCH FIRST 1 ROW ONLY
               END-EXEC
               IF SQLCODE = ZERO
                   SET WS-MOD-FOUND TO TRUE
                   EVALUATE WS-MOD-ACTION
                       WHEN 'RD'
                           COMPUTE WS-CALC-FEE-AMT =
                               WS-CALC-FEE-AMT * WS-MOD-PERCENT
                       WHEN 'IN'
                           COMPUTE WS-CALC-FEE-AMT =
                               WS-CALC-FEE-AMT *
                               (1 + WS-MOD-PERCENT)
                       WHEN 'RP'
                           EXEC SQL
                               SELECT BASE_AMOUNT
                               INTO :WS-CALC-FEE-AMT
                               FROM HCAS.FEE_SCHEDULE
                               WHERE FEE_SCHED_ID =
                                     :WS-FEE-SCHED-ID
                                 AND PROC_CODE =
                                     :WS-FEE-PROC-CODE
                                 AND MODIFIER =
                                     :WS-FEE-MODIFIER
                               FETCH FIRST 1 ROW ONLY
                           END-EXEC
                   END-EVALUATE
               END-IF
           END-IF.

       2500-APPLY-MULTI-PROC-DISCOUNT.
      *--- FIND HIGHEST-PRICED SURGICAL LINE ---
           MOVE ZERO TO WS-MPD-HIGHEST-FEE
           MOVE ZERO TO WS-MPD-HIGHEST-IDX
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               IF WS-LW-SURGICAL(WS-LINE-IDX)
                   IF WS-LW-ALLOWED(WS-LINE-IDX) >
                      WS-MPD-HIGHEST-FEE
                       MOVE WS-LW-ALLOWED(WS-LINE-IDX)
                           TO WS-MPD-HIGHEST-FEE
                       MOVE WS-LINE-IDX
                           TO WS-MPD-HIGHEST-IDX
                   END-IF
               END-IF
           END-PERFORM
      *--- APPLY 50% DISCOUNT TO ALL BUT HIGHEST ---
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               IF WS-LW-SURGICAL(WS-LINE-IDX)
               AND WS-LINE-IDX NOT = WS-MPD-HIGHEST-IDX
                   COMPUTE WS-LW-ALLOWED(WS-LINE-IDX) =
                       WS-LW-ALLOWED(WS-LINE-IDX) *
                       WS-MPD-DISCOUNT-PCT
                   MOVE '59' TO WS-LW-ADJ-REASON
                                (WS-LINE-IDX, 2)
               END-IF
           END-PERFORM.

       3000-PRICE-INSTITUTIONAL.
      *--- INSTITUTIONAL CLAIM: DRG-BASED WITH OUTLIER ---
      *    FIND DRG CODE FROM FIRST LINE
           MOVE SPACES TO WS-DRG-CODE
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               OR WS-DRG-CODE NOT = SPACES
               IF WS-LW-DRG(WS-LINE-IDX) NOT = SPACES
                   MOVE WS-LW-DRG(WS-LINE-IDX) TO WS-DRG-CODE
               END-IF
           END-PERFORM
           IF WS-DRG-CODE NOT = SPACES
               PERFORM 3100-LOOKUP-DRG-WEIGHT
               IF WS-DRG-FOUND
                   PERFORM 3200-CALCULATE-DRG-PAYMENT
                   PERFORM 3300-DISTRIBUTE-TO-LINES
                   PERFORM 3400-CHECK-OUTLIER
               ELSE
      *            DRG NOT FOUND - FALLBACK TO PER-DIEM
                   PERFORM 3500-PRICE-PER-DIEM
               END-IF
           ELSE
      *        NO DRG - USE PER-DIEM PRICING
               PERFORM 3500-PRICE-PER-DIEM
           END-IF.

       3100-LOOKUP-DRG-WEIGHT.
           SET WS-DRG-NOT-FOUND TO TRUE
           EXEC SQL
               SELECT DRG_WEIGHT,
                      GEOMETRIC_MEAN_LOS,
                      ARITHMETIC_MEAN_LOS
               INTO :WS-DRG-WEIGHT,
                    :WS-DRG-GMLOS,
                    :WS-DRG-AMLOS
               FROM HCAS.DRG_WEIGHTS
               WHERE DRG_CODE = :WS-DRG-CODE
                 AND EFF_DATE <= :WS-LW-DOS(1)
                 AND (TERM_DATE >= :WS-LW-DOS(1)
                      OR TERM_DATE = '99991231')
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = ZERO
               SET WS-DRG-FOUND TO TRUE
           END-IF.

       3200-CALCULATE-DRG-PAYMENT.
      *--- IPPS PAYMENT CALCULATION ---
      *    LOOKUP BASE RATES
           EXEC SQL
               SELECT OPERATING_BASE_RATE,
                      CAPITAL_BASE_RATE
               INTO :WS-DRG-OPER-RATE,
                    :WS-DRG-CAPITAL-RATE
               FROM HCAS.FACILITY_RATES
               WHERE NETWORK_ID = :LS-PRC-NETWORK-ID
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE NOT = ZERO
               MOVE 5000.00 TO WS-DRG-OPER-RATE
               MOVE 500.00  TO WS-DRG-CAPITAL-RATE
           END-IF
      *    LOOKUP WAGE INDEX
           PERFORM 2210-LOOKUP-WAGE-INDEX
           IF WS-WI-FOUND
               COMPUTE WS-CALC-OPER-AMT =
                   WS-DRG-OPER-RATE * WS-DRG-WEIGHT *
                   WS-WI-FACTOR
               COMPUTE WS-CALC-CAPITAL-AMT =
                   WS-DRG-CAPITAL-RATE * WS-DRG-WEIGHT
           ELSE
               COMPUTE WS-CALC-OPER-AMT =
                   WS-DRG-OPER-RATE * WS-DRG-WEIGHT
               COMPUTE WS-CALC-CAPITAL-AMT =
                   WS-DRG-CAPITAL-RATE * WS-DRG-WEIGHT
           END-IF
           COMPUTE WS-CALC-TOTAL-DRG ROUNDED =
               WS-CALC-OPER-AMT + WS-CALC-CAPITAL-AMT.

       3300-DISTRIBUTE-TO-LINES.
      *--- DISTRIBUTE DRG PAYMENT PROPORTIONALLY ---
           MOVE ZERO TO WS-CALC-ALLOWED
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               ADD WS-LW-CHARGE(WS-LINE-IDX) TO WS-CALC-ALLOWED
           END-PERFORM
           IF WS-CALC-ALLOWED > ZERO
               PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
                   UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
                   COMPUTE WS-LW-ALLOWED(WS-LINE-IDX) ROUNDED =
                       WS-CALC-TOTAL-DRG *
                       (WS-LW-CHARGE(WS-LINE-IDX) /
                        WS-CALC-ALLOWED)
               END-PERFORM
           END-IF.

       3400-CHECK-OUTLIER.
           SET WS-OTL-NOT-FOUND TO TRUE
           EXEC SQL
               SELECT FIXED_LOSS_AMOUNT,
                      MARGINAL_COST_PCT
               INTO :WS-OTL-FIXED-LOSS,
                    :WS-OTL-MARGINAL-PCT
               FROM HCAS.OUTLIER_THRESHOLD
               WHERE NETWORK_ID = :LS-PRC-NETWORK-ID
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = ZERO
               SET WS-OTL-FOUND TO TRUE
               COMPUTE WS-OTL-THRESHOLD =
                   WS-CALC-TOTAL-DRG + WS-OTL-FIXED-LOSS
      *        SUM ALL LINE CHARGES
               MOVE ZERO TO WS-CALC-ALLOWED
               PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
                   UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
                   ADD WS-LW-CHARGE(WS-LINE-IDX)
                       TO WS-CALC-ALLOWED
               END-PERFORM
               IF WS-CALC-ALLOWED > WS-OTL-THRESHOLD
                   COMPUTE WS-OTL-PAYMENT =
                       (WS-CALC-ALLOWED - WS-OTL-THRESHOLD) *
                       WS-OTL-MARGINAL-PCT
      *            ADD OUTLIER TO LAST LINE
                   ADD WS-OTL-PAYMENT TO
                       WS-LW-ALLOWED(LS-PRC-LINE-COUNT)
               END-IF
           END-IF.

       3500-PRICE-PER-DIEM.
      *--- PER-DIEM FALLBACK: USE FEE SCHEDULE PER LINE ---
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               PERFORM 2100-LOOKUP-FEE-SCHEDULE
               IF WS-FEE-FOUND
                   COMPUTE WS-LW-ALLOWED(WS-LINE-IDX) =
                       WS-FEE-BASE-AMOUNT *
                       WS-LW-UNITS(WS-LINE-IDX)
               ELSE
      *            USE PERCENT OF CHARGE AS LAST RESORT
                   IF WS-CR-FOUND
                       COMPUTE WS-LW-ALLOWED(WS-LINE-IDX) =
                           WS-LW-CHARGE(WS-LINE-IDX) *
                           WS-CR-PERCENT-CHARGE
                   ELSE
                       MOVE ZERO
                           TO WS-LW-ALLOWED(WS-LINE-IDX)
                       MOVE '45'
                           TO WS-LW-DENY-CODE(WS-LINE-IDX)
                   END-IF
               END-IF
           END-PERFORM.

       4000-PRICE-DENTAL.
      *--- DENTAL CLAIM: STANDARD FEE SCHEDULE LOOKUP ---
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               PERFORM 2100-LOOKUP-FEE-SCHEDULE
               IF WS-FEE-FOUND
                   COMPUTE WS-LW-ALLOWED(WS-LINE-IDX) =
                       WS-FEE-BASE-AMOUNT *
                       WS-LW-UNITS(WS-LINE-IDX)
               ELSE
      *            DENTAL PLANS OFTEN USE UCR
                   COMPUTE WS-LW-ALLOWED(WS-LINE-IDX) =
                       WS-LW-CHARGE(WS-LINE-IDX) * .80
                   MOVE 'UCR  '
                       TO WS-LW-ADJ-REASON(WS-LINE-IDX, 1)
               END-IF
           END-PERFORM.

       5000-APPLY-LESSER-OF-LOGIC.
      *--- ALLOWED CANNOT EXCEED BILLED CHARGE ---
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               IF WS-LW-ALLOWED(WS-LINE-IDX) >
                  WS-LW-CHARGE(WS-LINE-IDX)
                   MOVE WS-LW-CHARGE(WS-LINE-IDX)
                       TO WS-LW-ALLOWED(WS-LINE-IDX)
                   MOVE '45'
                       TO WS-LW-ADJ-REASON(WS-LINE-IDX, 3)
               END-IF
      *        SET PAID = ALLOWED FOR NOW (BENEFITS APPLIED LATER)
               MOVE WS-LW-ALLOWED(WS-LINE-IDX)
                   TO WS-LW-PAID(WS-LINE-IDX)
           END-PERFORM.

       6000-BUILD-RESPONSE.
           MOVE ZERO TO LS-PRC-RSP-TOTAL-ALLOWED
           MOVE ZERO TO LS-PRC-RSP-TOTAL-PAID
           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > LS-PRC-LINE-COUNT
               MOVE WS-LW-SEQ(WS-LINE-IDX)
                   TO LS-PRC-RSP-LN-SEQ(WS-LINE-IDX)
               MOVE WS-LW-ALLOWED(WS-LINE-IDX)
                   TO LS-PRC-RSP-LN-ALLOW(WS-LINE-IDX)
               MOVE WS-LW-PAID(WS-LINE-IDX)
                   TO LS-PRC-RSP-LN-PAID(WS-LINE-IDX)
               MOVE WS-LW-DENY-CODE(WS-LINE-IDX)
                   TO LS-PRC-RSP-LN-DENY(WS-LINE-IDX)
               PERFORM VARYING WS-SORT-IDX FROM 1 BY 1
                   UNTIL WS-SORT-IDX > 5
                   MOVE WS-LW-ADJ-REASON(WS-LINE-IDX,
                                         WS-SORT-IDX)
                       TO LS-PRC-RSP-LN-ADJ(WS-LINE-IDX,
                                            WS-SORT-IDX)
               END-PERFORM
               ADD WS-LW-ALLOWED(WS-LINE-IDX)
                   TO LS-PRC-RSP-TOTAL-ALLOWED
               ADD WS-LW-PAID(WS-LINE-IDX)
                   TO LS-PRC-RSP-TOTAL-PAID
           END-PERFORM.
