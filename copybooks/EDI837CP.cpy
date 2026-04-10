      *================================================================*
      * COPYBOOK:    EDI837CP                                          *
      * DESCRIPTION: COMPLEX X12 837 CLAIM DATA STRUCTURE WITH         *
      *              REDEFINES FOR CLAIM TYPE DISCRIMINATION AND       *
      *              OCCURS DEPENDING ON FOR DYNAMIC ARRAYS            *
      *                                                                *
      * THIS COPYBOOK DEMONSTRATES TWO CRITICAL COBOL DATA PATTERNS:  *
      *  1. REDEFINES - MEMORY OVERLAY FOR INSTITUTIONAL VS            *
      *     PROFESSIONAL CLAIM FORMATS (SAME PHYSICAL MEMORY,         *
      *     DIFFERENT LOGICAL VIEWS)                                   *
      *  2. OCCURS DEPENDING ON - DYNAMIC-LENGTH ARRAYS FOR            *
      *     DIAGNOSIS CODES AND SERVICE LINE ITEMS                     *
      *                                                                *
      * THE COMBINATION OF THESE PATTERNS MAKES THIS COPYBOOK ONE     *
      * OF THE HARDEST STRUCTURES TO CONVERT TO JAVA, BECAUSE:        *
      *  - REDEFINES HAS NO DIRECT JAVA EQUIVALENT (JAVA DOESN'T     *
      *    ALLOW MEMORY OVERLAY)                                       *
      *  - OCCURS DEPENDING ON CREATES VARIABLE-LENGTH RECORDS        *
      *    THAT CHANGE SIZE AT RUNTIME                                 *
      *                                                                *
      * USED BY: CLMADJ01, CLMDTL00, EXTAPI01                        *
      *================================================================*

      *----------------------------------------------------------------*
      * CLAIM MASTER RECORD - TOP-LEVEL STRUCTURE                       *
      * THE CLAIM-TYPE FIELD ACTS AS A DISCRIMINATOR THAT DETERMINES   *
      * WHICH REDEFINES VIEW IS VALID FOR THE CLAIM DETAIL AREA       *
      *----------------------------------------------------------------*
       01  CLAIM-MASTER-RECORD.
      *--- COMMON HEADER - APPLIES TO ALL CLAIM TYPES ---
           05  CMR-HEADER.
               10  CMR-CLAIM-ID            PIC X(15).
               10  CMR-CLAIM-TYPE          PIC X(02).
                   88  CMR-INSTITUTIONAL               VALUE 'IN'.
                   88  CMR-PROFESSIONAL                VALUE 'PR'.
                   88  CMR-DENTAL                      VALUE 'DN'.
               10  CMR-SUBMITTER-ID        PIC X(10).
               10  CMR-RECEIVER-ID         PIC X(10).
               10  CMR-TRANSACTION-DATE    PIC X(08).
               10  CMR-TRANSACTION-TIME    PIC X(06).

      *--- SUBSCRIBER/PATIENT INFORMATION ---
           05  CMR-SUBSCRIBER-INFO.
               10  CMR-SUB-MEMBER-ID       PIC X(12).
               10  CMR-SUB-LAST-NAME       PIC X(35).
               10  CMR-SUB-FIRST-NAME      PIC X(25).
               10  CMR-SUB-MI              PIC X(01).
               10  CMR-SUB-DOB             PIC X(08).
               10  CMR-SUB-GENDER          PIC X(01).
                   88  CMR-MALE                        VALUE 'M'.
                   88  CMR-FEMALE                      VALUE 'F'.
                   88  CMR-UNKNOWN-GENDER              VALUE 'U'.
               10  CMR-SUB-SSN             PIC X(09).
               10  CMR-SUB-GROUP-ID        PIC X(10).
               10  CMR-SUB-PLAN-CODE       PIC X(08).
               10  CMR-SUB-ADDR.
                   15  CMR-SUB-ADDR1       PIC X(55).
                   15  CMR-SUB-ADDR2       PIC X(55).
                   15  CMR-SUB-CITY        PIC X(30).
                   15  CMR-SUB-STATE       PIC X(02).
                   15  CMR-SUB-ZIP         PIC X(09).
               10  CMR-PATIENT-IS-SUB-SW   PIC X(01).
                   88  CMR-PATIENT-IS-SUBSCRIBER       VALUE 'Y'.
                   88  CMR-PATIENT-IS-DEPENDENT        VALUE 'N'.
               10  CMR-PATIENT-RELATIONSHIP PIC X(02).

      *--- PATIENT (IF DIFFERENT FROM SUBSCRIBER) ---
           05  CMR-PATIENT-INFO.
               10  CMR-PAT-LAST-NAME       PIC X(35).
               10  CMR-PAT-FIRST-NAME      PIC X(25).
               10  CMR-PAT-MI              PIC X(01).
               10  CMR-PAT-DOB             PIC X(08).
               10  CMR-PAT-GENDER          PIC X(01).

      *--- BILLING PROVIDER ---
           05  CMR-BILLING-PROVIDER.
               10  CMR-BILL-NPI            PIC X(10).
               10  CMR-BILL-TAXONOMY       PIC X(10).
               10  CMR-BILL-ORG-NAME       PIC X(60).
               10  CMR-BILL-LAST-NAME      PIC X(35).
               10  CMR-BILL-FIRST-NAME     PIC X(25).
               10  CMR-BILL-ADDR.
                   15  CMR-BILL-ADDR1      PIC X(55).
                   15  CMR-BILL-CITY       PIC X(30).
                   15  CMR-BILL-STATE      PIC X(02).
                   15  CMR-BILL-ZIP        PIC X(09).
               10  CMR-BILL-TAX-ID         PIC X(10).
               10  CMR-BILL-ENTITY-TYPE    PIC X(01).

      *--- RENDERING PROVIDER ---
           05  CMR-RENDERING-PROVIDER.
               10  CMR-REND-NPI            PIC X(10).
               10  CMR-REND-TAXONOMY       PIC X(10).
               10  CMR-REND-LAST-NAME      PIC X(35).
               10  CMR-REND-FIRST-NAME     PIC X(25).

      *================================================================*
      * DIAGNOSIS CODE SECTION                                          *
      * USES OCCURS DEPENDING ON FOR VARIABLE-LENGTH ARRAY             *
      * A CLAIM CAN HAVE 1 TO 12 DIAGNOSIS CODES                       *
      * THE ACTUAL COUNT IS STORED IN CMR-DIAG-CODE-COUNT              *
      *================================================================*
           05  CMR-DIAGNOSIS-SECTION.
               10  CMR-DIAG-CODE-COUNT     PIC S9(02) COMP
                                           VALUE ZERO.
               10  CMR-DIAG-CODE-QUALIFIER PIC X(03).
                   88  CMR-DIAG-ICD10-CM               VALUE 'ABK'.
                   88  CMR-DIAG-ICD10-PCS              VALUE 'ABF'.
                   88  CMR-DIAG-ICD9-CM                VALUE 'BK '.
               10  CMR-DIAG-ENTRY
                       OCCURS 1 TO 12 TIMES
                       DEPENDING ON CMR-DIAG-CODE-COUNT.
                   15  CMR-DIAG-CODE       PIC X(08).
                   15  CMR-DIAG-POA        PIC X(01).
                       88  CMR-POA-YES                 VALUE 'Y'.
                       88  CMR-POA-NO                  VALUE 'N'.
                       88  CMR-POA-UNKNOWN             VALUE 'U'.
                       88  CMR-POA-EXEMPT              VALUE 'W'.
                   15  CMR-DIAG-SEQ        PIC S9(02) COMP.

      *================================================================*
      * CLAIM DETAIL AREA - USES REDEFINES FOR TYPE DISCRIMINATION     *
      * THIS IS THE CORE MEMORY OVERLAY PATTERN:                        *
      *  - INSTITUTIONAL-CLAIM AND PROFESSIONAL-CLAIM OCCUPY THE       *
      *    EXACT SAME PHYSICAL MEMORY                                   *
      *  - THE CMR-CLAIM-TYPE FIELD DETERMINES WHICH VIEW IS VALID     *
      *  - ACCESSING THE WRONG VIEW YIELDS GARBAGE DATA                *
      *================================================================*

      *--- INSTITUTIONAL CLAIM (UB-04 / 837I) ---
      *--- INCLUDES FACILITY-SPECIFIC FIELDS LIKE ADMIT DATE,         *
      *--- DRG, REVENUE CODES, TYPE OF BILL, CONDITION CODES          *
           05  CMR-INST-CLAIM-DETAIL.
               10  CMR-INST-TYPE-OF-BILL       PIC X(04).
               10  CMR-INST-ADMIT-DATE         PIC X(08).
               10  CMR-INST-ADMIT-HOUR         PIC X(02).
               10  CMR-INST-ADMIT-TYPE         PIC X(01).
                   88  CMR-ADMIT-EMERGENCY                 VALUE '1'.
                   88  CMR-ADMIT-URGENT                    VALUE '2'.
                   88  CMR-ADMIT-ELECTIVE                  VALUE '3'.
                   88  CMR-ADMIT-NEWBORN                   VALUE '4'.
               10  CMR-INST-ADMIT-SOURCE       PIC X(01).
               10  CMR-INST-DISCHARGE-DATE     PIC X(08).
               10  CMR-INST-DISCHARGE-HOUR     PIC X(02).
               10  CMR-INST-DISCHARGE-STATUS   PIC X(02).
               10  CMR-INST-DRG-CODE           PIC X(04).
               10  CMR-INST-DRG-WEIGHT         PIC S9(03)V9(04)
                                               COMP-3.
               10  CMR-INST-FACILITY-CODE      PIC X(02).
               10  CMR-INST-CONDITION-CODES.
                   15  CMR-INST-COND-CODE
                       OCCURS 5 TIMES          PIC X(02).
               10  CMR-INST-OCCURRENCE-CODES.
                   15  CMR-INST-OCC-ENTRY
                       OCCURS 4 TIMES.
                       20  CMR-INST-OCC-CODE   PIC X(02).
                       20  CMR-INST-OCC-DATE   PIC X(08).
               10  CMR-INST-VALUE-CODES.
                   15  CMR-INST-VAL-ENTRY
                       OCCURS 4 TIMES.
                       20  CMR-INST-VAL-CODE   PIC X(02).
                       20  CMR-INST-VAL-AMOUNT PIC S9(07)V99
                                               COMP-3.
               10  CMR-INST-TOTAL-CHARGES      PIC S9(09)V99
                                               COMP-3.
               10  CMR-INST-NON-COVERED-CHG    PIC S9(09)V99
                                               COMP-3.
               10  CMR-INST-PAYER-ID           PIC X(10).
               10  CMR-INST-AUTH-NUMBER        PIC X(20).
               10  CMR-INST-REFERRAL-NUMBER    PIC X(20).
               10  FILLER                      PIC X(56).

      *--- PROFESSIONAL CLAIM (CMS-1500 / 837P) ---
      *--- REDEFINES THE INSTITUTIONAL AREA WITH DIFFERENT FIELDS      *
           05  CMR-PROF-CLAIM-DETAIL
                   REDEFINES CMR-INST-CLAIM-DETAIL.
               10  CMR-PROF-PLACE-OF-SVC       PIC X(02).
               10  CMR-PROF-FREQUENCY-CODE     PIC X(01).
               10  CMR-PROF-ONSET-DATE         PIC X(08).
               10  CMR-PROF-SIMILAR-ILLNESS-DT PIC X(08).
               10  CMR-PROF-LAST-SEEN-DATE     PIC X(08).
               10  CMR-PROF-REFERRING-NPI       PIC X(10).
               10  CMR-PROF-REFERRING-NAME      PIC X(35).
               10  CMR-PROF-SUPERVISING-NPI     PIC X(10).
               10  CMR-PROF-AUTH-NUMBER         PIC X(20).
               10  CMR-PROF-REFERRAL-NUMBER     PIC X(20).
               10  CMR-PROF-CLIA-NUMBER         PIC X(10).
               10  CMR-PROF-AMBULANCE-INFO.
                   15  CMR-PROF-AMB-PICKUP-ADDR PIC X(55).
                   15  CMR-PROF-AMB-DROPOFF-ADDR
                                                PIC X(55).
                   15  CMR-PROF-AMB-REASON      PIC X(02).
                   15  CMR-PROF-AMB-TRANSPORT   PIC X(01).
               10  CMR-PROF-ACCIDENT-INFO.
                   15  CMR-PROF-ACC-TYPE        PIC X(02).
                       88  CMR-ACC-AUTO                    VALUE 'AA'.
                       88  CMR-ACC-EMPLOYMENT              VALUE 'EM'.
                       88  CMR-ACC-OTHER                   VALUE 'OA'.
                   15  CMR-PROF-ACC-DATE        PIC X(08).
                   15  CMR-PROF-ACC-STATE       PIC X(02).
               10  CMR-PROF-TOTAL-CHARGES       PIC S9(09)V99
                                                COMP-3.
               10  CMR-PROF-PAYER-ID            PIC X(10).
               10  FILLER                       PIC X(07).

      *================================================================*
      * SERVICE LINE ITEMS                                              *
      * USES OCCURS DEPENDING ON FOR VARIABLE NUMBER OF LINES          *
      * A CLAIM CAN HAVE 1 TO 50 SERVICE LINES                         *
      *================================================================*
           05  CMR-LINE-SECTION.
               10  CMR-LINE-COUNT              PIC S9(02) COMP
                                               VALUE ZERO.
               10  CMR-LINE-ITEM
                       OCCURS 1 TO 50 TIMES
                       DEPENDING ON CMR-LINE-COUNT.

      *---     COMMON LINE FIELDS ---
                   15  CMR-LN-SEQ-NBR          PIC S9(04) COMP.
                   15  CMR-LN-FROM-DATE        PIC X(08).
                   15  CMR-LN-TO-DATE          PIC X(08).

      *---     LINE TYPE-SPECIFIC DETAIL WITH REDEFINES ---
      *---     INSTITUTIONAL LINES USE REVENUE CODES                  *
      *---     PROFESSIONAL LINES USE PROCEDURE CODES                 *
                   15  CMR-LN-INST-DETAIL.
                       20  CMR-LN-REV-CODE     PIC X(04).
                       20  CMR-LN-REV-DESC     PIC X(30).
                       20  CMR-LN-HCPCS        PIC X(05).
                       20  CMR-LN-RATE         PIC S9(07)V99
                                               COMP-3.
                       20  CMR-LN-COVERED-DAYS PIC S9(03) COMP.
                       20  CMR-LN-NON-COV-DAYS PIC S9(03) COMP.
                       20  FILLER              PIC X(09).

                   15  CMR-LN-PROF-DETAIL
                           REDEFINES CMR-LN-INST-DETAIL.
                       20  CMR-LN-PROC-CODE    PIC X(05).
                       20  CMR-LN-MODIFIERS.
                           25  CMR-LN-MOD
                               OCCURS 4 TIMES  PIC X(02).
                       20  CMR-LN-PLACE-SVC    PIC X(02).
                       20  CMR-LN-TYPE-SVC     PIC X(01).
                       20  CMR-LN-EMG-IND      PIC X(01).
                       20  CMR-LN-EPSDT-IND    PIC X(01).
                       20  CMR-LN-FAMILY-PLAN  PIC X(01).
                       20  CMR-LN-NDC-CODE     PIC X(11).
                       20  CMR-LN-NDC-UNIT     PIC X(02).
                       20  CMR-LN-NDC-QTY      PIC S9(05)V99
                                               COMP-3.
                       20  CMR-LN-REND-NPI     PIC X(10).
                       20  FILLER              PIC X(08).

      *---     COMMON LINE FINANCIAL FIELDS ---
                   15  CMR-LN-CHARGES          PIC S9(07)V99
                                               COMP-3.
                   15  CMR-LN-UNITS            PIC S9(05)V99
                                               COMP-3.
                   15  CMR-LN-UNIT-TYPE        PIC X(02).
                       88  CMR-UNIT-MINUTES                VALUE 'MJ'.
                       88  CMR-UNIT-UNITS                  VALUE 'UN'.
                       88  CMR-UNIT-DAYS                   VALUE 'DA'.
                       88  CMR-UNIT-VISITS                 VALUE 'VS'.

      *---     LINE-LEVEL DIAGNOSIS POINTERS ---
                   15  CMR-LN-DIAG-POINTER-CNT PIC S9(02) COMP
                                               VALUE ZERO.
                   15  CMR-LN-DIAG-POINTER
                       OCCURS 1 TO 4 TIMES
                       DEPENDING ON CMR-LN-DIAG-POINTER-CNT
                                               PIC S9(02) COMP.

      *---     LINE-LEVEL ADJUSTMENT REASONS ---
                   15  CMR-LN-ADJ-REASON-CNT   PIC S9(02) COMP
                                               VALUE ZERO.
                   15  CMR-LN-ADJUSTMENT
                       OCCURS 0 TO 5 TIMES
                       DEPENDING ON CMR-LN-ADJ-REASON-CNT.
                       20  CMR-LN-ADJ-GROUP    PIC X(02).
                           88  CMR-ADJ-CONTRACTUAL         VALUE 'CO'.
                           88  CMR-ADJ-PATIENT-RESP        VALUE 'PR'.
                           88  CMR-ADJ-PAYER-INIT          VALUE 'PI'.
                           88  CMR-ADJ-OTHER-ADJ           VALUE 'OA'.
                       20  CMR-LN-ADJ-REASON   PIC X(05).
                       20  CMR-LN-ADJ-AMOUNT   PIC S9(07)V99
                                               COMP-3.
                       20  CMR-LN-ADJ-QUANTITY PIC S9(05)V99
                                               COMP-3.

      *================================================================*
      * CLAIM TOTALS AND SUMMARY                                        *
      *================================================================*
           05  CMR-TOTALS.
               10  CMR-TOT-CHARGES         PIC S9(09)V99 COMP-3.
               10  CMR-TOT-ALLOWED         PIC S9(09)V99 COMP-3.
               10  CMR-TOT-PAID            PIC S9(09)V99 COMP-3.
               10  CMR-TOT-MBR-RESP        PIC S9(09)V99 COMP-3.
               10  CMR-TOT-DEDUCTIBLE      PIC S9(07)V99 COMP-3.
               10  CMR-TOT-COPAY           PIC S9(07)V99 COMP-3.
               10  CMR-TOT-COINSURANCE     PIC S9(07)V99 COMP-3.

      *================================================================*
      * CLAIM PROCESSING METADATA                                       *
      *================================================================*
           05  CMR-METADATA.
               10  CMR-META-CLAIM-STATUS   PIC X(02).
                   88  CMR-STATUS-NEW                  VALUE '00'.
                   88  CMR-STATUS-REVIEW               VALUE '10'.
                   88  CMR-STATUS-PENDED               VALUE '20'.
                   88  CMR-STATUS-ADJUDICATING         VALUE '30'.
                   88  CMR-STATUS-DENIED               VALUE '40'.
                   88  CMR-STATUS-PAID                 VALUE '50'.
                   88  CMR-STATUS-ADJUSTED             VALUE '60'.
                   88  CMR-STATUS-VOIDED               VALUE '70'.
               10  CMR-META-RECEIPT-DATE   PIC X(08).
               10  CMR-META-PROCESS-DATE   PIC X(08).
               10  CMR-META-ADJ-DATE       PIC X(08).
               10  CMR-META-REMIT-DATE     PIC X(08).
               10  CMR-META-REMIT-STATUS   PIC X(02).
               10  CMR-META-CHECK-NUMBER   PIC X(12).
               10  CMR-META-EFT-TRACE      PIC X(20).
               10  CMR-META-CREATED-BY     PIC X(08).
               10  CMR-META-CREATED-TS     PIC X(26).
               10  CMR-META-UPDATED-BY     PIC X(08).
               10  CMR-META-UPDATED-TS     PIC X(26).
