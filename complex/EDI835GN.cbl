       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EDI835GN.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-06-01.
      *================================================================*
      * PROGRAM:  EDI835GN                                             *
      * PURPOSE:  EDI 835 HEALTH CARE CLAIM PAYMENT/ADVICE GENERATOR  *
      *           READS ADJUDICATED CLAIMS FROM THE CLAIM MASTER AND   *
      *           GENERATES X12 835 REMITTANCE ADVICE FILES FOR        *
      *           TRANSMISSION TO PROVIDERS AND CLEARINGHOUSES.        *
      *           ALSO PRODUCES THE EXPLANATION OF BENEFITS (EOB)      *
      *           FLAT FILE FOR MEMBER MAILING.                        *
      *           HANDLES PRECISE FINANCIAL FORMATTING PER X12 5010    *
      *           POSITIONAL REQUIREMENTS INCLUDING ZERO-FILL AND      *
      *           SPACE-FILL LOGIC FOR COMP-3 TO FLAT-FILE CONVERSION. *
      * INPUTS:   DB2 TABLES - HCAS.CLAIM_MASTER, HCAS.CLAIM_LINE     *
      *           DB2 TABLES - HCAS.PROVIDER, HCAS.MEMBER              *
      * OUTPUTS:  EDI835-FILE  - X12 835 REMITTANCE FILE               *
      *           EOBOUT-FILE  - EOB FLAT FILE FOR MEMBER MAILING      *
      *           CHKRPT-FILE  - CHECK REGISTER REPORT                 *
      * FREQUENCY: DAILY BATCH (POST-ADJUDICATION)                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EDI835-FILE
               ASSIGN TO EDI835OT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-835-STATUS.

           SELECT EOBOUT-FILE
               ASSIGN TO EOBOUTFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-EOB-STATUS.

           SELECT CHKRPT-FILE
               ASSIGN TO CHKRPTOT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CHK-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EDI835-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 10 TO 2000 CHARACTERS.
       01  EDI835-RECORD               PIC X(2000).

       FD  EOBOUT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 400 CHARACTERS.
       01  EOBOUT-RECORD               PIC X(400).

       FD  CHKRPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  CHKRPT-RECORD               PIC X(133).

       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY CLMCOPY.
       COPY ABORTWSC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  WS-FILE-STATUSES.
           05  WS-835-STATUS           PIC X(02).
           05  WS-EOB-STATUS           PIC X(02).
           05  WS-CHK-STATUS           PIC X(02).

       01  WS-SWITCHES.
           05  WS-MORE-CLAIMS-SW       PIC X(01) VALUE 'Y'.
               88  WS-MORE-CLAIMS      VALUE 'Y'.
               88  WS-NO-MORE-CLAIMS   VALUE 'N'.
           05  WS-PROVIDER-BREAK-SW    PIC X(01) VALUE 'N'.
               88  WS-PROVIDER-BREAK   VALUE 'Y'.
               88  WS-SAME-PROVIDER    VALUE 'N'.
           05  WS-MORE-LINES-SW        PIC X(01) VALUE 'Y'.
               88  WS-MORE-LINES       VALUE 'Y'.
               88  WS-NO-MORE-LINES    VALUE 'N'.

      *--- INTERCHANGE CONTROL FIELDS ---
       01  WS-ISA-FIELDS.
           05  WS-ISA-CONTROL-NO       PIC 9(09) VALUE ZERO.
           05  WS-GS-CONTROL-NO        PIC 9(09) VALUE ZERO.
           05  WS-ST-CONTROL-NO        PIC 9(09) VALUE ZERO.
           05  WS-SEGMENT-COUNT        PIC 9(07) VALUE ZERO.
           05  WS-SE-SEGMENT-COUNT     PIC 9(07) VALUE ZERO.
           05  WS-CLAIM-SEQ            PIC 9(07) VALUE ZERO.
           05  WS-TX-SET-COUNT         PIC 9(05) VALUE ZERO.

      *--- CURRENT PROVIDER TRACKING ---
       01  WS-CURRENT-PROVIDER.
           05  WS-CURR-BILLING-NPI     PIC X(10) VALUE SPACES.
           05  WS-CURR-PRV-NAME        PIC X(60).
           05  WS-CURR-PRV-ADDR1       PIC X(55).
           05  WS-CURR-PRV-CITY        PIC X(30).
           05  WS-CURR-PRV-STATE       PIC X(02).
           05  WS-CURR-PRV-ZIP         PIC X(09).
           05  WS-CURR-PRV-TIN         PIC X(09).
           05  WS-CURR-PRV-PAY-METHOD  PIC X(02).
           05  WS-CURR-PRV-BANK-RTN    PIC X(09).
           05  WS-CURR-PRV-BANK-ACCT   PIC X(17).

      *--- DB2 CURSOR FOR PAYABLE CLAIMS ---
           EXEC SQL DECLARE PAY-CURSOR CURSOR FOR
               SELECT CM.CLAIM_ID,
                      CM.CLAIM_TYPE,
                      CM.CLAIM_STATUS,
                      CM.MEMBER_ID,
                      CM.BILLING_NPI,
                      CM.RENDERING_NPI,
                      CM.TOTAL_CHARGES,
                      CM.ALLOWED_AMT,
                      CM.PAID_AMT,
                      CM.COPAY_AMT,
                      CM.DEDUCTIBLE_AMT,
                      CM.COINSURANCE_AMT,
                      CM.RECEIPT_DATE,
                      CM.ADJUD_DATE,
                      CM.PRINCIPAL_DIAG,
                      CM.PATIENT_ACCT_NO,
                      M.LAST_NAME,
                      M.FIRST_NAME,
                      M.DATE_OF_BIRTH,
                      M.SUBSCRIBER_ID,
                      P.ORG_NAME,
                      P.LAST_NAME,
                      P.TAX_ID,
                      P.PAY_METHOD,
                      P.BANK_ROUTING,
                      P.BANK_ACCT_NO,
                      P.ADDR_LINE_1,
                      P.CITY,
                      P.STATE_CODE,
                      P.ZIP_CODE
               FROM HCAS.CLAIM_MASTER CM
               INNER JOIN HCAS.MEMBER M
                   ON CM.MEMBER_ID = M.MEMBER_ID
               INNER JOIN HCAS.PROVIDER P
                   ON CM.BILLING_NPI = P.NPI
               WHERE CM.CLAIM_STATUS IN ('50', '40')
                 AND CM.REMIT_STATUS = 'N'
               ORDER BY CM.BILLING_NPI, CM.CLAIM_ID
           END-EXEC

      *--- DB2 HOST VARIABLES ---
       01  WS-PAY-CLAIM.
           05  WS-PAY-CLAIM-ID         PIC X(15).
           05  WS-PAY-CLAIM-TYPE       PIC X(02).
           05  WS-PAY-CLAIM-STATUS     PIC X(02).
           05  WS-PAY-MEMBER-ID        PIC X(12).
           05  WS-PAY-BILLING-NPI      PIC X(10).
           05  WS-PAY-RENDERING-NPI    PIC X(10).
           05  WS-PAY-TOTAL-CHARGES    PIC S9(09)V99 COMP-3.
           05  WS-PAY-ALLOWED-AMT      PIC S9(09)V99 COMP-3.
           05  WS-PAY-PAID-AMT         PIC S9(09)V99 COMP-3.
           05  WS-PAY-COPAY-AMT        PIC S9(07)V99 COMP-3.
           05  WS-PAY-DEDUCT-AMT       PIC S9(07)V99 COMP-3.
           05  WS-PAY-COINS-AMT        PIC S9(07)V99 COMP-3.
           05  WS-PAY-RECEIPT-DATE     PIC X(08).
           05  WS-PAY-ADJUD-DATE       PIC X(08).
           05  WS-PAY-PRINC-DIAG       PIC X(08).
           05  WS-PAY-PATIENT-ACCT     PIC X(20).
           05  WS-PAY-MBR-LAST         PIC X(35).
           05  WS-PAY-MBR-FIRST        PIC X(25).
           05  WS-PAY-MBR-DOB          PIC X(08).
           05  WS-PAY-SUBSCRIBER-ID    PIC X(12).
           05  WS-PAY-PRV-ORG          PIC X(60).
           05  WS-PAY-PRV-LAST         PIC X(35).
           05  WS-PAY-PRV-TIN          PIC X(09).
           05  WS-PAY-PRV-PAY-METHOD   PIC X(02).
           05  WS-PAY-PRV-BANK-RTN     PIC X(09).
           05  WS-PAY-PRV-BANK-ACCT    PIC X(17).
           05  WS-PAY-PRV-ADDR1        PIC X(55).
           05  WS-PAY-PRV-CITY         PIC X(30).
           05  WS-PAY-PRV-STATE        PIC X(02).
           05  WS-PAY-PRV-ZIP          PIC X(09).

      *--- CLAIM LINE CURSOR ---
           EXEC SQL DECLARE REMIT-LINE-CURSOR CURSOR FOR
               SELECT LINE_SEQ,
                      PROC_CODE,
                      LINE_CHARGE,
                      ALLOWED_AMT,
                      PAID_AMT,
                      DENY_CODE,
                      ADJ_REASON_1
               FROM HCAS.CLAIM_LINE
               WHERE CLAIM_ID = :WS-PAY-CLAIM-ID
               ORDER BY LINE_SEQ
           END-EXEC

       01  WS-REMIT-LINE.
           05  WS-RL-LINE-SEQ          PIC 9(03).
           05  WS-RL-PROC-CODE         PIC X(05).
           05  WS-RL-LINE-CHARGE       PIC S9(07)V99 COMP-3.
           05  WS-RL-ALLOWED-AMT       PIC S9(07)V99 COMP-3.
           05  WS-RL-PAID-AMT          PIC S9(07)V99 COMP-3.
           05  WS-RL-DENY-CODE         PIC X(05).
           05  WS-RL-ADJ-REASON        PIC X(05).

      *--- PROVIDER PAYMENT ACCUMULATORS ---
       01  WS-PRV-PAYMENT-TOTALS.
           05  WS-PRV-TOTAL-CHARGES    PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-PRV-TOTAL-PAID       PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-PRV-CLAIM-COUNT      PIC 9(05) VALUE ZERO.

      *--- GRAND TOTALS ---
       01  WS-GRAND-TOTALS.
           05  WS-GRAND-CHARGES        PIC S9(13)V99 COMP-3
                                       VALUE ZERO.
           05  WS-GRAND-PAID           PIC S9(13)V99 COMP-3
                                       VALUE ZERO.
           05  WS-GRAND-CLAIM-COUNT    PIC 9(07) VALUE ZERO.
           05  WS-GRAND-PRV-COUNT      PIC 9(07) VALUE ZERO.
           05  WS-GRAND-835-SEGMENTS   PIC 9(07) VALUE ZERO.

      *--- EDI SEGMENT BUILDING AREA ---
       01  WS-EDI-SEGMENT              PIC X(2000).
       01  WS-EDI-POS                  PIC 9(04).
       01  WS-ELEMENT-DELIM            PIC X(01) VALUE '*'.
       01  WS-SEGMENT-TERM             PIC X(01) VALUE '~'.

      *--- FINANCIAL FORMATTING WORK AREAS ---
       01  WS-FMT-AMOUNT-SIGNED        PIC S9(08)V99.
       01  WS-FMT-AMOUNT-DISPLAY       PIC -(8)9.99.
       01  WS-FMT-AMOUNT-EDI           PIC X(18).
       01  WS-FMT-ZERO-FILL            PIC 9(10)V99.

      *--- EOB RECORD LAYOUT ---
       01  WS-EOB-RECORD.
           05  WS-EOB-MEMBER-ID        PIC X(12).
           05  WS-EOB-MBR-NAME         PIC X(60).
           05  WS-EOB-CLAIM-ID         PIC X(15).
           05  WS-EOB-DOS              PIC X(08).
           05  WS-EOB-PROVIDER-NAME    PIC X(60).
           05  WS-EOB-TOTAL-CHARGES    PIC S9(09)V99 COMP-3.
           05  WS-EOB-ALLOWED-AMT      PIC S9(09)V99 COMP-3.
           05  WS-EOB-PLAN-PAID        PIC S9(09)V99 COMP-3.
           05  WS-EOB-YOUR-COPAY       PIC S9(07)V99 COMP-3.
           05  WS-EOB-YOUR-DEDUCT      PIC S9(07)V99 COMP-3.
           05  WS-EOB-YOUR-COINS       PIC S9(07)V99 COMP-3.
           05  WS-EOB-YOUR-TOTAL-RESP  PIC S9(09)V99 COMP-3.
           05  WS-EOB-STATUS-DESC      PIC X(20).
           05  WS-EOB-DIAG-CODE        PIC X(08).
           05  FILLER                  PIC X(69).

      *--- CHECK REGISTER FIELDS ---
       01  WS-CHK-LINE                 PIC X(133).
       01  WS-CHK-PAGE-COUNT           PIC 9(05) VALUE ZERO.
       01  WS-CHK-LINE-COUNT           PIC 9(03) VALUE 99.
       01  WS-CHK-CHECK-NO            PIC 9(10) VALUE 1000000.

       01  WS-TRACE-NUMBER             PIC 9(15) VALUE ZERO.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-PAYMENTS
               UNTIL WS-NO-MORE-CLAIMS
           IF WS-CURR-BILLING-NPI NOT = SPACES
               PERFORM 4900-WRITE-SE-TRAILER
               PERFORM 4950-WRITE-GE-IEA
           END-IF
           PERFORM 8000-WRITE-GRAND-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           MOVE 'EDI835GN' TO WS-PROGRAM-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN OUTPUT EDI835-FILE
           IF WS-835-STATUS NOT = '00'
               DISPLAY 'EDI835GN: OPEN ERROR 835 - ' WS-835-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT EOBOUT-FILE
           OPEN OUTPUT CHKRPT-FILE
           EXEC SQL OPEN PAY-CURSOR END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'EDI835GN: CURSOR OPEN ERROR SQLCODE='
                       SQLCODE
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 8100-FETCH-NEXT-CLAIM.

       2000-PROCESS-PAYMENTS.
      *--- CHECK FOR PROVIDER BREAK ---
           IF WS-PAY-BILLING-NPI NOT = WS-CURR-BILLING-NPI
               IF WS-CURR-BILLING-NPI NOT = SPACES
      *            CLOSE PREVIOUS PROVIDER'S 835
                   PERFORM 4900-WRITE-SE-TRAILER
               END-IF
               SET WS-PROVIDER-BREAK TO TRUE
               PERFORM 3000-START-NEW-PROVIDER
           END-IF
      *--- PROCESS THIS CLAIM ---
           PERFORM 4000-GENERATE-CLAIM-SEGMENTS
      *--- GENERATE EOB RECORD ---
           PERFORM 5000-GENERATE-EOB-RECORD
      *--- UPDATE REMIT STATUS ---
           PERFORM 6000-UPDATE-REMIT-STATUS
      *--- FETCH NEXT ---
           PERFORM 8100-FETCH-NEXT-CLAIM.

       3000-START-NEW-PROVIDER.
           ADD 1 TO WS-GRAND-PRV-COUNT
           MOVE ZERO TO WS-PRV-TOTAL-CHARGES
           MOVE ZERO TO WS-PRV-TOTAL-PAID
           MOVE ZERO TO WS-PRV-CLAIM-COUNT
           MOVE WS-PAY-BILLING-NPI TO WS-CURR-BILLING-NPI
           MOVE WS-PAY-PRV-ORG    TO WS-CURR-PRV-NAME
           MOVE WS-PAY-PRV-TIN    TO WS-CURR-PRV-TIN
           MOVE WS-PAY-PRV-PAY-METHOD
                                   TO WS-CURR-PRV-PAY-METHOD
           MOVE WS-PAY-PRV-BANK-RTN
                                   TO WS-CURR-PRV-BANK-RTN
           MOVE WS-PAY-PRV-BANK-ACCT
                                   TO WS-CURR-PRV-BANK-ACCT
           MOVE WS-PAY-PRV-ADDR1  TO WS-CURR-PRV-ADDR1
           MOVE WS-PAY-PRV-CITY   TO WS-CURR-PRV-CITY
           MOVE WS-PAY-PRV-STATE  TO WS-CURR-PRV-STATE
           MOVE WS-PAY-PRV-ZIP    TO WS-CURR-PRV-ZIP
      *--- WRITE ISA/GS/ST HEADERS FOR NEW PROVIDER ---
           IF WS-ISA-CONTROL-NO = ZERO
               PERFORM 3100-WRITE-ISA-HEADER
               PERFORM 3200-WRITE-GS-HEADER
           END-IF
           PERFORM 3300-WRITE-ST-HEADER
           PERFORM 3400-WRITE-BPR-SEGMENT
           PERFORM 3500-WRITE-TRN-SEGMENT
           PERFORM 3600-WRITE-PAYER-ID
           PERFORM 3700-WRITE-PAYEE-ID.

       3100-WRITE-ISA-HEADER.
           ADD 1 TO WS-ISA-CONTROL-NO
           INITIALIZE WS-EDI-SEGMENT
           STRING 'ISA' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '00' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '          ' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '00' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '          ' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'ZZ' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-NPI-PAYER DELIMITED SIZE
                  '     ' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'ZZ' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-CURR-BILLING-NPI DELIMITED SIZE
                  '     ' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-CURRENT-DATE(3:6) DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SEGMENT-COUNT.

       3200-WRITE-GS-HEADER.
           ADD 1 TO WS-GS-CONTROL-NO
           INITIALIZE WS-EDI-SEGMENT
           STRING 'GS' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'HP' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-TAX-ID-PAYER DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-CURR-PRV-TIN DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-CURRENT-DATE DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '1200' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-GS-CONTROL-NO DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'X' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '005010X221A1' DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SEGMENT-COUNT.

       3300-WRITE-ST-HEADER.
           ADD 1 TO WS-ST-CONTROL-NO
           ADD 1 TO WS-TX-SET-COUNT
           MOVE ZERO TO WS-SE-SEGMENT-COUNT
           INITIALIZE WS-EDI-SEGMENT
           STRING 'ST' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '835' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-ST-CONTROL-NO DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '005010X221A1' DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       3400-WRITE-BPR-SEGMENT.
      *--- BPR - FINANCIAL INFORMATION ---
           INITIALIZE WS-EDI-SEGMENT
           EVALUATE WS-CURR-PRV-PAY-METHOD
               WHEN 'EF'
                   STRING 'BPR' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'I' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          '0.00' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'C' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'ACH' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'CCP' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          '01' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-CURR-PRV-BANK-RTN DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'DA' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-CURR-PRV-BANK-ACCT DELIMITED SIZE
                          WS-SEGMENT-TERM DELIMITED SIZE
                          INTO WS-EDI-SEGMENT
                   END-STRING
               WHEN OTHER
                   STRING 'BPR' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'I' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          '0.00' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'C' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          'CHK' DELIMITED SIZE
                          WS-SEGMENT-TERM DELIMITED SIZE
                          INTO WS-EDI-SEGMENT
                   END-STRING
           END-EVALUATE
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       3500-WRITE-TRN-SEGMENT.
      *--- TRN - REASSOCIATION TRACE NUMBER ---
           ADD 1 TO WS-TRACE-NUMBER
           INITIALIZE WS-EDI-SEGMENT
           STRING 'TRN' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '1' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-TRACE-NUMBER DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '1' DELIMITED SIZE
                  WS-TAX-ID-PAYER DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       3600-WRITE-PAYER-ID.
      *--- N1*PR PAYER IDENTIFICATION ---
           INITIALIZE WS-EDI-SEGMENT
           STRING 'N1' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'PR' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-PAYER-NAME DELIMITED SPACES
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'XV' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-NPI-PAYER DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       3700-WRITE-PAYEE-ID.
      *--- N1*PE PAYEE IDENTIFICATION ---
           INITIALIZE WS-EDI-SEGMENT
           STRING 'N1' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'PE' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-CURR-PRV-NAME DELIMITED SPACES
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'XX' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-CURR-BILLING-NPI DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       4000-GENERATE-CLAIM-SEGMENTS.
           ADD 1 TO WS-CLAIM-SEQ
           ADD 1 TO WS-PRV-CLAIM-COUNT
           ADD 1 TO WS-GRAND-CLAIM-COUNT
           ADD WS-PAY-TOTAL-CHARGES TO WS-PRV-TOTAL-CHARGES
           ADD WS-PAY-PAID-AMT      TO WS-PRV-TOTAL-PAID
           ADD WS-PAY-TOTAL-CHARGES TO WS-GRAND-CHARGES
           ADD WS-PAY-PAID-AMT      TO WS-GRAND-PAID
      *--- CLP SEGMENT - CLAIM PAYMENT ---
           PERFORM 4100-WRITE-CLP-SEGMENT
      *--- NM1*QC PATIENT NAME ---
           PERFORM 4200-WRITE-PATIENT-NM1
      *--- SVC SEGMENTS - SERVICE LINES ---
           PERFORM 4300-WRITE-SERVICE-LINES
      *--- WRITE CHECK REGISTER LINE ---
           PERFORM 4500-WRITE-CHECK-LINE.

       4100-WRITE-CLP-SEGMENT.
           MOVE WS-PAY-PAID-AMT TO WS-FMT-AMOUNT-SIGNED
           MOVE WS-FMT-AMOUNT-SIGNED TO WS-FMT-AMOUNT-DISPLAY
           INSPECT WS-FMT-AMOUNT-DISPLAY
               REPLACING LEADING SPACES BY ZEROS
           INITIALIZE WS-EDI-SEGMENT
           EVALUATE WS-PAY-CLAIM-STATUS
               WHEN '50'
                   STRING 'CLP' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-PAY-PATIENT-ACCT DELIMITED SPACES
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          '1' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-PAY-TOTAL-CHARGES DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-FMT-AMOUNT-DISPLAY DELIMITED SPACES
                          WS-SEGMENT-TERM DELIMITED SIZE
                          INTO WS-EDI-SEGMENT
                   END-STRING
               WHEN '40'
                   STRING 'CLP' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-PAY-PATIENT-ACCT DELIMITED SPACES
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          '4' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-PAY-TOTAL-CHARGES DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          '0.00' DELIMITED SIZE
                          WS-SEGMENT-TERM DELIMITED SIZE
                          INTO WS-EDI-SEGMENT
                   END-STRING
               WHEN OTHER
                   STRING 'CLP' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-PAY-PATIENT-ACCT DELIMITED SPACES
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          '22' DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-PAY-TOTAL-CHARGES DELIMITED SIZE
                          WS-ELEMENT-DELIM DELIMITED SIZE
                          WS-FMT-AMOUNT-DISPLAY DELIMITED SPACES
                          WS-SEGMENT-TERM DELIMITED SIZE
                          INTO WS-EDI-SEGMENT
                   END-STRING
           END-EVALUATE
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       4200-WRITE-PATIENT-NM1.
           INITIALIZE WS-EDI-SEGMENT
           STRING 'NM1' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'QC' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '1' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-PAY-MBR-LAST DELIMITED SPACES
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-PAY-MBR-FIRST DELIMITED SPACES
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'MI' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-PAY-SUBSCRIBER-ID DELIMITED SPACES
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       4300-WRITE-SERVICE-LINES.
           EXEC SQL OPEN REMIT-LINE-CURSOR END-EXEC
           IF SQLCODE = ZERO
               SET WS-MORE-LINES TO TRUE
               PERFORM 4310-FETCH-WRITE-SVC
                   UNTIL WS-NO-MORE-LINES
               EXEC SQL CLOSE REMIT-LINE-CURSOR END-EXEC
           END-IF.

       4310-FETCH-WRITE-SVC.
           EXEC SQL
               FETCH REMIT-LINE-CURSOR
               INTO :WS-RL-LINE-SEQ,
                    :WS-RL-PROC-CODE,
                    :WS-RL-LINE-CHARGE,
                    :WS-RL-ALLOWED-AMT,
                    :WS-RL-PAID-AMT,
                    :WS-RL-DENY-CODE,
                    :WS-RL-ADJ-REASON
           END-EXEC
           IF SQLCODE = +100
               SET WS-NO-MORE-LINES TO TRUE
           ELSE IF SQLCODE = ZERO
      *        SVC SEGMENT
               MOVE WS-RL-PAID-AMT TO WS-FMT-AMOUNT-SIGNED
               MOVE WS-FMT-AMOUNT-SIGNED
                   TO WS-FMT-AMOUNT-DISPLAY
               INITIALIZE WS-EDI-SEGMENT
               STRING 'SVC' DELIMITED SIZE
                      WS-ELEMENT-DELIM DELIMITED SIZE
                      'HC:' DELIMITED SIZE
                      WS-RL-PROC-CODE DELIMITED SPACES
                      WS-ELEMENT-DELIM DELIMITED SIZE
                      WS-RL-LINE-CHARGE DELIMITED SIZE
                      WS-ELEMENT-DELIM DELIMITED SIZE
                      WS-FMT-AMOUNT-DISPLAY DELIMITED SPACES
                      WS-SEGMENT-TERM DELIMITED SIZE
                      INTO WS-EDI-SEGMENT
               END-STRING
               WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
               ADD 1 TO WS-SE-SEGMENT-COUNT
               ADD 1 TO WS-SEGMENT-COUNT
      *        CAS SEGMENT IF ADJUSTMENT EXISTS
               IF WS-RL-ADJ-REASON NOT = SPACES
                   PERFORM 4320-WRITE-CAS-SEGMENT
               END-IF
           END-IF.

       4320-WRITE-CAS-SEGMENT.
           COMPUTE WS-FMT-AMOUNT-SIGNED =
               WS-RL-LINE-CHARGE - WS-RL-PAID-AMT
           MOVE WS-FMT-AMOUNT-SIGNED TO WS-FMT-AMOUNT-DISPLAY
           INITIALIZE WS-EDI-SEGMENT
           STRING 'CAS' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  'CO' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-RL-ADJ-REASON DELIMITED SPACES
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-FMT-AMOUNT-DISPLAY DELIMITED SPACES
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SE-SEGMENT-COUNT
           ADD 1 TO WS-SEGMENT-COUNT.

       4500-WRITE-CHECK-LINE.
           IF WS-CHK-LINE-COUNT >= 55
               PERFORM 4510-WRITE-CHECK-HEADER
           END-IF
           MOVE SPACES TO WS-CHK-LINE
           STRING WS-PAY-CLAIM-ID      DELIMITED SIZE
                  ' '                   DELIMITED SIZE
                  WS-PAY-MEMBER-ID      DELIMITED SIZE
                  ' '                   DELIMITED SIZE
                  WS-PAY-MBR-LAST(1:20) DELIMITED SIZE
                  ' '                   DELIMITED SIZE
                  WS-PAY-TOTAL-CHARGES  DELIMITED SIZE
                  ' '                   DELIMITED SIZE
                  WS-PAY-PAID-AMT       DELIMITED SIZE
                  INTO WS-CHK-LINE
           END-STRING
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
           ADD 1 TO WS-CHK-LINE-COUNT.

       4510-WRITE-CHECK-HEADER.
           ADD 1 TO WS-CHK-PAGE-COUNT
           MOVE SPACES TO WS-CHK-LINE
           STRING 'CHECK REGISTER - PAGE '
                  DELIMITED SIZE
                  WS-CHK-PAGE-COUNT DELIMITED SIZE
                  '  DATE: ' DELIMITED SIZE
                  WS-CURRENT-DATE DELIMITED SIZE
                  INTO WS-CHK-LINE
           END-STRING
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
               AFTER ADVANCING PAGE
           MOVE SPACES TO WS-CHK-LINE
           STRING 'CLAIM ID        '    DELIMITED SIZE
                  'MEMBER ID    '       DELIMITED SIZE
                  'MEMBER NAME          ' DELIMITED SIZE
                  'CHARGES       '      DELIMITED SIZE
                  'PAID AMT'            DELIMITED SIZE
                  INTO WS-CHK-LINE
           END-STRING
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
           MOVE ALL '-' TO WS-CHK-LINE
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
           MOVE 3 TO WS-CHK-LINE-COUNT.

       4900-WRITE-SE-TRAILER.
           ADD 1 TO WS-SE-SEGMENT-COUNT
           INITIALIZE WS-EDI-SEGMENT
           STRING 'SE' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-SE-SEGMENT-COUNT DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-ST-CONTROL-NO DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SEGMENT-COUNT.

       4950-WRITE-GE-IEA.
           INITIALIZE WS-EDI-SEGMENT
           STRING 'GE' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-TX-SET-COUNT DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-GS-CONTROL-NO DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SEGMENT-COUNT
           INITIALIZE WS-EDI-SEGMENT
           STRING 'IEA' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  '1' DELIMITED SIZE
                  WS-ELEMENT-DELIM DELIMITED SIZE
                  WS-ISA-CONTROL-NO DELIMITED SIZE
                  WS-SEGMENT-TERM DELIMITED SIZE
                  INTO WS-EDI-SEGMENT
           END-STRING
           WRITE EDI835-RECORD FROM WS-EDI-SEGMENT
           ADD 1 TO WS-SEGMENT-COUNT.

       5000-GENERATE-EOB-RECORD.
           INITIALIZE WS-EOB-RECORD
           MOVE WS-PAY-MEMBER-ID    TO WS-EOB-MEMBER-ID
           STRING WS-PAY-MBR-FIRST DELIMITED SPACES
                  ' '               DELIMITED SIZE
                  WS-PAY-MBR-LAST  DELIMITED SPACES
                  INTO WS-EOB-MBR-NAME
           END-STRING
           MOVE WS-PAY-CLAIM-ID     TO WS-EOB-CLAIM-ID
           MOVE WS-PAY-RECEIPT-DATE  TO WS-EOB-DOS
           MOVE WS-CURR-PRV-NAME    TO WS-EOB-PROVIDER-NAME
           MOVE WS-PAY-TOTAL-CHARGES TO WS-EOB-TOTAL-CHARGES
           MOVE WS-PAY-ALLOWED-AMT  TO WS-EOB-ALLOWED-AMT
           MOVE WS-PAY-PAID-AMT     TO WS-EOB-PLAN-PAID
           MOVE WS-PAY-COPAY-AMT    TO WS-EOB-YOUR-COPAY
           MOVE WS-PAY-DEDUCT-AMT   TO WS-EOB-YOUR-DEDUCT
           MOVE WS-PAY-COINS-AMT    TO WS-EOB-YOUR-COINS
           COMPUTE WS-EOB-YOUR-TOTAL-RESP =
               WS-PAY-COPAY-AMT +
               WS-PAY-DEDUCT-AMT +
               WS-PAY-COINS-AMT
           EVALUATE WS-PAY-CLAIM-STATUS
               WHEN '50'
                   MOVE 'CLAIM PAID'
                       TO WS-EOB-STATUS-DESC
               WHEN '40'
                   MOVE 'CLAIM DENIED'
                       TO WS-EOB-STATUS-DESC
               WHEN OTHER
                   MOVE 'PROCESSED'
                       TO WS-EOB-STATUS-DESC
           END-EVALUATE
           MOVE WS-PAY-PRINC-DIAG TO WS-EOB-DIAG-CODE
           MOVE WS-EOB-RECORD TO EOBOUT-RECORD
           WRITE EOBOUT-RECORD.

       6000-UPDATE-REMIT-STATUS.
           EXEC SQL
               UPDATE HCAS.CLAIM_MASTER
               SET    REMIT_STATUS = 'Y',
                      REMIT_DATE = :WS-CURRENT-DATE
               WHERE  CLAIM_ID = :WS-PAY-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'EDI835GN: REMIT UPDATE ERROR SQLCODE='
                       SQLCODE ' CLM=' WS-PAY-CLAIM-ID
           END-IF.

       8000-WRITE-GRAND-SUMMARY.
           MOVE SPACES TO WS-CHK-LINE
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
           MOVE SPACES TO WS-CHK-LINE
           STRING '=== GRAND TOTALS ===' DELIMITED SIZE
                  INTO WS-CHK-LINE
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
           MOVE SPACES TO WS-CHK-LINE
           STRING 'TOTAL PROVIDERS:  ' DELIMITED SIZE
                  WS-GRAND-PRV-COUNT   DELIMITED SIZE
                  INTO WS-CHK-LINE
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
           MOVE SPACES TO WS-CHK-LINE
           STRING 'TOTAL CLAIMS:     ' DELIMITED SIZE
                  WS-GRAND-CLAIM-COUNT DELIMITED SIZE
                  INTO WS-CHK-LINE
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE
           MOVE SPACES TO WS-CHK-LINE
           STRING 'TOTAL 835 SEGMENTS: ' DELIMITED SIZE
                  WS-SEGMENT-COUNT     DELIMITED SIZE
                  INTO WS-CHK-LINE
           WRITE CHKRPT-RECORD FROM WS-CHK-LINE.

       8100-FETCH-NEXT-CLAIM.
           EXEC SQL
               FETCH PAY-CURSOR
               INTO :WS-PAY-CLAIM-ID,
                    :WS-PAY-CLAIM-TYPE,
                    :WS-PAY-CLAIM-STATUS,
                    :WS-PAY-MEMBER-ID,
                    :WS-PAY-BILLING-NPI,
                    :WS-PAY-RENDERING-NPI,
                    :WS-PAY-TOTAL-CHARGES,
                    :WS-PAY-ALLOWED-AMT,
                    :WS-PAY-PAID-AMT,
                    :WS-PAY-COPAY-AMT,
                    :WS-PAY-DEDUCT-AMT,
                    :WS-PAY-COINS-AMT,
                    :WS-PAY-RECEIPT-DATE,
                    :WS-PAY-ADJUD-DATE,
                    :WS-PAY-PRINC-DIAG,
                    :WS-PAY-PATIENT-ACCT,
                    :WS-PAY-MBR-LAST,
                    :WS-PAY-MBR-FIRST,
                    :WS-PAY-MBR-DOB,
                    :WS-PAY-SUBSCRIBER-ID,
                    :WS-PAY-PRV-ORG,
                    :WS-PAY-PRV-LAST,
                    :WS-PAY-PRV-TIN,
                    :WS-PAY-PRV-PAY-METHOD,
                    :WS-PAY-PRV-BANK-RTN,
                    :WS-PAY-PRV-BANK-ACCT,
                    :WS-PAY-PRV-ADDR1,
                    :WS-PAY-PRV-CITY,
                    :WS-PAY-PRV-STATE,
                    :WS-PAY-PRV-ZIP
           END-EXEC
           IF SQLCODE = +100
               SET WS-NO-MORE-CLAIMS TO TRUE
           ELSE IF SQLCODE NOT = ZERO
               DISPLAY 'EDI835GN: FETCH ERROR SQLCODE=' SQLCODE
               SET WS-NO-MORE-CLAIMS TO TRUE
           END-IF.

       9000-TERMINATE.
           EXEC SQL CLOSE PAY-CURSOR END-EXEC
           EXEC SQL COMMIT END-EXEC
           DISPLAY '================================================'
           DISPLAY 'EDI835GN: 835 REMITTANCE GENERATION COMPLETE'
           DISPLAY '================================================'
           DISPLAY '  PROVIDERS PROCESSED: ' WS-GRAND-PRV-COUNT
           DISPLAY '  CLAIMS REMITTED:     ' WS-GRAND-CLAIM-COUNT
           DISPLAY '  835 SEGMENTS WRITTEN:' WS-SEGMENT-COUNT
           DISPLAY '================================================'
           CLOSE EDI835-FILE
           CLOSE EOBOUT-FILE
           CLOSE CHKRPT-FILE
           MOVE ZERO TO RETURN-CODE.
