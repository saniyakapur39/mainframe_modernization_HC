       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MEMMGR01.
       AUTHOR.        HEALTHCARE PAYER SYSTEMS.
       DATE-WRITTEN.  2024-01-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM:     MEMMGR01                                          *
      * DESCRIPTION: EDI BATCH PROCESSOR DEMONSTRATING DYNAMIC MEMORY  *
      *              MANAGEMENT WITH USAGE IS POINTER, ADDRESS OF,     *
      *              AND CICS CONTAINERS FOR LARGE PAYLOADS            *
      *                                                                *
      * THIS PROGRAM DEMONSTRATES:                                     *
      *  1. USAGE IS POINTER AND SET ADDRESS OF TO MAP LINKAGE        *
      *     SECTION STRUCTURES OVER RAW MEMORY OBTAINED FROM          *
      *     CICS CONTAINERS                                            *
      *  2. BYPASSING THE 32KB COMMAREA LIMIT USING CICS CHANNELS    *
      *     AND CONTAINERS TO HANDLE LARGE EDI BATCH PAYLOADS         *
      *  3. PROCESSING BULK X12 EDI TRANSACTIONS WITH ISA/GS/ST       *
      *     SEGMENT PARSING                                            *
      *  4. PROPER MEMORY MANAGEMENT AND POINTER ARITHMETIC            *
      *                                                                *
      * BUSINESS CONTEXT:                                              *
      *  RECEIVES LARGE EDI 837 BATCH FILES (UP TO 5MB) VIA CICS     *
      *  CONTAINER, PARSES THE ISA/GS/ST ENVELOPE STRUCTURE,          *
      *  EXTRACTS INDIVIDUAL CLAIMS, AND INSERTS THEM INTO A DB2      *
      *  STAGING TABLE FOR DOWNSTREAM ADJUDICATION.                    *
      *                                                                *
      * TRANSACTION: MEDI                                              *
      *                                                                *
      * DEPENDENCIES:                                                  *
      *  - COPYBOOKS: HCCOMMON, EDI837CP, ABORTWSC                   *
      *  - CHANNEL:  EDI-BATCH-CHANNEL                                 *
      *  - CONTAINERS: EDI-PAYLOAD, BATCH-CONTROL, BATCH-RESULTS      *
      *  - DB2 TABLES: HCAS.CLAIM_STAGING, HCAS.EDI_BATCH_LOG        *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-ZOS.
       OBJECT-COMPUTER.    IBM-ZOS.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * PROGRAM IDENTIFICATION                                          *
      *----------------------------------------------------------------*
       01  WS-PROGRAM-ID              PIC X(08)  VALUE 'MEMMGR01'.
       01  WS-PROGRAM-VERSION         PIC X(06)  VALUE '01.00 '.
       01  WS-TRANSACTION-ID          PIC X(04)  VALUE 'MEDI'.

      *----------------------------------------------------------------*
      * COMMON HEALTHCARE FIELDS                                        *
      *----------------------------------------------------------------*
           COPY HCCOMMON.

      *----------------------------------------------------------------*
      * ERROR HANDLING                                                  *
      *----------------------------------------------------------------*
           COPY ABORTWSC.

      *----------------------------------------------------------------*
      * DB2 COMMUNICATION AREA                                          *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.

      *----------------------------------------------------------------*
      * CICS RESPONSE CODES                                             *
      *----------------------------------------------------------------*
       01  WS-CICS-RESP               PIC S9(08) COMP VALUE ZERO.
       01  WS-CICS-RESP2              PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * POINTER VARIABLES FOR DYNAMIC MEMORY MANAGEMENT                 *
      * THESE ARE THE KEY MODERNIZATION CHALLENGE:                      *
      * COBOL POINTERS ALLOW DIRECT MEMORY ADDRESS MANIPULATION        *
      * WHICH HAS NO DIRECT JAVA EQUIVALENT                            *
      *----------------------------------------------------------------*
       01  WS-POINTERS.
           05  WS-EDI-PAYLOAD-PTR     USAGE IS POINTER.
           05  WS-CURRENT-SEG-PTR     USAGE IS POINTER.
           05  WS-NEXT-SEG-PTR        USAGE IS POINTER.
           05  WS-CLAIM-START-PTR     USAGE IS POINTER.
           05  WS-WORK-AREA-PTR       USAGE IS POINTER.
           05  WS-NULL-PTR            USAGE IS POINTER VALUE NULL.

      *----------------------------------------------------------------*
      * POINTER ARITHMETIC WORK AREAS                                   *
      *----------------------------------------------------------------*
       01  WS-PTR-ARITHMETIC.
           05  WS-PAYLOAD-OFFSET      PIC S9(08) COMP VALUE ZERO.
           05  WS-PAYLOAD-LENGTH      PIC S9(08) COMP VALUE ZERO.
           05  WS-SEGMENT-OFFSET      PIC S9(08) COMP VALUE ZERO.
           05  WS-SEGMENT-LENGTH      PIC S9(08) COMP VALUE ZERO.
           05  WS-REMAINING-LENGTH    PIC S9(08) COMP VALUE ZERO.
           05  WS-BYTES-PROCESSED     PIC S9(08) COMP VALUE ZERO.
           05  WS-MAX-PAYLOAD-SIZE    PIC S9(08) COMP
                                       VALUE 5242880.

      *----------------------------------------------------------------*
      * PROCESSING FLAGS AND SWITCHES                                   *
      *----------------------------------------------------------------*
       01  WS-FLAGS.
           05  WS-END-OF-PAYLOAD-SW   PIC X(01) VALUE 'N'.
               88 END-OF-PAYLOAD                  VALUE 'Y'.
               88 MORE-PAYLOAD                    VALUE 'N'.
           05  WS-IN-TRANSACTION-SW   PIC X(01) VALUE 'N'.
               88 IN-TRANSACTION                  VALUE 'Y'.
               88 NOT-IN-TRANSACTION              VALUE 'N'.
           05  WS-IN-FUNC-GROUP-SW    PIC X(01) VALUE 'N'.
               88 IN-FUNCTIONAL-GROUP             VALUE 'Y'.
               88 NOT-IN-FUNC-GROUP               VALUE 'N'.
           05  WS-IN-CLAIM-SW         PIC X(01) VALUE 'N'.
               88 IN-CLAIM                        VALUE 'Y'.
               88 NOT-IN-CLAIM                    VALUE 'N'.
           05  WS-CONTAINER-OK-SW     PIC X(01) VALUE 'N'.
               88 CONTAINER-OK                    VALUE 'Y'.
               88 CONTAINER-FAILED                VALUE 'N'.
           05  WS-PARSE-OK-SW         PIC X(01) VALUE 'N'.
               88 PARSE-OK                        VALUE 'Y'.
               88 PARSE-FAILED                    VALUE 'N'.

      *----------------------------------------------------------------*
      * EDI ENVELOPE TRACKING                                           *
      *----------------------------------------------------------------*
       01  WS-EDI-ENVELOPE.
           05  WS-ISA-FIELDS.
               10  WS-ISA-SENDER-ID   PIC X(15).
               10  WS-ISA-RECEIVER-ID PIC X(15).
               10  WS-ISA-DATE        PIC X(06).
               10  WS-ISA-TIME        PIC X(04).
               10  WS-ISA-CONTROL-NUM PIC X(09).
               10  WS-ISA-VERSION     PIC X(05).
               10  WS-ISA-SEGMENT-TERM
                                      PIC X(01) VALUE '~'.
               10  WS-ISA-ELEMENT-SEP PIC X(01) VALUE '*'.
               10  WS-ISA-SUB-SEP     PIC X(01) VALUE ':'.
           05  WS-GS-FIELDS.
               10  WS-GS-FUNC-CODE   PIC X(02).
               10  WS-GS-SENDER-CODE PIC X(15).
               10  WS-GS-RECEIVER-CODE
                                      PIC X(15).
               10  WS-GS-DATE        PIC X(08).
               10  WS-GS-CONTROL-NUM PIC X(09).
               10  WS-GS-VERSION     PIC X(12).
           05  WS-ST-FIELDS.
               10  WS-ST-TRANS-CODE  PIC X(03).
               10  WS-ST-CONTROL-NUM PIC X(09).
               10  WS-ST-VERSION     PIC X(35).

      *----------------------------------------------------------------*
      * SEGMENT PARSING WORK AREAS                                      *
      *----------------------------------------------------------------*
       01  WS-SEGMENT-WORK.
           05  WS-CURRENT-SEGMENT     PIC X(512).
           05  WS-SEGMENT-ID          PIC X(03).
           05  WS-ELEMENT-COUNT       PIC S9(04) COMP VALUE ZERO.
           05  WS-ELEMENTS.
               10  WS-ELEMENT OCCURS 30 TIMES PIC X(80).
           05  WS-ELEMENT-IDX         PIC S9(04) COMP VALUE ZERO.
           05  WS-UNSTRING-PTR        PIC S9(08) COMP VALUE ZERO.
           05  WS-SEGMENT-TALLY       PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * CLAIM ACCUMULATION BUFFER                                       *
      * HOLDS DATA FOR ONE CLAIM BEING PARSED FROM SEGMENTS            *
      *----------------------------------------------------------------*
       01  WS-CLAIM-BUFFER.
           05  WS-CLM-CLAIM-ID        PIC X(15).
           05  WS-CLM-CLAIM-TYPE      PIC X(02).
           05  WS-CLM-MEMBER-ID       PIC X(12).
           05  WS-CLM-SUBSCRIBER-ID   PIC X(12).
           05  WS-CLM-LAST-NAME       PIC X(35).
           05  WS-CLM-FIRST-NAME      PIC X(25).
           05  WS-CLM-DOB             PIC X(08).
           05  WS-CLM-GENDER          PIC X(01).
           05  WS-CLM-BILLING-NPI     PIC X(10).
           05  WS-CLM-RENDERING-NPI   PIC X(10).
           05  WS-CLM-TOTAL-CHARGES   PIC S9(09)V99 COMP-3
                                       VALUE ZERO.
           05  WS-CLM-PLACE-SVC       PIC X(02).
           05  WS-CLM-FREQUENCY       PIC X(01).
           05  WS-CLM-ADMIT-DATE      PIC X(08).
           05  WS-CLM-DISCHARGE-DATE  PIC X(08).
           05  WS-CLM-DRG-CODE        PIC X(04).
           05  WS-CLM-DIAG-COUNT      PIC S9(02) COMP VALUE ZERO.
           05  WS-CLM-DIAG-CODES.
               10  WS-CLM-DIAG OCCURS 12 TIMES PIC X(08).
           05  WS-CLM-LINE-COUNT      PIC S9(02) COMP VALUE ZERO.
           05  WS-CLM-LINES OCCURS 50 TIMES.
               10  WS-CLM-LN-SEQ      PIC S9(04) COMP.
               10  WS-CLM-LN-PROC     PIC X(05).
               10  WS-CLM-LN-MOD1     PIC X(02).
               10  WS-CLM-LN-MOD2     PIC X(02).
               10  WS-CLM-LN-REV-CODE PIC X(04).
               10  WS-CLM-LN-FROM-DT  PIC X(08).
               10  WS-CLM-LN-TO-DT    PIC X(08).
               10  WS-CLM-LN-CHARGES  PIC S9(07)V99 COMP-3.
               10  WS-CLM-LN-UNITS    PIC S9(05)V99 COMP-3.

      *----------------------------------------------------------------*
      * BATCH CONTROL DATA - RECEIVED FROM CALLING PROGRAM              *
      *----------------------------------------------------------------*
       01  WS-BATCH-CONTROL.
           05  WS-BATCH-ID            PIC X(15).
           05  WS-BATCH-SOURCE        PIC X(10).
           05  WS-BATCH-RECEIVED-TS   PIC X(26).
           05  WS-BATCH-EXPECTED-CNT  PIC S9(06) COMP VALUE ZERO.
           05  WS-BATCH-PAYER-ID      PIC X(10).

      *----------------------------------------------------------------*
      * BATCH RESULTS - RETURNED TO CALLING PROGRAM                     *
      *----------------------------------------------------------------*
       01  WS-BATCH-RESULTS.
           05  WS-RES-RETURN-CODE     PIC X(02).
               88 RES-SUCCESS                     VALUE '00'.
               88 RES-PARTIAL                     VALUE '01'.
               88 RES-FAILED                      VALUE '90'.
           05  WS-RES-MESSAGE         PIC X(80).
           05  WS-RES-BATCH-ID        PIC X(15).
           05  WS-RES-ISA-COUNT       PIC S9(06) COMP VALUE ZERO.
           05  WS-RES-GS-COUNT        PIC S9(06) COMP VALUE ZERO.
           05  WS-RES-ST-COUNT        PIC S9(06) COMP VALUE ZERO.
           05  WS-RES-CLAIMS-PARSED   PIC S9(06) COMP VALUE ZERO.
           05  WS-RES-CLAIMS-STAGED   PIC S9(06) COMP VALUE ZERO.
           05  WS-RES-CLAIMS-ERRORS   PIC S9(06) COMP VALUE ZERO.
           05  WS-RES-TOTAL-CHARGES   PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-RES-BYTES-PROCESSED PIC S9(08) COMP VALUE ZERO.
           05  WS-RES-ELAPSED-TIME    PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * DB2 HOST VARIABLES FOR CLAIM STAGING INSERT                     *
      *----------------------------------------------------------------*
       01  WS-DB2-STAGING.
           05  HV-STG-BATCH-ID        PIC X(15).
           05  HV-STG-CLAIM-SEQ       PIC S9(06) COMP VALUE ZERO.
           05  HV-STG-CLAIM-ID        PIC X(15).
           05  HV-STG-CLAIM-TYPE      PIC X(02).
           05  HV-STG-MEMBER-ID       PIC X(12).
           05  HV-STG-SUBSCRIBER-ID   PIC X(12).
           05  HV-STG-LAST-NAME       PIC X(35).
           05  HV-STG-FIRST-NAME      PIC X(25).
           05  HV-STG-DOB             PIC X(08).
           05  HV-STG-GENDER          PIC X(01).
           05  HV-STG-BILLING-NPI     PIC X(10).
           05  HV-STG-RENDERING-NPI   PIC X(10).
           05  HV-STG-TOTAL-CHARGES   PIC S9(09)V99 COMP-3.
           05  HV-STG-PLACE-SVC       PIC X(02).
           05  HV-STG-DIAG-COUNT      PIC S9(02) COMP.
           05  HV-STG-PRIMARY-DIAG    PIC X(08).
           05  HV-STG-LINE-COUNT      PIC S9(02) COMP.
           05  HV-STG-STATUS          PIC X(02) VALUE '00'.
           05  HV-STG-ISA-CONTROL     PIC X(09).
           05  HV-STG-GS-CONTROL      PIC X(09).
           05  HV-STG-ST-CONTROL      PIC X(09).

      *----------------------------------------------------------------*
      * CHANNEL AND CONTAINER NAMES                                     *
      *----------------------------------------------------------------*
       01  WS-CHANNEL-NAME            PIC X(16)
               VALUE 'EDI-BATCH-CHAN  '.
       01  WS-CTR-PAYLOAD             PIC X(16)
               VALUE 'EDI-PAYLOAD     '.
       01  WS-CTR-CONTROL             PIC X(16)
               VALUE 'BATCH-CONTROL   '.
       01  WS-CTR-RESULTS             PIC X(16)
               VALUE 'BATCH-RESULTS   '.

      *----------------------------------------------------------------*
      * TIMING VARIABLES                                                *
      *----------------------------------------------------------------*
       01  WS-TIMING.
           05  WS-START-ABSTIME       PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-END-ABSTIME         PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-ELAPSED-MS          PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * WORK VARIABLES                                                  *
      *----------------------------------------------------------------*
       01  WS-WORK-AREAS.
           05  WS-CHAR-IDX            PIC S9(08) COMP VALUE ZERO.
           05  WS-SCAN-POS            PIC S9(08) COMP VALUE ZERO.
           05  WS-SEG-START           PIC S9(08) COMP VALUE ZERO.
           05  WS-SEG-END             PIC S9(08) COMP VALUE ZERO.
           05  WS-CONTAINER-LEN       PIC S9(08) COMP VALUE ZERO.
           05  WS-COMMIT-INTERVAL     PIC S9(04) COMP VALUE 100.
           05  WS-COMMIT-COUNTER      PIC S9(06) COMP VALUE ZERO.
           05  WS-TIMESTAMP           PIC X(26)  VALUE SPACES.
           05  WS-LOG-MESSAGE         PIC X(120) VALUE SPACES.

      *================================================================*
      * LINKAGE SECTION - MAPPED OVER CONTAINER MEMORY VIA POINTER     *
      * THIS IS THE KEY PATTERN: THE LINKAGE SECTION STRUCTURES ARE    *
      * NOT PASSED VIA CALL - THEY ARE MAPPED OVER RAW MEMORY USING   *
      * SET ADDRESS OF                                                  *
      *================================================================*
       LINKAGE SECTION.

      *--- RAW EDI PAYLOAD AREA - MAPPED VIA POINTER ---
      *--- THIS CAN BE UP TO 5MB OF RAW X12 EDI DATA ---
       01  LS-EDI-PAYLOAD.
           05  LS-EDI-BYTE            PIC X(01)
               OCCURS 1 TO 5242880 TIMES
               DEPENDING ON WS-PAYLOAD-LENGTH.

      *--- OVERLAY FOR READING SEGMENTS FROM THE RAW PAYLOAD ---
       01  LS-SEGMENT-OVERLAY.
           05  LS-SEG-DATA            PIC X(512).

      *--- OVERLAY FOR ISA FIXED-LENGTH HEADER (106 BYTES) ---
       01  LS-ISA-OVERLAY.
           05  LS-ISA-SEGMENT-ID      PIC X(03).
           05  LS-ISA-SEP-1           PIC X(01).
           05  LS-ISA-AUTH-QUAL       PIC X(02).
           05  LS-ISA-SEP-2           PIC X(01).
           05  LS-ISA-AUTH-INFO       PIC X(10).
           05  LS-ISA-SEP-3           PIC X(01).
           05  LS-ISA-SEC-QUAL        PIC X(02).
           05  LS-ISA-SEP-4           PIC X(01).
           05  LS-ISA-SEC-INFO        PIC X(10).
           05  LS-ISA-SEP-5           PIC X(01).
           05  LS-ISA-SNDR-QUAL      PIC X(02).
           05  LS-ISA-SEP-6           PIC X(01).
           05  LS-ISA-SENDER-ID       PIC X(15).
           05  LS-ISA-SEP-7           PIC X(01).
           05  LS-ISA-RCVR-QUAL      PIC X(02).
           05  LS-ISA-SEP-8           PIC X(01).
           05  LS-ISA-RECEIVER-ID     PIC X(15).
           05  LS-ISA-SEP-9           PIC X(01).
           05  LS-ISA-DATE            PIC X(06).
           05  LS-ISA-SEP-10          PIC X(01).
           05  LS-ISA-TIME            PIC X(04).
           05  LS-ISA-SEP-11          PIC X(01).
           05  LS-ISA-REP-SEP         PIC X(01).
           05  LS-ISA-SEP-12          PIC X(01).
           05  LS-ISA-VERSION         PIC X(05).
           05  LS-ISA-SEP-13          PIC X(01).
           05  LS-ISA-USAGE-IND      PIC X(01).
           05  LS-ISA-SEP-14          PIC X(01).
           05  LS-ISA-COMP-SEP       PIC X(01).
           05  LS-ISA-SEP-15          PIC X(01).
           05  LS-ISA-CONTROL-NUM     PIC X(09).
           05  LS-ISA-SEP-16          PIC X(01).
           05  LS-ISA-ACK-REQ        PIC X(01).
           05  LS-ISA-SEP-17          PIC X(01).
           05  LS-ISA-TEST-IND       PIC X(01).
           05  LS-ISA-SUB-SEP        PIC X(01).
           05  LS-ISA-SEG-TERM       PIC X(01).

      *================================================================*
       PROCEDURE DIVISION.

      *================================================================*
      * 0000-MAIN-CONTROL                                               *
      *   ORCHESTRATES THE EDI BATCH PROCESSING FLOW                   *
      *================================================================*
       0000-MAIN-CONTROL.

           EXEC CICS HANDLE ABEND
               LABEL   (9500-ABEND-HANDLER)
           END-EXEC

           PERFORM 1000-INITIALIZE
           PERFORM 2000-GET-BATCH-CONTROL
           IF CONTAINER-OK
               PERFORM 3000-GET-EDI-PAYLOAD
               IF CONTAINER-OK
                   PERFORM 4000-PROCESS-EDI-PAYLOAD
                   PERFORM 5000-FINALIZE-BATCH
               END-IF
           END-IF
           PERFORM 6000-PUT-RESULTS-CONTAINER
           PERFORM 7000-RETURN-TO-CALLER
           .

       0000-EXIT.
           EXIT.

      *================================================================*
      * 1000-INITIALIZE                                                 *
      *   INITIALIZE WORKING STORAGE AND GET START TIMESTAMP            *
      *================================================================*
       1000-INITIALIZE.

           INITIALIZE WS-BATCH-RESULTS
           INITIALIZE WS-CLAIM-BUFFER
           INITIALIZE WS-EDI-ENVELOPE
           MOVE 'N' TO WS-END-OF-PAYLOAD-SW
           MOVE 'N' TO WS-IN-TRANSACTION-SW
           MOVE 'N' TO WS-IN-FUNC-GROUP-SW
           MOVE 'N' TO WS-IN-CLAIM-SW
           MOVE 'N' TO WS-CONTAINER-OK-SW
           MOVE ZERO TO WS-BYTES-PROCESSED
           MOVE ZERO TO WS-COMMIT-COUNTER

      *--- CAPTURE START TIME ---
           EXEC CICS ASKTIME
               ABSTIME(WS-START-ABSTIME)
           END-EXEC
           .

       1000-EXIT.
           EXIT.

      *================================================================*
      * 2000-GET-BATCH-CONTROL                                          *
      *   RETRIEVE BATCH CONTROL PARAMETERS FROM CONTAINER              *
      *================================================================*
       2000-GET-BATCH-CONTROL.

           MOVE LENGTH OF WS-BATCH-CONTROL TO WS-CONTAINER-LEN

           EXEC CICS GET CONTAINER(WS-CTR-CONTROL)
               CHANNEL (WS-CHANNEL-NAME)
               INTO    (WS-BATCH-CONTROL)
               FLENGTH (WS-CONTAINER-LEN)
               RESP    (WS-CICS-RESP)
               RESP2   (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP = DFHRESP(NORMAL)
               MOVE 'Y' TO WS-CONTAINER-OK-SW
               MOVE WS-BATCH-ID TO WS-RES-BATCH-ID
           ELSE
               MOVE 'N' TO WS-CONTAINER-OK-SW
               MOVE '90' TO WS-RES-RETURN-CODE
               MOVE 'FAILED TO GET BATCH CONTROL CONTAINER'
                   TO WS-RES-MESSAGE
           END-IF
           .

       2000-EXIT.
           EXIT.

      *================================================================*
      * 3000-GET-EDI-PAYLOAD                                            *
      *   RETRIEVE LARGE EDI PAYLOAD FROM CONTAINER AND MAP TO          *
      *   LINKAGE SECTION USING SET ADDRESS OF                          *
      *                                                                 *
      *   THIS IS THE KEY MEMORY MANAGEMENT PATTERN:                    *
      *   - GET CONTAINER RETURNS A POINTER TO THE DATA                *
      *   - SET ADDRESS OF MAPS OUR LINKAGE SECTION OVER IT            *
      *   - NO DATA COPY IS NEEDED - WE READ IN-PLACE                 *
      *================================================================*
       3000-GET-EDI-PAYLOAD.

      *--- FIRST, GET THE PAYLOAD LENGTH ---
           EXEC CICS GET CONTAINER(WS-CTR-PAYLOAD)
               CHANNEL   (WS-CHANNEL-NAME)
               NODATA
               FLENGTH   (WS-PAYLOAD-LENGTH)
               RESP      (WS-CICS-RESP)
               RESP2     (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'N' TO WS-CONTAINER-OK-SW
               MOVE '90' TO WS-RES-RETURN-CODE
               MOVE 'FAILED TO GET PAYLOAD LENGTH'
                   TO WS-RES-MESSAGE
               GO TO 3000-EXIT
           END-IF

      *--- VALIDATE PAYLOAD SIZE ---
           IF WS-PAYLOAD-LENGTH <= ZERO
               MOVE 'N' TO WS-CONTAINER-OK-SW
               MOVE '90' TO WS-RES-RETURN-CODE
               MOVE 'EMPTY EDI PAYLOAD RECEIVED'
                   TO WS-RES-MESSAGE
               GO TO 3000-EXIT
           END-IF

           IF WS-PAYLOAD-LENGTH > WS-MAX-PAYLOAD-SIZE
               MOVE 'N' TO WS-CONTAINER-OK-SW
               MOVE '90' TO WS-RES-RETURN-CODE
               MOVE 'EDI PAYLOAD EXCEEDS 5MB MAXIMUM'
                   TO WS-RES-MESSAGE
               GO TO 3000-EXIT
           END-IF

      *--- GET THE CONTAINER DATA WITH SET ---
      *--- THIS RETURNS A POINTER DIRECTLY INTO CONTAINER MEMORY ---
           EXEC CICS GET CONTAINER(WS-CTR-PAYLOAD)
               CHANNEL   (WS-CHANNEL-NAME)
               SET       (WS-EDI-PAYLOAD-PTR)
               FLENGTH   (WS-PAYLOAD-LENGTH)
               RESP      (WS-CICS-RESP)
               RESP2     (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP = DFHRESP(NORMAL)
      *---     MAP LINKAGE SECTION OVER THE RAW MEMORY ---
               SET ADDRESS OF LS-EDI-PAYLOAD
                   TO WS-EDI-PAYLOAD-PTR
               MOVE 'Y' TO WS-CONTAINER-OK-SW

      *---     LOG PAYLOAD RECEIPT ---
               STRING 'RECEIVED EDI PAYLOAD: '
                      DELIMITED SIZE
                      ' BYTES FOR BATCH '
                      DELIMITED SIZE
                      WS-BATCH-ID DELIMITED SPACES
                   INTO WS-LOG-MESSAGE
               PERFORM 9100-LOG-INFO
           ELSE
               MOVE 'N' TO WS-CONTAINER-OK-SW
               MOVE '90' TO WS-RES-RETURN-CODE
               MOVE 'FAILED TO MAP EDI PAYLOAD MEMORY'
                   TO WS-RES-MESSAGE
           END-IF
           .

       3000-EXIT.
           EXIT.

      *================================================================*
      * 4000-PROCESS-EDI-PAYLOAD                                        *
      *   SCAN THROUGH THE RAW EDI PAYLOAD USING POINTER ARITHMETIC    *
      *   IDENTIFY SEGMENT BOUNDARIES AND PROCESS EACH SEGMENT         *
      *================================================================*
       4000-PROCESS-EDI-PAYLOAD.

           MOVE ZERO TO WS-PAYLOAD-OFFSET
           MOVE 'N' TO WS-END-OF-PAYLOAD-SW
           MOVE 'Y' TO WS-PARSE-OK-SW

      *--- DETECT DELIMITERS FROM ISA SEGMENT ---
           PERFORM 4050-DETECT-DELIMITERS

      *--- MAIN SEGMENT PROCESSING LOOP ---
           PERFORM 4100-GET-NEXT-SEGMENT
           PERFORM UNTIL END-OF-PAYLOAD
               OR PARSE-FAILED
               PERFORM 4200-PROCESS-SEGMENT
               PERFORM 4100-GET-NEXT-SEGMENT
           END-PERFORM

      *--- COMMIT ANY REMAINING STAGED CLAIMS ---
           IF WS-COMMIT-COUNTER > ZERO
               EXEC SQL COMMIT END-EXEC
           END-IF

           MOVE WS-BYTES-PROCESSED TO WS-RES-BYTES-PROCESSED
           .

       4000-EXIT.
           EXIT.

      *================================================================*
      * 4050-DETECT-DELIMITERS                                          *
      *   READ THE ISA SEGMENT TO DETERMINE ELEMENT AND SEGMENT        *
      *   DELIMITERS (THEY CAN VARY BY TRADING PARTNER)                *
      *================================================================*
       4050-DETECT-DELIMITERS.

      *--- MAP ISA OVERLAY ONTO THE START OF PAYLOAD ---
           SET ADDRESS OF LS-ISA-OVERLAY
               TO WS-EDI-PAYLOAD-PTR

      *--- VERIFY THIS IS AN ISA SEGMENT ---
           IF LS-ISA-SEGMENT-ID = 'ISA'
      *---     ELEMENT SEPARATOR IS THE 4TH BYTE (AFTER "ISA") ---
               MOVE LS-ISA-SEP-1 TO WS-ISA-ELEMENT-SEP
      *---     SEGMENT TERMINATOR IS THE LAST BYTE OF ISA (106) ---
               MOVE LS-ISA-SEG-TERM TO WS-ISA-SEGMENT-TERM
      *---     SUB-ELEMENT SEPARATOR ---
               MOVE LS-ISA-SUB-SEP TO WS-ISA-SUB-SEP
           ELSE
               MOVE 'N' TO WS-PARSE-OK-SW
               MOVE '90' TO WS-RES-RETURN-CODE
               MOVE 'PAYLOAD DOES NOT START WITH ISA SEGMENT'
                   TO WS-RES-MESSAGE
           END-IF
           .

       4050-EXIT.
           EXIT.

      *================================================================*
      * 4100-GET-NEXT-SEGMENT                                           *
      *   EXTRACT THE NEXT SEGMENT FROM THE RAW PAYLOAD                *
      *   USES POINTER ARITHMETIC TO ADVANCE THROUGH MEMORY            *
      *================================================================*
       4100-GET-NEXT-SEGMENT.

           IF WS-PAYLOAD-OFFSET >= WS-PAYLOAD-LENGTH
               MOVE 'Y' TO WS-END-OF-PAYLOAD-SW
               GO TO 4100-EXIT
           END-IF

           INITIALIZE WS-CURRENT-SEGMENT
           MOVE ZERO TO WS-SEGMENT-LENGTH

      *--- CALCULATE POINTER TO CURRENT POSITION ---
           SET WS-CURRENT-SEG-PTR TO WS-EDI-PAYLOAD-PTR
           SET WS-CURRENT-SEG-PTR UP BY WS-PAYLOAD-OFFSET

      *--- MAP SEGMENT OVERLAY ONTO CURRENT POSITION ---
           SET ADDRESS OF LS-SEGMENT-OVERLAY
               TO WS-CURRENT-SEG-PTR

      *--- SCAN FOR SEGMENT TERMINATOR ---
           COMPUTE WS-REMAINING-LENGTH =
               WS-PAYLOAD-LENGTH - WS-PAYLOAD-OFFSET

           MOVE ZERO TO WS-SEGMENT-LENGTH
           PERFORM VARYING WS-CHAR-IDX FROM 1 BY 1
               UNTIL WS-CHAR-IDX > WS-REMAINING-LENGTH
               OR WS-CHAR-IDX > 512
               IF LS-SEG-DATA(WS-CHAR-IDX:1) =
                  WS-ISA-SEGMENT-TERM
                   MOVE WS-CHAR-IDX TO WS-SEGMENT-LENGTH
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-SEGMENT-LENGTH > 0
      *---     COPY SEGMENT DATA TO WORKING STORAGE ---
               MOVE LS-SEG-DATA(1:WS-SEGMENT-LENGTH)
                   TO WS-CURRENT-SEGMENT
      *---     EXTRACT SEGMENT ID (FIRST 2-3 CHARS BEFORE SEP) ---
               MOVE SPACES TO WS-SEGMENT-ID
               UNSTRING WS-CURRENT-SEGMENT
                   DELIMITED BY WS-ISA-ELEMENT-SEP
                   INTO WS-SEGMENT-ID
                   WITH POINTER WS-UNSTRING-PTR
      *---     ADVANCE OFFSET PAST THIS SEGMENT ---
               ADD WS-SEGMENT-LENGTH TO WS-PAYLOAD-OFFSET
               ADD WS-SEGMENT-LENGTH TO WS-BYTES-PROCESSED
      *---     SKIP ANY TRAILING LINE FEEDS OR CARRIAGE RETURNS ---
               PERFORM UNTIL WS-PAYLOAD-OFFSET >=
                   WS-PAYLOAD-LENGTH
                   SET WS-CURRENT-SEG-PTR
                       TO WS-EDI-PAYLOAD-PTR
                   SET WS-CURRENT-SEG-PTR
                       UP BY WS-PAYLOAD-OFFSET
                   SET ADDRESS OF LS-SEGMENT-OVERLAY
                       TO WS-CURRENT-SEG-PTR
                   IF LS-SEG-DATA(1:1) = X'0D'
                       OR LS-SEG-DATA(1:1) = X'0A'
                       ADD 1 TO WS-PAYLOAD-OFFSET
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           ELSE
      *---     NO TERMINATOR FOUND - END OF PAYLOAD ---
               MOVE 'Y' TO WS-END-OF-PAYLOAD-SW
           END-IF
           .

       4100-EXIT.
           EXIT.

      *================================================================*
      * 4200-PROCESS-SEGMENT                                            *
      *   ROUTE SEGMENT PROCESSING BASED ON SEGMENT ID                 *
      *================================================================*
       4200-PROCESS-SEGMENT.

           EVALUATE WS-SEGMENT-ID
               WHEN 'ISA'
                   PERFORM 4210-PROCESS-ISA
               WHEN 'GS '
                   PERFORM 4220-PROCESS-GS
               WHEN 'ST '
                   PERFORM 4230-PROCESS-ST
               WHEN 'CLM'
                   PERFORM 4240-PROCESS-CLM
               WHEN 'NM1'
                   PERFORM 4250-PROCESS-NM1
               WHEN 'SBR'
                   PERFORM 4260-PROCESS-SBR
               WHEN 'HI '
                   PERFORM 4270-PROCESS-HI
               WHEN 'SV1'
                   PERFORM 4280-PROCESS-SV1
               WHEN 'SV2'
                   PERFORM 4285-PROCESS-SV2
               WHEN 'DTP'
                   PERFORM 4290-PROCESS-DTP
               WHEN 'SE '
                   PERFORM 4300-PROCESS-SE
               WHEN 'GE '
                   PERFORM 4310-PROCESS-GE
               WHEN 'IEA'
                   PERFORM 4320-PROCESS-IEA
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .

       4200-EXIT.
           EXIT.

      *================================================================*
      * 4210-PROCESS-ISA                                                *
      *   PROCESS INTERCHANGE CONTROL HEADER                            *
      *================================================================*
       4210-PROCESS-ISA.

           ADD 1 TO WS-RES-ISA-COUNT
           MOVE 'Y' TO WS-IN-TRANSACTION-SW

      *--- MAP ISA OVERLAY ---
           SET ADDRESS OF LS-ISA-OVERLAY
               TO WS-CURRENT-SEG-PTR

           MOVE LS-ISA-SENDER-ID   TO WS-ISA-SENDER-ID
           MOVE LS-ISA-RECEIVER-ID TO WS-ISA-RECEIVER-ID
           MOVE LS-ISA-DATE        TO WS-ISA-DATE
           MOVE LS-ISA-TIME        TO WS-ISA-TIME
           MOVE LS-ISA-CONTROL-NUM TO WS-ISA-CONTROL-NUM
           MOVE LS-ISA-VERSION     TO WS-ISA-VERSION
           .

       4210-EXIT.
           EXIT.

      *================================================================*
      * 4220-PROCESS-GS                                                 *
      *   PROCESS FUNCTIONAL GROUP HEADER                                *
      *================================================================*
       4220-PROCESS-GS.

           ADD 1 TO WS-RES-GS-COUNT
           MOVE 'Y' TO WS-IN-FUNC-GROUP-SW

      *--- PARSE GS ELEMENTS ---
           MOVE 1 TO WS-UNSTRING-PTR
           MOVE ZERO TO WS-ELEMENT-COUNT
           PERFORM 4205-PARSE-ELEMENTS

           IF WS-ELEMENT-COUNT >= 8
               MOVE WS-ELEMENT(1) TO WS-GS-FUNC-CODE
               MOVE WS-ELEMENT(2) TO WS-GS-SENDER-CODE
               MOVE WS-ELEMENT(3) TO WS-GS-RECEIVER-CODE
               MOVE WS-ELEMENT(4) TO WS-GS-DATE
               MOVE WS-ELEMENT(6) TO WS-GS-CONTROL-NUM
               MOVE WS-ELEMENT(8) TO WS-GS-VERSION
           END-IF
           .

       4220-EXIT.
           EXIT.

      *================================================================*
      * 4205-PARSE-ELEMENTS                                             *
      *   COMMON ROUTINE TO PARSE SEGMENT ELEMENTS                     *
      *================================================================*
       4205-PARSE-ELEMENTS.

           INITIALIZE WS-ELEMENTS
           MOVE 1 TO WS-UNSTRING-PTR
           MOVE ZERO TO WS-ELEMENT-COUNT

      *--- SKIP SEGMENT ID ---
           UNSTRING WS-CURRENT-SEGMENT
               DELIMITED BY WS-ISA-ELEMENT-SEP
                         OR WS-ISA-SEGMENT-TERM
               INTO WS-ELEMENT(1)
               WITH POINTER WS-UNSTRING-PTR

      *--- PARSE REMAINING ELEMENTS ---
           PERFORM VARYING WS-ELEMENT-IDX FROM 1 BY 1
               UNTIL WS-ELEMENT-IDX > 30
               OR WS-UNSTRING-PTR > WS-SEGMENT-LENGTH
               UNSTRING WS-CURRENT-SEGMENT
                   DELIMITED BY WS-ISA-ELEMENT-SEP
                             OR WS-ISA-SEGMENT-TERM
                   INTO WS-ELEMENT(WS-ELEMENT-IDX)
                   WITH POINTER WS-UNSTRING-PTR
               ADD 1 TO WS-ELEMENT-COUNT
           END-PERFORM
           .

       4205-EXIT.
           EXIT.

      *================================================================*
      * 4230-PROCESS-ST                                                 *
      *   PROCESS TRANSACTION SET HEADER (START OF 837)                *
      *================================================================*
       4230-PROCESS-ST.

           ADD 1 TO WS-RES-ST-COUNT

           PERFORM 4205-PARSE-ELEMENTS

           IF WS-ELEMENT-COUNT >= 2
               MOVE WS-ELEMENT(1) TO WS-ST-TRANS-CODE
               MOVE WS-ELEMENT(2) TO WS-ST-CONTROL-NUM
           END-IF

      *--- VERIFY THIS IS AN 837 TRANSACTION ---
           IF WS-ST-TRANS-CODE = '837'
               CONTINUE
           ELSE
               STRING 'UNEXPECTED TRANSACTION TYPE: '
                      DELIMITED SIZE
                      WS-ST-TRANS-CODE DELIMITED SIZE
                   INTO WS-LOG-MESSAGE
               PERFORM 9100-LOG-INFO
           END-IF
           .

       4230-EXIT.
           EXIT.

      *================================================================*
      * 4240-PROCESS-CLM                                                *
      *   PROCESS CLAIM SEGMENT - START OF NEW CLAIM                    *
      *================================================================*
       4240-PROCESS-CLM.

      *--- IF WE WERE BUILDING A PREVIOUS CLAIM, STAGE IT ---
           IF IN-CLAIM
               PERFORM 4500-STAGE-COMPLETED-CLAIM
           END-IF

      *--- INITIALIZE NEW CLAIM BUFFER ---
           INITIALIZE WS-CLAIM-BUFFER
           MOVE 'Y' TO WS-IN-CLAIM-SW
           ADD 1 TO WS-RES-CLAIMS-PARSED

           PERFORM 4205-PARSE-ELEMENTS

           IF WS-ELEMENT-COUNT >= 5
               MOVE WS-ELEMENT(1) TO WS-CLM-CLAIM-ID
               COMPUTE WS-CLM-TOTAL-CHARGES =
                   FUNCTION NUMVAL(WS-ELEMENT(2))
      *---     ELEMENT 5 CONTAINS PLACE:FREQUENCY ---
               UNSTRING WS-ELEMENT(5)
                   DELIMITED BY WS-ISA-SUB-SEP
                   INTO WS-CLM-PLACE-SVC
                        WS-CLM-FREQUENCY
           END-IF

      *--- DETERMINE CLAIM TYPE FROM BILLING CONTEXT ---
      *--- (WILL BE REFINED WHEN SV1/SV2 SEGMENTS ARRIVE) ---
           MOVE 'PR' TO WS-CLM-CLAIM-TYPE
           .

       4240-EXIT.
           EXIT.

      *================================================================*
      * 4250-PROCESS-NM1                                                *
      *   PROCESS NAME SEGMENT - IDENTIFIES VARIOUS ENTITIES            *
      *================================================================*
       4250-PROCESS-NM1.

           PERFORM 4205-PARSE-ELEMENTS

           IF WS-ELEMENT-COUNT >= 3
               EVALUATE WS-ELEMENT(1)
                   WHEN '85'
      *---             BILLING PROVIDER
                       IF WS-ELEMENT-COUNT >= 9
                           MOVE WS-ELEMENT(9)
                               TO WS-CLM-BILLING-NPI
                       END-IF
                   WHEN '82'
      *---             RENDERING PROVIDER
                       IF WS-ELEMENT-COUNT >= 9
                           MOVE WS-ELEMENT(9)
                               TO WS-CLM-RENDERING-NPI
                       END-IF
                   WHEN 'IL'
      *---             SUBSCRIBER
                       MOVE WS-ELEMENT(3)
                           TO WS-CLM-LAST-NAME
                       IF WS-ELEMENT-COUNT >= 4
                           MOVE WS-ELEMENT(4)
                               TO WS-CLM-FIRST-NAME
                       END-IF
                       IF WS-ELEMENT-COUNT >= 9
                           MOVE WS-ELEMENT(9)
                               TO WS-CLM-SUBSCRIBER-ID
                       END-IF
                   WHEN 'QC'
      *---             PATIENT
                       MOVE WS-ELEMENT(3)
                           TO WS-CLM-LAST-NAME
                       IF WS-ELEMENT-COUNT >= 4
                           MOVE WS-ELEMENT(4)
                               TO WS-CLM-FIRST-NAME
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
           .

       4250-EXIT.
           EXIT.

      *================================================================*
      * 4260-PROCESS-SBR                                                *
      *   PROCESS SUBSCRIBER INFORMATION SEGMENT                        *
      *================================================================*
       4260-PROCESS-SBR.

           PERFORM 4205-PARSE-ELEMENTS

      *--- SBR ELEMENT 9 IS CLAIM FILING INDICATOR ---
           IF WS-ELEMENT-COUNT >= 9
               EVALUATE WS-ELEMENT(9)
                   WHEN 'BL' MOVE 'IN' TO WS-CLM-CLAIM-TYPE
                   WHEN 'MB' MOVE 'PR' TO WS-CLM-CLAIM-TYPE
                   WHEN 'MC' MOVE 'PR' TO WS-CLM-CLAIM-TYPE
                   WHEN OTHER CONTINUE
               END-EVALUATE
           END-IF
           .

       4260-EXIT.
           EXIT.

      *================================================================*
      * 4270-PROCESS-HI                                                 *
      *   PROCESS HEALTH CARE INFORMATION (DIAGNOSIS CODES)             *
      *================================================================*
       4270-PROCESS-HI.

           PERFORM 4205-PARSE-ELEMENTS

           PERFORM VARYING WS-ELEMENT-IDX FROM 1 BY 1
               UNTIL WS-ELEMENT-IDX > WS-ELEMENT-COUNT
               OR WS-CLM-DIAG-COUNT >= 12
               IF WS-ELEMENT(WS-ELEMENT-IDX) NOT = SPACES
                   ADD 1 TO WS-CLM-DIAG-COUNT
      *---         EXTRACT CODE FROM QUALIFIER:CODE FORMAT ---
                   UNSTRING WS-ELEMENT(WS-ELEMENT-IDX)
                       DELIMITED BY WS-ISA-SUB-SEP
                       INTO WS-SEGMENT-ID
                            WS-CLM-DIAG(WS-CLM-DIAG-COUNT)
               END-IF
           END-PERFORM
           .

       4270-EXIT.
           EXIT.

      *================================================================*
      * 4280-PROCESS-SV1                                                *
      *   PROCESS PROFESSIONAL SERVICE LINE                              *
      *================================================================*
       4280-PROCESS-SV1.

           ADD 1 TO WS-CLM-LINE-COUNT
           MOVE WS-CLM-LINE-COUNT
               TO WS-CLM-LN-SEQ(WS-CLM-LINE-COUNT)

           PERFORM 4205-PARSE-ELEMENTS

           IF WS-ELEMENT-COUNT >= 1
      *---     ELEMENT 1 IS COMPOSITE: PROCEDURE:MOD1:MOD2:... ---
               UNSTRING WS-ELEMENT(1)
                   DELIMITED BY WS-ISA-SUB-SEP
                   INTO WS-SEGMENT-ID
                        WS-CLM-LN-PROC(WS-CLM-LINE-COUNT)
                        WS-CLM-LN-MOD1(WS-CLM-LINE-COUNT)
                        WS-CLM-LN-MOD2(WS-CLM-LINE-COUNT)
           END-IF

           IF WS-ELEMENT-COUNT >= 2
               COMPUTE WS-CLM-LN-CHARGES(WS-CLM-LINE-COUNT) =
                   FUNCTION NUMVAL(WS-ELEMENT(2))
           END-IF

           IF WS-ELEMENT-COUNT >= 4
               COMPUTE WS-CLM-LN-UNITS(WS-CLM-LINE-COUNT) =
                   FUNCTION NUMVAL(WS-ELEMENT(4))
           END-IF
           .

       4280-EXIT.
           EXIT.

      *================================================================*
      * 4285-PROCESS-SV2                                                *
      *   PROCESS INSTITUTIONAL SERVICE LINE                             *
      *================================================================*
       4285-PROCESS-SV2.

           MOVE 'IN' TO WS-CLM-CLAIM-TYPE
           ADD 1 TO WS-CLM-LINE-COUNT
           MOVE WS-CLM-LINE-COUNT
               TO WS-CLM-LN-SEQ(WS-CLM-LINE-COUNT)

           PERFORM 4205-PARSE-ELEMENTS

           IF WS-ELEMENT-COUNT >= 1
               MOVE WS-ELEMENT(1)
                   TO WS-CLM-LN-REV-CODE(WS-CLM-LINE-COUNT)
           END-IF

           IF WS-ELEMENT-COUNT >= 2
      *---     ELEMENT 2 IS COMPOSITE: PROCEDURE CODE ---
               UNSTRING WS-ELEMENT(2)
                   DELIMITED BY WS-ISA-SUB-SEP
                   INTO WS-SEGMENT-ID
                        WS-CLM-LN-PROC(WS-CLM-LINE-COUNT)
           END-IF

           IF WS-ELEMENT-COUNT >= 3
               COMPUTE WS-CLM-LN-CHARGES(WS-CLM-LINE-COUNT) =
                   FUNCTION NUMVAL(WS-ELEMENT(3))
           END-IF

           IF WS-ELEMENT-COUNT >= 5
               COMPUTE WS-CLM-LN-UNITS(WS-CLM-LINE-COUNT) =
                   FUNCTION NUMVAL(WS-ELEMENT(5))
           END-IF
           .

       4285-EXIT.
           EXIT.

      *================================================================*
      * 4290-PROCESS-DTP                                                *
      *   PROCESS DATE/TIME PERIOD SEGMENT                               *
      *================================================================*
       4290-PROCESS-DTP.

           PERFORM 4205-PARSE-ELEMENTS

           IF WS-ELEMENT-COUNT >= 3
               EVALUATE WS-ELEMENT(1)
                   WHEN '472'
      *---             SERVICE DATE ---
                       IF WS-CLM-LINE-COUNT > ZERO
                           MOVE WS-ELEMENT(3)
                               TO WS-CLM-LN-FROM-DT
                                  (WS-CLM-LINE-COUNT)
                       END-IF
                   WHEN '435'
      *---             ADMIT DATE ---
                       MOVE WS-ELEMENT(3)
                           TO WS-CLM-ADMIT-DATE
                   WHEN '096'
      *---             DISCHARGE DATE ---
                       MOVE WS-ELEMENT(3)
                           TO WS-CLM-DISCHARGE-DATE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
           .

       4290-EXIT.
           EXIT.

      *================================================================*
      * 4300-PROCESS-SE                                                 *
      *   PROCESS TRANSACTION SET TRAILER - END OF 837                 *
      *================================================================*
       4300-PROCESS-SE.

      *--- STAGE THE FINAL CLAIM IN THIS TRANSACTION SET ---
           IF IN-CLAIM
               PERFORM 4500-STAGE-COMPLETED-CLAIM
               MOVE 'N' TO WS-IN-CLAIM-SW
           END-IF
           .

       4300-EXIT.
           EXIT.

      *================================================================*
      * 4310-PROCESS-GE                                                 *
      *   PROCESS FUNCTIONAL GROUP TRAILER                               *
      *================================================================*
       4310-PROCESS-GE.

           MOVE 'N' TO WS-IN-FUNC-GROUP-SW
           .

       4310-EXIT.
           EXIT.

      *================================================================*
      * 4320-PROCESS-IEA                                                *
      *   PROCESS INTERCHANGE CONTROL TRAILER                            *
      *================================================================*
       4320-PROCESS-IEA.

           MOVE 'N' TO WS-IN-TRANSACTION-SW
           .

       4320-EXIT.
           EXIT.

      *================================================================*
      * 4500-STAGE-COMPLETED-CLAIM                                      *
      *   INSERT PARSED CLAIM INTO DB2 STAGING TABLE                    *
      *================================================================*
       4500-STAGE-COMPLETED-CLAIM.

      *--- VALIDATE MINIMUM REQUIRED FIELDS ---
           IF WS-CLM-CLAIM-ID = SPACES
               ADD 1 TO WS-RES-CLAIMS-ERRORS
               GO TO 4500-EXIT
           END-IF

           IF WS-CLM-BILLING-NPI = SPACES
               ADD 1 TO WS-RES-CLAIMS-ERRORS
               GO TO 4500-EXIT
           END-IF

      *--- POPULATE DB2 HOST VARIABLES ---
           MOVE WS-BATCH-ID          TO HV-STG-BATCH-ID
           ADD 1 TO HV-STG-CLAIM-SEQ
           MOVE WS-CLM-CLAIM-ID      TO HV-STG-CLAIM-ID
           MOVE WS-CLM-CLAIM-TYPE    TO HV-STG-CLAIM-TYPE
           MOVE WS-CLM-MEMBER-ID     TO HV-STG-MEMBER-ID
           MOVE WS-CLM-SUBSCRIBER-ID TO HV-STG-SUBSCRIBER-ID
           MOVE WS-CLM-LAST-NAME     TO HV-STG-LAST-NAME
           MOVE WS-CLM-FIRST-NAME    TO HV-STG-FIRST-NAME
           MOVE WS-CLM-DOB           TO HV-STG-DOB
           MOVE WS-CLM-GENDER        TO HV-STG-GENDER
           MOVE WS-CLM-BILLING-NPI   TO HV-STG-BILLING-NPI
           MOVE WS-CLM-RENDERING-NPI TO HV-STG-RENDERING-NPI
           MOVE WS-CLM-TOTAL-CHARGES TO HV-STG-TOTAL-CHARGES
           MOVE WS-CLM-PLACE-SVC     TO HV-STG-PLACE-SVC
           MOVE WS-CLM-DIAG-COUNT    TO HV-STG-DIAG-COUNT
           IF WS-CLM-DIAG-COUNT > ZERO
               MOVE WS-CLM-DIAG(1)   TO HV-STG-PRIMARY-DIAG
           ELSE
               MOVE SPACES            TO HV-STG-PRIMARY-DIAG
           END-IF
           MOVE WS-CLM-LINE-COUNT    TO HV-STG-LINE-COUNT
           MOVE '00'                  TO HV-STG-STATUS
           MOVE WS-ISA-CONTROL-NUM   TO HV-STG-ISA-CONTROL
           MOVE WS-GS-CONTROL-NUM    TO HV-STG-GS-CONTROL
           MOVE WS-ST-CONTROL-NUM    TO HV-STG-ST-CONTROL

      *--- INSERT INTO STAGING TABLE ---
           EXEC SQL
               INSERT INTO HCAS.CLAIM_STAGING
                   (BATCH_ID, CLAIM_SEQ, CLAIM_ID,
                    CLAIM_TYPE, MEMBER_ID, SUBSCRIBER_ID,
                    LAST_NAME, FIRST_NAME, DOB, GENDER,
                    BILLING_NPI, RENDERING_NPI,
                    TOTAL_CHARGES, PLACE_OF_SERVICE,
                    DIAG_COUNT, PRIMARY_DIAGNOSIS,
                    LINE_COUNT, STATUS,
                    ISA_CONTROL, GS_CONTROL, ST_CONTROL,
                    CREATED_TIMESTAMP)
               VALUES
                   (:HV-STG-BATCH-ID, :HV-STG-CLAIM-SEQ,
                    :HV-STG-CLAIM-ID,
                    :HV-STG-CLAIM-TYPE, :HV-STG-MEMBER-ID,
                    :HV-STG-SUBSCRIBER-ID,
                    :HV-STG-LAST-NAME, :HV-STG-FIRST-NAME,
                    :HV-STG-DOB, :HV-STG-GENDER,
                    :HV-STG-BILLING-NPI, :HV-STG-RENDERING-NPI,
                    :HV-STG-TOTAL-CHARGES, :HV-STG-PLACE-SVC,
                    :HV-STG-DIAG-COUNT, :HV-STG-PRIMARY-DIAG,
                    :HV-STG-LINE-COUNT, :HV-STG-STATUS,
                    :HV-STG-ISA-CONTROL, :HV-STG-GS-CONTROL,
                    :HV-STG-ST-CONTROL,
                    CURRENT TIMESTAMP)
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-RES-CLAIMS-STAGED
                   ADD WS-CLM-TOTAL-CHARGES
                       TO WS-RES-TOTAL-CHARGES
                   ADD 1 TO WS-COMMIT-COUNTER
      *---         PERIODIC COMMIT TO AVOID LOCK ESCALATION ---
                   IF WS-COMMIT-COUNTER >= WS-COMMIT-INTERVAL
                       EXEC SQL COMMIT END-EXEC
                       MOVE ZERO TO WS-COMMIT-COUNTER
                   END-IF
               WHEN -803
      *---         DUPLICATE KEY - LOG AND CONTINUE ---
                   ADD 1 TO WS-RES-CLAIMS-ERRORS
                   STRING 'DUPLICATE CLAIM: '
                          DELIMITED SIZE
                          WS-CLM-CLAIM-ID DELIMITED SPACES
                       INTO WS-LOG-MESSAGE
                   PERFORM 9100-LOG-INFO
               WHEN OTHER
                   ADD 1 TO WS-RES-CLAIMS-ERRORS
                   STRING 'DB2 INSERT ERROR SQLCODE='
                          DELIMITED SIZE
                       INTO WS-LOG-MESSAGE
                   PERFORM 9100-LOG-INFO
           END-EVALUATE
           .

       4500-EXIT.
           EXIT.

      *================================================================*
      * 5000-FINALIZE-BATCH                                             *
      *   LOG BATCH COMPLETION TO DB2 AUDIT TABLE                       *
      *================================================================*
       5000-FINALIZE-BATCH.

      *--- CALCULATE ELAPSED TIME ---
           EXEC CICS ASKTIME
               ABSTIME(WS-END-ABSTIME)
           END-EXEC

           COMPUTE WS-ELAPSED-MS =
               WS-END-ABSTIME - WS-START-ABSTIME
           MOVE WS-ELAPSED-MS TO WS-RES-ELAPSED-TIME

      *--- INSERT BATCH LOG RECORD ---
           EXEC SQL
               INSERT INTO HCAS.EDI_BATCH_LOG
                   (BATCH_ID, BATCH_SOURCE, RECEIVED_TS,
                    ISA_COUNT, GS_COUNT, ST_COUNT,
                    CLAIMS_PARSED, CLAIMS_STAGED,
                    CLAIMS_ERRORS, TOTAL_CHARGES,
                    BYTES_PROCESSED, ELAPSED_MS,
                    COMPLETED_TIMESTAMP)
               VALUES
                   (:WS-BATCH-ID, :WS-BATCH-SOURCE,
                    :WS-BATCH-RECEIVED-TS,
                    :WS-RES-ISA-COUNT, :WS-RES-GS-COUNT,
                    :WS-RES-ST-COUNT,
                    :WS-RES-CLAIMS-PARSED,
                    :WS-RES-CLAIMS-STAGED,
                    :WS-RES-CLAIMS-ERRORS,
                    :WS-RES-TOTAL-CHARGES,
                    :WS-RES-BYTES-PROCESSED,
                    :WS-ELAPSED-MS,
                    CURRENT TIMESTAMP)
           END-EXEC

           IF SQLCODE = 0
               EXEC SQL COMMIT END-EXEC
           END-IF

      *--- DETERMINE OVERALL RESULT ---
           IF WS-RES-CLAIMS-ERRORS = ZERO
               MOVE '00' TO WS-RES-RETURN-CODE
               MOVE 'BATCH PROCESSED SUCCESSFULLY'
                   TO WS-RES-MESSAGE
           ELSE
               IF WS-RES-CLAIMS-STAGED > ZERO
                   MOVE '01' TO WS-RES-RETURN-CODE
                   MOVE 'BATCH PROCESSED WITH ERRORS'
                       TO WS-RES-MESSAGE
               ELSE
                   MOVE '90' TO WS-RES-RETURN-CODE
                   MOVE 'BATCH PROCESSING FAILED - ALL ERRORS'
                       TO WS-RES-MESSAGE
               END-IF
           END-IF
           .

       5000-EXIT.
           EXIT.

      *================================================================*
      * 6000-PUT-RESULTS-CONTAINER                                      *
      *   STORE BATCH RESULTS IN CICS CHANNEL/CONTAINER                 *
      *================================================================*
       6000-PUT-RESULTS-CONTAINER.

           EXEC CICS PUT CONTAINER(WS-CTR-RESULTS)
               CHANNEL  (WS-CHANNEL-NAME)
               FROM     (WS-BATCH-RESULTS)
               FLENGTH  (LENGTH OF WS-BATCH-RESULTS)
               RESP     (WS-CICS-RESP)
               RESP2    (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9200-LOG-ERROR
           END-IF
           .

       6000-EXIT.
           EXIT.

      *================================================================*
      * 7000-RETURN-TO-CALLER                                           *
      *   RETURN CONTROL WITH CHANNEL TO CALLING PROGRAM                *
      *================================================================*
       7000-RETURN-TO-CALLER.

           STRING WS-PROGRAM-ID DELIMITED SIZE
                  ' COMPLETE: PARSED=' DELIMITED SIZE
                  ' STAGED=' DELIMITED SIZE
                  ' ERRORS=' DELIMITED SIZE
               INTO WS-LOG-MESSAGE
           PERFORM 9100-LOG-INFO

           EXEC CICS RETURN
               CHANNEL (WS-CHANNEL-NAME)
               RESP    (WS-CICS-RESP)
               RESP2   (WS-CICS-RESP2)
           END-EXEC
           .

       7000-EXIT.
           EXIT.

      *================================================================*
      * 9100-LOG-INFO                                                   *
      *   LOG INFORMATIONAL MESSAGE TO TD QUEUE                         *
      *================================================================*
       9100-LOG-INFO.

           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-LOG-MESSAGE)
               LENGTH (120)
               RESP   (WS-CICS-RESP)
           END-EXEC

           INITIALIZE WS-LOG-MESSAGE
           .

       9100-EXIT.
           EXIT.

      *================================================================*
      * 9200-LOG-ERROR                                                  *
      *   LOG ERROR MESSAGE TO TD QUEUE                                 *
      *================================================================*
       9200-LOG-ERROR.

           STRING 'ERROR IN ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ': ' DELIMITED SIZE
                  WS-RES-MESSAGE DELIMITED SIZE
               INTO WS-LOG-MESSAGE

           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-LOG-MESSAGE)
               LENGTH (120)
               RESP   (WS-CICS-RESP)
           END-EXEC

           INITIALIZE WS-LOG-MESSAGE
           .

       9200-EXIT.
           EXIT.

      *================================================================*
      * 9500-ABEND-HANDLER                                              *
      *   HANDLE UNEXPECTED PROGRAM ABENDS                              *
      *================================================================*
       9500-ABEND-HANDLER.

           MOVE '90' TO WS-RES-RETURN-CODE
           STRING 'ABEND IN ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ' DURING EDI BATCH PROCESSING'
                  DELIMITED SIZE
               INTO WS-RES-MESSAGE

      *--- ATTEMPT TO ROLLBACK ANY UNCOMMITTED WORK ---
           EXEC SQL ROLLBACK END-EXEC

      *--- STORE ERROR RESULTS ---
           PERFORM 6000-PUT-RESULTS-CONTAINER

      *--- LOG ERROR ---
           PERFORM 9200-LOG-ERROR

           EXEC CICS RETURN
               CHANNEL (WS-CHANNEL-NAME)
               RESP    (WS-CICS-RESP)
           END-EXEC
           .

       9500-EXIT.
           EXIT.
