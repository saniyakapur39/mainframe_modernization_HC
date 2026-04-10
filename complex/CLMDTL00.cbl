       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CLMDTL00.
       AUTHOR.        HEALTHCARE PAYER SYSTEMS.
       DATE-WRITTEN.  2024-01-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM:     CLMDTL00                                          *
      * DESCRIPTION: CLAIMS DETAIL INQUIRY - CICS ONLINE TRANSACTION   *
      *              THAT SIMULTANEOUSLY USES DB2 CURSORS AND VSAM     *
      *              READS TO PRESENT A UNIFIED CLAIMS HISTORY VIEW.   *
      *                                                                *
      * THIS PROGRAM DEMONSTRATES THE "MONOLITHIC INTERSECTION"        *
      * PATTERN WHERE CICS, DB2, AND VSAM ARE ALL USED IN A SINGLE    *
      * PROGRAM. THE TRANSACTION:                                      *
      *  1. RECEIVES A MEMBER ID VIA CICS COMMAREA                    *
      *  2. OPENS A DB2 CURSOR TO FETCH CLAIM HISTORY ROWS            *
      *  3. FOR EACH CLAIM, USES THE BILLING NPI FROM DB2 AS A       *
      *     RIDFLD TO READ THE VSAM PROVIDER MASTER FILE              *
      *  4. MERGES THE RELATIONAL AND INDEXED DATA INTO A SINGLE      *
      *     RESPONSE STRUCTURE RETURNED TO THE CALLING MAP/PROGRAM    *
      *                                                                *
      * TRANSACTION: CDTL                                              *
      * MAPSET:      CDTLMS                                            *
      * MAP:         CDTLM1 (INQUIRY INPUT), CDTLM2 (DETAIL OUTPUT)  *
      *                                                                *
      * DEPENDENCIES:                                                  *
      *  - COPYBOOKS: HCCOMMON, CLMCOPY, PRVCOPY, ABORTWSC           *
      *  - DB2 TABLES: HCAS.CLAIM_MASTER, HCAS.CLAIM_LINE,           *
      *                HCAS.MEMBER, HCAS.ENROLLMENT                   *
      *  - VSAM FILE: PRVMAST (PROVIDER MASTER KSDS)                  *
      *  - BMS MAPS:  CDTLMS/CDTLM1, CDTLMS/CDTLM2                  *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-ZOS.
       OBJECT-COMPUTER.    IBM-ZOS.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * PROGRAM IDENTIFICATION AND VERSION CONTROL                      *
      *----------------------------------------------------------------*
       01  WS-PROGRAM-ID              PIC X(08)  VALUE 'CLMDTL00'.
       01  WS-PROGRAM-VERSION         PIC X(06)  VALUE '01.00 '.
       01  WS-TRANSACTION-ID          PIC X(04)  VALUE 'CDTL'.

      *----------------------------------------------------------------*
      * COMMON HEALTHCARE FIELDS                                        *
      *----------------------------------------------------------------*
           COPY HCCOMMON.

      *----------------------------------------------------------------*
      * CLAIMS DATA STRUCTURES                                          *
      *----------------------------------------------------------------*
           COPY CLMCOPY.

      *----------------------------------------------------------------*
      * PROVIDER DATA STRUCTURES                                        *
      *----------------------------------------------------------------*
           COPY PRVCOPY.

      *----------------------------------------------------------------*
      * ERROR HANDLING WORK AREAS                                       *
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
      * PROCESSING FLAGS AND SWITCHES                                   *
      *----------------------------------------------------------------*
       01  WS-FLAGS.
           05  WS-END-OF-CURSOR-SW    PIC X(01)  VALUE 'N'.
               88 END-OF-CURSOR                   VALUE 'Y'.
               88 MORE-ROWS                        VALUE 'N'.
           05  WS-VSAM-FOUND-SW       PIC X(01)  VALUE 'N'.
               88 VSAM-PROVIDER-FOUND              VALUE 'Y'.
               88 VSAM-PROVIDER-NOT-FOUND          VALUE 'N'.
           05  WS-FIRST-TIME-SW       PIC X(01)  VALUE 'Y'.
               88 FIRST-TIME                       VALUE 'Y'.
               88 NOT-FIRST-TIME                   VALUE 'N'.
           05  WS-MAP-MODE-SW         PIC X(01)  VALUE 'I'.
               88 MAP-MODE-INPUT                   VALUE 'I'.
               88 MAP-MODE-DETAIL                  VALUE 'D'.
               88 MAP-MODE-ERROR                   VALUE 'E'.
           05  WS-VALID-INPUT-SW      PIC X(01)  VALUE 'N'.
               88 VALID-INPUT                      VALUE 'Y'.
               88 INVALID-INPUT                    VALUE 'N'.

      *----------------------------------------------------------------*
      * VSAM PROVIDER MASTER FILE DEFINITIONS                           *
      * USED AS RIDFLD TARGET FROM DB2 CURSOR RESULTS                  *
      *----------------------------------------------------------------*
       01  WS-VSAM-PROVIDER-REC.
           05  WS-VSAM-PRV-NPI        PIC X(10).
           05  WS-VSAM-PRV-LAST-NAME  PIC X(35).
           05  WS-VSAM-PRV-FIRST-NAME PIC X(25).
           05  WS-VSAM-PRV-MI         PIC X(01).
           05  WS-VSAM-PRV-ORG-NAME   PIC X(60).
           05  WS-VSAM-PRV-ENTITY-TYPE
                                       PIC X(01).
               88 PRV-INDIVIDUAL                   VALUE '1'.
               88 PRV-ORGANIZATION                 VALUE '2'.
           05  WS-VSAM-PRV-TAXONOMY   PIC X(10).
           05  WS-VSAM-PRV-SPECIALTY  PIC X(03).
           05  WS-VSAM-PRV-ADDR.
               10  WS-VSAM-PRV-ADDR1  PIC X(55).
               10  WS-VSAM-PRV-ADDR2  PIC X(55).
               10  WS-VSAM-PRV-CITY   PIC X(30).
               10  WS-VSAM-PRV-STATE  PIC X(02).
               10  WS-VSAM-PRV-ZIP    PIC X(09).
           05  WS-VSAM-PRV-PHONE      PIC X(10).
           05  WS-VSAM-PRV-NETWORK-ID PIC X(04).
           05  WS-VSAM-PRV-PAR-STATUS PIC X(01).
               88 PRV-PAR                          VALUE 'Y'.
               88 PRV-NON-PAR                      VALUE 'N'.
           05  WS-VSAM-PRV-EFF-DATE   PIC X(10).
           05  WS-VSAM-PRV-TERM-DATE  PIC X(10).
           05  WS-VSAM-PRV-STATUS     PIC X(01).
               88 PRV-ACTIVE                       VALUE 'A'.
               88 PRV-INACTIVE                     VALUE 'I'.
               88 PRV-TERMINATED                   VALUE 'T'.
           05  FILLER                  PIC X(78).

       01  WS-VSAM-KEY                PIC X(10).
       01  WS-VSAM-FILE-STATUS        PIC X(02).
           88 VSAM-OK                              VALUE '00'.
           88 VSAM-NOT-FOUND                       VALUE '23'.
           88 VSAM-EOF                             VALUE '10'.

      *----------------------------------------------------------------*
      * DB2 HOST VARIABLES FOR CLAIM CURSOR                             *
      *----------------------------------------------------------------*
       01  WS-DB2-CLAIM-ROW.
           05  HV-CLAIM-ID            PIC X(15).
           05  HV-CLAIM-TYPE          PIC X(02).
           05  HV-CLAIM-STATUS        PIC X(02).
           05  HV-MEMBER-ID           PIC X(12).
           05  HV-MEMBER-LAST-NAME    PIC X(35).
           05  HV-MEMBER-FIRST-NAME   PIC X(25).
           05  HV-BILLING-NPI         PIC X(10).
           05  HV-RENDERING-NPI       PIC X(10).
           05  HV-SERVICE-FROM-DATE   PIC X(10).
           05  HV-SERVICE-TO-DATE     PIC X(10).
           05  HV-TOTAL-CHARGES       PIC S9(09)V99 COMP-3.
           05  HV-TOTAL-ALLOWED       PIC S9(09)V99 COMP-3.
           05  HV-TOTAL-PAID          PIC S9(09)V99 COMP-3.
           05  HV-MEMBER-RESP         PIC S9(09)V99 COMP-3.
           05  HV-DEDUCTIBLE-AMT      PIC S9(07)V99 COMP-3.
           05  HV-COPAY-AMT           PIC S9(07)V99 COMP-3.
           05  HV-COINSURANCE-AMT     PIC S9(07)V99 COMP-3.
           05  HV-DENY-REASON         PIC X(05).
           05  HV-ADJUDICATION-DATE   PIC X(10).
           05  HV-RECEIPT-DATE        PIC X(10).
           05  HV-REMIT-STATUS        PIC X(02).
           05  HV-PLAN-CODE           PIC X(08).
           05  HV-GROUP-ID            PIC X(10).
           05  HV-PRIMARY-DIAG        PIC X(08).

      *----------------------------------------------------------------*
      * DB2 HOST VARIABLES FOR CLAIM LINE DETAIL                        *
      *----------------------------------------------------------------*
       01  WS-DB2-LINE-ROW.
           05  HV-LINE-CLAIM-ID       PIC X(15).
           05  HV-LINE-SEQ            PIC S9(04) COMP.
           05  HV-LINE-PROC-CODE      PIC X(05).
           05  HV-LINE-MODIFIER1      PIC X(02).
           05  HV-LINE-MODIFIER2      PIC X(02).
           05  HV-LINE-REV-CODE       PIC X(04).
           05  HV-LINE-FROM-DATE      PIC X(10).
           05  HV-LINE-TO-DATE        PIC X(10).
           05  HV-LINE-UNITS          PIC S9(05)V99 COMP-3.
           05  HV-LINE-CHARGES        PIC S9(07)V99 COMP-3.
           05  HV-LINE-ALLOWED        PIC S9(07)V99 COMP-3.
           05  HV-LINE-PAID           PIC S9(07)V99 COMP-3.
           05  HV-LINE-DENY-REASON    PIC X(05).
           05  HV-LINE-NDC            PIC X(11).
           05  HV-LINE-POS            PIC X(02).

      *----------------------------------------------------------------*
      * CLAIM LINE NULL INDICATORS                                      *
      *----------------------------------------------------------------*
       01  WS-LINE-NULL-IND.
           05  NI-LINE-MODIFIER1      PIC S9(04) COMP VALUE ZERO.
           05  NI-LINE-MODIFIER2      PIC S9(04) COMP VALUE ZERO.
           05  NI-LINE-REV-CODE       PIC S9(04) COMP VALUE ZERO.
           05  NI-LINE-NDC            PIC S9(04) COMP VALUE ZERO.
           05  NI-LINE-POS            PIC S9(04) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * CLAIM DETAIL RESPONSE - COMMAREA OUTPUT STRUCTURE               *
      * RETURNED TO CALLING PROGRAM OR MAP                              *
      *----------------------------------------------------------------*
       01  WS-DETAIL-RESPONSE.
           05  WS-RSP-RETURN-CODE     PIC X(02).
               88 RSP-SUCCESS                      VALUE '00'.
               88 RSP-MEMBER-NOT-FOUND             VALUE '01'.
               88 RSP-NO-CLAIMS                    VALUE '02'.
               88 RSP-DB2-ERROR                    VALUE '90'.
               88 RSP-VSAM-ERROR                   VALUE '91'.
               88 RSP-CICS-ERROR                   VALUE '92'.
           05  WS-RSP-ERROR-MSG       PIC X(80).
           05  WS-RSP-MEMBER-INFO.
               10  WS-RSP-MBR-ID      PIC X(12).
               10  WS-RSP-MBR-NAME    PIC X(60).
               10  WS-RSP-MBR-GROUP   PIC X(10).
               10  WS-RSP-MBR-PLAN    PIC X(08).
           05  WS-RSP-CLAIM-COUNT     PIC S9(04) COMP VALUE ZERO.
           05  WS-RSP-CLAIMS OCCURS 20 TIMES.
               10  WS-RSP-CLM-ID      PIC X(15).
               10  WS-RSP-CLM-TYPE    PIC X(02).
               10  WS-RSP-CLM-STATUS  PIC X(02).
               10  WS-RSP-CLM-DOS     PIC X(10).
               10  WS-RSP-CLM-CHARGES PIC S9(09)V99 COMP-3.
               10  WS-RSP-CLM-ALLOWED PIC S9(09)V99 COMP-3.
               10  WS-RSP-CLM-PAID    PIC S9(09)V99 COMP-3.
               10  WS-RSP-CLM-MBR-RESP
                                       PIC S9(09)V99 COMP-3.
               10  WS-RSP-CLM-PRV-NAME
                                       PIC X(60).
               10  WS-RSP-CLM-PRV-NPI PIC X(10).
               10  WS-RSP-CLM-PRV-NETWORK
                                       PIC X(04).
               10  WS-RSP-CLM-PRV-PAR PIC X(01).
               10  WS-RSP-CLM-DIAG    PIC X(08).
               10  WS-RSP-CLM-ADJ-DATE
                                       PIC X(10).
               10  WS-RSP-CLM-DENY    PIC X(05).
               10  WS-RSP-CLM-LINE-CT PIC S9(04) COMP.
               10  WS-RSP-CLM-LINES OCCURS 50 TIMES.
                   15  WS-RSP-LN-SEQ  PIC S9(04) COMP.
                   15  WS-RSP-LN-PROC PIC X(05).
                   15  WS-RSP-LN-MOD1 PIC X(02).
                   15  WS-RSP-LN-MOD2 PIC X(02).
                   15  WS-RSP-LN-DOS  PIC X(10).
                   15  WS-RSP-LN-CHG  PIC S9(07)V99 COMP-3.
                   15  WS-RSP-LN-ALW  PIC S9(07)V99 COMP-3.
                   15  WS-RSP-LN-PAID PIC S9(07)V99 COMP-3.
                   15  WS-RSP-LN-DENY PIC X(05).

      *----------------------------------------------------------------*
      * FINANCIAL SUMMARY ACCUMULATORS                                  *
      *----------------------------------------------------------------*
       01  WS-FINANCIAL-SUMMARY.
           05  WS-SUM-TOTAL-CHARGES   PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-SUM-TOTAL-ALLOWED   PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-SUM-TOTAL-PAID      PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-SUM-TOTAL-MBR-RESP  PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-SUM-DEDUCTIBLE      PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-SUM-COPAY           PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-SUM-COINSURANCE     PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-SUM-CLAIMS-PAID     PIC S9(04) COMP VALUE ZERO.
           05  WS-SUM-CLAIMS-DENIED   PIC S9(04) COMP VALUE ZERO.
           05  WS-SUM-CLAIMS-PENDED   PIC S9(04) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * DISPLAY FORMATTING WORK AREAS                                   *
      *----------------------------------------------------------------*
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CHARGES        PIC $$$,$$$,$$9.99-.
           05  WS-DISP-ALLOWED        PIC $$$,$$$,$$9.99-.
           05  WS-DISP-PAID           PIC $$$,$$$,$$9.99-.
           05  WS-DISP-MBR-RESP       PIC $$$,$$$,$$9.99-.
           05  WS-DISP-LINE-CHG       PIC $$$$,$$9.99-.
           05  WS-DISP-LINE-ALW       PIC $$$$,$$9.99-.
           05  WS-DISP-LINE-PAID      PIC $$$$,$$9.99-.
           05  WS-DISP-CLAIM-STATUS   PIC X(20).
           05  WS-DISP-CLAIM-TYPE     PIC X(20).
           05  WS-DISP-PRV-NAME       PIC X(60).

      *----------------------------------------------------------------*
      * MAP INPUT/OUTPUT FIELDS                                         *
      *----------------------------------------------------------------*
       01  WS-MAP-INPUT.
           05  WS-MAP-MEMBER-ID       PIC X(12).
           05  WS-MAP-DOS-FROM        PIC X(10).
           05  WS-MAP-DOS-TO          PIC X(10).
           05  WS-MAP-CLAIM-TYPE-FILTER
                                       PIC X(02).
           05  WS-MAP-STATUS-FILTER   PIC X(02).

      *----------------------------------------------------------------*
      * PAGINATION CONTROL                                              *
      *----------------------------------------------------------------*
       01  WS-PAGE-CONTROL.
           05  WS-PAGE-NUMBER         PIC S9(04) COMP VALUE 1.
           05  WS-CLAIMS-PER-PAGE     PIC S9(04) COMP VALUE 10.
           05  WS-TOTAL-CLAIM-ROWS    PIC S9(04) COMP VALUE ZERO.
           05  WS-CURRENT-ROW-NUM     PIC S9(04) COMP VALUE ZERO.
           05  WS-START-ROW           PIC S9(04) COMP VALUE 1.
           05  WS-END-ROW             PIC S9(04) COMP VALUE 10.
           05  WS-MORE-PAGES-SW       PIC X(01)  VALUE 'N'.
               88 MORE-PAGES                       VALUE 'Y'.
               88 NO-MORE-PAGES                    VALUE 'N'.

      *----------------------------------------------------------------*
      * CLAIM STATUS DECODE TABLE                                       *
      *----------------------------------------------------------------*
       01  WS-STATUS-DECODE-TABLE.
           05  FILLER PIC X(22) VALUE '00NEW               '.
           05  FILLER PIC X(22) VALUE '10IN REVIEW         '.
           05  FILLER PIC X(22) VALUE '20PENDED            '.
           05  FILLER PIC X(22) VALUE '30IN ADJUDICATION   '.
           05  FILLER PIC X(22) VALUE '40DENIED            '.
           05  FILLER PIC X(22) VALUE '50PAID              '.
           05  FILLER PIC X(22) VALUE '60ADJUSTED          '.
           05  FILLER PIC X(22) VALUE '70VOIDED            '.
           05  FILLER PIC X(22) VALUE '99UNKNOWN           '.
       01  WS-STATUS-DECODE-REDEF REDEFINES WS-STATUS-DECODE-TABLE.
           05  WS-STATUS-ENTRY OCCURS 9 TIMES.
               10  WS-STATUS-CODE     PIC X(02).
               10  WS-STATUS-DESC     PIC X(20).

      *----------------------------------------------------------------*
      * CLAIM TYPE DECODE TABLE                                         *
      *----------------------------------------------------------------*
       01  WS-TYPE-DECODE-TABLE.
           05  FILLER PIC X(22) VALUE 'PRPROFESSIONAL      '.
           05  FILLER PIC X(22) VALUE 'ININSTITUTIONAL     '.
           05  FILLER PIC X(22) VALUE 'DNDENTAL            '.
           05  FILLER PIC X(22) VALUE 'RXPHARMACY          '.
       01  WS-TYPE-DECODE-REDEF REDEFINES WS-TYPE-DECODE-TABLE.
           05  WS-TYPE-ENTRY OCCURS 4 TIMES.
               10  WS-TYPE-CODE       PIC X(02).
               10  WS-TYPE-DESC       PIC X(20).

      *----------------------------------------------------------------*
      * WORK VARIABLES                                                  *
      *----------------------------------------------------------------*
       01  WS-WORK-AREAS.
           05  WS-CLAIM-INDEX         PIC S9(04) COMP VALUE ZERO.
           05  WS-LINE-INDEX          PIC S9(04) COMP VALUE ZERO.
           05  WS-STATUS-INDEX        PIC S9(04) COMP VALUE ZERO.
           05  WS-TYPE-INDEX          PIC S9(04) COMP VALUE ZERO.
           05  WS-SAVE-CLAIM-ID       PIC X(15)  VALUE SPACES.
           05  WS-SAVE-NPI            PIC X(10)  VALUE SPACES.
           05  WS-PROVIDER-DISPLAY    PIC X(60)  VALUE SPACES.
           05  WS-TIMESTAMP           PIC X(26)  VALUE SPACES.
           05  WS-DATE-WORK           PIC 9(08)  VALUE ZERO.
           05  WS-TIME-WORK           PIC 9(06)  VALUE ZERO.
           05  WS-ABSTIME             PIC S9(15) COMP-3 VALUE ZERO.

      *----------------------------------------------------------------*
      * DB2 CURSOR DECLARATIONS                                         *
      *----------------------------------------------------------------*
      * CLAIM HISTORY CURSOR - FETCHES ALL CLAIMS FOR A MEMBER         *
      * ORDERED BY SERVICE DATE DESCENDING (MOST RECENT FIRST)         *
      *----------------------------------------------------------------*
           EXEC SQL
               DECLARE CLAIM-HISTORY-CURSOR CURSOR FOR
               SELECT  CM.CLAIM_ID,
                       CM.CLAIM_TYPE,
                       CM.CLAIM_STATUS,
                       CM.MEMBER_ID,
                       MB.LAST_NAME,
                       MB.FIRST_NAME,
                       CM.BILLING_NPI,
                       CM.RENDERING_NPI,
                       CM.SERVICE_FROM_DATE,
                       CM.SERVICE_TO_DATE,
                       CM.TOTAL_CHARGES,
                       CM.ALLOWED_AMOUNT,
                       CM.PAID_AMOUNT,
                       CM.MEMBER_RESPONSIBILITY,
                       CM.DEDUCTIBLE_APPLIED,
                       CM.COPAY_APPLIED,
                       CM.COINSURANCE_APPLIED,
                       CM.DENY_REASON_CODE,
                       CM.ADJUDICATION_DATE,
                       CM.RECEIPT_DATE,
                       CM.REMIT_STATUS,
                       EN.PLAN_CODE,
                       EN.GROUP_ID,
                       CM.PRIMARY_DIAGNOSIS
               FROM    HCAS.CLAIM_MASTER CM
               INNER JOIN HCAS.MEMBER MB
                       ON CM.MEMBER_ID = MB.MEMBER_ID
               LEFT JOIN HCAS.ENROLLMENT EN
                       ON CM.MEMBER_ID = EN.MEMBER_ID
                       AND CM.SERVICE_FROM_DATE
                           BETWEEN EN.EFFECTIVE_DATE
                               AND COALESCE(EN.TERM_DATE,
                                   '9999-12-31')
               WHERE   CM.MEMBER_ID = :HV-MEMBER-ID
               ORDER BY CM.SERVICE_FROM_DATE DESC,
                        CM.CLAIM_ID DESC
           END-EXEC.

      *----------------------------------------------------------------*
      * CLAIM LINE CURSOR - FETCHES LINES FOR A SPECIFIC CLAIM         *
      *----------------------------------------------------------------*
           EXEC SQL
               DECLARE LINE-DETAIL-CURSOR CURSOR FOR
               SELECT  CL.CLAIM_ID,
                       CL.LINE_SEQ_NBR,
                       CL.PROCEDURE_CODE,
                       CL.MODIFIER_1,
                       CL.MODIFIER_2,
                       CL.REVENUE_CODE,
                       CL.SERVICE_FROM_DATE,
                       CL.SERVICE_TO_DATE,
                       CL.UNITS,
                       CL.BILLED_AMOUNT,
                       CL.ALLOWED_AMOUNT,
                       CL.PAID_AMOUNT,
                       CL.DENY_REASON_CODE,
                       CL.NDC_CODE,
                       CL.PLACE_OF_SERVICE
               FROM    HCAS.CLAIM_LINE CL
               WHERE   CL.CLAIM_ID = :WS-SAVE-CLAIM-ID
               ORDER BY CL.LINE_SEQ_NBR
           END-EXEC.

      *----------------------------------------------------------------*
      * LINKAGE SECTION - CICS COMMAREA                                 *
      *----------------------------------------------------------------*
       LINKAGE SECTION.

       01  DFHCOMMAREA.
           05  CA-FUNCTION-CODE       PIC X(02).
               88 CA-INITIAL-INQUIRY               VALUE 'IQ'.
               88 CA-PAGE-FORWARD                  VALUE 'PF'.
               88 CA-PAGE-BACKWARD                 VALUE 'PB'.
               88 CA-CLAIM-DETAIL                  VALUE 'CD'.
               88 CA-RETURN-TO-LIST                VALUE 'RL'.
           05  CA-MEMBER-ID           PIC X(12).
           05  CA-DOS-FROM            PIC X(10).
           05  CA-DOS-TO              PIC X(10).
           05  CA-CLAIM-TYPE-FILTER   PIC X(02).
           05  CA-STATUS-FILTER       PIC X(02).
           05  CA-SELECTED-CLAIM-ID   PIC X(15).
           05  CA-PAGE-NUMBER         PIC S9(04) COMP.
           05  CA-RESPONSE-AREA.
               10  CA-RSP-RETURN-CODE PIC X(02).
               10  CA-RSP-MSG         PIC X(80).
           05  CA-DETAIL-DATA         PIC X(4000).

      *================================================================*
       PROCEDURE DIVISION.

      *================================================================*
      * 0000-MAIN-CONTROL                                               *
      *   MAIN CONTROL PARAGRAPH - ROUTES PROCESSING BASED ON          *
      *   COMMAREA FUNCTION CODE AND EIBCALEN                          *
      *================================================================*
       0000-MAIN-CONTROL.

           EXEC CICS HANDLE CONDITION
               ERROR   (9000-CICS-ERROR-HANDLER)
               NOTFND  (9100-NOT-FOUND-HANDLER)
           END-EXEC

           EXEC CICS HANDLE ABEND
               LABEL   (9500-ABEND-HANDLER)
           END-EXEC

      *--- CHECK IF THIS IS AN INITIAL ENTRY (NO COMMAREA) ---
           IF EIBCALEN = ZERO
               PERFORM 1000-INITIAL-ENTRY
           ELSE
               MOVE DFHCOMMAREA TO WS-MAP-INPUT
               EVALUATE TRUE
                   WHEN CA-INITIAL-INQUIRY
                       PERFORM 2000-PROCESS-INQUIRY
                   WHEN CA-PAGE-FORWARD
                       ADD 1 TO WS-PAGE-NUMBER
                       PERFORM 2000-PROCESS-INQUIRY
                   WHEN CA-PAGE-BACKWARD
                       IF WS-PAGE-NUMBER > 1
                           SUBTRACT 1 FROM WS-PAGE-NUMBER
                       END-IF
                       PERFORM 2000-PROCESS-INQUIRY
                   WHEN CA-CLAIM-DETAIL
                       PERFORM 3000-PROCESS-CLAIM-DETAIL
                   WHEN CA-RETURN-TO-LIST
                       PERFORM 2000-PROCESS-INQUIRY
                   WHEN OTHER
                       MOVE 'E' TO WS-MAP-MODE-SW
                       MOVE '92' TO WS-RSP-RETURN-CODE
                       MOVE 'INVALID FUNCTION CODE RECEIVED'
                           TO WS-RSP-ERROR-MSG
                       PERFORM 8000-SEND-ERROR-MAP
               END-EVALUATE
           END-IF

           EXEC CICS RETURN
               TRANSID  (WS-TRANSACTION-ID)
               COMMAREA (DFHCOMMAREA)
               LENGTH   (LENGTH OF DFHCOMMAREA)
           END-EXEC
           .

       0000-EXIT.
           EXIT.

      *================================================================*
      * 1000-INITIAL-ENTRY                                              *
      *   FIRST TIME ENTRY - SEND THE INQUIRY INPUT MAP                *
      *================================================================*
       1000-INITIAL-ENTRY.

           INITIALIZE WS-MAP-INPUT
           INITIALIZE WS-DETAIL-RESPONSE

           EXEC CICS SEND MAP    ('CDTLM1')
                         MAPSET  ('CDTLMS')
                         ERASE
                         RESP    (WS-CICS-RESP)
                         RESP2   (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-CICS-ERROR-HANDLER
           END-IF
           .

       1000-EXIT.
           EXIT.

      *================================================================*
      * 2000-PROCESS-INQUIRY                                            *
      *   RECEIVES INPUT FROM MAP, VALIDATES, AND FETCHES CLAIMS       *
      *================================================================*
       2000-PROCESS-INQUIRY.

           PERFORM 2100-RECEIVE-INPUT
           IF VALID-INPUT
               PERFORM 2200-VALIDATE-MEMBER
               IF VALID-INPUT
                   PERFORM 2300-FETCH-CLAIM-HISTORY
                   IF WS-RSP-CLAIM-COUNT > ZERO
                       PERFORM 2400-BUILD-DETAIL-MAP
                   ELSE
                       MOVE '02' TO WS-RSP-RETURN-CODE
                       MOVE 'NO CLAIMS FOUND FOR THIS MEMBER'
                           TO WS-RSP-ERROR-MSG
                       PERFORM 8000-SEND-ERROR-MAP
                   END-IF
               ELSE
                   PERFORM 8000-SEND-ERROR-MAP
               END-IF
           END-IF
           .

       2000-EXIT.
           EXIT.

      *================================================================*
      * 2100-RECEIVE-INPUT                                              *
      *   RECEIVE AND VALIDATE MAP INPUT FIELDS                        *
      *================================================================*
       2100-RECEIVE-INPUT.

           EXEC CICS RECEIVE MAP    ('CDTLM1')
                            MAPSET  ('CDTLMS')
                            INTO    (WS-MAP-INPUT)
                            RESP    (WS-CICS-RESP)
                            RESP2   (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               IF WS-CICS-RESP = DFHRESP(MAPFAIL)
                   MOVE 'N' TO WS-VALID-INPUT-SW
                   MOVE 'E' TO WS-MAP-MODE-SW
                   MOVE '92' TO WS-RSP-RETURN-CODE
                   MOVE 'NO DATA ENTERED - PLEASE ENTER MEMBER ID'
                       TO WS-RSP-ERROR-MSG
               ELSE
                   PERFORM 9000-CICS-ERROR-HANDLER
               END-IF
           ELSE
               MOVE 'Y' TO WS-VALID-INPUT-SW
           END-IF

      *--- VALIDATE MEMBER ID IS NOT BLANK ---
           IF VALID-INPUT
               IF WS-MAP-MEMBER-ID = SPACES OR
                  WS-MAP-MEMBER-ID = LOW-VALUES
                   MOVE 'N' TO WS-VALID-INPUT-SW
                   MOVE '92' TO WS-RSP-RETURN-CODE
                   MOVE 'MEMBER ID IS REQUIRED'
                       TO WS-RSP-ERROR-MSG
               ELSE
                   MOVE WS-MAP-MEMBER-ID TO HV-MEMBER-ID
               END-IF
           END-IF
           .

       2100-EXIT.
           EXIT.

      *================================================================*
      * 2200-VALIDATE-MEMBER                                            *
      *   VERIFY MEMBER EXISTS IN DB2 BEFORE FETCHING CLAIMS           *
      *================================================================*
       2200-VALIDATE-MEMBER.

           EXEC SQL
               SELECT  MEMBER_ID,
                       LAST_NAME,
                       FIRST_NAME
               INTO    :HV-MEMBER-ID,
                       :HV-MEMBER-LAST-NAME,
                       :HV-MEMBER-FIRST-NAME
               FROM    HCAS.MEMBER
               WHERE   MEMBER_ID = :HV-MEMBER-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'N' TO WS-VALID-INPUT-SW
                   MOVE '01' TO WS-RSP-RETURN-CODE
                   STRING 'MEMBER ID ' DELIMITED SIZE
                          HV-MEMBER-ID DELIMITED SPACES
                          ' NOT FOUND IN MEMBER MASTER'
                          DELIMITED SIZE
                       INTO WS-RSP-ERROR-MSG
               WHEN OTHER
                   MOVE 'N' TO WS-VALID-INPUT-SW
                   MOVE '90' TO WS-RSP-RETURN-CODE
                   STRING 'DB2 ERROR VALIDATING MEMBER: SQLCODE='
                          DELIMITED SIZE
                       INTO WS-RSP-ERROR-MSG
                   PERFORM 9200-DB2-ERROR-HANDLER
           END-EVALUATE

      *--- POPULATE MEMBER INFO IN RESPONSE ---
           IF VALID-INPUT
               MOVE HV-MEMBER-ID TO WS-RSP-MBR-ID
               STRING HV-MEMBER-LAST-NAME DELIMITED SPACES
                      ', ' DELIMITED SIZE
                      HV-MEMBER-FIRST-NAME DELIMITED SPACES
                   INTO WS-RSP-MBR-NAME
           END-IF
           .

       2200-EXIT.
           EXIT.

      *================================================================*
      * 2300-FETCH-CLAIM-HISTORY                                        *
      *   OPENS DB2 CURSOR, FETCHES CLAIM ROWS, AND FOR EACH CLAIM    *
      *   READS VSAM PROVIDER MASTER TO ENRICH WITH PROVIDER DATA      *
      *================================================================*
       2300-FETCH-CLAIM-HISTORY.

           INITIALIZE WS-FINANCIAL-SUMMARY
           MOVE ZERO TO WS-RSP-CLAIM-COUNT
           MOVE ZERO TO WS-CLAIM-INDEX
           MOVE 'N'  TO WS-END-OF-CURSOR-SW

      *--- OPEN THE CLAIM HISTORY CURSOR ---
           EXEC SQL
               OPEN CLAIM-HISTORY-CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE '90' TO WS-RSP-RETURN-CODE
               MOVE 'DB2 ERROR OPENING CLAIM CURSOR'
                   TO WS-RSP-ERROR-MSG
               PERFORM 9200-DB2-ERROR-HANDLER
               GO TO 2300-EXIT
           END-IF

      *--- FETCH ROWS IN A LOOP ---
           PERFORM 2310-FETCH-NEXT-CLAIM
               UNTIL END-OF-CURSOR
               OR WS-CLAIM-INDEX >= 20

      *--- CLOSE THE CURSOR ---
           EXEC SQL
               CLOSE CLAIM-HISTORY-CURSOR
           END-EXEC

           MOVE WS-CLAIM-INDEX TO WS-RSP-CLAIM-COUNT
           .

       2300-EXIT.
           EXIT.

      *================================================================*
      * 2310-FETCH-NEXT-CLAIM                                           *
      *   FETCHES ONE CLAIM ROW FROM DB2 CURSOR AND ENRICHES IT       *
      *   WITH VSAM PROVIDER DATA                                      *
      *================================================================*
       2310-FETCH-NEXT-CLAIM.

           EXEC SQL
               FETCH CLAIM-HISTORY-CURSOR
               INTO  :HV-CLAIM-ID,
                     :HV-CLAIM-TYPE,
                     :HV-CLAIM-STATUS,
                     :HV-MEMBER-ID,
                     :HV-MEMBER-LAST-NAME,
                     :HV-MEMBER-FIRST-NAME,
                     :HV-BILLING-NPI,
                     :HV-RENDERING-NPI,
                     :HV-SERVICE-FROM-DATE,
                     :HV-SERVICE-TO-DATE,
                     :HV-TOTAL-CHARGES,
                     :HV-TOTAL-ALLOWED,
                     :HV-TOTAL-PAID,
                     :HV-MEMBER-RESP,
                     :HV-DEDUCTIBLE-AMT,
                     :HV-COPAY-AMT,
                     :HV-COINSURANCE-AMT,
                     :HV-DENY-REASON,
                     :HV-ADJUDICATION-DATE,
                     :HV-RECEIPT-DATE,
                     :HV-REMIT-STATUS,
                     :HV-PLAN-CODE,
                     :HV-GROUP-ID,
                     :HV-PRIMARY-DIAG
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-CLAIM-INDEX
                   PERFORM 2320-ENRICH-WITH-VSAM-PROVIDER
                   PERFORM 2330-POPULATE-RESPONSE-CLAIM
                   PERFORM 2340-ACCUMULATE-FINANCIALS
               WHEN +100
                   MOVE 'Y' TO WS-END-OF-CURSOR-SW
               WHEN OTHER
                   MOVE 'Y' TO WS-END-OF-CURSOR-SW
                   MOVE '90' TO WS-RSP-RETURN-CODE
                   MOVE 'DB2 ERROR FETCHING CLAIM HISTORY'
                       TO WS-RSP-ERROR-MSG
                   PERFORM 9200-DB2-ERROR-HANDLER
           END-EVALUATE
           .

       2310-EXIT.
           EXIT.

      *================================================================*
      * 2320-ENRICH-WITH-VSAM-PROVIDER                                  *
      *   USES THE BILLING NPI FROM DB2 AS A RIDFLD TO READ THE       *
      *   VSAM PROVIDER MASTER FILE FOR PROVIDER DEMOGRAPHICS          *
      *   THIS IS THE KEY INTERSECTION POINT: DB2 RESULT DRIVES       *
      *   A VSAM READ IN THE SAME TRANSACTION                         *
      *================================================================*
       2320-ENRICH-WITH-VSAM-PROVIDER.

           MOVE HV-BILLING-NPI TO WS-VSAM-KEY
           MOVE 'N' TO WS-VSAM-FOUND-SW

      *--- READ VSAM PROVIDER MASTER USING CICS FILE CONTROL ---
           EXEC CICS READ
               FILE    ('PRVMAST')
               INTO    (WS-VSAM-PROVIDER-REC)
               RIDFLD  (WS-VSAM-KEY)
               LENGTH  (LENGTH OF WS-VSAM-PROVIDER-REC)
               RESP    (WS-CICS-RESP)
               RESP2   (WS-CICS-RESP2)
           END-EXEC

           EVALUATE WS-CICS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Y' TO WS-VSAM-FOUND-SW
                   PERFORM 2321-FORMAT-PROVIDER-NAME
               WHEN DFHRESP(NOTFND)
      *---       PROVIDER NOT IN VSAM - USE NPI AS DISPLAY NAME ---
                   MOVE 'N' TO WS-VSAM-FOUND-SW
                   STRING 'PROVIDER NPI: ' DELIMITED SIZE
                          HV-BILLING-NPI DELIMITED SIZE
                       INTO WS-PROVIDER-DISPLAY
               WHEN DFHRESP(DISABLED)
                   MOVE 'N' TO WS-VSAM-FOUND-SW
                   MOVE 'VSAM FILE DISABLED' TO WS-PROVIDER-DISPLAY
               WHEN OTHER
      *---       LOG VSAM ERROR BUT DO NOT ABORT THE TRANSACTION ---
                   MOVE 'N' TO WS-VSAM-FOUND-SW
                   STRING 'VSAM ERROR RESP=' DELIMITED SIZE
                       INTO WS-PROVIDER-DISPLAY
                   PERFORM 9300-LOG-VSAM-WARNING
           END-EVALUATE
           .

       2320-EXIT.
           EXIT.

      *================================================================*
      * 2321-FORMAT-PROVIDER-NAME                                       *
      *   FORMAT THE PROVIDER NAME FROM VSAM RECORD FOR DISPLAY        *
      *================================================================*
       2321-FORMAT-PROVIDER-NAME.

           INITIALIZE WS-PROVIDER-DISPLAY

           IF PRV-INDIVIDUAL
               STRING WS-VSAM-PRV-LAST-NAME DELIMITED SPACES
                      ', ' DELIMITED SIZE
                      WS-VSAM-PRV-FIRST-NAME DELIMITED SPACES
                   INTO WS-PROVIDER-DISPLAY
           ELSE
               MOVE WS-VSAM-PRV-ORG-NAME
                   TO WS-PROVIDER-DISPLAY
           END-IF
           .

       2321-EXIT.
           EXIT.

      *================================================================*
      * 2330-POPULATE-RESPONSE-CLAIM                                    *
      *   POPULATES ONE ENTRY IN THE RESPONSE CLAIMS ARRAY             *
      *================================================================*
       2330-POPULATE-RESPONSE-CLAIM.

           MOVE HV-CLAIM-ID
               TO WS-RSP-CLM-ID(WS-CLAIM-INDEX)
           MOVE HV-CLAIM-TYPE
               TO WS-RSP-CLM-TYPE(WS-CLAIM-INDEX)
           MOVE HV-CLAIM-STATUS
               TO WS-RSP-CLM-STATUS(WS-CLAIM-INDEX)
           MOVE HV-SERVICE-FROM-DATE
               TO WS-RSP-CLM-DOS(WS-CLAIM-INDEX)
           MOVE HV-TOTAL-CHARGES
               TO WS-RSP-CLM-CHARGES(WS-CLAIM-INDEX)
           MOVE HV-TOTAL-ALLOWED
               TO WS-RSP-CLM-ALLOWED(WS-CLAIM-INDEX)
           MOVE HV-TOTAL-PAID
               TO WS-RSP-CLM-PAID(WS-CLAIM-INDEX)
           MOVE HV-MEMBER-RESP
               TO WS-RSP-CLM-MBR-RESP(WS-CLAIM-INDEX)
           MOVE WS-PROVIDER-DISPLAY
               TO WS-RSP-CLM-PRV-NAME(WS-CLAIM-INDEX)
           MOVE HV-BILLING-NPI
               TO WS-RSP-CLM-PRV-NPI(WS-CLAIM-INDEX)
           MOVE HV-PRIMARY-DIAG
               TO WS-RSP-CLM-DIAG(WS-CLAIM-INDEX)
           MOVE HV-ADJUDICATION-DATE
               TO WS-RSP-CLM-ADJ-DATE(WS-CLAIM-INDEX)
           MOVE HV-DENY-REASON
               TO WS-RSP-CLM-DENY(WS-CLAIM-INDEX)

      *--- ADD VSAM PROVIDER NETWORK INFO IF FOUND ---
           IF VSAM-PROVIDER-FOUND
               MOVE WS-VSAM-PRV-NETWORK-ID
                   TO WS-RSP-CLM-PRV-NETWORK(WS-CLAIM-INDEX)
               MOVE WS-VSAM-PRV-PAR-STATUS
                   TO WS-RSP-CLM-PRV-PAR(WS-CLAIM-INDEX)
           ELSE
               MOVE 'UNKN'
                   TO WS-RSP-CLM-PRV-NETWORK(WS-CLAIM-INDEX)
               MOVE 'U'
                   TO WS-RSP-CLM-PRV-PAR(WS-CLAIM-INDEX)
           END-IF
           .

       2330-EXIT.
           EXIT.

      *================================================================*
      * 2340-ACCUMULATE-FINANCIALS                                      *
      *   ADD CLAIM-LEVEL AMOUNTS TO RUNNING TOTALS                    *
      *================================================================*
       2340-ACCUMULATE-FINANCIALS.

           ADD HV-TOTAL-CHARGES   TO WS-SUM-TOTAL-CHARGES
           ADD HV-TOTAL-ALLOWED   TO WS-SUM-TOTAL-ALLOWED
           ADD HV-TOTAL-PAID      TO WS-SUM-TOTAL-PAID
           ADD HV-MEMBER-RESP     TO WS-SUM-TOTAL-MBR-RESP
           ADD HV-DEDUCTIBLE-AMT  TO WS-SUM-DEDUCTIBLE
           ADD HV-COPAY-AMT       TO WS-SUM-COPAY
           ADD HV-COINSURANCE-AMT TO WS-SUM-COINSURANCE

           EVALUATE HV-CLAIM-STATUS
               WHEN '50'
                   ADD 1 TO WS-SUM-CLAIMS-PAID
               WHEN '40'
                   ADD 1 TO WS-SUM-CLAIMS-DENIED
               WHEN '20'
                   ADD 1 TO WS-SUM-CLAIMS-PENDED
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .

       2340-EXIT.
           EXIT.

      *================================================================*
      * 2400-BUILD-DETAIL-MAP                                           *
      *   CONSTRUCTS AND SENDS THE CLAIMS DETAIL OUTPUT MAP             *
      *================================================================*
       2400-BUILD-DETAIL-MAP.

           MOVE '00' TO WS-RSP-RETURN-CODE
           MOVE SPACES TO WS-RSP-ERROR-MSG

      *--- FORMAT FINANCIAL SUMMARY FOR DISPLAY ---
           MOVE WS-SUM-TOTAL-CHARGES TO WS-DISP-CHARGES
           MOVE WS-SUM-TOTAL-ALLOWED TO WS-DISP-ALLOWED
           MOVE WS-SUM-TOTAL-PAID    TO WS-DISP-PAID
           MOVE WS-SUM-TOTAL-MBR-RESP TO WS-DISP-MBR-RESP

      *--- DECODE STATUS AND TYPE FOR EACH CLAIM ---
           PERFORM VARYING WS-CLAIM-INDEX FROM 1 BY 1
               UNTIL WS-CLAIM-INDEX > WS-RSP-CLAIM-COUNT
               PERFORM 2410-DECODE-CLAIM-STATUS
               PERFORM 2420-DECODE-CLAIM-TYPE
           END-PERFORM

      *--- SEND THE DETAIL MAP ---
           EXEC CICS SEND MAP    ('CDTLM2')
                         MAPSET  ('CDTLMS')
                         FROM    (WS-DETAIL-RESPONSE)
                         ERASE
                         RESP    (WS-CICS-RESP)
                         RESP2   (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-CICS-ERROR-HANDLER
           END-IF
           .

       2400-EXIT.
           EXIT.

      *================================================================*
      * 2410-DECODE-CLAIM-STATUS                                        *
      *   TRANSLATE STATUS CODE TO DISPLAY DESCRIPTION                  *
      *================================================================*
       2410-DECODE-CLAIM-STATUS.

           MOVE 'UNKNOWN' TO WS-DISP-CLAIM-STATUS
           PERFORM VARYING WS-STATUS-INDEX FROM 1 BY 1
               UNTIL WS-STATUS-INDEX > 9
               IF WS-STATUS-CODE(WS-STATUS-INDEX) =
                  WS-RSP-CLM-STATUS(WS-CLAIM-INDEX)
                   MOVE WS-STATUS-DESC(WS-STATUS-INDEX)
                       TO WS-DISP-CLAIM-STATUS
               END-IF
           END-PERFORM
           .

       2410-EXIT.
           EXIT.

      *================================================================*
      * 2420-DECODE-CLAIM-TYPE                                          *
      *   TRANSLATE CLAIM TYPE CODE TO DISPLAY DESCRIPTION              *
      *================================================================*
       2420-DECODE-CLAIM-TYPE.

           MOVE 'UNKNOWN' TO WS-DISP-CLAIM-TYPE
           PERFORM VARYING WS-TYPE-INDEX FROM 1 BY 1
               UNTIL WS-TYPE-INDEX > 4
               IF WS-TYPE-CODE(WS-TYPE-INDEX) =
                  WS-RSP-CLM-TYPE(WS-CLAIM-INDEX)
                   MOVE WS-TYPE-DESC(WS-TYPE-INDEX)
                       TO WS-DISP-CLAIM-TYPE
               END-IF
           END-PERFORM
           .

       2420-EXIT.
           EXIT.

      *================================================================*
      * 3000-PROCESS-CLAIM-DETAIL                                       *
      *   FETCHES LINE-LEVEL DETAIL FOR A SELECTED CLAIM               *
      *================================================================*
       3000-PROCESS-CLAIM-DETAIL.

           MOVE CA-SELECTED-CLAIM-ID TO WS-SAVE-CLAIM-ID
           MOVE ZERO TO WS-LINE-INDEX

      *--- OPEN LINE DETAIL CURSOR ---
           EXEC SQL
               OPEN LINE-DETAIL-CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE '90' TO WS-RSP-RETURN-CODE
               MOVE 'DB2 ERROR OPENING LINE CURSOR'
                   TO WS-RSP-ERROR-MSG
               PERFORM 9200-DB2-ERROR-HANDLER
               GO TO 3000-EXIT
           END-IF

      *--- FETCH LINE ROWS ---
           MOVE 'N' TO WS-END-OF-CURSOR-SW
           PERFORM 3100-FETCH-NEXT-LINE
               UNTIL END-OF-CURSOR
               OR WS-LINE-INDEX >= 50

      *--- CLOSE THE LINE CURSOR ---
           EXEC SQL
               CLOSE LINE-DETAIL-CURSOR
           END-EXEC

      *--- FIND THE CLAIM IN THE RESPONSE ARRAY AND SET LINE COUNT ---
           PERFORM VARYING WS-CLAIM-INDEX FROM 1 BY 1
               UNTIL WS-CLAIM-INDEX > WS-RSP-CLAIM-COUNT
               IF WS-RSP-CLM-ID(WS-CLAIM-INDEX) =
                  WS-SAVE-CLAIM-ID
                   MOVE WS-LINE-INDEX
                       TO WS-RSP-CLM-LINE-CT(WS-CLAIM-INDEX)
               END-IF
           END-PERFORM

      *--- ALSO READ VSAM FOR PROVIDER DETAIL ON THIS CLAIM ---
           PERFORM 3200-GET-PROVIDER-DETAIL

      *--- SEND UPDATED MAP ---
           PERFORM 2400-BUILD-DETAIL-MAP
           .

       3000-EXIT.
           EXIT.

      *================================================================*
      * 3100-FETCH-NEXT-LINE                                            *
      *   FETCH ONE CLAIM LINE FROM DB2 CURSOR                         *
      *================================================================*
       3100-FETCH-NEXT-LINE.

           EXEC SQL
               FETCH LINE-DETAIL-CURSOR
               INTO  :HV-LINE-CLAIM-ID,
                     :HV-LINE-SEQ,
                     :HV-LINE-PROC-CODE,
                     :HV-LINE-MODIFIER1  :NI-LINE-MODIFIER1,
                     :HV-LINE-MODIFIER2  :NI-LINE-MODIFIER2,
                     :HV-LINE-REV-CODE   :NI-LINE-REV-CODE,
                     :HV-LINE-FROM-DATE,
                     :HV-LINE-TO-DATE,
                     :HV-LINE-UNITS,
                     :HV-LINE-CHARGES,
                     :HV-LINE-ALLOWED,
                     :HV-LINE-PAID,
                     :HV-LINE-DENY-REASON,
                     :HV-LINE-NDC        :NI-LINE-NDC,
                     :HV-LINE-POS        :NI-LINE-POS
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-LINE-INDEX
                   PERFORM 3110-POPULATE-LINE-RESPONSE
               WHEN +100
                   MOVE 'Y' TO WS-END-OF-CURSOR-SW
               WHEN OTHER
                   MOVE 'Y' TO WS-END-OF-CURSOR-SW
                   PERFORM 9200-DB2-ERROR-HANDLER
           END-EVALUATE
           .

       3100-EXIT.
           EXIT.

      *================================================================*
      * 3110-POPULATE-LINE-RESPONSE                                     *
      *   MAP LINE DETAIL TO RESPONSE STRUCTURE                         *
      *================================================================*
       3110-POPULATE-LINE-RESPONSE.

      *--- FIND THE CLAIM INDEX IN THE ARRAY ---
           PERFORM VARYING WS-CLAIM-INDEX FROM 1 BY 1
               UNTIL WS-CLAIM-INDEX > WS-RSP-CLAIM-COUNT
               OR WS-RSP-CLM-ID(WS-CLAIM-INDEX) =
                  HV-LINE-CLAIM-ID
               CONTINUE
           END-PERFORM

           IF WS-CLAIM-INDEX <= WS-RSP-CLAIM-COUNT
               MOVE HV-LINE-SEQ
                   TO WS-RSP-LN-SEQ
                      (WS-CLAIM-INDEX, WS-LINE-INDEX)
               MOVE HV-LINE-PROC-CODE
                   TO WS-RSP-LN-PROC
                      (WS-CLAIM-INDEX, WS-LINE-INDEX)
               IF NI-LINE-MODIFIER1 >= 0
                   MOVE HV-LINE-MODIFIER1
                       TO WS-RSP-LN-MOD1
                          (WS-CLAIM-INDEX, WS-LINE-INDEX)
               END-IF
               IF NI-LINE-MODIFIER2 >= 0
                   MOVE HV-LINE-MODIFIER2
                       TO WS-RSP-LN-MOD2
                          (WS-CLAIM-INDEX, WS-LINE-INDEX)
               END-IF
               MOVE HV-LINE-FROM-DATE
                   TO WS-RSP-LN-DOS
                      (WS-CLAIM-INDEX, WS-LINE-INDEX)
               MOVE HV-LINE-CHARGES
                   TO WS-RSP-LN-CHG
                      (WS-CLAIM-INDEX, WS-LINE-INDEX)
               MOVE HV-LINE-ALLOWED
                   TO WS-RSP-LN-ALW
                      (WS-CLAIM-INDEX, WS-LINE-INDEX)
               MOVE HV-LINE-PAID
                   TO WS-RSP-LN-PAID
                      (WS-CLAIM-INDEX, WS-LINE-INDEX)
               MOVE HV-LINE-DENY-REASON
                   TO WS-RSP-LN-DENY
                      (WS-CLAIM-INDEX, WS-LINE-INDEX)
           END-IF
           .

       3110-EXIT.
           EXIT.

      *================================================================*
      * 3200-GET-PROVIDER-DETAIL                                        *
      *   READ VSAM FOR FULL PROVIDER DEMOGRAPHIC DETAIL               *
      *   FOR THE SELECTED CLAIM'S BILLING NPI                         *
      *================================================================*
       3200-GET-PROVIDER-DETAIL.

      *--- GET THE BILLING NPI FOR THE SELECTED CLAIM ---
           PERFORM VARYING WS-CLAIM-INDEX FROM 1 BY 1
               UNTIL WS-CLAIM-INDEX > WS-RSP-CLAIM-COUNT
               OR WS-RSP-CLM-ID(WS-CLAIM-INDEX) =
                  WS-SAVE-CLAIM-ID
               CONTINUE
           END-PERFORM

           IF WS-CLAIM-INDEX <= WS-RSP-CLAIM-COUNT
               MOVE WS-RSP-CLM-PRV-NPI(WS-CLAIM-INDEX)
                   TO WS-VSAM-KEY
               PERFORM 2320-ENRICH-WITH-VSAM-PROVIDER
           END-IF
           .

       3200-EXIT.
           EXIT.

      *================================================================*
      * 8000-SEND-ERROR-MAP                                             *
      *   SEND ERROR MESSAGE VIA MAP                                    *
      *================================================================*
       8000-SEND-ERROR-MAP.

           EXEC CICS SEND MAP    ('CDTLM1')
                         MAPSET  ('CDTLMS')
                         FROM    (WS-DETAIL-RESPONSE)
                         ERASE
                         RESP    (WS-CICS-RESP)
                         RESP2   (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-CICS-ERROR-HANDLER
           END-IF
           .

       8000-EXIT.
           EXIT.

      *================================================================*
      * 9000-CICS-ERROR-HANDLER                                         *
      *   HANDLE CICS ERRORS - LOG AND RETURN GRACEFULLY                *
      *================================================================*
       9000-CICS-ERROR-HANDLER.

           MOVE '92' TO WS-RSP-RETURN-CODE
           STRING 'CICS ERROR IN ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ' RESP=' DELIMITED SIZE
               INTO WS-RSP-ERROR-MSG

      *--- WRITE TD QUEUE ERROR LOG ---
           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-RSP-ERROR-MSG)
               LENGTH (80)
               RESP   (WS-CICS-RESP)
           END-EXEC

      *--- ATTEMPT TO SEND ERROR MAP ---
           EXEC CICS SEND MAP    ('CDTLM1')
                         MAPSET  ('CDTLMS')
                         FROM    (WS-DETAIL-RESPONSE)
                         ERASE
                         RESP    (WS-CICS-RESP)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC
           .

       9000-EXIT.
           EXIT.

      *================================================================*
      * 9100-NOT-FOUND-HANDLER                                          *
      *   HANDLE NOTFND CONDITIONS                                      *
      *================================================================*
       9100-NOT-FOUND-HANDLER.

           MOVE '01' TO WS-RSP-RETURN-CODE
           MOVE 'REQUESTED RECORD NOT FOUND'
               TO WS-RSP-ERROR-MSG
           PERFORM 8000-SEND-ERROR-MAP
           .

       9100-EXIT.
           EXIT.

      *================================================================*
      * 9200-DB2-ERROR-HANDLER                                          *
      *   HANDLE DB2 SQL ERRORS - LOG SQLCODE AND SQLERRMC              *
      *================================================================*
       9200-DB2-ERROR-HANDLER.

           MOVE '90' TO WS-RSP-RETURN-CODE

      *--- BUILD DETAILED ERROR MESSAGE WITH SQLCODE ---
           STRING 'DB2 ERROR IN ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ': SQLCODE=' DELIMITED SIZE
               INTO WS-RSP-ERROR-MSG

      *--- LOG TO CICS TRANSIENT DATA QUEUE ---
           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-RSP-ERROR-MSG)
               LENGTH (80)
               RESP   (WS-CICS-RESP)
           END-EXEC

      *--- LOG SQLERRMC FOR ADDITIONAL DIAGNOSTICS ---
           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (SQLERRMC)
               LENGTH (70)
               RESP   (WS-CICS-RESP)
           END-EXEC
           .

       9200-EXIT.
           EXIT.

      *================================================================*
      * 9300-LOG-VSAM-WARNING                                           *
      *   LOG VSAM READ WARNINGS WITHOUT ABORTING TRANSACTION          *
      *================================================================*
       9300-LOG-VSAM-WARNING.

           STRING 'VSAM WARNING IN ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ': NPI=' DELIMITED SIZE
                  WS-VSAM-KEY DELIMITED SIZE
                  ' RESP=' DELIMITED SIZE
               INTO WS-RSP-ERROR-MSG

           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-RSP-ERROR-MSG)
               LENGTH (80)
               RESP   (WS-CICS-RESP)
           END-EXEC
           .

       9300-EXIT.
           EXIT.

      *================================================================*
      * 9500-ABEND-HANDLER                                              *
      *   HANDLE PROGRAM ABENDS - ATTEMPT GRACEFUL CLEANUP              *
      *================================================================*
       9500-ABEND-HANDLER.

      *--- ATTEMPT TO CLOSE ANY OPEN CURSORS ---
           EXEC SQL
               CLOSE CLAIM-HISTORY-CURSOR
           END-EXEC

           EXEC SQL
               CLOSE LINE-DETAIL-CURSOR
           END-EXEC

      *--- LOG THE ABEND ---
           STRING 'ABEND IN PROGRAM ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ' - EIBRESP=' DELIMITED SIZE
               INTO WS-RSP-ERROR-MSG

           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-RSP-ERROR-MSG)
               LENGTH (80)
               RESP   (WS-CICS-RESP)
           END-EXEC

      *--- RETURN WITH ERROR TO CICS ---
           MOVE '92' TO CA-RSP-RETURN-CODE
           MOVE WS-RSP-ERROR-MSG TO CA-RSP-MSG

           EXEC CICS RETURN
           END-EXEC
           .

       9500-EXIT.
           EXIT.
