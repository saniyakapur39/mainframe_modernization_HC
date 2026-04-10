       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXTAPI01.
       AUTHOR.        HEALTHCARE PAYER SYSTEMS.
       DATE-WRITTEN.  2024-01-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM:     EXTAPI01                                          *
      * DESCRIPTION: CICS PROGRAM THAT CALLS EXTERNAL REST APIS       *
      *              USING EXEC CICS WEB CONVERSE FOR REAL-TIME        *
      *              CLAIM PRICING FROM AN EXTERNAL PRICING SERVICE    *
      *                                                                *
      * THIS PROGRAM DEMONSTRATES:                                     *
      *  1. CICS WEB SUPPORT (EXEC CICS WEB CONVERSE) FOR HTTP        *
      *     CALLS TO EXTERNAL REST ENDPOINTS                           *
      *  2. CICS CHANNELS AND CONTAINERS INSTEAD OF COMMAREA          *
      *     (BYPASSES THE 32KB COMMAREA LIMIT)                        *
      *  3. MANUAL JSON CONSTRUCTION AND PARSING IN COBOL              *
      *  4. HTTP RESPONSE CODE HANDLING                                *
      *  5. TIMEOUT AND RETRY LOGIC FOR EXTERNAL SERVICE CALLS        *
      *                                                                *
      * BUSINESS CONTEXT:                                              *
      *  THE HEALTH PLAN CONTRACTS WITH AN EXTERNAL PRICING VENDOR    *
      *  FOR SPECIALTY DRUG AND OUT-OF-NETWORK PRICING. THIS PROGRAM  *
      *  CONSTRUCTS A JSON REQUEST WITH CLAIM/LINE DETAILS, CALLS     *
      *  THE VENDOR'S REST API, PARSES THE JSON RESPONSE, AND         *
      *  RETURNS PRICING DATA VIA CICS CHANNELS/CONTAINERS.           *
      *                                                                *
      * TRANSACTION: XPRC                                              *
      *                                                                *
      * DEPENDENCIES:                                                  *
      *  - COPYBOOKS: HCCOMMON, CLMCOPY, EDI837CP, ABORTWSC          *
      *  - URIMAP:   PRICINGAPI (DEFINED IN CICS CSD)                *
      *  - CHANNEL:  PRICING-CHANNEL                                   *
      *  - CONTAINERS: REQUEST-DATA, RESPONSE-DATA, ERROR-DATA        *
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
       01  WS-PROGRAM-ID              PIC X(08)  VALUE 'EXTAPI01'.
       01  WS-PROGRAM-VERSION         PIC X(06)  VALUE '01.00 '.
       01  WS-TRANSACTION-ID          PIC X(04)  VALUE 'XPRC'.

      *----------------------------------------------------------------*
      * COMMON HEALTHCARE FIELDS                                        *
      *----------------------------------------------------------------*
           COPY HCCOMMON.

      *----------------------------------------------------------------*
      * CLAIMS DATA STRUCTURES                                          *
      *----------------------------------------------------------------*
           COPY CLMCOPY.

      *----------------------------------------------------------------*
      * ERROR HANDLING                                                  *
      *----------------------------------------------------------------*
           COPY ABORTWSC.

      *----------------------------------------------------------------*
      * CICS RESPONSE CODES                                             *
      *----------------------------------------------------------------*
       01  WS-CICS-RESP               PIC S9(08) COMP VALUE ZERO.
       01  WS-CICS-RESP2              PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * PROCESSING FLAGS AND SWITCHES                                   *
      *----------------------------------------------------------------*
       01  WS-FLAGS.
           05  WS-API-SUCCESS-SW       PIC X(01) VALUE 'N'.
               88 API-CALL-SUCCESS                VALUE 'Y'.
               88 API-CALL-FAILED                 VALUE 'N'.
           05  WS-RETRY-EXHAUSTED-SW   PIC X(01) VALUE 'N'.
               88 RETRIES-EXHAUSTED               VALUE 'Y'.
               88 RETRIES-REMAINING               VALUE 'N'.
           05  WS-JSON-PARSE-OK-SW     PIC X(01) VALUE 'N'.
               88 JSON-PARSE-OK                   VALUE 'Y'.
               88 JSON-PARSE-FAILED               VALUE 'N'.
           05  WS-CONTAINER-OK-SW      PIC X(01) VALUE 'N'.
               88 CONTAINER-OK                    VALUE 'Y'.
               88 CONTAINER-FAILED                VALUE 'N'.

      *----------------------------------------------------------------*
      * API CONFIGURATION                                               *
      *----------------------------------------------------------------*
       01  WS-API-CONFIG.
           05  WS-API-URIMAP          PIC X(08)  VALUE 'PRCAPI  '.
           05  WS-API-PATH            PIC X(80)
               VALUE '/api/v2/pricing/claim-estimate'.
           05  WS-API-PATH-LEN        PIC S9(08) COMP VALUE 36.
           05  WS-API-HOST            PIC X(60)
               VALUE 'pricing-vendor.healthplan.com'.
           05  WS-API-HOST-LEN        PIC S9(08) COMP VALUE 28.
           05  WS-API-SCHEME          PIC X(05)  VALUE 'HTTPS'.
           05  WS-API-PORT            PIC S9(08) COMP VALUE 443.
           05  WS-API-TIMEOUT         PIC S9(08) COMP VALUE 30.
           05  WS-API-MAX-RETRIES     PIC S9(04) COMP VALUE 3.
           05  WS-API-RETRY-COUNT     PIC S9(04) COMP VALUE ZERO.
           05  WS-API-RETRY-WAIT-SECS PIC S9(04) COMP VALUE 2.

      *----------------------------------------------------------------*
      * HTTP REQUEST/RESPONSE AREAS                                     *
      *----------------------------------------------------------------*
       01  WS-HTTP-REQUEST.
           05  WS-REQ-METHOD          PIC X(04)  VALUE 'POST'.
           05  WS-REQ-MEDIA-TYPE      PIC X(56)
               VALUE 'application/json'.
           05  WS-REQ-MEDIA-LEN       PIC S9(08) COMP VALUE 16.
           05  WS-REQ-AUTH-HEADER     PIC X(80).
           05  WS-REQ-AUTH-HEADER-LEN PIC S9(08) COMP VALUE ZERO.
           05  WS-REQ-CORRELID        PIC X(20).

       01  WS-HTTP-RESPONSE.
           05  WS-RSP-STATUS-CODE     PIC S9(04) COMP VALUE ZERO.
               88 HTTP-OK                         VALUE 200.
               88 HTTP-CREATED                    VALUE 201.
               88 HTTP-BAD-REQUEST                VALUE 400.
               88 HTTP-UNAUTHORIZED               VALUE 401.
               88 HTTP-NOT-FOUND                  VALUE 404.
               88 HTTP-TIMEOUT                    VALUE 408.
               88 HTTP-TOO-MANY-REQUESTS          VALUE 429.
               88 HTTP-SERVER-ERROR               VALUE 500.
               88 HTTP-SERVICE-UNAVAILABLE        VALUE 503.
           05  WS-RSP-MEDIA-TYPE      PIC X(56).
           05  WS-RSP-CONTENT-LEN     PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * JSON REQUEST BUFFER (UP TO 16KB)                                *
      *----------------------------------------------------------------*
       01  WS-JSON-REQUEST.
           05  WS-JSON-REQ-BUFFER     PIC X(16384).
           05  WS-JSON-REQ-LEN        PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * JSON RESPONSE BUFFER (UP TO 32KB)                               *
      *----------------------------------------------------------------*
       01  WS-JSON-RESPONSE.
           05  WS-JSON-RSP-BUFFER     PIC X(32768).
           05  WS-JSON-RSP-LEN        PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * JSON CONSTRUCTION WORK AREAS                                    *
      *----------------------------------------------------------------*
       01  WS-JSON-WORK.
           05  WS-JSON-PTR            PIC S9(08) COMP VALUE 1.
           05  WS-JSON-TEMP           PIC X(256).
           05  WS-JSON-NUM-DISP       PIC Z(7)9.99-.
           05  WS-JSON-INT-DISP       PIC Z(4)9-.
           05  WS-JSON-UNIT-DISP      PIC Z(4)9.99-.
           05  WS-QUOTE               PIC X(01)  VALUE '"'.
           05  WS-COMMA               PIC X(01)  VALUE ','.
           05  WS-COLON               PIC X(01)  VALUE ':'.
           05  WS-OPEN-BRACE          PIC X(01)  VALUE '{'.
           05  WS-CLOSE-BRACE         PIC X(01)  VALUE '}'.
           05  WS-OPEN-BRACKET        PIC X(01)  VALUE '['.
           05  WS-CLOSE-BRACKET       PIC X(01)  VALUE ']'.

      *----------------------------------------------------------------*
      * JSON RESPONSE PARSING WORK AREAS                                *
      *----------------------------------------------------------------*
       01  WS-PARSE-WORK.
           05  WS-PARSE-POS           PIC S9(08) COMP VALUE 1.
           05  WS-PARSE-KEY           PIC X(50).
           05  WS-PARSE-VALUE         PIC X(256).
           05  WS-PARSE-NUM           PIC S9(09)V99 COMP-3.
           05  WS-PARSE-DEPTH         PIC S9(04) COMP VALUE ZERO.
           05  WS-PARSE-IN-STRING-SW  PIC X(01) VALUE 'N'.
               88 IN-JSON-STRING                  VALUE 'Y'.
               88 NOT-IN-JSON-STRING              VALUE 'N'.
           05  WS-PARSE-ARRAY-IDX     PIC S9(04) COMP VALUE ZERO.
           05  WS-TOKEN-START         PIC S9(08) COMP VALUE ZERO.
           05  WS-TOKEN-END           PIC S9(08) COMP VALUE ZERO.
           05  WS-TOKEN-LEN           PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * PRICING REQUEST DATA                                            *
      *----------------------------------------------------------------*
       01  WS-PRICING-REQUEST.
           05  WS-PRQ-CLAIM-ID        PIC X(15).
           05  WS-PRQ-CLAIM-TYPE      PIC X(02).
           05  WS-PRQ-MEMBER-ID       PIC X(12).
           05  WS-PRQ-BILLING-NPI     PIC X(10).
           05  WS-PRQ-RENDERING-NPI   PIC X(10).
           05  WS-PRQ-PLAN-CODE       PIC X(08).
           05  WS-PRQ-NETWORK-ID      PIC X(04).
           05  WS-PRQ-LINE-COUNT      PIC S9(04) COMP VALUE ZERO.
           05  WS-PRQ-LINES OCCURS 50 TIMES.
               10  WS-PRQ-LN-SEQ      PIC S9(04) COMP.
               10  WS-PRQ-LN-PROC     PIC X(05).
               10  WS-PRQ-LN-MOD1     PIC X(02).
               10  WS-PRQ-LN-MOD2     PIC X(02).
               10  WS-PRQ-LN-REV-CODE PIC X(04).
               10  WS-PRQ-LN-FROM-DT  PIC X(10).
               10  WS-PRQ-LN-TO-DT    PIC X(10).
               10  WS-PRQ-LN-CHARGES  PIC S9(07)V99 COMP-3.
               10  WS-PRQ-LN-UNITS    PIC S9(05)V99 COMP-3.
               10  WS-PRQ-LN-POS      PIC X(02).
               10  WS-PRQ-LN-NDC      PIC X(11).
               10  WS-PRQ-LN-DIAG1    PIC X(08).

      *----------------------------------------------------------------*
      * PRICING RESPONSE DATA                                           *
      *----------------------------------------------------------------*
       01  WS-PRICING-RESPONSE.
           05  WS-PRS-RETURN-CODE     PIC X(02).
               88 PRS-SUCCESS                     VALUE '00'.
               88 PRS-PARTIAL                     VALUE '01'.
               88 PRS-DENIED                      VALUE '40'.
               88 PRS-API-ERROR                   VALUE '90'.
               88 PRS-TIMEOUT                     VALUE '91'.
               88 PRS-PARSE-ERROR                 VALUE '92'.
           05  WS-PRS-MESSAGE         PIC X(80).
           05  WS-PRS-VENDOR-REF      PIC X(20).
           05  WS-PRS-TOTAL-ALLOWED   PIC S9(09)V99 COMP-3
                                       VALUE ZERO.
           05  WS-PRS-TOTAL-PAID      PIC S9(09)V99 COMP-3
                                       VALUE ZERO.
           05  WS-PRS-LINE-COUNT      PIC S9(04) COMP VALUE ZERO.
           05  WS-PRS-LINES OCCURS 50 TIMES.
               10  WS-PRS-LN-SEQ      PIC S9(04) COMP.
               10  WS-PRS-LN-ALLOWED  PIC S9(07)V99 COMP-3.
               10  WS-PRS-LN-PAID     PIC S9(07)V99 COMP-3.
               10  WS-PRS-LN-DENY-RSN PIC X(05).
               10  WS-PRS-LN-METHOD   PIC X(10).
               10  WS-PRS-LN-FEE-SCHED
                                       PIC X(08).

      *----------------------------------------------------------------*
      * CHANNEL AND CONTAINER NAMES                                     *
      *----------------------------------------------------------------*
       01  WS-CHANNEL-NAME            PIC X(16)
               VALUE 'PRICING-CHANNEL '.
       01  WS-CTR-REQUEST             PIC X(16)
               VALUE 'REQUEST-DATA    '.
       01  WS-CTR-RESPONSE            PIC X(16)
               VALUE 'RESPONSE-DATA   '.
       01  WS-CTR-ERROR               PIC X(16)
               VALUE 'ERROR-DATA      '.
       01  WS-CTR-STATUS              PIC X(16)
               VALUE 'STATUS-DATA     '.

      *----------------------------------------------------------------*
      * API AUTHENTICATION TOKEN                                        *
      *----------------------------------------------------------------*
       01  WS-AUTH-TOKEN.
           05  WS-AUTH-PREFIX          PIC X(07) VALUE 'Bearer '.
           05  WS-AUTH-TOKEN-VALUE     PIC X(256).
           05  WS-AUTH-TOKEN-LEN       PIC S9(08) COMP VALUE ZERO.
           05  WS-AUTH-FULL-HEADER     PIC X(280).
           05  WS-AUTH-FULL-LEN        PIC S9(08) COMP VALUE ZERO.

      *----------------------------------------------------------------*
      * SESSION TOKEN RETRIEVED FROM CONTAINER                          *
      *----------------------------------------------------------------*
       01  WS-SESSION-DATA.
           05  WS-SESSION-TOKEN        PIC X(256).
           05  WS-SESSION-EXPIRY       PIC X(26).
           05  WS-SESSION-TENANT       PIC X(10).

      *----------------------------------------------------------------*
      * WORK VARIABLES                                                  *
      *----------------------------------------------------------------*
       01  WS-WORK-AREAS.
           05  WS-LINE-IDX            PIC S9(04) COMP VALUE ZERO.
           05  WS-CHAR-IDX            PIC S9(08) COMP VALUE ZERO.
           05  WS-SEARCH-START        PIC S9(08) COMP VALUE ZERO.
           05  WS-FOUND-POS           PIC S9(08) COMP VALUE ZERO.
           05  WS-EXTRACT-LEN         PIC S9(08) COMP VALUE ZERO.
           05  WS-ABSTIME             PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-TIMESTAMP           PIC X(26)  VALUE SPACES.
           05  WS-CONTAINER-LEN       PIC S9(08) COMP VALUE ZERO.

      *================================================================*
       PROCEDURE DIVISION.

      *================================================================*
      * 0000-MAIN-CONTROL                                               *
      *   ORCHESTRATES THE EXTERNAL PRICING API CALL FLOW              *
      *================================================================*
       0000-MAIN-CONTROL.

           EXEC CICS HANDLE ABEND
               LABEL   (9500-ABEND-HANDLER)
           END-EXEC

           PERFORM 1000-INITIALIZE
           PERFORM 2000-GET-REQUEST-FROM-CONTAINER
           IF CONTAINER-OK
               PERFORM 3000-BUILD-JSON-REQUEST
               PERFORM 4000-CALL-PRICING-API
               IF API-CALL-SUCCESS
                   PERFORM 5000-PARSE-JSON-RESPONSE
                   IF JSON-PARSE-OK
                       PERFORM 6000-PUT-RESPONSE-CONTAINER
                   ELSE
                       PERFORM 6500-PUT-ERROR-CONTAINER
                   END-IF
               ELSE
                   PERFORM 6500-PUT-ERROR-CONTAINER
               END-IF
           END-IF

           PERFORM 7000-RETURN-TO-CALLER
           .

       0000-EXIT.
           EXIT.

      *================================================================*
      * 1000-INITIALIZE                                                 *
      *   SET UP WORKING STORAGE AND GET CURRENT TIMESTAMP              *
      *================================================================*
       1000-INITIALIZE.

           INITIALIZE WS-PRICING-REQUEST
           INITIALIZE WS-PRICING-RESPONSE
           INITIALIZE WS-JSON-REQUEST
           INITIALIZE WS-JSON-RESPONSE
           MOVE 'N' TO WS-API-SUCCESS-SW
           MOVE 'N' TO WS-RETRY-EXHAUSTED-SW
           MOVE 'N' TO WS-JSON-PARSE-OK-SW
           MOVE 'N' TO WS-CONTAINER-OK-SW
           MOVE ZERO TO WS-API-RETRY-COUNT

      *--- GET CURRENT TIMESTAMP FOR CORRELATION ---
           EXEC CICS ASKTIME
               ABSTIME(WS-ABSTIME)
           END-EXEC

           EXEC CICS FORMATTIME
               ABSTIME (WS-ABSTIME)
               YYYYMMDD(WS-TIMESTAMP)
               TIME    (WS-TIMESTAMP)
               DATESEP ('/')
               TIMESEP (':')
           END-EXEC

      *--- BUILD CORRELATION ID ---
           STRING WS-PROGRAM-ID DELIMITED SIZE
                  '-' DELIMITED SIZE
                  WS-TIMESTAMP(1:8) DELIMITED SIZE
                  '-' DELIMITED SIZE
               INTO WS-REQ-CORRELID
           .

       1000-EXIT.
           EXIT.

      *================================================================*
      * 2000-GET-REQUEST-FROM-CONTAINER                                 *
      *   RETRIEVE PRICING REQUEST DATA FROM CICS CHANNEL/CONTAINER    *
      *================================================================*
       2000-GET-REQUEST-FROM-CONTAINER.

           MOVE LENGTH OF WS-PRICING-REQUEST TO WS-CONTAINER-LEN

           EXEC CICS GET CONTAINER(WS-CTR-REQUEST)
               CHANNEL (WS-CHANNEL-NAME)
               INTO    (WS-PRICING-REQUEST)
               FLENGTH (WS-CONTAINER-LEN)
               RESP    (WS-CICS-RESP)
               RESP2   (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP = DFHRESP(NORMAL)
               MOVE 'Y' TO WS-CONTAINER-OK-SW
           ELSE
               MOVE 'N' TO WS-CONTAINER-OK-SW
               MOVE '92' TO WS-PRS-RETURN-CODE
               STRING 'FAILED TO GET REQUEST CONTAINER: RESP='
                      DELIMITED SIZE
                   INTO WS-PRS-MESSAGE
               PERFORM 6500-PUT-ERROR-CONTAINER
           END-IF

      *--- ALSO RETRIEVE SESSION/AUTH TOKEN IF AVAILABLE ---
           IF CONTAINER-OK
               MOVE LENGTH OF WS-SESSION-DATA TO WS-CONTAINER-LEN
               EXEC CICS GET CONTAINER(WS-CTR-STATUS)
                   CHANNEL (WS-CHANNEL-NAME)
                   INTO    (WS-SESSION-DATA)
                   FLENGTH (WS-CONTAINER-LEN)
                   RESP    (WS-CICS-RESP)
                   RESP2   (WS-CICS-RESP2)
               END-EXEC

               IF WS-CICS-RESP = DFHRESP(NORMAL)
                   MOVE WS-SESSION-TOKEN TO WS-AUTH-TOKEN-VALUE
                   STRING WS-AUTH-PREFIX DELIMITED SIZE
                          WS-AUTH-TOKEN-VALUE DELIMITED SPACES
                       INTO WS-AUTH-FULL-HEADER
                   MOVE FUNCTION LENGTH(
                       FUNCTION TRIM(WS-AUTH-FULL-HEADER
                                     TRAILING))
                       TO WS-AUTH-FULL-LEN
               ELSE
      *---         NO AUTH TOKEN - PROCEED WITHOUT IT
                   MOVE SPACES TO WS-AUTH-FULL-HEADER
                   MOVE ZERO TO WS-AUTH-FULL-LEN
               END-IF
           END-IF
           .

       2000-EXIT.
           EXIT.

      *================================================================*
      * 3000-BUILD-JSON-REQUEST                                         *
      *   CONSTRUCT JSON PAYLOAD FOR THE EXTERNAL PRICING API          *
      *   THIS IS MANUAL JSON BUILDING - A KEY MODERNIZATION           *
      *   CHALLENGE SINCE COBOL HAS NO NATIVE JSON SUPPORT             *
      *================================================================*
       3000-BUILD-JSON-REQUEST.

           MOVE 1 TO WS-JSON-PTR
           INITIALIZE WS-JSON-REQ-BUFFER

      *--- OPEN ROOT OBJECT ---
           STRING '{"claimRequest":{'
               DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

      *--- CLAIM HEADER FIELDS ---
           STRING '"claimId":"' DELIMITED SIZE
               WS-PRQ-CLAIM-ID DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"claimType":"' DELIMITED SIZE
               WS-PRQ-CLAIM-TYPE DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"memberId":"' DELIMITED SIZE
               WS-PRQ-MEMBER-ID DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"billingNpi":"' DELIMITED SIZE
               WS-PRQ-BILLING-NPI DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"renderingNpi":"' DELIMITED SIZE
               WS-PRQ-RENDERING-NPI DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"planCode":"' DELIMITED SIZE
               WS-PRQ-PLAN-CODE DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"networkId":"' DELIMITED SIZE
               WS-PRQ-NETWORK-ID DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

      *--- CORRELATION ID ---
           STRING '"correlationId":"' DELIMITED SIZE
               WS-REQ-CORRELID DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

      *--- SERVICE LINES ARRAY ---
           STRING '"serviceLines":[' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           PERFORM VARYING WS-LINE-IDX FROM 1 BY 1
               UNTIL WS-LINE-IDX > WS-PRQ-LINE-COUNT
               PERFORM 3100-BUILD-LINE-JSON
           END-PERFORM

      *--- REMOVE TRAILING COMMA IF LINES WERE ADDED ---
           IF WS-PRQ-LINE-COUNT > ZERO
               SUBTRACT 1 FROM WS-JSON-PTR
           END-IF

      *--- CLOSE ARRAYS AND OBJECTS ---
           STRING ']}}' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           SUBTRACT 1 FROM WS-JSON-PTR
           MOVE WS-JSON-PTR TO WS-JSON-REQ-LEN
           .

       3000-EXIT.
           EXIT.

      *================================================================*
      * 3100-BUILD-LINE-JSON                                            *
      *   BUILD JSON FOR A SINGLE SERVICE LINE                          *
      *================================================================*
       3100-BUILD-LINE-JSON.

           MOVE WS-PRQ-LN-SEQ(WS-LINE-IDX) TO WS-JSON-INT-DISP
           MOVE WS-PRQ-LN-CHARGES(WS-LINE-IDX) TO WS-JSON-NUM-DISP
           MOVE WS-PRQ-LN-UNITS(WS-LINE-IDX) TO WS-JSON-UNIT-DISP

           STRING '{"lineSeq":' DELIMITED SIZE
               FUNCTION TRIM(WS-JSON-INT-DISP LEADING)
               DELIMITED SIZE
               ',' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"procedureCode":"' DELIMITED SIZE
               WS-PRQ-LN-PROC(WS-LINE-IDX) DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           IF WS-PRQ-LN-MOD1(WS-LINE-IDX) NOT = SPACES
               STRING '"modifier1":"' DELIMITED SIZE
                   WS-PRQ-LN-MOD1(WS-LINE-IDX) DELIMITED SPACES
                   '",' DELIMITED SIZE
                   INTO WS-JSON-REQ-BUFFER
                   WITH POINTER WS-JSON-PTR
           END-IF

           IF WS-PRQ-LN-MOD2(WS-LINE-IDX) NOT = SPACES
               STRING '"modifier2":"' DELIMITED SIZE
                   WS-PRQ-LN-MOD2(WS-LINE-IDX) DELIMITED SPACES
                   '",' DELIMITED SIZE
                   INTO WS-JSON-REQ-BUFFER
                   WITH POINTER WS-JSON-PTR
           END-IF

           IF WS-PRQ-LN-REV-CODE(WS-LINE-IDX) NOT = SPACES
               STRING '"revenueCode":"' DELIMITED SIZE
                   WS-PRQ-LN-REV-CODE(WS-LINE-IDX) DELIMITED SPACES
                   '",' DELIMITED SIZE
                   INTO WS-JSON-REQ-BUFFER
                   WITH POINTER WS-JSON-PTR
           END-IF

           STRING '"fromDate":"' DELIMITED SIZE
               WS-PRQ-LN-FROM-DT(WS-LINE-IDX) DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"toDate":"' DELIMITED SIZE
               WS-PRQ-LN-TO-DT(WS-LINE-IDX) DELIMITED SPACES
               '",' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"billedAmount":' DELIMITED SIZE
               FUNCTION TRIM(WS-JSON-NUM-DISP LEADING)
               DELIMITED SIZE
               ',' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           STRING '"units":' DELIMITED SIZE
               FUNCTION TRIM(WS-JSON-UNIT-DISP LEADING)
               DELIMITED SIZE
               ',' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR

           IF WS-PRQ-LN-POS(WS-LINE-IDX) NOT = SPACES
               STRING '"placeOfService":"' DELIMITED SIZE
                   WS-PRQ-LN-POS(WS-LINE-IDX) DELIMITED SIZE
                   '",' DELIMITED SIZE
                   INTO WS-JSON-REQ-BUFFER
                   WITH POINTER WS-JSON-PTR
           END-IF

           IF WS-PRQ-LN-NDC(WS-LINE-IDX) NOT = SPACES
               STRING '"ndcCode":"' DELIMITED SIZE
                   WS-PRQ-LN-NDC(WS-LINE-IDX) DELIMITED SPACES
                   '",' DELIMITED SIZE
                   INTO WS-JSON-REQ-BUFFER
                   WITH POINTER WS-JSON-PTR
           END-IF

           IF WS-PRQ-LN-DIAG1(WS-LINE-IDX) NOT = SPACES
               STRING '"primaryDiagnosis":"' DELIMITED SIZE
                   WS-PRQ-LN-DIAG1(WS-LINE-IDX) DELIMITED SPACES
                   '",' DELIMITED SIZE
                   INTO WS-JSON-REQ-BUFFER
                   WITH POINTER WS-JSON-PTR
           END-IF

      *--- REMOVE TRAILING COMMA AND CLOSE LINE OBJECT ---
           SUBTRACT 1 FROM WS-JSON-PTR
           STRING '},' DELIMITED SIZE
               INTO WS-JSON-REQ-BUFFER
               WITH POINTER WS-JSON-PTR
           .

       3100-EXIT.
           EXIT.

      *================================================================*
      * 4000-CALL-PRICING-API                                           *
      *   EXECUTE HTTP POST TO EXTERNAL PRICING SERVICE                *
      *   WITH RETRY LOGIC FOR TRANSIENT FAILURES                      *
      *================================================================*
       4000-CALL-PRICING-API.

           MOVE 'N' TO WS-API-SUCCESS-SW
           MOVE ZERO TO WS-API-RETRY-COUNT

           PERFORM 4100-EXECUTE-WEB-CONVERSE
           PERFORM UNTIL API-CALL-SUCCESS
               OR RETRIES-EXHAUSTED
               ADD 1 TO WS-API-RETRY-COUNT
               IF WS-API-RETRY-COUNT >= WS-API-MAX-RETRIES
                   MOVE 'Y' TO WS-RETRY-EXHAUSTED-SW
               ELSE
      *---         WAIT BEFORE RETRY ---
                   EXEC CICS DELAY
                       FOR SECONDS(WS-API-RETRY-WAIT-SECS)
                       RESP (WS-CICS-RESP)
                   END-EXEC
                   PERFORM 4100-EXECUTE-WEB-CONVERSE
               END-IF
           END-PERFORM

           IF RETRIES-EXHAUSTED AND NOT API-CALL-SUCCESS
               MOVE '91' TO WS-PRS-RETURN-CODE
               STRING 'PRICING API CALL FAILED AFTER '
                      DELIMITED SIZE
                      ' RETRIES' DELIMITED SIZE
                   INTO WS-PRS-MESSAGE
           END-IF
           .

       4000-EXIT.
           EXIT.

      *================================================================*
      * 4100-EXECUTE-WEB-CONVERSE                                       *
      *   SINGLE HTTP REQUEST/RESPONSE CYCLE VIA CICS WEB CONVERSE    *
      *================================================================*
       4100-EXECUTE-WEB-CONVERSE.

      *--- SET UP HTTP HEADERS ---
           EXEC CICS WEB OPEN
               HOST      (WS-API-HOST)
               HOSTLENGTH(WS-API-HOST-LEN)
               PORTNUMBER(WS-API-PORT)
               SCHEME    (HTTPS)
               SESSTOKEN (WS-REQ-CORRELID)
               RESP      (WS-CICS-RESP)
               RESP2     (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               MOVE '90' TO WS-PRS-RETURN-CODE
               STRING 'WEB OPEN FAILED: RESP='
                      DELIMITED SIZE
                   INTO WS-PRS-MESSAGE
               GO TO 4100-EXIT
           END-IF

      *--- ADD AUTHORIZATION HEADER IF TOKEN AVAILABLE ---
           IF WS-AUTH-FULL-LEN > ZERO
               EXEC CICS WEB WRITE HTTPHEADER
                   HTTPHEADER ('Authorization')
                   NAMELENGTH (13)
                   VALUE      (WS-AUTH-FULL-HEADER)
                   VALUELENGTH(WS-AUTH-FULL-LEN)
                   SESSTOKEN  (WS-REQ-CORRELID)
                   RESP       (WS-CICS-RESP)
               END-EXEC
           END-IF

      *--- ADD CONTENT-TYPE HEADER ---
           EXEC CICS WEB WRITE HTTPHEADER
               HTTPHEADER ('Content-Type')
               NAMELENGTH (12)
               VALUE      (WS-REQ-MEDIA-TYPE)
               VALUELENGTH(WS-REQ-MEDIA-LEN)
               SESSTOKEN  (WS-REQ-CORRELID)
               RESP       (WS-CICS-RESP)
           END-EXEC

      *--- ADD CORRELATION ID HEADER ---
           EXEC CICS WEB WRITE HTTPHEADER
               HTTPHEADER ('X-Correlation-Id')
               NAMELENGTH (16)
               VALUE      (WS-REQ-CORRELID)
               VALUELENGTH(20)
               SESSTOKEN  (WS-REQ-CORRELID)
               RESP       (WS-CICS-RESP)
           END-EXEC

      *--- SEND REQUEST AND RECEIVE RESPONSE ---
           EXEC CICS WEB CONVERSE
               SESSTOKEN  (WS-REQ-CORRELID)
               PATH       (WS-API-PATH)
               PATHLENGTH (WS-API-PATH-LEN)
               METHOD     (POST)
               MEDIATYPE  (WS-REQ-MEDIA-TYPE)
               FROM       (WS-JSON-REQ-BUFFER)
               FROMLENGTH (WS-JSON-REQ-LEN)
               INTO       (WS-JSON-RSP-BUFFER)
               TOLENGTH   (WS-JSON-RSP-LEN)
               STATUSCODE (WS-RSP-STATUS-CODE)
               RESP       (WS-CICS-RESP)
               RESP2      (WS-CICS-RESP2)
           END-EXEC

      *--- CLOSE THE WEB SESSION ---
           EXEC CICS WEB CLOSE
               SESSTOKEN (WS-REQ-CORRELID)
               RESP      (WS-CICS-RESP)
           END-EXEC

      *--- EVALUATE HTTP RESPONSE ---
           PERFORM 4200-EVALUATE-HTTP-RESPONSE
           .

       4100-EXIT.
           EXIT.

      *================================================================*
      * 4200-EVALUATE-HTTP-RESPONSE                                     *
      *   DETERMINE SUCCESS/FAILURE BASED ON HTTP STATUS CODE          *
      *================================================================*
       4200-EVALUATE-HTTP-RESPONSE.

           EVALUATE TRUE
               WHEN HTTP-OK
               WHEN HTTP-CREATED
                   MOVE 'Y' TO WS-API-SUCCESS-SW
                   MOVE '00' TO WS-PRS-RETURN-CODE
               WHEN HTTP-BAD-REQUEST
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE 'Y' TO WS-RETRY-EXHAUSTED-SW
                   MOVE '90' TO WS-PRS-RETURN-CODE
                   MOVE 'BAD REQUEST (400) FROM PRICING API'
                       TO WS-PRS-MESSAGE
               WHEN HTTP-UNAUTHORIZED
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE 'Y' TO WS-RETRY-EXHAUSTED-SW
                   MOVE '90' TO WS-PRS-RETURN-CODE
                   MOVE 'UNAUTHORIZED (401) - CHECK API TOKEN'
                       TO WS-PRS-MESSAGE
               WHEN HTTP-NOT-FOUND
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE 'Y' TO WS-RETRY-EXHAUSTED-SW
                   MOVE '90' TO WS-PRS-RETURN-CODE
                   MOVE 'PRICING ENDPOINT NOT FOUND (404)'
                       TO WS-PRS-MESSAGE
               WHEN HTTP-TIMEOUT
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE '91' TO WS-PRS-RETURN-CODE
                   MOVE 'REQUEST TIMEOUT (408) - WILL RETRY'
                       TO WS-PRS-MESSAGE
               WHEN HTTP-TOO-MANY-REQUESTS
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE '91' TO WS-PRS-RETURN-CODE
                   MOVE 'RATE LIMITED (429) - WILL RETRY'
                       TO WS-PRS-MESSAGE
               WHEN HTTP-SERVER-ERROR
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE '90' TO WS-PRS-RETURN-CODE
                   MOVE 'SERVER ERROR (500) - WILL RETRY'
                       TO WS-PRS-MESSAGE
               WHEN HTTP-SERVICE-UNAVAILABLE
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE '91' TO WS-PRS-RETURN-CODE
                   MOVE 'SERVICE UNAVAILABLE (503) - WILL RETRY'
                       TO WS-PRS-MESSAGE
               WHEN OTHER
                   MOVE 'N' TO WS-API-SUCCESS-SW
                   MOVE '90' TO WS-PRS-RETURN-CODE
                   STRING 'UNEXPECTED HTTP STATUS: '
                       DELIMITED SIZE
                       INTO WS-PRS-MESSAGE
           END-EVALUATE
           .

       4200-EXIT.
           EXIT.

      *================================================================*
      * 5000-PARSE-JSON-RESPONSE                                        *
      *   PARSE THE JSON RESPONSE BODY INTO WORKING STORAGE             *
      *   THIS IS MANUAL JSON PARSING - ANOTHER KEY MODERNIZATION      *
      *   CHALLENGE SINCE COBOL HAS NO NATIVE JSON PARSER              *
      *================================================================*
       5000-PARSE-JSON-RESPONSE.

           MOVE 'N' TO WS-JSON-PARSE-OK-SW
           INITIALIZE WS-PRICING-RESPONSE
           MOVE 1 TO WS-PARSE-POS

      *--- EXTRACT VENDOR REFERENCE ---
           PERFORM 5100-EXTRACT-STRING-VALUE
           IF WS-PARSE-VALUE NOT = SPACES
               MOVE WS-PARSE-VALUE(1:20) TO WS-PRS-VENDOR-REF
           END-IF

      *--- EXTRACT TOTAL ALLOWED ---
           MOVE 'totalAllowed' TO WS-PARSE-KEY
           PERFORM 5200-EXTRACT-NUMERIC-VALUE
           MOVE WS-PARSE-NUM TO WS-PRS-TOTAL-ALLOWED

      *--- EXTRACT TOTAL PAID ---
           MOVE 'totalPaid' TO WS-PARSE-KEY
           PERFORM 5200-EXTRACT-NUMERIC-VALUE
           MOVE WS-PARSE-NUM TO WS-PRS-TOTAL-PAID

      *--- EXTRACT LINE-LEVEL PRICING ---
           PERFORM 5300-PARSE-LINE-ARRAY

           IF WS-PRS-LINE-COUNT > ZERO
               MOVE 'Y' TO WS-JSON-PARSE-OK-SW
               MOVE '00' TO WS-PRS-RETURN-CODE
           ELSE
               MOVE '92' TO WS-PRS-RETURN-CODE
               MOVE 'FAILED TO PARSE LINE PRICING FROM RESPONSE'
                   TO WS-PRS-MESSAGE
           END-IF
           .

       5000-EXIT.
           EXIT.

      *================================================================*
      * 5100-EXTRACT-STRING-VALUE                                       *
      *   FIND A KEY IN JSON AND EXTRACT ITS STRING VALUE               *
      *================================================================*
       5100-EXTRACT-STRING-VALUE.

           MOVE SPACES TO WS-PARSE-VALUE

      *--- SEARCH FOR THE KEY IN THE JSON BUFFER ---
           MOVE 1 TO WS-SEARCH-START

           INSPECT WS-JSON-RSP-BUFFER
               TALLYING WS-FOUND-POS
               FOR ALL WS-PARSE-KEY

           IF WS-FOUND-POS > ZERO
      *---     FIND THE OPENING QUOTE OF THE VALUE ---
               MOVE 1 TO WS-SEARCH-START
               PERFORM VARYING WS-CHAR-IDX
                   FROM WS-SEARCH-START BY 1
                   UNTIL WS-CHAR-IDX > WS-JSON-RSP-LEN
                   OR WS-JSON-RSP-BUFFER(WS-CHAR-IDX:1) = '"'
                   CONTINUE
               END-PERFORM

               IF WS-CHAR-IDX < WS-JSON-RSP-LEN
                   ADD 1 TO WS-CHAR-IDX
                   MOVE WS-CHAR-IDX TO WS-TOKEN-START
      *---         FIND THE CLOSING QUOTE ---
                   PERFORM VARYING WS-CHAR-IDX
                       FROM WS-TOKEN-START BY 1
                       UNTIL WS-CHAR-IDX > WS-JSON-RSP-LEN
                       OR WS-JSON-RSP-BUFFER(WS-CHAR-IDX:1) = '"'
                       CONTINUE
                   END-PERFORM
                   COMPUTE WS-TOKEN-LEN =
                       WS-CHAR-IDX - WS-TOKEN-START
                   IF WS-TOKEN-LEN > 0 AND WS-TOKEN-LEN <= 256
                       MOVE WS-JSON-RSP-BUFFER(
                           WS-TOKEN-START:WS-TOKEN-LEN)
                           TO WS-PARSE-VALUE
                   END-IF
               END-IF
           END-IF
           .

       5100-EXIT.
           EXIT.

      *================================================================*
      * 5200-EXTRACT-NUMERIC-VALUE                                      *
      *   FIND A KEY IN JSON AND EXTRACT ITS NUMERIC VALUE              *
      *================================================================*
       5200-EXTRACT-NUMERIC-VALUE.

           MOVE ZERO TO WS-PARSE-NUM

      *--- LOCATE THE KEY AND EXTRACT DIGITS/DECIMAL ---
           MOVE SPACES TO WS-PARSE-VALUE
           PERFORM 5100-EXTRACT-STRING-VALUE

      *--- CONVERT STRING VALUE TO NUMERIC ---
           IF WS-PARSE-VALUE NOT = SPACES
               COMPUTE WS-PARSE-NUM =
                   FUNCTION NUMVAL(WS-PARSE-VALUE)
           END-IF
           .

       5200-EXIT.
           EXIT.

      *================================================================*
      * 5300-PARSE-LINE-ARRAY                                           *
      *   PARSE THE SERVICE LINE PRICING ARRAY FROM JSON RESPONSE      *
      *================================================================*
       5300-PARSE-LINE-ARRAY.

           MOVE ZERO TO WS-PRS-LINE-COUNT
           MOVE ZERO TO WS-PARSE-ARRAY-IDX

      *--- FIND THE "serviceLines" ARRAY IN RESPONSE ---
           MOVE 'serviceLines' TO WS-PARSE-KEY

      *--- ITERATE THROUGH LINE OBJECTS IN THE ARRAY ---
           PERFORM VARYING WS-PARSE-ARRAY-IDX FROM 1 BY 1
               UNTIL WS-PARSE-ARRAY-IDX > WS-PRQ-LINE-COUNT
               OR WS-PARSE-ARRAY-IDX > 50
               PERFORM 5310-PARSE-SINGLE-LINE
           END-PERFORM
           .

       5300-EXIT.
           EXIT.

      *================================================================*
      * 5310-PARSE-SINGLE-LINE                                          *
      *   PARSE PRICING DATA FOR ONE SERVICE LINE                       *
      *================================================================*
       5310-PARSE-SINGLE-LINE.

           ADD 1 TO WS-PRS-LINE-COUNT
           MOVE WS-PARSE-ARRAY-IDX
               TO WS-PRS-LN-SEQ(WS-PRS-LINE-COUNT)

      *--- EXTRACT LINE ALLOWED AMOUNT ---
           MOVE 'allowedAmount' TO WS-PARSE-KEY
           PERFORM 5200-EXTRACT-NUMERIC-VALUE
           MOVE WS-PARSE-NUM
               TO WS-PRS-LN-ALLOWED(WS-PRS-LINE-COUNT)

      *--- EXTRACT LINE PAID AMOUNT ---
           MOVE 'paidAmount' TO WS-PARSE-KEY
           PERFORM 5200-EXTRACT-NUMERIC-VALUE
           MOVE WS-PARSE-NUM
               TO WS-PRS-LN-PAID(WS-PRS-LINE-COUNT)

      *--- EXTRACT DENY REASON IF PRESENT ---
           MOVE 'denyReason' TO WS-PARSE-KEY
           PERFORM 5100-EXTRACT-STRING-VALUE
           IF WS-PARSE-VALUE NOT = SPACES
               MOVE WS-PARSE-VALUE(1:5)
                   TO WS-PRS-LN-DENY-RSN(WS-PRS-LINE-COUNT)
           END-IF

      *--- EXTRACT PRICING METHOD ---
           MOVE 'pricingMethod' TO WS-PARSE-KEY
           PERFORM 5100-EXTRACT-STRING-VALUE
           IF WS-PARSE-VALUE NOT = SPACES
               MOVE WS-PARSE-VALUE(1:10)
                   TO WS-PRS-LN-METHOD(WS-PRS-LINE-COUNT)
           END-IF

      *--- EXTRACT FEE SCHEDULE REFERENCE ---
           MOVE 'feeSchedule' TO WS-PARSE-KEY
           PERFORM 5100-EXTRACT-STRING-VALUE
           IF WS-PARSE-VALUE NOT = SPACES
               MOVE WS-PARSE-VALUE(1:8)
                   TO WS-PRS-LN-FEE-SCHED(WS-PRS-LINE-COUNT)
           END-IF
           .

       5310-EXIT.
           EXIT.

      *================================================================*
      * 6000-PUT-RESPONSE-CONTAINER                                     *
      *   STORE PRICING RESULTS IN CICS CHANNEL/CONTAINER               *
      *================================================================*
       6000-PUT-RESPONSE-CONTAINER.

           EXEC CICS PUT CONTAINER(WS-CTR-RESPONSE)
               CHANNEL  (WS-CHANNEL-NAME)
               FROM     (WS-PRICING-RESPONSE)
               FLENGTH  (LENGTH OF WS-PRICING-RESPONSE)
               RESP     (WS-CICS-RESP)
               RESP2    (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-LOG-ERROR
           END-IF
           .

       6000-EXIT.
           EXIT.

      *================================================================*
      * 6500-PUT-ERROR-CONTAINER                                        *
      *   STORE ERROR INFORMATION IN CICS CHANNEL/CONTAINER             *
      *================================================================*
       6500-PUT-ERROR-CONTAINER.

           EXEC CICS PUT CONTAINER(WS-CTR-ERROR)
               CHANNEL  (WS-CHANNEL-NAME)
               FROM     (WS-PRICING-RESPONSE)
               FLENGTH  (LENGTH OF WS-PRICING-RESPONSE)
               RESP     (WS-CICS-RESP)
               RESP2    (WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-LOG-ERROR
           END-IF
           .

       6500-EXIT.
           EXIT.

      *================================================================*
      * 7000-RETURN-TO-CALLER                                           *
      *   RETURN CONTROL TO THE CALLING PROGRAM                         *
      *   CHANNEL PERSISTS AUTOMATICALLY WITHIN THE CICS TASK          *
      *================================================================*
       7000-RETURN-TO-CALLER.

      *--- LOG COMPLETION STATUS ---
           STRING WS-PROGRAM-ID DELIMITED SIZE
                  ' COMPLETED: RC=' DELIMITED SIZE
                  WS-PRS-RETURN-CODE DELIMITED SIZE
                  ' LINES=' DELIMITED SIZE
               INTO WS-JSON-TEMP

           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-JSON-TEMP)
               LENGTH (80)
               RESP   (WS-CICS-RESP)
           END-EXEC

           EXEC CICS RETURN
               CHANNEL (WS-CHANNEL-NAME)
               RESP    (WS-CICS-RESP)
               RESP2   (WS-CICS-RESP2)
           END-EXEC
           .

       7000-EXIT.
           EXIT.

      *================================================================*
      * 9000-LOG-ERROR                                                  *
      *   LOG ERROR TO CICS TRANSIENT DATA QUEUE                        *
      *================================================================*
       9000-LOG-ERROR.

           STRING 'ERROR IN ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ': ' DELIMITED SIZE
                  WS-PRS-MESSAGE DELIMITED SIZE
               INTO WS-JSON-TEMP

           EXEC CICS WRITEQ TD
               QUEUE  ('CSML')
               FROM   (WS-JSON-TEMP)
               LENGTH (80)
               RESP   (WS-CICS-RESP)
           END-EXEC
           .

       9000-EXIT.
           EXIT.

      *================================================================*
      * 9500-ABEND-HANDLER                                              *
      *   HANDLE UNEXPECTED PROGRAM ABENDS                              *
      *================================================================*
       9500-ABEND-HANDLER.

           MOVE '90' TO WS-PRS-RETURN-CODE
           STRING 'ABEND IN ' DELIMITED SIZE
                  WS-PROGRAM-ID DELIMITED SIZE
                  ' - PRICING API SESSION ABANDONED'
                  DELIMITED SIZE
               INTO WS-PRS-MESSAGE

      *--- PUT ERROR INTO CONTAINER FOR CALLER ---
           PERFORM 6500-PUT-ERROR-CONTAINER

      *--- LOG TO TD QUEUE ---
           PERFORM 9000-LOG-ERROR

           EXEC CICS RETURN
               CHANNEL (WS-CHANNEL-NAME)
               RESP    (WS-CICS-RESP)
           END-EXEC
           .

       9500-EXIT.
           EXIT.
