       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MEMXREF1.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-03-12.
      *================================================================*
      * PROGRAM:  MEMXREF1                                             *
      * PURPOSE:  MEMBER IDENTITY CROSS-REFERENCE AND MATCHING         *
      *           READS ENROLLMENT STAGING RECORDS AND PERFORMS         *
      *           IDENTITY MATCHING AGAINST THE MEMBER MASTER VSAM     *
      *           FILE AND DB2 CROSS-REFERENCE TABLE. LINKS INCOMING   *
      *           ENROLLMENTS TO EXISTING MEMBER IDS OR GENERATES      *
      *           NEW MEMBER IDS FOR FIRST-TIME ENROLLEES.             *
      * INPUTS:   ENRSTG-FILE  - ENROLLMENT STAGING FILE               *
      *           MBRVSAM-FILE - MEMBER MASTER VSAM KSDS               *
      * OUTPUTS:  ENROUT-FILE  - MATCHED ENROLLMENT OUTPUT             *
      *           ENRERR-FILE  - MATCH ERROR/REJECT FILE               *
      *           DB2 TABLE    - HCAS.MEMBER_XREF                      *
      * FREQUENCY: DAILY BATCH (POST-EDI834IN)                         *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENRSTG-FILE
               ASSIGN TO ENRSTGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STG-STATUS.

           SELECT MBRVSAM-FILE
               ASSIGN TO MBRMSTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MBRV-MEMBER-ID
               ALTERNATE RECORD KEY IS MBRV-SSN
                   WITH DUPLICATES
               FILE STATUS IS WS-MBRV-STATUS.

           SELECT ENROUT-FILE
               ASSIGN TO ENROTPUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

           SELECT ENRERR-FILE
               ASSIGN TO ENRERROT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ENRSTG-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 500 CHARACTERS.
       01  ENRSTG-IN                   PIC X(500).

       FD  MBRVSAM-FILE
           RECORD CONTAINS 600 CHARACTERS.
       01  MBRVSAM-RECORD.
           05  MBRV-MEMBER-ID          PIC X(12).
           05  MBRV-LAST-NAME          PIC X(35).
           05  MBRV-FIRST-NAME         PIC X(25).
           05  MBRV-DOB                PIC 9(08).
           05  MBRV-SSN                PIC X(09).
           05  MBRV-DATA               PIC X(511).

       FD  ENROUT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 520 CHARACTERS.
       01  ENROUT-RECORD               PIC X(520).

       FD  ENRERR-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  ENRERR-RECORD               PIC X(200).

       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY MBRCOPY.
       COPY ABORTWSC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  WS-FILE-STATUSES.
           05  WS-STG-STATUS           PIC X(02).
           05  WS-MBRV-STATUS          PIC X(02).
           05  WS-OUT-STATUS           PIC X(02).
           05  WS-ERR-STATUS           PIC X(02).

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-EOF              VALUE 'Y'.
               88  WS-NOT-EOF          VALUE 'N'.
           05  WS-MATCH-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-MATCH-FOUND      VALUE 'Y'.
               88  WS-NO-MATCH         VALUE 'N'.

       01  WS-INPUT-STAGING.
           05  WS-STG-RECORD-TYPE      PIC X(02).
           05  WS-STG-MEMBER-ID        PIC X(12).
           05  WS-STG-SSN              PIC X(09).
           05  WS-STG-LAST-NAME        PIC X(35).
           05  WS-STG-FIRST-NAME       PIC X(25).
           05  WS-STG-MIDDLE-INIT      PIC X(01).
           05  WS-STG-DOB              PIC X(08).
           05  WS-STG-SEX              PIC X(01).
           05  FILLER                  PIC X(407).

       01  WS-OUTPUT-RECORD.
           05  WS-OUT-MATCH-STATUS     PIC X(02).
               88  WS-OUT-EXACT-MATCH  VALUE 'EM'.
               88  WS-OUT-FUZZY-MATCH  VALUE 'FM'.
               88  WS-OUT-NEW-MEMBER   VALUE 'NW'.
           05  WS-OUT-MEMBER-ID        PIC X(12).
           05  WS-OUT-MATCH-SCORE      PIC 9(03).
           05  WS-OUT-MATCH-METHOD     PIC X(03).
               88  WS-MATCH-BY-ID      VALUE 'MID'.
               88  WS-MATCH-BY-SSN     VALUE 'SSN'.
               88  WS-MATCH-BY-DEMO    VALUE 'DMO'.
           05  WS-OUT-SOURCE-DATA      PIC X(500).

       01  WS-DB2-XREF-FIELDS.
           05  WS-XR-MEMBER-ID         PIC X(12).
           05  WS-XR-EXTERNAL-ID       PIC X(20).
           05  WS-XR-ID-TYPE           PIC X(03).
           05  WS-XR-SOURCE-SYSTEM     PIC X(10).
           05  WS-XR-EFF-DATE          PIC X(08).
           05  WS-XR-TERM-DATE         PIC X(08).

       01  WS-NEXT-MEMBER-ID          PIC 9(12).
       01  WS-NEXT-MBR-ID-ALPHA       PIC X(12).
       01  WS-MATCH-SCORE             PIC 9(03).
       01  WS-NAME-MATCH-SW           PIC X(01).
       01  WS-DOB-MATCH-SW            PIC X(01).
       01  WS-UPPER-STG-LAST          PIC X(35).
       01  WS-UPPER-MBRV-LAST         PIC X(35).

       01  WS-COUNTERS.
           05  WS-EXACT-MATCH-CNT     PIC 9(07) VALUE ZERO.
           05  WS-FUZZY-MATCH-CNT     PIC 9(07) VALUE ZERO.
           05  WS-NEW-MEMBER-CNT      PIC 9(07) VALUE ZERO.
           05  WS-XREF-INSERT-CNT     PIC 9(07) VALUE ZERO.
           05  WS-ERROR-CNT           PIC 9(07) VALUE ZERO.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-STAGING
               UNTIL WS-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           MOVE 'MEMXREF1' TO WS-PROGRAM-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN INPUT  ENRSTG-FILE
           IF WS-STG-STATUS NOT = '00'
               DISPLAY 'MEMXREF1: OPEN ERROR STG - ' WS-STG-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN I-O    MBRVSAM-FILE
           OPEN OUTPUT ENROUT-FILE
           OPEN OUTPUT ENRERR-FILE
           PERFORM 1100-GET-NEXT-MEMBER-ID
           PERFORM 8100-READ-STAGING.

       1100-GET-NEXT-MEMBER-ID.
      *    RETRIEVE NEXT AVAILABLE MEMBER ID FROM DB2 SEQUENCE
           EXEC SQL
               SELECT NEXT_VALUE
               INTO :WS-NEXT-MEMBER-ID
               FROM HCAS.ID_SEQUENCE
               WHERE SEQ_NAME = 'MEMBER_ID'
               FOR UPDATE
           END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'MEMXREF1: ERROR GETTING NEXT ID - SQLCODE='
                       SQLCODE
               MOVE 999999000000 TO WS-NEXT-MEMBER-ID
           END-IF.

       2000-PROCESS-STAGING.
           MOVE ENRSTG-IN TO WS-INPUT-STAGING
           SET WS-NO-MATCH TO TRUE
           MOVE ZERO TO WS-MATCH-SCORE
           INITIALIZE WS-OUTPUT-RECORD
      *    STEP 1: TRY EXACT MATCH BY MEMBER ID
           IF WS-STG-MEMBER-ID NOT = SPACES
               PERFORM 3000-MATCH-BY-MEMBER-ID
           END-IF
      *    STEP 2: TRY MATCH BY SSN
           IF WS-NO-MATCH
               IF WS-STG-SSN NOT = SPACES
               AND WS-STG-SSN IS NUMERIC
                   PERFORM 4000-MATCH-BY-SSN
               END-IF
           END-IF
      *    STEP 3: TRY DEMOGRAPHIC MATCH IN DB2
           IF WS-NO-MATCH
               PERFORM 5000-MATCH-BY-DEMOGRAPHICS
           END-IF
      *    STEP 4: IF NO MATCH, CREATE NEW MEMBER
           IF WS-NO-MATCH
               PERFORM 6000-CREATE-NEW-MEMBER
           END-IF
      *    WRITE OUTPUT RECORD
           MOVE ENRSTG-IN TO WS-OUT-SOURCE-DATA
           MOVE WS-OUTPUT-RECORD TO ENROUT-RECORD
           WRITE ENROUT-RECORD
           ADD 1 TO WS-RECORDS-WRITTEN
           PERFORM 8100-READ-STAGING.

       3000-MATCH-BY-MEMBER-ID.
           MOVE WS-STG-MEMBER-ID TO MBRV-MEMBER-ID
           READ MBRVSAM-FILE
               KEY IS MBRV-MEMBER-ID
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   SET WS-MATCH-FOUND TO TRUE
                   SET WS-OUT-EXACT-MATCH TO TRUE
                   MOVE MBRV-MEMBER-ID TO WS-OUT-MEMBER-ID
                   MOVE 100 TO WS-OUT-MATCH-SCORE
                   SET WS-MATCH-BY-ID TO TRUE
                   ADD 1 TO WS-EXACT-MATCH-CNT
           END-READ.

       4000-MATCH-BY-SSN.
           MOVE WS-STG-SSN TO MBRV-SSN
           READ MBRVSAM-FILE
               KEY IS MBRV-SSN
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
      *            VERIFY ADDITIONAL FIELDS FOR SSN MATCH
                   MOVE 50 TO WS-MATCH-SCORE
                   PERFORM 4100-VERIFY-SSN-MATCH
           END-READ.

       4100-VERIFY-SSN-MATCH.
      *    SSN MATCH REQUIRES SECONDARY VERIFICATION
           MOVE 'N' TO WS-NAME-MATCH-SW
           MOVE 'N' TO WS-DOB-MATCH-SW
           MOVE FUNCTION UPPER-CASE(WS-STG-LAST-NAME)
               TO WS-UPPER-STG-LAST
           MOVE FUNCTION UPPER-CASE(MBRV-LAST-NAME)
               TO WS-UPPER-MBRV-LAST
           IF WS-UPPER-STG-LAST = WS-UPPER-MBRV-LAST
               MOVE 'Y' TO WS-NAME-MATCH-SW
               ADD 30 TO WS-MATCH-SCORE
           ELSE
      *        PARTIAL LAST NAME MATCH (FIRST 5 CHARS)
               IF WS-UPPER-STG-LAST(1:5) =
                  WS-UPPER-MBRV-LAST(1:5)
                   MOVE 'Y' TO WS-NAME-MATCH-SW
                   ADD 15 TO WS-MATCH-SCORE
               END-IF
           END-IF
           IF WS-STG-DOB = MBRV-DOB
               MOVE 'Y' TO WS-DOB-MATCH-SW
               ADD 20 TO WS-MATCH-SCORE
           END-IF
      *    ACCEPT MATCH IF SCORE >= 80
           IF WS-MATCH-SCORE >= 80
               SET WS-MATCH-FOUND TO TRUE
               SET WS-OUT-EXACT-MATCH TO TRUE
               MOVE MBRV-MEMBER-ID TO WS-OUT-MEMBER-ID
               MOVE WS-MATCH-SCORE TO WS-OUT-MATCH-SCORE
               SET WS-MATCH-BY-SSN TO TRUE
               ADD 1 TO WS-EXACT-MATCH-CNT
           ELSE IF WS-MATCH-SCORE >= 65
               SET WS-MATCH-FOUND TO TRUE
               SET WS-OUT-FUZZY-MATCH TO TRUE
               MOVE MBRV-MEMBER-ID TO WS-OUT-MEMBER-ID
               MOVE WS-MATCH-SCORE TO WS-OUT-MATCH-SCORE
               SET WS-MATCH-BY-SSN TO TRUE
               ADD 1 TO WS-FUZZY-MATCH-CNT
           END-IF.

       5000-MATCH-BY-DEMOGRAPHICS.
      *    QUERY DB2 FOR MATCHING LAST NAME + DOB + FIRST INITIAL
           MOVE FUNCTION UPPER-CASE(WS-STG-LAST-NAME)
               TO WS-UPPER-STG-LAST
           EXEC SQL
               SELECT MEMBER_ID
               INTO :WS-XR-MEMBER-ID
               FROM HCAS.MEMBER
               WHERE UPPER(LAST_NAME) = :WS-UPPER-STG-LAST
                 AND DATE_OF_BIRTH = :WS-STG-DOB
                 AND SUBSTR(FIRST_NAME, 1, 1) =
                     SUBSTR(:WS-STG-FIRST-NAME, 1, 1)
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = ZERO
               SET WS-MATCH-FOUND TO TRUE
               SET WS-OUT-FUZZY-MATCH TO TRUE
               MOVE WS-XR-MEMBER-ID TO WS-OUT-MEMBER-ID
               MOVE 070 TO WS-OUT-MATCH-SCORE
               SET WS-MATCH-BY-DEMO TO TRUE
               ADD 1 TO WS-FUZZY-MATCH-CNT
      *        INSERT CROSS-REFERENCE RECORD
               PERFORM 7000-INSERT-XREF
           END-IF.

       6000-CREATE-NEW-MEMBER.
      *    NO MATCH FOUND - ASSIGN NEW MEMBER ID
           ADD 1 TO WS-NEXT-MEMBER-ID
           MOVE WS-NEXT-MEMBER-ID TO WS-NEXT-MBR-ID-ALPHA
           SET WS-OUT-NEW-MEMBER TO TRUE
           MOVE WS-NEXT-MBR-ID-ALPHA TO WS-OUT-MEMBER-ID
           MOVE 000 TO WS-OUT-MATCH-SCORE
           ADD 1 TO WS-NEW-MEMBER-CNT
      *    INSERT NEW MEMBER INTO VSAM
           INITIALIZE MBRVSAM-RECORD
           MOVE WS-NEXT-MBR-ID-ALPHA TO MBRV-MEMBER-ID
           MOVE WS-STG-LAST-NAME     TO MBRV-LAST-NAME
           MOVE WS-STG-FIRST-NAME    TO MBRV-FIRST-NAME
           MOVE WS-STG-DOB           TO MBRV-DOB
           MOVE WS-STG-SSN           TO MBRV-SSN
           WRITE MBRVSAM-RECORD
           IF WS-MBRV-STATUS NOT = '00'
               DISPLAY 'MEMXREF1: VSAM WRITE ERROR - '
                       WS-MBRV-STATUS
                       ' MBR=' WS-NEXT-MBR-ID-ALPHA
           END-IF
      *    INSERT CROSS-REFERENCE IF EXTERNAL IDS PRESENT
           IF WS-STG-SUBSCRIBER-ID NOT = SPACES
               MOVE WS-NEXT-MBR-ID-ALPHA TO WS-XR-MEMBER-ID
               PERFORM 7000-INSERT-XREF
           END-IF
      *    UPDATE SEQUENCE IN DB2
           EXEC SQL
               UPDATE HCAS.ID_SEQUENCE
               SET    NEXT_VALUE = :WS-NEXT-MEMBER-ID
               WHERE  SEQ_NAME = 'MEMBER_ID'
           END-EXEC.

       7000-INSERT-XREF.
           MOVE WS-OUT-MEMBER-ID    TO WS-XR-MEMBER-ID
           IF WS-STG-SUBSCRIBER-ID NOT = SPACES
               MOVE WS-STG-SUBSCRIBER-ID TO WS-XR-EXTERNAL-ID
               MOVE 'SUB'                TO WS-XR-ID-TYPE
           ELSE
               MOVE WS-STG-SSN          TO WS-XR-EXTERNAL-ID
               MOVE 'SSN'               TO WS-XR-ID-TYPE
           END-IF
           MOVE 'EDI834'             TO WS-XR-SOURCE-SYSTEM
           MOVE WS-CURRENT-DATE     TO WS-XR-EFF-DATE
           MOVE '99991231'           TO WS-XR-TERM-DATE
           EXEC SQL
               INSERT INTO HCAS.MEMBER_XREF
                   (MEMBER_ID, EXTERNAL_ID, ID_TYPE,
                    SOURCE_SYSTEM, EFF_DATE, TERM_DATE)
               VALUES
                   (:WS-XR-MEMBER-ID, :WS-XR-EXTERNAL-ID,
                    :WS-XR-ID-TYPE, :WS-XR-SOURCE-SYSTEM,
                    :WS-XR-EFF-DATE, :WS-XR-TERM-DATE)
           END-EXEC
           IF SQLCODE = ZERO
               ADD 1 TO WS-XREF-INSERT-CNT
           ELSE
               IF SQLCODE = -803
                   CONTINUE
               ELSE
                   DISPLAY 'MEMXREF1: XREF INSERT ERROR - SQLCODE='
                           SQLCODE
               END-IF
           END-IF.

       8100-READ-STAGING.
           READ ENRSTG-FILE
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORDS-READ
           END-READ.

       9000-TERMINATE.
           EXEC SQL COMMIT END-EXEC
           DISPLAY '================================================'
           DISPLAY 'MEMXREF1: MEMBER CROSS-REFERENCE COMPLETE'
           DISPLAY '================================================'
           DISPLAY '  STAGING RECORDS READ:  ' WS-RECORDS-READ
           DISPLAY '  EXACT MATCHES:         ' WS-EXACT-MATCH-CNT
           DISPLAY '  FUZZY MATCHES:         ' WS-FUZZY-MATCH-CNT
           DISPLAY '  NEW MEMBERS CREATED:   ' WS-NEW-MEMBER-CNT
           DISPLAY '  XREF RECORDS INSERTED: ' WS-XREF-INSERT-CNT
           DISPLAY '  OUTPUT RECORDS WRITTEN:' WS-RECORDS-WRITTEN
           DISPLAY '================================================'
           CLOSE ENRSTG-FILE
           CLOSE MBRVSAM-FILE
           CLOSE ENROUT-FILE
           CLOSE ENRERR-FILE
           MOVE ZERO TO RETURN-CODE.
