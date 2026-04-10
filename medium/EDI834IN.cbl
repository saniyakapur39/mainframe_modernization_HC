       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EDI834IN.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-02-28.
      *================================================================*
      * PROGRAM:  EDI834IN                                             *
      * PURPOSE:  EDI 834 BENEFIT ENROLLMENT AND MAINTENANCE PARSER    *
      *           RECEIVES INBOUND EDI 834 FILES FROM EMPLOYERS AND    *
      *           EXCHANGES, PARSES THE X12 TRANSACTION STRUCTURE,     *
      *           VALIDATES MEMBER DATA, AND WRITES PARSED RECORDS     *
      *           TO AN INTERNAL ENROLLMENT STAGING FILE.              *
      * INPUTS:   EDI834-FILE  - RAW X12 834 EDI FILE                  *
      * OUTPUTS:  ENRSTG-FILE  - PARSED ENROLLMENT STAGING FILE        *
      *           ENRERR-FILE  - ENROLLMENT ERROR/REJECT FILE          *
      * FREQUENCY: DAILY BATCH                                         *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EDI834-FILE
               ASSIGN TO EDI834IN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-834-STATUS.

           SELECT ENRSTG-FILE
               ASSIGN TO ENRSTGOT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STG-STATUS.

           SELECT ENRERR-FILE
               ASSIGN TO ENRERROT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EDI834-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 10 TO 2000 CHARACTERS.
       01  EDI834-RECORD               PIC X(2000).

       FD  ENRSTG-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 500 CHARACTERS.
       01  ENRSTG-RECORD               PIC X(500).

       FD  ENRERR-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  ENRERR-RECORD               PIC X(200).

       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY MBRCOPY.
       COPY ABORTWSC.

       01  WS-FILE-STATUSES.
           05  WS-834-STATUS           PIC X(02).
           05  WS-STG-STATUS           PIC X(02).
           05  WS-ERR-STATUS           PIC X(02).

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-EOF              VALUE 'Y'.
               88  WS-NOT-EOF          VALUE 'N'.
           05  WS-VALID-SW             PIC X(01) VALUE 'Y'.
               88  WS-VALID-RECORD     VALUE 'Y'.
               88  WS-INVALID-RECORD   VALUE 'N'.
           05  WS-IN-MEMBER-LOOP-SW    PIC X(01) VALUE 'N'.
               88  WS-IN-MEMBER-LOOP   VALUE 'Y'.
               88  WS-NOT-IN-MBR-LOOP  VALUE 'N'.

       01  WS-EDI-PARSING.
           05  WS-SEGMENT-ID           PIC X(03).
           05  WS-ELEMENT-DELIM        PIC X(01) VALUE '*'.
           05  WS-SEGMENT-TERM         PIC X(01) VALUE '~'.
           05  WS-SUB-DELIM            PIC X(01) VALUE ':'.
           05  WS-RECORD-LENGTH        PIC 9(04).
           05  WS-PARSE-POS            PIC 9(04).
           05  WS-ELEMENT-COUNT        PIC 9(03).

       01  WS-EDI-ELEMENTS.
           05  WS-ELEMENT-TABLE.
               10  WS-ELEMENT OCCURS 30 TIMES PIC X(80).
           05  WS-ELEMENT-IDX          PIC 9(03).

       01  WS-ISA-FIELDS.
           05  WS-ISA-SENDER-ID        PIC X(15).
           05  WS-ISA-RECEIVER-ID      PIC X(15).
           05  WS-ISA-CONTROL-NO       PIC X(09).
           05  WS-ISA-DATE             PIC X(06).

       01  WS-GS-FIELDS.
           05  WS-GS-FUNC-ID          PIC X(02).
           05  WS-GS-SENDER-CODE      PIC X(15).
           05  WS-GS-RECEIVER-CODE    PIC X(15).
           05  WS-GS-DATE             PIC X(08).
           05  WS-GS-CONTROL-NO       PIC X(09).

       01  WS-STAGING-RECORD.
           05  WS-STG-RECORD-TYPE      PIC X(02).
               88  WS-STG-TYPE-ADD     VALUE 'AD'.
               88  WS-STG-TYPE-CHANGE  VALUE 'CH'.
               88  WS-STG-TYPE-TERM    VALUE 'TM'.
               88  WS-STG-TYPE-REINST  VALUE 'RI'.
           05  WS-STG-MEMBER-ID        PIC X(12).
           05  WS-STG-SSN              PIC X(09).
           05  WS-STG-LAST-NAME        PIC X(35).
           05  WS-STG-FIRST-NAME       PIC X(25).
           05  WS-STG-MIDDLE-INIT      PIC X(01).
           05  WS-STG-DOB              PIC X(08).
           05  WS-STG-SEX              PIC X(01).
           05  WS-STG-ADDR-LINE-1      PIC X(55).
           05  WS-STG-ADDR-LINE-2      PIC X(55).
           05  WS-STG-CITY             PIC X(30).
           05  WS-STG-STATE            PIC X(02).
           05  WS-STG-ZIP-CODE         PIC X(09).
           05  WS-STG-PHONE            PIC X(10).
           05  WS-STG-GROUP-ID         PIC X(10).
           05  WS-STG-SUBSCRIBER-ID    PIC X(12).
           05  WS-STG-RELATIONSHIP     PIC X(02).
           05  WS-STG-PLAN-CODE        PIC X(08).
           05  WS-STG-COVERAGE-TYPE    PIC X(02).
           05  WS-STG-EFF-DATE         PIC X(08).
           05  WS-STG-TERM-DATE        PIC X(08).
           05  WS-STG-MAINT-TYPE       PIC X(03).
           05  WS-STG-MAINT-REASON     PIC X(03).
           05  WS-STG-ISA-CONTROL      PIC X(09).
           05  WS-STG-GS-CONTROL       PIC X(09).
           05  WS-STG-SOURCE-FILE      PIC X(20).
           05  WS-STG-PROCESS-DATE     PIC X(08).
           05  FILLER                  PIC X(42).

       01  WS-ERROR-RECORD.
           05  WS-EREC-MEMBER-ID       PIC X(12).
           05  WS-EREC-SUBSCRIBER-ID   PIC X(12).
           05  WS-EREC-GROUP-ID        PIC X(10).
           05  WS-EREC-ERROR-CODE      PIC X(04).
           05  WS-EREC-ERROR-DESC      PIC X(60).
           05  WS-EREC-TXN-TYPE        PIC X(02).
           05  WS-EREC-SEVERITY        PIC X(01).
           05  WS-EREC-FIELD-NAME      PIC X(30).
           05  WS-EREC-FIELD-VALUE     PIC X(30).
           05  WS-EREC-PROCESS-DATE    PIC 9(08).
           05  WS-EREC-SOURCE-FILE     PIC X(20).
           05  FILLER                  PIC X(11).

       01  WS-COUNTERS.
           05  WS-834-RECORDS-IN       PIC 9(07) VALUE ZERO.
           05  WS-MEMBERS-PARSED       PIC 9(07) VALUE ZERO.
           05  WS-STAGING-WRITTEN      PIC 9(07) VALUE ZERO.
           05  WS-ERRORS-WRITTEN       PIC 9(07) VALUE ZERO.
           05  WS-ISA-COUNT            PIC 9(05) VALUE ZERO.
           05  WS-GS-COUNT            PIC 9(05) VALUE ZERO.
           05  WS-ST-COUNT            PIC 9(05) VALUE ZERO.

       01  WS-WORK-BUFFER              PIC X(2000).
       01  WS-TALLY-COUNT             PIC 9(04).
       01  WS-TEMP-FIELD              PIC X(80).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-EDI-RECORDS
               UNTIL WS-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           MOVE 'EDI834IN' TO WS-PROGRAM-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN INPUT  EDI834-FILE
           IF WS-834-STATUS NOT = '00'
               DISPLAY 'EDI834IN: ERROR OPENING 834 INPUT - '
                       WS-834-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT ENRSTG-FILE
           OPEN OUTPUT ENRERR-FILE
           PERFORM 8100-READ-834.

       2000-PROCESS-EDI-RECORDS.
           PERFORM 2100-IDENTIFY-SEGMENT
           EVALUATE WS-SEGMENT-ID
               WHEN 'ISA'
                   PERFORM 3000-PROCESS-ISA
               WHEN 'GS '
                   PERFORM 3100-PROCESS-GS
               WHEN 'ST '
                   PERFORM 3200-PROCESS-ST
               WHEN 'INS'
                   PERFORM 4000-PROCESS-INS
               WHEN 'REF'
                   PERFORM 4100-PROCESS-REF
               WHEN 'DTP'
                   PERFORM 4200-PROCESS-DTP
               WHEN 'NM1'
                   PERFORM 4300-PROCESS-NM1
               WHEN 'N3 '
                   PERFORM 4400-PROCESS-N3
               WHEN 'N4 '
                   PERFORM 4500-PROCESS-N4
               WHEN 'DMG'
                   PERFORM 4600-PROCESS-DMG
               WHEN 'HD '
                   PERFORM 4700-PROCESS-HD
               WHEN 'SE '
                   PERFORM 5000-PROCESS-SE
               WHEN 'GE '
                   CONTINUE
               WHEN 'IEA'
                   CONTINUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM 8100-READ-834.

       2100-IDENTIFY-SEGMENT.
           MOVE SPACES TO WS-SEGMENT-ID
           IF EDI834-RECORD(1:3) NOT = SPACES
               MOVE EDI834-RECORD(1:3) TO WS-SEGMENT-ID
           END-IF
           PERFORM 2200-PARSE-ELEMENTS.

       2200-PARSE-ELEMENTS.
           INITIALIZE WS-ELEMENT-TABLE
           MOVE ZERO TO WS-ELEMENT-COUNT
           MOVE 1 TO WS-PARSE-POS
           MOVE EDI834-RECORD TO WS-WORK-BUFFER
           INSPECT WS-WORK-BUFFER TALLYING WS-TALLY-COUNT
               FOR ALL WS-ELEMENT-DELIM
           ADD 1 TO WS-TALLY-COUNT
           UNSTRING WS-WORK-BUFFER
               DELIMITED BY WS-ELEMENT-DELIM
               OR WS-SEGMENT-TERM
               INTO WS-ELEMENT(1) WS-ELEMENT(2)
                    WS-ELEMENT(3) WS-ELEMENT(4)
                    WS-ELEMENT(5) WS-ELEMENT(6)
                    WS-ELEMENT(7) WS-ELEMENT(8)
                    WS-ELEMENT(9) WS-ELEMENT(10)
                    WS-ELEMENT(11) WS-ELEMENT(12)
                    WS-ELEMENT(13) WS-ELEMENT(14)
                    WS-ELEMENT(15) WS-ELEMENT(16)
                    WS-ELEMENT(17) WS-ELEMENT(18)
                    WS-ELEMENT(19) WS-ELEMENT(20)
               TALLYING IN WS-ELEMENT-COUNT
           END-UNSTRING.

       3000-PROCESS-ISA.
           ADD 1 TO WS-ISA-COUNT
           MOVE WS-ELEMENT(7)  TO WS-ISA-SENDER-ID
           MOVE WS-ELEMENT(9)  TO WS-ISA-RECEIVER-ID
           MOVE WS-ELEMENT(10) TO WS-ISA-DATE
           MOVE WS-ELEMENT(14) TO WS-ISA-CONTROL-NO
           MOVE WS-ELEMENT(17)(1:1) TO WS-SUB-DELIM
           DISPLAY 'EDI834IN: ISA ENVELOPE - SENDER='
                   WS-ISA-SENDER-ID
                   ' CTRL=' WS-ISA-CONTROL-NO.

       3100-PROCESS-GS.
           ADD 1 TO WS-GS-COUNT
           MOVE WS-ELEMENT(2) TO WS-GS-FUNC-ID
           MOVE WS-ELEMENT(3) TO WS-GS-SENDER-CODE
           MOVE WS-ELEMENT(4) TO WS-GS-RECEIVER-CODE
           MOVE WS-ELEMENT(5) TO WS-GS-DATE
           MOVE WS-ELEMENT(7) TO WS-GS-CONTROL-NO.

       3200-PROCESS-ST.
           ADD 1 TO WS-ST-COUNT
           IF WS-ELEMENT(2) NOT = '834'
               DISPLAY 'EDI834IN: WARNING - NON-834 TX SET: '
                       WS-ELEMENT(2)
           END-IF.

       4000-PROCESS-INS.
      *    INS SEGMENT STARTS A NEW MEMBER LOOP
           IF WS-IN-MEMBER-LOOP
      *        WRITE PREVIOUS MEMBER RECORD IF ANY
               PERFORM 5500-WRITE-STAGING
           END-IF
           SET WS-IN-MEMBER-LOOP TO TRUE
           SET WS-VALID-RECORD TO TRUE
           INITIALIZE WS-STAGING-RECORD
           ADD 1 TO WS-MEMBERS-PARSED
           MOVE WS-ISA-CONTROL-NO TO WS-STG-ISA-CONTROL
           MOVE WS-GS-CONTROL-NO  TO WS-STG-GS-CONTROL
           MOVE WS-CURRENT-DATE   TO WS-STG-PROCESS-DATE
      *    INS01 = Y/N (SUBSCRIBER INDICATOR)
      *    INS02 = RELATIONSHIP CODE (18=SELF)
           MOVE WS-ELEMENT(3)     TO WS-STG-RELATIONSHIP
      *    INS03 = MAINTENANCE TYPE CODE
           MOVE WS-ELEMENT(4)     TO WS-STG-MAINT-TYPE
      *    INS04 = MAINTENANCE REASON CODE
           MOVE WS-ELEMENT(5)     TO WS-STG-MAINT-REASON
      *    DETERMINE RECORD TYPE FROM MAINT TYPE CODE
           EVALUATE WS-ELEMENT(4)(1:3)
               WHEN '021'
                   SET WS-STG-TYPE-ADD TO TRUE
               WHEN '001'
                   SET WS-STG-TYPE-CHANGE TO TRUE
               WHEN '024'
                   SET WS-STG-TYPE-TERM TO TRUE
               WHEN '025'
                   SET WS-STG-TYPE-REINST TO TRUE
               WHEN OTHER
                   SET WS-STG-TYPE-CHANGE TO TRUE
           END-EVALUATE.

       4100-PROCESS-REF.
      *    REF SEGMENT - REFERENCE IDENTIFICATION
           IF WS-IN-MEMBER-LOOP
               EVALUATE WS-ELEMENT(2)(1:2)
                   WHEN '0F'
                       MOVE WS-ELEMENT(3)
                           TO WS-STG-SUBSCRIBER-ID
                   WHEN '1L'
                       MOVE WS-ELEMENT(3)
                           TO WS-STG-GROUP-ID
                   WHEN '17'
                       MOVE WS-ELEMENT(3)
                           TO WS-STG-MEMBER-ID
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF.

       4200-PROCESS-DTP.
      *    DTP SEGMENT - DATE/TIME PERIOD
           IF WS-IN-MEMBER-LOOP
               EVALUATE WS-ELEMENT(2)(1:3)
                   WHEN '348'
                       MOVE WS-ELEMENT(4)
                           TO WS-STG-EFF-DATE
                   WHEN '349'
                       MOVE WS-ELEMENT(4)
                           TO WS-STG-TERM-DATE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF.

       4300-PROCESS-NM1.
      *    NM1 SEGMENT - NAME
           IF WS-IN-MEMBER-LOOP
               IF WS-ELEMENT(2)(1:2) = 'IL'
               OR WS-ELEMENT(2)(1:2) = '74'
                   MOVE WS-ELEMENT(4)
                       TO WS-STG-LAST-NAME
                   MOVE WS-ELEMENT(5)
                       TO WS-STG-FIRST-NAME
                   MOVE WS-ELEMENT(6)
                       TO WS-STG-MIDDLE-INIT
                   IF WS-ELEMENT(9) NOT = SPACES
                       MOVE WS-ELEMENT(10)
                           TO WS-STG-SSN
                   END-IF
               END-IF
           END-IF.

       4400-PROCESS-N3.
      *    N3 SEGMENT - ADDRESS
           IF WS-IN-MEMBER-LOOP
               MOVE WS-ELEMENT(2) TO WS-STG-ADDR-LINE-1
               MOVE WS-ELEMENT(3) TO WS-STG-ADDR-LINE-2
           END-IF.

       4500-PROCESS-N4.
      *    N4 SEGMENT - CITY/STATE/ZIP
           IF WS-IN-MEMBER-LOOP
               MOVE WS-ELEMENT(2) TO WS-STG-CITY
               MOVE WS-ELEMENT(3) TO WS-STG-STATE
               MOVE WS-ELEMENT(4) TO WS-STG-ZIP-CODE
           END-IF.

       4600-PROCESS-DMG.
      *    DMG SEGMENT - DEMOGRAPHICS
           IF WS-IN-MEMBER-LOOP
               MOVE WS-ELEMENT(3) TO WS-STG-DOB
               MOVE WS-ELEMENT(4) TO WS-STG-SEX
           END-IF.

       4700-PROCESS-HD.
      *    HD SEGMENT - HEALTH COVERAGE
           IF WS-IN-MEMBER-LOOP
               MOVE WS-ELEMENT(4) TO WS-STG-COVERAGE-TYPE
               MOVE WS-ELEMENT(5) TO WS-STG-PLAN-CODE
           END-IF.

       5000-PROCESS-SE.
      *    SE SEGMENT - TRANSACTION SET TRAILER
           IF WS-IN-MEMBER-LOOP
               PERFORM 5500-WRITE-STAGING
               SET WS-NOT-IN-MBR-LOOP TO TRUE
           END-IF.

       5500-WRITE-STAGING.
           PERFORM 5600-VALIDATE-MEMBER
           IF WS-VALID-RECORD
               MOVE WS-STAGING-RECORD TO ENRSTG-RECORD
               WRITE ENRSTG-RECORD
               ADD 1 TO WS-STAGING-WRITTEN
           END-IF.

       5600-VALIDATE-MEMBER.
           IF WS-STG-LAST-NAME = SPACES
               SET WS-INVALID-RECORD TO TRUE
               MOVE 'V001' TO WS-EREC-ERROR-CODE
               MOVE 'MEMBER LAST NAME IS REQUIRED'
                   TO WS-EREC-ERROR-DESC
               MOVE 'LAST-NAME' TO WS-EREC-FIELD-NAME
               PERFORM 5900-WRITE-ERROR
           END-IF
           IF WS-VALID-RECORD
               IF WS-STG-DOB = SPACES
               OR WS-STG-DOB IS NOT NUMERIC
                   SET WS-INVALID-RECORD TO TRUE
                   MOVE 'V002' TO WS-EREC-ERROR-CODE
                   MOVE 'DATE OF BIRTH MISSING OR INVALID'
                       TO WS-EREC-ERROR-DESC
                   MOVE 'DOB' TO WS-EREC-FIELD-NAME
                   PERFORM 5900-WRITE-ERROR
               END-IF
           END-IF
           IF WS-VALID-RECORD
               IF WS-STG-GROUP-ID = SPACES
                   SET WS-INVALID-RECORD TO TRUE
                   MOVE 'V003' TO WS-EREC-ERROR-CODE
                   MOVE 'GROUP ID IS REQUIRED FOR ENROLLMENT'
                       TO WS-EREC-ERROR-DESC
                   MOVE 'GROUP-ID' TO WS-EREC-FIELD-NAME
                   PERFORM 5900-WRITE-ERROR
               END-IF
           END-IF
           IF WS-VALID-RECORD
               IF WS-STG-EFF-DATE = SPACES
               OR WS-STG-EFF-DATE IS NOT NUMERIC
                   SET WS-INVALID-RECORD TO TRUE
                   MOVE 'V004' TO WS-EREC-ERROR-CODE
                   MOVE 'EFFECTIVE DATE IS REQUIRED'
                       TO WS-EREC-ERROR-DESC
                   MOVE 'EFF-DATE' TO WS-EREC-FIELD-NAME
                   PERFORM 5900-WRITE-ERROR
               END-IF
           END-IF
           IF WS-VALID-RECORD
               IF WS-STG-SUBSCRIBER-ID = SPACES
               AND WS-STG-SSN = SPACES
                   SET WS-INVALID-RECORD TO TRUE
                   MOVE 'V005' TO WS-EREC-ERROR-CODE
                   MOVE 'SUBSCRIBER ID OR SSN REQUIRED'
                       TO WS-EREC-ERROR-DESC
                   MOVE 'SUBSCRIBER-ID' TO WS-EREC-FIELD-NAME
                   PERFORM 5900-WRITE-ERROR
               END-IF
           END-IF.

       5900-WRITE-ERROR.
           MOVE WS-STG-MEMBER-ID     TO WS-EREC-MEMBER-ID
           MOVE WS-STG-SUBSCRIBER-ID TO WS-EREC-SUBSCRIBER-ID
           MOVE WS-STG-GROUP-ID      TO WS-EREC-GROUP-ID
           MOVE WS-STG-RECORD-TYPE   TO WS-EREC-TXN-TYPE
           MOVE 'W'                  TO WS-EREC-SEVERITY
           MOVE WS-CURRENT-DATE      TO WS-EREC-PROCESS-DATE
           MOVE WS-ERROR-RECORD TO ENRERR-RECORD
           WRITE ENRERR-RECORD
           ADD 1 TO WS-ERRORS-WRITTEN.

       8100-READ-834.
           READ EDI834-FILE
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-834-RECORDS-IN
           END-READ.

       9000-TERMINATE.
           DISPLAY '================================================'
           DISPLAY 'EDI834IN: 834 ENROLLMENT PARSER COMPLETE'
           DISPLAY '================================================'
           DISPLAY '  834 RECORDS READ:     ' WS-834-RECORDS-IN
           DISPLAY '  MEMBERS PARSED:       ' WS-MEMBERS-PARSED
           DISPLAY '  STAGING RECORDS WRITTEN: ' WS-STAGING-WRITTEN
           DISPLAY '  ERROR RECORDS WRITTEN:   ' WS-ERRORS-WRITTEN
           DISPLAY '  ISA ENVELOPES:        ' WS-ISA-COUNT
           DISPLAY '  GS GROUPS:            ' WS-GS-COUNT
           DISPLAY '  ST TRANSACTION SETS:  ' WS-ST-COUNT
           DISPLAY '================================================'
           CLOSE EDI834-FILE
           CLOSE ENRSTG-FILE
           CLOSE ENRERR-FILE
           MOVE ZERO TO RETURN-CODE.
