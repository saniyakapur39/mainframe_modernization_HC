       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRVLD001.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-02-10.
      *================================================================*
      * PROGRAM:  PRVLD001                                             *
      * PURPOSE:  PROVIDER MASTER FILE LOADER                          *
      *           READS FLAT FILE OF NEW/UPDATED PROVIDER RECORDS      *
      *           AND LOADS THEM INTO THE PROVIDER MASTER VSAM KSDS.   *
      *           VALIDATES NPI FORMAT, TAXONOMY CODE, AND EFFECTIVE   *
      *           DATE LOGIC BEFORE WRITING TO THE INDEXED FILE.       *
      * INPUTS:   PRVIN-FILE   - SEQUENTIAL PROVIDER INPUT FILE        *
      * OUTPUTS:  PRVMST-FILE  - VSAM KSDS PROVIDER MASTER FILE       *
      *           PRVERR-FILE  - SEQUENTIAL ERROR/REJECT FILE          *
      * FREQUENCY: WEEKLY (PROVIDER DATA REFRESH)                      *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRVIN-FILE
               ASSIGN TO PRVINPUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PRVIN-STATUS.

           SELECT PRVMST-FILE
               ASSIGN TO PRVMSTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PMF-NPI
               FILE STATUS IS WS-PRVMST-STATUS.

           SELECT PRVERR-FILE
               ASSIGN TO PRVERROR
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PRVERR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PRVIN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 500 CHARACTERS.
       01  PRVIN-RECORD                PIC X(500).

       FD  PRVMST-FILE
           RECORD CONTAINS 600 CHARACTERS.
       01  PMF-RECORD.
           05  PMF-NPI                 PIC X(10).
           05  PMF-DATA                PIC X(590).

       FD  PRVERR-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 580 CHARACTERS.
       01  PRVERR-RECORD.
           05  PRVERR-NPI              PIC X(10).
           05  PRVERR-REASON-CODE      PIC X(04).
           05  PRVERR-REASON-DESC      PIC X(60).
           05  PRVERR-FIELD-NAME       PIC X(30).
           05  PRVERR-SOURCE-DATA      PIC X(476).

       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY PRVCOPY.
       COPY ABORTWSC.

       01  WS-FILE-STATUSES.
           05  WS-PRVIN-STATUS         PIC X(02).
           05  WS-PRVMST-STATUS        PIC X(02).
           05  WS-PRVERR-STATUS        PIC X(02).

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-EOF              VALUE 'Y'.
               88  WS-NOT-EOF          VALUE 'N'.
           05  WS-VALID-RECORD-SW      PIC X(01) VALUE 'Y'.
               88  WS-RECORD-VALID     VALUE 'Y'.
               88  WS-RECORD-INVALID   VALUE 'N'.

       01  WS-INPUT-RECORD.
           05  WS-IN-ACTION-CODE       PIC X(01).
               88  WS-ACTION-ADD       VALUE 'A'.
               88  WS-ACTION-UPDATE    VALUE 'U'.
               88  WS-ACTION-DELETE    VALUE 'D'.
           05  WS-IN-NPI               PIC X(10).
           05  WS-IN-LAST-NAME         PIC X(35).
           05  WS-IN-FIRST-NAME        PIC X(25).
           05  WS-IN-MIDDLE-INIT       PIC X(01).
           05  WS-IN-ORG-NAME          PIC X(60).
           05  WS-IN-TAX-ID            PIC X(09).
           05  WS-IN-TAX-ID-TYPE       PIC X(02).
           05  WS-IN-ENTITY-TYPE       PIC X(01).
           05  WS-IN-TAXONOMY-CODE     PIC X(10).
           05  WS-IN-SPECIALTY-CODE    PIC X(04).
           05  WS-IN-LICENSE-NO        PIC X(20).
           05  WS-IN-LICENSE-STATE     PIC X(02).
           05  WS-IN-ADDR-LINE-1       PIC X(55).
           05  WS-IN-ADDR-LINE-2       PIC X(55).
           05  WS-IN-CITY              PIC X(30).
           05  WS-IN-STATE             PIC X(02).
           05  WS-IN-ZIP-CODE          PIC X(09).
           05  WS-IN-PHONE             PIC X(10).
           05  WS-IN-NETWORK-ID        PIC X(06).
           05  WS-IN-CONTRACT-TYPE     PIC X(02).
           05  WS-IN-PAR-STATUS        PIC X(01).
           05  WS-IN-EFF-DATE          PIC 9(08).
           05  WS-IN-TERM-DATE         PIC 9(08).
           05  WS-IN-FEE-SCHED-ID     PIC X(08).
           05  WS-IN-PAY-METHOD        PIC X(02).
           05  FILLER                  PIC X(37).

       01  WS-NPI-CHECK-DIGIT-AREA.
           05  WS-NPI-NUMERIC          PIC 9(10).
           05  WS-NPI-DIGITS REDEFINES WS-NPI-NUMERIC.
               10  WS-NPI-DIG         PIC 9(01) OCCURS 10 TIMES.
           05  WS-NPI-SUM              PIC 9(05).
           05  WS-NPI-DOUBLED          PIC 9(02).
           05  WS-NPI-WORK             PIC 9(02).

       01  WS-COUNTERS.
           05  WS-ADD-COUNT            PIC 9(07) VALUE ZERO.
           05  WS-UPD-COUNT            PIC 9(07) VALUE ZERO.
           05  WS-DEL-COUNT            PIC 9(07) VALUE ZERO.
           05  WS-ERR-COUNT            PIC 9(07) VALUE ZERO.
           05  WS-SUB                  PIC 9(03).

       01  WS-VALID-STATES-TABLE.
           05  FILLER PIC X(100) VALUE
               'ALAKAZABORCCOCTDCDEFLGAHIIDILINIAKSKYLAMA'
               'MDMABORMIMNMSMOORNENVNHNJNMNYNCNDOHOKORPA'.
           05  FILLER PIC X(28) VALUE
               'RABORSCSDTNTXUTVTVAWAVWWIWY'.
       01  WS-VALID-STATES REDEFINES WS-VALID-STATES-TABLE.
           05  WS-STATE-CODE          PIC X(02) OCCURS 64 TIMES.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-PROVIDER
               UNTIL WS-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           MOVE 'PRVLD001' TO WS-PROGRAM-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           OPEN INPUT  PRVIN-FILE
           IF WS-PRVIN-STATUS NOT = '00'
               DISPLAY 'PRVLD001: OPEN ERROR INPUT - ' WS-PRVIN-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN I-O    PRVMST-FILE
           IF WS-PRVMST-STATUS NOT = '00'
               DISPLAY 'PRVLD001: OPEN ERROR MASTER - '
                       WS-PRVMST-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT PRVERR-FILE
           PERFORM 8000-READ-INPUT.

       2000-PROCESS-PROVIDER.
           MOVE PRVIN-RECORD TO WS-INPUT-RECORD
           SET WS-RECORD-VALID TO TRUE
           PERFORM 2100-VALIDATE-RECORD
           IF WS-RECORD-VALID
               EVALUATE TRUE
                   WHEN WS-ACTION-ADD
                       PERFORM 3000-ADD-PROVIDER
                   WHEN WS-ACTION-UPDATE
                       PERFORM 4000-UPDATE-PROVIDER
                   WHEN WS-ACTION-DELETE
                       PERFORM 5000-DELETE-PROVIDER
                   WHEN OTHER
                       MOVE 'E001' TO PRVERR-REASON-CODE
                       MOVE 'INVALID ACTION CODE'
                           TO PRVERR-REASON-DESC
                       MOVE 'ACTION-CODE' TO PRVERR-FIELD-NAME
                       PERFORM 6000-WRITE-ERROR
               END-EVALUATE
           END-IF
           PERFORM 8000-READ-INPUT.

       2100-VALIDATE-RECORD.
           PERFORM 2110-VALIDATE-NPI
           IF WS-RECORD-VALID
               PERFORM 2120-VALIDATE-TAXONOMY
           END-IF
           IF WS-RECORD-VALID
               PERFORM 2130-VALIDATE-DATES
           END-IF
           IF WS-RECORD-VALID
               PERFORM 2140-VALIDATE-STATE
           END-IF.

       2110-VALIDATE-NPI.
           IF WS-IN-NPI = SPACES OR LOW-VALUES
               SET WS-RECORD-INVALID TO TRUE
               MOVE 'E010' TO PRVERR-REASON-CODE
               MOVE 'NPI IS REQUIRED' TO PRVERR-REASON-DESC
               MOVE 'NPI' TO PRVERR-FIELD-NAME
               PERFORM 6000-WRITE-ERROR
           ELSE
               IF WS-IN-NPI IS NOT NUMERIC
                   SET WS-RECORD-INVALID TO TRUE
                   MOVE 'E011' TO PRVERR-REASON-CODE
                   MOVE 'NPI MUST BE NUMERIC 10 DIGITS'
                       TO PRVERR-REASON-DESC
                   MOVE 'NPI' TO PRVERR-FIELD-NAME
                   PERFORM 6000-WRITE-ERROR
               ELSE
                   PERFORM 2115-LUHN-CHECK-NPI
               END-IF
           END-IF.

       2115-LUHN-CHECK-NPI.
           MOVE WS-IN-NPI TO WS-NPI-NUMERIC
           MOVE ZERO TO WS-NPI-SUM
           MOVE 24 TO WS-NPI-SUM
           PERFORM VARYING WS-SUB FROM 9 BY -2
               UNTIL WS-SUB < 1
               COMPUTE WS-NPI-DOUBLED =
                   WS-NPI-DIG(WS-SUB) * 2
               IF WS-NPI-DOUBLED > 9
                   SUBTRACT 9 FROM WS-NPI-DOUBLED
               END-IF
               ADD WS-NPI-DOUBLED TO WS-NPI-SUM
           END-PERFORM
           PERFORM VARYING WS-SUB FROM 8 BY -2
               UNTIL WS-SUB < 1
               ADD WS-NPI-DIG(WS-SUB) TO WS-NPI-SUM
           END-PERFORM
           COMPUTE WS-NPI-WORK =
               FUNCTION MOD(WS-NPI-SUM, 10)
           IF WS-NPI-WORK NOT = WS-NPI-DIG(10)
               SET WS-RECORD-INVALID TO TRUE
               MOVE 'E012' TO PRVERR-REASON-CODE
               MOVE 'NPI FAILED LUHN CHECK DIGIT VALIDATION'
                   TO PRVERR-REASON-DESC
               MOVE 'NPI' TO PRVERR-FIELD-NAME
               PERFORM 6000-WRITE-ERROR
           END-IF.

       2120-VALIDATE-TAXONOMY.
           IF WS-IN-TAXONOMY-CODE = SPACES
               SET WS-RECORD-INVALID TO TRUE
               MOVE 'E020' TO PRVERR-REASON-CODE
               MOVE 'TAXONOMY CODE IS REQUIRED'
                   TO PRVERR-REASON-DESC
               MOVE 'TAXONOMY-CODE' TO PRVERR-FIELD-NAME
               PERFORM 6000-WRITE-ERROR
           END-IF.

       2130-VALIDATE-DATES.
           IF WS-IN-EFF-DATE NOT NUMERIC
               SET WS-RECORD-INVALID TO TRUE
               MOVE 'E030' TO PRVERR-REASON-CODE
               MOVE 'EFFECTIVE DATE MUST BE NUMERIC CCYYMMDD'
                   TO PRVERR-REASON-DESC
               MOVE 'EFF-DATE' TO PRVERR-FIELD-NAME
               PERFORM 6000-WRITE-ERROR
           ELSE
               IF WS-IN-EFF-DATE < 19000101
               OR WS-IN-EFF-DATE > 20991231
                   SET WS-RECORD-INVALID TO TRUE
                   MOVE 'E031' TO PRVERR-REASON-CODE
                   MOVE 'EFFECTIVE DATE OUT OF VALID RANGE'
                       TO PRVERR-REASON-DESC
                   MOVE 'EFF-DATE' TO PRVERR-FIELD-NAME
                   PERFORM 6000-WRITE-ERROR
               END-IF
           END-IF
           IF WS-IN-TERM-DATE NOT = ZEROS
           AND WS-IN-TERM-DATE NOT = SPACES
               IF WS-IN-TERM-DATE < WS-IN-EFF-DATE
                   SET WS-RECORD-INVALID TO TRUE
                   MOVE 'E032' TO PRVERR-REASON-CODE
                   MOVE 'TERM DATE CANNOT PRECEDE EFF DATE'
                       TO PRVERR-REASON-DESC
                   MOVE 'TERM-DATE' TO PRVERR-FIELD-NAME
                   PERFORM 6000-WRITE-ERROR
               END-IF
           END-IF.

       2140-VALIDATE-STATE.
           IF WS-IN-STATE = SPACES
               SET WS-RECORD-INVALID TO TRUE
               MOVE 'E040' TO PRVERR-REASON-CODE
               MOVE 'STATE CODE IS REQUIRED'
                   TO PRVERR-REASON-DESC
               MOVE 'STATE' TO PRVERR-FIELD-NAME
               PERFORM 6000-WRITE-ERROR
           END-IF.

       3000-ADD-PROVIDER.
           MOVE WS-IN-NPI TO PMF-NPI
           READ PRVMST-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   SET WS-RECORD-INVALID TO TRUE
                   MOVE 'E050' TO PRVERR-REASON-CODE
                   MOVE 'PROVIDER NPI ALREADY EXISTS IN MASTER'
                       TO PRVERR-REASON-DESC
                   MOVE 'NPI' TO PRVERR-FIELD-NAME
                   PERFORM 6000-WRITE-ERROR
           END-READ
           IF WS-RECORD-VALID
               PERFORM 7000-BUILD-MASTER-RECORD
               WRITE PMF-RECORD
               IF WS-PRVMST-STATUS = '00'
                   ADD 1 TO WS-ADD-COUNT
               ELSE
                   DISPLAY 'PRVLD001: WRITE ERROR - '
                           WS-PRVMST-STATUS ' NPI=' WS-IN-NPI
               END-IF
           END-IF.

       4000-UPDATE-PROVIDER.
           MOVE WS-IN-NPI TO PMF-NPI
           READ PRVMST-FILE
               INVALID KEY
                   SET WS-RECORD-INVALID TO TRUE
                   MOVE 'E060' TO PRVERR-REASON-CODE
                   MOVE 'PROVIDER NPI NOT FOUND FOR UPDATE'
                       TO PRVERR-REASON-DESC
                   MOVE 'NPI' TO PRVERR-FIELD-NAME
                   PERFORM 6000-WRITE-ERROR
           END-READ
           IF WS-RECORD-VALID
               PERFORM 7000-BUILD-MASTER-RECORD
               REWRITE PMF-RECORD
               IF WS-PRVMST-STATUS = '00'
                   ADD 1 TO WS-UPD-COUNT
               ELSE
                   DISPLAY 'PRVLD001: REWRITE ERROR - '
                           WS-PRVMST-STATUS ' NPI=' WS-IN-NPI
               END-IF
           END-IF.

       5000-DELETE-PROVIDER.
           MOVE WS-IN-NPI TO PMF-NPI
           READ PRVMST-FILE
               INVALID KEY
                   SET WS-RECORD-INVALID TO TRUE
                   MOVE 'E070' TO PRVERR-REASON-CODE
                   MOVE 'PROVIDER NPI NOT FOUND FOR DELETE'
                       TO PRVERR-REASON-DESC
                   MOVE 'NPI' TO PRVERR-FIELD-NAME
                   PERFORM 6000-WRITE-ERROR
           END-READ
           IF WS-RECORD-VALID
               DELETE PRVMST-FILE
               IF WS-PRVMST-STATUS = '00'
                   ADD 1 TO WS-DEL-COUNT
               END-IF
           END-IF.

       6000-WRITE-ERROR.
           MOVE WS-IN-NPI     TO PRVERR-NPI
           MOVE PRVIN-RECORD   TO PRVERR-SOURCE-DATA
           WRITE PRVERR-RECORD
           ADD 1 TO WS-ERR-COUNT.

       7000-BUILD-MASTER-RECORD.
           INITIALIZE PROVIDER-MASTER-RECORD
           MOVE WS-IN-NPI             TO PRV-NPI
           MOVE WS-IN-LAST-NAME       TO PRV-LAST-NAME
           MOVE WS-IN-FIRST-NAME      TO PRV-FIRST-NAME
           MOVE WS-IN-MIDDLE-INIT     TO PRV-MIDDLE-INIT
           MOVE WS-IN-ORG-NAME        TO PRV-ORG-NAME
           MOVE WS-IN-TAX-ID          TO PRV-TAX-ID
           MOVE WS-IN-TAX-ID-TYPE     TO PRV-TAX-ID-TYPE
           MOVE WS-IN-ENTITY-TYPE     TO PRV-ENTITY-TYPE
           MOVE WS-IN-TAXONOMY-CODE   TO PRV-TAXONOMY-CODE
           MOVE WS-IN-SPECIALTY-CODE  TO PRV-SPECIALTY-CODE
           MOVE WS-IN-LICENSE-NO      TO PRV-LICENSE-NO
           MOVE WS-IN-LICENSE-STATE   TO PRV-LICENSE-STATE
           MOVE WS-IN-ADDR-LINE-1     TO PRV-ADDR-LINE-1
           MOVE WS-IN-ADDR-LINE-2     TO PRV-ADDR-LINE-2
           MOVE WS-IN-CITY            TO PRV-CITY
           MOVE WS-IN-STATE           TO PRV-STATE
           MOVE WS-IN-ZIP-CODE        TO PRV-ZIP-CODE
           MOVE WS-IN-PHONE           TO PRV-PHONE
           MOVE WS-IN-NETWORK-ID      TO PRV-NETWORK-ID
           MOVE WS-IN-CONTRACT-TYPE   TO PRV-CONTRACT-TYPE
           MOVE WS-IN-PAR-STATUS      TO PRV-PAR-STATUS
           MOVE WS-IN-EFF-DATE        TO PRV-EFF-DATE
           MOVE WS-IN-TERM-DATE       TO PRV-TERM-DATE
           MOVE WS-IN-FEE-SCHED-ID    TO PRV-FEE-SCHED-ID
           MOVE WS-IN-PAY-METHOD      TO PRV-PAY-METHOD
           MOVE WS-CURRENT-DATE       TO PRV-UPDATE-DATE
           MOVE 'PRVLD001'             TO PRV-UPDATE-USER
           SET PRV-REC-ACTIVE          TO TRUE
           MOVE PROVIDER-MASTER-RECORD TO PMF-RECORD.

       8000-READ-INPUT.
           READ PRVIN-FILE INTO PRVIN-RECORD
               AT END
                   SET WS-EOF TO TRUE
           END-READ.

       9000-TERMINATE.
           DISPLAY '============================================'
           DISPLAY 'PRVLD001: PROVIDER MASTER FILE LOADER STATS'
           DISPLAY '============================================'
           DISPLAY '  RECORDS READ:     ' WS-RECORDS-READ
           DISPLAY '  PROVIDERS ADDED:  ' WS-ADD-COUNT
           DISPLAY '  PROVIDERS UPDATED:' WS-UPD-COUNT
           DISPLAY '  PROVIDERS DELETED:' WS-DEL-COUNT
           DISPLAY '  RECORDS REJECTED: ' WS-ERR-COUNT
           DISPLAY '============================================'
           CLOSE PRVIN-FILE
           CLOSE PRVMST-FILE
           CLOSE PRVERR-FILE
           MOVE ZERO TO RETURN-CODE.
