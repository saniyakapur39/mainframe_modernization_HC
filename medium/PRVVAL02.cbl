       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRVVAL02.
       AUTHOR.        HCAS DEVELOPMENT TEAM.
       DATE-WRITTEN.  2024-04-05.
      *================================================================*
      * PROGRAM:  PRVVAL02                                             *
      * PURPOSE:  PROVIDER CREDENTIALING AND NETWORK VERIFICATION      *
      *           CALLED AS A SUBROUTINE DURING CLAIMS ADJUDICATION    *
      *           TO VERIFY PROVIDER CREDENTIALS, NETWORK STATUS,      *
      *           AND CONTRACT TERMS. RETURNS AUTHORIZATION STATUS     *
      *           AND FEE SCHEDULE INFORMATION TO THE CALLING          *
      *           CLAIMS PROCESSING MODULE.                            *
      * CALLED BY: CLMADJ01 (CLAIMS ADJUDICATION ENGINE)              *
      * INPUTS:   LINKAGE SECTION PARAMETERS FROM CALLING PROGRAM      *
      * OUTPUTS:  PROVIDER VALIDATION RESPONSE IN LINKAGE SECTION      *
      * FREQUENCY: PER-CLAIM (CALLED DURING ADJUDICATION)              *
      *================================================================*
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY HCCOMMON.
       COPY PRVCOPY.
       COPY ABORTWSC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  WS-PROGRAM-NAME             PIC X(08) VALUE 'PRVVAL02'.

       01  WS-DB2-PROVIDER.
           05  WS-DB-PRV-NPI           PIC X(10).
           05  WS-DB-PRV-LAST-NAME     PIC X(35).
           05  WS-DB-PRV-FIRST-NAME    PIC X(25).
           05  WS-DB-PRV-ORG-NAME      PIC X(60).
           05  WS-DB-PRV-TAXONOMY      PIC X(10).
           05  WS-DB-PRV-SPECIALTY     PIC X(04).
           05  WS-DB-PRV-ENTITY-TYPE   PIC X(01).
           05  WS-DB-PRV-NETWORK-ID    PIC X(06).
           05  WS-DB-PRV-CONTRACT-ID   PIC X(12).
           05  WS-DB-PRV-CONTRACT-TYPE PIC X(02).
           05  WS-DB-PRV-PAR-STATUS    PIC X(01).
           05  WS-DB-PRV-EFF-DATE      PIC X(08).
           05  WS-DB-PRV-TERM-DATE     PIC X(08).
           05  WS-DB-PRV-FEE-SCHED     PIC X(08).
           05  WS-DB-PRV-CRED-STATUS   PIC X(02).
           05  WS-DB-PRV-CRED-DATE     PIC X(08).
           05  WS-DB-PRV-CRED-EXPIRE   PIC X(08).
           05  WS-DB-PRV-REC-STATUS    PIC X(01).
           05  WS-DB-PRV-STATE         PIC X(02).
           05  WS-DB-PRV-LICENSE-NO    PIC X(20).
           05  WS-DB-PRV-LICENSE-STATE PIC X(02).

       01  WS-DB2-FEE-SCHEDULE.
           05  WS-FEE-SCHED-ID         PIC X(08).
           05  WS-FEE-PROC-CODE        PIC X(05).
           05  WS-FEE-MODIFIER         PIC X(02).
           05  WS-FEE-AMOUNT           PIC S9(07)V99 COMP-3.
           05  WS-FEE-EFF-DATE         PIC X(08).
           05  WS-FEE-TERM-DATE        PIC X(08).
           05  WS-FEE-LOCALITY         PIC X(02).

       01  WS-TAXONOMY-VALID-SW        PIC X(01) VALUE 'N'.
           88  WS-TAXONOMY-VALID       VALUE 'Y'.
           88  WS-TAXONOMY-INVALID     VALUE 'N'.

       01  WS-VALID-TAXONOMIES.
           05  WS-TAX-TABLE.
               10  FILLER  PIC X(14)  VALUE '207R00000XFPMD'.
               10  FILLER  PIC X(14)  VALUE '207Q00000XFPMD'.
               10  FILLER  PIC X(14)  VALUE '208D00000XGPMD'.
               10  FILLER  PIC X(14)  VALUE '261QM0855XFACL'.
               10  FILLER  PIC X(14)  VALUE '208600000XSRGN'.
               10  FILLER  PIC X(14)  VALUE '1041C0700XCHIR'.
               10  FILLER  PIC X(14)  VALUE '122300000XDNTS'.
               10  FILLER  PIC X(14)  VALUE '332B00000XDMEP'.
               10  FILLER  PIC X(14)  VALUE '363L00000XNPRC'.
               10  FILLER  PIC X(14)  VALUE '367A00000XPHYA'.
           05  WS-TAX-TABLE-R REDEFINES WS-TAX-TABLE.
               10  WS-TAX-ENTRY OCCURS 10 TIMES.
                   15  WS-TAX-CODE     PIC X(10).
                   15  WS-TAX-DESC     PIC X(04).
       01  WS-TAX-IDX                 PIC 9(03).

       01  WS-EDIT-RESULTS.
           05  WS-NPI-EDIT-RESULT      PIC X(02) VALUE SPACES.
           05  WS-CRED-EDIT-RESULT     PIC X(02) VALUE SPACES.
           05  WS-NETWORK-EDIT-RESULT  PIC X(02) VALUE SPACES.
           05  WS-TAX-EDIT-RESULT      PIC X(02) VALUE SPACES.
           05  WS-LICENSE-EDIT-RESULT  PIC X(02) VALUE SPACES.
           05  WS-DATE-EDIT-RESULT     PIC X(02) VALUE SPACES.

       LINKAGE SECTION.

       01  LS-PRV-REQUEST.
           05  LS-REQ-FUNCTION         PIC X(02).
               88  LS-REQ-VALIDATE     VALUE 'VL'.
               88  LS-REQ-FEE-LOOKUP   VALUE 'FL'.
               88  LS-REQ-BOTH         VALUE 'BT'.
           05  LS-REQ-NPI              PIC X(10).
           05  LS-REQ-TAXONOMY         PIC X(10).
           05  LS-REQ-DOS              PIC X(08).
           05  LS-REQ-PROC-CODE        PIC X(05).
           05  LS-REQ-MODIFIER         PIC X(02).
           05  LS-REQ-PLAN-NETWORK     PIC X(06).
           05  LS-REQ-SVC-STATE        PIC X(02).

       01  LS-PRV-RESPONSE.
           05  LS-RSP-RETURN-CODE      PIC X(02).
               88  LS-RSP-APPROVED     VALUE '00'.
               88  LS-RSP-DENIED       VALUE '01'.
               88  LS-RSP-PENDED       VALUE '02'.
               88  LS-RSP-ERROR        VALUE '99'.
           05  LS-RSP-EDIT-CODES.
               10  LS-RSP-EDIT OCCURS 10 TIMES PIC X(05).
           05  LS-RSP-PRV-NAME         PIC X(60).
           05  LS-RSP-PAR-STATUS       PIC X(01).
           05  LS-RSP-NETWORK-ID       PIC X(06).
           05  LS-RSP-CONTRACT-TYPE    PIC X(02).
           05  LS-RSP-FEE-SCHED-ID    PIC X(08).
           05  LS-RSP-FEE-AMOUNT       PIC S9(07)V99 COMP-3.
           05  LS-RSP-CRED-STATUS      PIC X(02).
           05  LS-RSP-MSG              PIC X(80).

       PROCEDURE DIVISION USING LS-PRV-REQUEST
                                LS-PRV-RESPONSE.

       0000-MAIN-PROCESS.
           INITIALIZE LS-PRV-RESPONSE
           INITIALIZE WS-EDIT-RESULTS
           SET LS-RSP-APPROVED TO TRUE
           EVALUATE TRUE
               WHEN LS-REQ-VALIDATE
                   PERFORM 1000-VALIDATE-PROVIDER
               WHEN LS-REQ-FEE-LOOKUP
                   PERFORM 2000-FEE-SCHEDULE-LOOKUP
               WHEN LS-REQ-BOTH
                   PERFORM 1000-VALIDATE-PROVIDER
                   IF LS-RSP-APPROVED
                       PERFORM 2000-FEE-SCHEDULE-LOOKUP
                   END-IF
               WHEN OTHER
                   SET LS-RSP-ERROR TO TRUE
                   MOVE 'INVALID FUNCTION CODE'
                       TO LS-RSP-MSG
           END-EVALUATE
           GOBACK.

       1000-VALIDATE-PROVIDER.
           PERFORM 1100-LOOKUP-PROVIDER
           IF WS-NPI-EDIT-RESULT = SPACES
               PERFORM 1200-CHECK-CREDENTIALING
               PERFORM 1300-CHECK-NETWORK-STATUS
               PERFORM 1400-VALIDATE-TAXONOMY
               PERFORM 1500-CHECK-LICENSE-STATE
               PERFORM 1600-CHECK-DATE-OF-SERVICE
           END-IF
           PERFORM 1700-BUILD-RESPONSE.

       1100-LOOKUP-PROVIDER.
           EXEC SQL
               SELECT P.NPI,
                      P.LAST_NAME,
                      P.FIRST_NAME,
                      P.ORG_NAME,
                      P.TAXONOMY_CODE,
                      P.SPECIALTY_CODE,
                      P.ENTITY_TYPE,
                      C.NETWORK_ID,
                      C.CONTRACT_ID,
                      C.CONTRACT_TYPE,
                      C.PAR_STATUS,
                      C.EFF_DATE,
                      C.TERM_DATE,
                      C.FEE_SCHEDULE_ID,
                      CR.CRED_STATUS,
                      CR.CRED_DATE,
                      CR.CRED_EXPIRE_DATE,
                      P.RECORD_STATUS,
                      P.STATE_CODE,
                      P.LICENSE_NO,
                      P.LICENSE_STATE
               INTO :WS-DB-PRV-NPI,
                    :WS-DB-PRV-LAST-NAME,
                    :WS-DB-PRV-FIRST-NAME,
                    :WS-DB-PRV-ORG-NAME,
                    :WS-DB-PRV-TAXONOMY,
                    :WS-DB-PRV-SPECIALTY,
                    :WS-DB-PRV-ENTITY-TYPE,
                    :WS-DB-PRV-NETWORK-ID,
                    :WS-DB-PRV-CONTRACT-ID,
                    :WS-DB-PRV-CONTRACT-TYPE,
                    :WS-DB-PRV-PAR-STATUS,
                    :WS-DB-PRV-EFF-DATE,
                    :WS-DB-PRV-TERM-DATE,
                    :WS-DB-PRV-FEE-SCHED,
                    :WS-DB-PRV-CRED-STATUS,
                    :WS-DB-PRV-CRED-DATE,
                    :WS-DB-PRV-CRED-EXPIRE,
                    :WS-DB-PRV-REC-STATUS,
                    :WS-DB-PRV-STATE,
                    :WS-DB-PRV-LICENSE-NO,
                    :WS-DB-PRV-LICENSE-STATE
               FROM  HCAS.PROVIDER P
               LEFT JOIN HCAS.PROVIDER_CONTRACT C
                   ON P.NPI = C.NPI
               LEFT JOIN HCAS.PROVIDER_CREDENTIAL CR
                   ON P.NPI = CR.NPI
               WHERE P.NPI = :LS-REQ-NPI
                 AND P.RECORD_STATUS = 'A'
                 AND (C.EFF_DATE <= :LS-REQ-DOS
                      OR C.EFF_DATE IS NULL)
                 AND (C.TERM_DATE >= :LS-REQ-DOS
                      OR C.TERM_DATE IS NULL
                      OR C.TERM_DATE = '99991231')
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE = +100
               MOVE 'NF' TO WS-NPI-EDIT-RESULT
               SET LS-RSP-DENIED TO TRUE
               MOVE 'PR001' TO LS-RSP-EDIT(1)
               MOVE 'PROVIDER NPI NOT FOUND IN MASTER FILE'
                   TO LS-RSP-MSG
           ELSE IF SQLCODE NOT = ZERO
               MOVE 'ER' TO WS-NPI-EDIT-RESULT
               SET LS-RSP-ERROR TO TRUE
               STRING 'DB2 ERROR ON PROVIDER LOOKUP SQLCODE='
                      DELIMITED SIZE
                      SQLCODE DELIMITED SIZE
                      INTO LS-RSP-MSG
               END-STRING
           END-IF.

       1200-CHECK-CREDENTIALING.
           IF WS-DB-PRV-CRED-STATUS = 'DN'
               MOVE 'DN' TO WS-CRED-EDIT-RESULT
               SET LS-RSP-DENIED TO TRUE
               MOVE 'PR002' TO LS-RSP-EDIT(2)
               MOVE 'PROVIDER CREDENTIALING DENIED'
                   TO LS-RSP-MSG
           ELSE IF WS-DB-PRV-CRED-STATUS = 'PN'
               MOVE 'PN' TO WS-CRED-EDIT-RESULT
               SET LS-RSP-PENDED TO TRUE
               MOVE 'PR003' TO LS-RSP-EDIT(2)
           ELSE IF WS-DB-PRV-CRED-STATUS = 'EX'
               IF WS-DB-PRV-CRED-EXPIRE < LS-REQ-DOS
                   MOVE 'EX' TO WS-CRED-EDIT-RESULT
                   SET LS-RSP-PENDED TO TRUE
                   MOVE 'PR004' TO LS-RSP-EDIT(2)
                   MOVE 'PROVIDER CREDENTIALS EXPIRED'
                       TO LS-RSP-MSG
               END-IF
           END-IF.

       1300-CHECK-NETWORK-STATUS.
           IF WS-DB-PRV-PAR-STATUS = 'N'
               MOVE 'NP' TO WS-NETWORK-EDIT-RESULT
               MOVE 'PR005' TO LS-RSP-EDIT(3)
           END-IF
           IF LS-REQ-PLAN-NETWORK NOT = SPACES
               IF WS-DB-PRV-NETWORK-ID NOT = LS-REQ-PLAN-NETWORK
                   MOVE 'OO' TO WS-NETWORK-EDIT-RESULT
                   MOVE 'PR006' TO LS-RSP-EDIT(4)
               END-IF
           END-IF.

       1400-VALIDATE-TAXONOMY.
           IF LS-REQ-TAXONOMY NOT = SPACES
               SET WS-TAXONOMY-INVALID TO TRUE
               PERFORM VARYING WS-TAX-IDX FROM 1 BY 1
                   UNTIL WS-TAX-IDX > 10
                   OR WS-TAXONOMY-VALID
                   IF WS-DB-PRV-TAXONOMY = WS-TAX-CODE(WS-TAX-IDX)
                       SET WS-TAXONOMY-VALID TO TRUE
                   END-IF
               END-PERFORM
               IF WS-TAXONOMY-INVALID
                   MOVE 'TX' TO WS-TAX-EDIT-RESULT
                   MOVE 'PR007' TO LS-RSP-EDIT(5)
               END-IF
           END-IF.

       1500-CHECK-LICENSE-STATE.
           IF LS-REQ-SVC-STATE NOT = SPACES
               IF WS-DB-PRV-LICENSE-STATE NOT = LS-REQ-SVC-STATE
                   MOVE 'LS' TO WS-LICENSE-EDIT-RESULT
                   MOVE 'PR008' TO LS-RSP-EDIT(6)
               END-IF
           END-IF.

       1600-CHECK-DATE-OF-SERVICE.
           IF WS-DB-PRV-EFF-DATE > LS-REQ-DOS
               MOVE 'ED' TO WS-DATE-EDIT-RESULT
               SET LS-RSP-DENIED TO TRUE
               MOVE 'PR009' TO LS-RSP-EDIT(7)
               MOVE 'PROVIDER NOT EFFECTIVE ON DOS'
                   TO LS-RSP-MSG
           END-IF
           IF WS-DB-PRV-TERM-DATE NOT = '99991231'
           AND WS-DB-PRV-TERM-DATE NOT = SPACES
               IF WS-DB-PRV-TERM-DATE < LS-REQ-DOS
                   MOVE 'TD' TO WS-DATE-EDIT-RESULT
                   SET LS-RSP-DENIED TO TRUE
                   MOVE 'PR010' TO LS-RSP-EDIT(8)
                   MOVE 'PROVIDER CONTRACT TERMINATED BEFORE DOS'
                       TO LS-RSP-MSG
               END-IF
           END-IF.

       1700-BUILD-RESPONSE.
           IF WS-DB-PRV-ENTITY-TYPE = '2'
               MOVE WS-DB-PRV-ORG-NAME TO LS-RSP-PRV-NAME
           ELSE
               STRING WS-DB-PRV-LAST-NAME DELIMITED SPACES
                      ', '                 DELIMITED SIZE
                      WS-DB-PRV-FIRST-NAME DELIMITED SPACES
                      INTO LS-RSP-PRV-NAME
               END-STRING
           END-IF
           MOVE WS-DB-PRV-PAR-STATUS   TO LS-RSP-PAR-STATUS
           MOVE WS-DB-PRV-NETWORK-ID   TO LS-RSP-NETWORK-ID
           MOVE WS-DB-PRV-CONTRACT-TYPE TO LS-RSP-CONTRACT-TYPE
           MOVE WS-DB-PRV-FEE-SCHED    TO LS-RSP-FEE-SCHED-ID
           MOVE WS-DB-PRV-CRED-STATUS  TO LS-RSP-CRED-STATUS.

       2000-FEE-SCHEDULE-LOOKUP.
           IF WS-DB-PRV-FEE-SCHED = SPACES
               MOVE 'NO FEE SCHEDULE ASSIGNED TO PROVIDER'
                   TO LS-RSP-MSG
               MOVE ZERO TO LS-RSP-FEE-AMOUNT
           ELSE
               EXEC SQL
                   SELECT FEE_AMOUNT
                   INTO :WS-FEE-AMOUNT
                   FROM HCAS.FEE_SCHEDULE
                   WHERE FEE_SCHED_ID = :WS-DB-PRV-FEE-SCHED
                     AND PROC_CODE = :LS-REQ-PROC-CODE
                     AND (MODIFIER = :LS-REQ-MODIFIER
                          OR MODIFIER = '  ')
                     AND EFF_DATE <= :LS-REQ-DOS
                     AND (TERM_DATE >= :LS-REQ-DOS
                          OR TERM_DATE = '99991231')
                   ORDER BY MODIFIER DESC
                   FETCH FIRST 1 ROW ONLY
               END-EXEC
               IF SQLCODE = ZERO
                   MOVE WS-FEE-AMOUNT TO LS-RSP-FEE-AMOUNT
               ELSE
                   MOVE ZERO TO LS-RSP-FEE-AMOUNT
                   IF SQLCODE = +100
                       MOVE 'NO FEE FOUND FOR PROC/MODIFIER'
                           TO LS-RSP-MSG
                   ELSE
                       STRING 'FEE LOOKUP DB2 ERR SQLCODE='
                              DELIMITED SIZE
                              SQLCODE DELIMITED SIZE
                              INTO LS-RSP-MSG
                       END-STRING
                   END-IF
               END-IF
           END-IF.
