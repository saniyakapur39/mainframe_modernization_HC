      *================================================================*
      * COPYBOOK: CLMCOPY                                              *
      * PURPOSE:  CLAIMS DATA STRUCTURES AND RECORD LAYOUTS            *
      * SYSTEM:   HEALTHCARE CLAIMS ADMINISTRATION SYSTEM (HCAS)       *
      *================================================================*

       01  CLAIM-MASTER-RECORD.
           05  CLM-HEADER.
               10  CLM-ID                  PIC X(15).
               10  CLM-TYPE-CODE           PIC X(02).
                   88  CLM-TYPE-INST       VALUE 'IN'.
                   88  CLM-TYPE-PROF       VALUE 'PR'.
                   88  CLM-TYPE-DENTAL     VALUE 'DN'.
               10  CLM-STATUS-CODE         PIC X(02).
                   88  CLM-STAT-RECEIVED   VALUE '00'.
                   88  CLM-STAT-ACCEPTED   VALUE '10'.
                   88  CLM-STAT-ADJUD      VALUE '20'.
                   88  CLM-STAT-PENDED     VALUE '30'.
                   88  CLM-STAT-DENIED     VALUE '40'.
                   88  CLM-STAT-PAID       VALUE '50'.
                   88  CLM-STAT-REVERSED   VALUE '99'.
               10  CLM-SOURCE-CODE         PIC X(02).
                   88  CLM-SRC-EDI         VALUE 'ED'.
                   88  CLM-SRC-PAPER       VALUE 'PP'.
                   88  CLM-SRC-DDE         VALUE 'DD'.
               10  CLM-RECEIPT-DATE        PIC 9(08).
               10  CLM-PROCESS-DATE        PIC 9(08).
               10  CLM-ADJUD-DATE          PIC 9(08).
           05  CLM-MEMBER-INFO.
               10  CLM-MEMBER-ID           PIC X(12).
               10  CLM-SUBSCRIBER-ID       PIC X(12).
               10  CLM-PATIENT-ACCT-NO     PIC X(20).
               10  CLM-MEMBER-DOB          PIC 9(08).
               10  CLM-MEMBER-SEX          PIC X(01).
               10  CLM-RELATIONSHIP-CODE   PIC X(02).
           05  CLM-PROVIDER-INFO.
               10  CLM-BILLING-NPI         PIC X(10).
               10  CLM-RENDERING-NPI       PIC X(10).
               10  CLM-FACILITY-NPI        PIC X(10).
               10  CLM-REFERRING-NPI       PIC X(10).
               10  CLM-PLACE-OF-SVC        PIC X(02).
               10  CLM-PROVIDER-TAX-ID     PIC X(09).
           05  CLM-DIAGNOSIS-INFO.
               10  CLM-PRINCIPAL-DIAG      PIC X(08).
               10  CLM-ADMIT-DIAG          PIC X(08).
               10  CLM-OTHER-DIAG          OCCURS 24 TIMES.
                   15  CLM-DIAG-CODE       PIC X(08).
           05  CLM-FINANCIAL-INFO.
               10  CLM-TOTAL-CHARGES       PIC S9(09)V99 COMP-3.
               10  CLM-ALLOWED-AMT         PIC S9(09)V99 COMP-3.
               10  CLM-PAID-AMT            PIC S9(09)V99 COMP-3.
               10  CLM-COPAY-AMT           PIC S9(07)V99 COMP-3.
               10  CLM-DEDUCTIBLE-AMT      PIC S9(07)V99 COMP-3.
               10  CLM-COINSURANCE-AMT     PIC S9(07)V99 COMP-3.
               10  CLM-COB-AMT             PIC S9(09)V99 COMP-3.
               10  CLM-INTEREST-AMT        PIC S9(07)V99 COMP-3.
               10  CLM-WITHHOLD-AMT        PIC S9(07)V99 COMP-3.
           05  CLM-LINE-ITEMS.
               10  CLM-LINE-COUNT          PIC 9(03) COMP-3.
               10  CLM-LINE OCCURS 50 TIMES
                          DEPENDING ON CLM-LINE-COUNT.
                   15  CLM-LN-SEQ          PIC 9(03).
                   15  CLM-LN-PROC-CODE    PIC X(05).
                   15  CLM-LN-MODIFIER     PIC X(02)
                                           OCCURS 4 TIMES.
                   15  CLM-LN-REV-CODE     PIC X(04).
                   15  CLM-LN-DOS-FROM     PIC 9(08).
                   15  CLM-LN-DOS-THRU     PIC 9(08).
                   15  CLM-LN-UNITS        PIC S9(05)V99 COMP-3.
                   15  CLM-LN-CHARGE       PIC S9(07)V99 COMP-3.
                   15  CLM-LN-ALLOWED      PIC S9(07)V99 COMP-3.
                   15  CLM-LN-PAID         PIC S9(07)V99 COMP-3.
                   15  CLM-LN-DENY-CODE    PIC X(05).
                   15  CLM-LN-ADJ-REASON   PIC X(05)
                                           OCCURS 5 TIMES.
                   15  CLM-LN-NDC-CODE     PIC X(11).
                   15  CLM-LN-DRG-CODE     PIC X(04).

       01  CLAIM-ADJUSTMENT-RECORD.
           05  CADJ-CLAIM-ID              PIC X(15).
           05  CADJ-LINE-SEQ              PIC 9(03).
           05  CADJ-GROUP-CODE            PIC X(02).
               88  CADJ-GRP-CONTRACTUAL   VALUE 'CO'.
               88  CADJ-GRP-PATIENT-RESP  VALUE 'PR'.
               88  CADJ-GRP-OTHER-ADJ     VALUE 'OA'.
               88  CADJ-GRP-PAYOR-INIT    VALUE 'PI'.
               88  CADJ-GRP-CORRECTION    VALUE 'CR'.
           05  CADJ-REASON-CODE           PIC X(05).
           05  CADJ-AMOUNT                PIC S9(09)V99 COMP-3.
           05  CADJ-QUANTITY              PIC S9(05)V99 COMP-3.
