      *================================================================*
      * COPYBOOK: PRVCOPY                                              *
      * PURPOSE:  PROVIDER NETWORK MANAGEMENT DATA STRUCTURES          *
      * SYSTEM:   HEALTHCARE CLAIMS ADMINISTRATION SYSTEM (HCAS)       *
      *================================================================*

       01  PROVIDER-MASTER-RECORD.
           05  PRV-KEY.
               10  PRV-NPI                 PIC X(10).
           05  PRV-DEMOGRAPHICS.
               10  PRV-LAST-NAME           PIC X(35).
               10  PRV-FIRST-NAME          PIC X(25).
               10  PRV-MIDDLE-INIT         PIC X(01).
               10  PRV-ORG-NAME            PIC X(60).
               10  PRV-TAX-ID              PIC X(09).
               10  PRV-TAX-ID-TYPE         PIC X(02).
                   88  PRV-TIN-EIN         VALUE 'EI'.
                   88  PRV-TIN-SSN         VALUE 'SN'.
               10  PRV-ENTITY-TYPE         PIC X(01).
                   88  PRV-ENTITY-IND      VALUE '1'.
                   88  PRV-ENTITY-ORG      VALUE '2'.
           05  PRV-PRACTICE-INFO.
               10  PRV-TAXONOMY-CODE       PIC X(10).
               10  PRV-SPECIALTY-CODE      PIC X(04).
               10  PRV-LICENSE-NO          PIC X(20).
               10  PRV-LICENSE-STATE       PIC X(02).
               10  PRV-DEA-NUMBER          PIC X(09).
               10  PRV-MEDICARE-ID         PIC X(15).
               10  PRV-MEDICAID-ID         PIC X(15).
           05  PRV-ADDRESS.
               10  PRV-ADDR-LINE-1         PIC X(55).
               10  PRV-ADDR-LINE-2         PIC X(55).
               10  PRV-CITY                PIC X(30).
               10  PRV-STATE               PIC X(02).
               10  PRV-ZIP-CODE            PIC X(09).
               10  PRV-PHONE               PIC X(10).
               10  PRV-FAX                 PIC X(10).
           05  PRV-CONTRACT-INFO.
               10  PRV-NETWORK-ID          PIC X(06).
               10  PRV-CONTRACT-ID         PIC X(12).
               10  PRV-CONTRACT-TYPE       PIC X(02).
                   88  PRV-CONTR-FFS       VALUE 'FF'.
                   88  PRV-CONTR-CAPITATED VALUE 'CP'.
                   88  PRV-CONTR-PERCENT   VALUE 'PC'.
               10  PRV-PAR-STATUS          PIC X(01).
                   88  PRV-IS-PAR          VALUE 'Y'.
                   88  PRV-IS-NON-PAR      VALUE 'N'.
               10  PRV-EFF-DATE            PIC 9(08).
               10  PRV-TERM-DATE           PIC 9(08).
               10  PRV-FEE-SCHED-ID        PIC X(08).
           05  PRV-CREDENTIALING.
               10  PRV-CRED-STATUS         PIC X(02).
                   88  PRV-CRED-APPROVED   VALUE 'AP'.
                   88  PRV-CRED-PENDING    VALUE 'PN'.
                   88  PRV-CRED-DENIED     VALUE 'DN'.
                   88  PRV-CRED-EXPIRED    VALUE 'EX'.
               10  PRV-CRED-DATE           PIC 9(08).
               10  PRV-CRED-EXPIRE-DATE    PIC 9(08).
               10  PRV-BOARD-CERT          PIC X(01).
                   88  PRV-IS-BOARD-CERT   VALUE 'Y'.
           05  PRV-PAYMENT-INFO.
               10  PRV-PAY-METHOD          PIC X(02).
                   88  PRV-PAY-CHECK       VALUE 'CK'.
                   88  PRV-PAY-EFT         VALUE 'EF'.
               10  PRV-BANK-ROUTING        PIC X(09).
               10  PRV-BANK-ACCT-NO        PIC X(17).
               10  PRV-BANK-ACCT-TYPE      PIC X(02).
           05  PRV-AUDIT.
               10  PRV-CREATE-DATE         PIC 9(08).
               10  PRV-UPDATE-DATE         PIC 9(08).
               10  PRV-UPDATE-USER         PIC X(08).
               10  PRV-RECORD-STATUS       PIC X(01).
                   88  PRV-REC-ACTIVE      VALUE 'A'.
                   88  PRV-REC-INACTIVE    VALUE 'I'.
                   88  PRV-REC-DELETED     VALUE 'D'.
