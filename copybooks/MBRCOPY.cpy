      *================================================================*
      * COPYBOOK: MBRCOPY                                              *
      * PURPOSE:  MEMBER ENROLLMENT AND ELIGIBILITY DATA STRUCTURES    *
      * SYSTEM:   HEALTHCARE CLAIMS ADMINISTRATION SYSTEM (HCAS)       *
      *================================================================*

       01  MEMBER-MASTER-RECORD.
           05  MBR-KEY.
               10  MBR-ID                  PIC X(12).
           05  MBR-DEMOGRAPHICS.
               10  MBR-LAST-NAME           PIC X(35).
               10  MBR-FIRST-NAME          PIC X(25).
               10  MBR-MIDDLE-INIT         PIC X(01).
               10  MBR-DOB                 PIC 9(08).
               10  MBR-SEX                 PIC X(01).
                   88  MBR-SEX-MALE        VALUE 'M'.
                   88  MBR-SEX-FEMALE      VALUE 'F'.
                   88  MBR-SEX-UNKNOWN     VALUE 'U'.
               10  MBR-SSN                 PIC X(09).
               10  MBR-LANGUAGE-CODE       PIC X(03).
           05  MBR-ADDRESS.
               10  MBR-ADDR-LINE-1         PIC X(55).
               10  MBR-ADDR-LINE-2         PIC X(55).
               10  MBR-CITY                PIC X(30).
               10  MBR-STATE               PIC X(02).
               10  MBR-ZIP-CODE            PIC X(09).
               10  MBR-COUNTY-CODE         PIC X(05).
               10  MBR-PHONE               PIC X(10).
           05  MBR-ENROLLMENT-INFO.
               10  MBR-SUBSCRIBER-ID       PIC X(12).
               10  MBR-RELATIONSHIP-CODE   PIC X(02).
                   88  MBR-REL-SELF        VALUE '18'.
                   88  MBR-REL-SPOUSE      VALUE '01'.
                   88  MBR-REL-CHILD       VALUE '19'.
                   88  MBR-REL-OTHER       VALUE '21'.
               10  MBR-GROUP-ID            PIC X(10).
               10  MBR-PLAN-CODE           PIC X(08).
               10  MBR-PRODUCT-TYPE        PIC X(03).
                   88  MBR-PROD-HMO        VALUE 'HMO'.
                   88  MBR-PROD-PPO        VALUE 'PPO'.
                   88  MBR-PROD-POS        VALUE 'POS'.
                   88  MBR-PROD-EPO        VALUE 'EPO'.
                   88  MBR-PROD-IND        VALUE 'IND'.
               10  MBR-COVERAGE-TYPE       PIC X(02).
                   88  MBR-COV-MEDICAL      VALUE 'MD'.
                   88  MBR-COV-DENTAL       VALUE 'DN'.
                   88  MBR-COV-VISION       VALUE 'VS'.
                   88  MBR-COV-PHARMACY     VALUE 'RX'.
               10  MBR-EFF-DATE            PIC 9(08).
               10  MBR-TERM-DATE           PIC 9(08).
               10  MBR-ELIG-STATUS         PIC X(01).
                   88  MBR-ELIG-ACTIVE     VALUE 'A'.
                   88  MBR-ELIG-TERMED     VALUE 'T'.
                   88  MBR-ELIG-PENDED     VALUE 'P'.
                   88  MBR-ELIG-COBRA      VALUE 'C'.
           05  MBR-BENEFIT-ACCUMULATORS.
               10  MBR-DEDUCT-YTD          PIC S9(07)V99 COMP-3.
               10  MBR-DEDUCT-LIMIT        PIC S9(07)V99 COMP-3.
               10  MBR-OOP-YTD             PIC S9(07)V99 COMP-3.
               10  MBR-OOP-LIMIT           PIC S9(07)V99 COMP-3.
               10  MBR-COPAY-YTD           PIC S9(07)V99 COMP-3.
               10  MBR-COINS-PCT           PIC V99   COMP-3.
               10  MBR-LIFETIME-MAX        PIC S9(09)V99 COMP-3.
               10  MBR-LIFETIME-USED       PIC S9(09)V99 COMP-3.
           05  MBR-AUDIT-INFO.
               10  MBR-CREATE-DATE         PIC 9(08).
               10  MBR-CREATE-USER         PIC X(08).
               10  MBR-UPDATE-DATE         PIC 9(08).
               10  MBR-UPDATE-USER         PIC X(08).
               10  MBR-UPDATE-COUNT        PIC 9(05) COMP-3.

       01  MEMBER-ELIGIBILITY-RESPONSE.
           05  ELIG-RESP-CODE             PIC X(02).
               88  ELIG-RESP-ACTIVE       VALUE 'AA'.
               88  ELIG-RESP-INACTIVE     VALUE 'IA'.
               88  ELIG-RESP-NOT-FOUND    VALUE 'NF'.
               88  ELIG-RESP-ERROR        VALUE 'ER'.
           05  ELIG-RESP-MSG              PIC X(80).
           05  ELIG-RESP-EFF-DATE         PIC 9(08).
           05  ELIG-RESP-TERM-DATE        PIC 9(08).
           05  ELIG-RESP-PLAN-CODE        PIC X(08).
           05  ELIG-RESP-GROUP-ID         PIC X(10).
           05  ELIG-RESP-PRODUCT-TYPE     PIC X(03).
           05  ELIG-RESP-COVERAGE-TYPE    PIC X(02).
           05  ELIG-RESP-COPAY-AMT        PIC S9(05)V99 COMP-3.
           05  ELIG-RESP-DEDUCT-REMAIN    PIC S9(07)V99 COMP-3.
           05  ELIG-RESP-OOP-REMAIN       PIC S9(07)V99 COMP-3.
           05  ELIG-RESP-PCP-NPI          PIC X(10).
           05  ELIG-RESP-PCP-NAME         PIC X(35).
