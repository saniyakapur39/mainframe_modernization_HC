      *================================================================*
      * COPYBOOK: PLNCOPY                                              *
      * PURPOSE:  PLAN AND BENEFIT CONFIGURATION DATA STRUCTURES       *
      * SYSTEM:   HEALTHCARE CLAIMS ADMINISTRATION SYSTEM (HCAS)       *
      *================================================================*

       01  BENEFIT-PLAN-RECORD.
           05  BPL-KEY.
               10  BPL-PLAN-CODE           PIC X(08).
               10  BPL-EFF-DATE            PIC 9(08).
           05  BPL-PLAN-INFO.
               10  BPL-PLAN-NAME           PIC X(50).
               10  BPL-PRODUCT-TYPE        PIC X(03).
               10  BPL-COVERAGE-TYPE       PIC X(02).
               10  BPL-LOB-CODE            PIC X(03).
                   88  BPL-LOB-COMMERCIAL  VALUE 'COM'.
                   88  BPL-LOB-MEDICARE    VALUE 'MCR'.
                   88  BPL-LOB-MEDICAID    VALUE 'MCD'.
                   88  BPL-LOB-EXCHANGE    VALUE 'EXC'.
               10  BPL-METAL-LEVEL         PIC X(02).
                   88  BPL-METAL-BRONZE    VALUE 'BR'.
                   88  BPL-METAL-SILVER    VALUE 'SV'.
                   88  BPL-METAL-GOLD      VALUE 'GD'.
                   88  BPL-METAL-PLATINUM  VALUE 'PT'.
               10  BPL-STATUS              PIC X(01).
                   88  BPL-STATUS-ACTIVE   VALUE 'A'.
                   88  BPL-STATUS-CLOSED   VALUE 'C'.
                   88  BPL-STATUS-PENDING  VALUE 'P'.
               10  BPL-TERM-DATE           PIC 9(08).
           05  BPL-COST-SHARING.
               10  BPL-IND-DEDUCTIBLE      PIC S9(07)V99 COMP-3.
               10  BPL-FAM-DEDUCTIBLE      PIC S9(07)V99 COMP-3.
               10  BPL-IND-OOP-MAX         PIC S9(07)V99 COMP-3.
               10  BPL-FAM-OOP-MAX         PIC S9(07)V99 COMP-3.
               10  BPL-COPAY-PCP           PIC S9(05)V99 COMP-3.
               10  BPL-COPAY-SPEC          PIC S9(05)V99 COMP-3.
               10  BPL-COPAY-ER            PIC S9(05)V99 COMP-3.
               10  BPL-COPAY-URGENT        PIC S9(05)V99 COMP-3.
               10  BPL-COINS-IN-NETWORK    PIC V99   COMP-3.
               10  BPL-COINS-OUT-NETWORK   PIC V99   COMP-3.
               10  BPL-LIFETIME-MAX        PIC S9(09)V99 COMP-3.
           05  BPL-NETWORK-CONFIG.
               10  BPL-NETWORK-ID          PIC X(06).
               10  BPL-REFERRAL-REQ        PIC X(01).
                   88  BPL-REFERRAL-YES    VALUE 'Y'.
                   88  BPL-REFERRAL-NO     VALUE 'N'.
               10  BPL-PREAUTH-REQ         PIC X(01).
                   88  BPL-PREAUTH-YES     VALUE 'Y'.
                   88  BPL-PREAUTH-NO      VALUE 'N'.
           05  BPL-SERVICE-LIMITS OCCURS 20 TIMES.
               10  BPL-SVC-CATEGORY        PIC X(04).
               10  BPL-SVC-LIMIT-TYPE      PIC X(02).
                   88  BPL-LIM-VISITS      VALUE 'VS'.
                   88  BPL-LIM-DAYS        VALUE 'DY'.
                   88  BPL-LIM-DOLLAR      VALUE 'DL'.
               10  BPL-SVC-LIMIT-QTY       PIC 9(05).
               10  BPL-SVC-LIMIT-PERIOD    PIC X(02).
                   88  BPL-PER-CALENDAR-YR VALUE 'CY'.
                   88  BPL-PER-PLAN-YEAR   VALUE 'PY'.
                   88  BPL-PER-LIFETIME    VALUE 'LT'.
               10  BPL-SVC-COVERED         PIC X(01).
                   88  BPL-SVC-IS-COVERED  VALUE 'Y'.
                   88  BPL-SVC-NOT-COVERED VALUE 'N'.
           05  BPL-AUDIT.
               10  BPL-CREATE-DATE         PIC 9(08).
               10  BPL-CREATE-USER         PIC X(08).
               10  BPL-UPDATE-DATE         PIC 9(08).
               10  BPL-UPDATE-USER         PIC X(08).
