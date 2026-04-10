      *================================================================*
      * COPYBOOK: HCCOMMON                                             *
      * PURPOSE:  COMMON DATA STRUCTURES FOR HEALTHCARE PAYER SYSTEM   *
      *           SHARED ACROSS ALL PROGRAM MODULES                    *
      * SYSTEM:   HEALTHCARE CLAIMS ADMINISTRATION SYSTEM (HCAS)       *
      *================================================================*

       01  WS-COMMON-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YYYY        PIC 9(04).
               10  WS-CURR-MM          PIC 9(02).
               10  WS-CURR-DD          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HH          PIC 9(02).
               10  WS-CURR-MI          PIC 9(02).
               10  WS-CURR-SS          PIC 9(02).
               10  WS-CURR-MS          PIC 9(02).
           05  WS-PROGRAM-ID           PIC X(08).
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-ABEND-CODE           PIC X(04)   VALUE SPACES.
           05  WS-ERROR-MSG            PIC X(80)   VALUE SPACES.

       01  WS-FILE-STATUS-CODES.
           05  WS-FS-CODE              PIC X(02).
               88  WS-FS-SUCCESS       VALUE '00'.
               88  WS-FS-EOF           VALUE '10'.
               88  WS-FS-DUP-KEY       VALUE '22'.
               88  WS-FS-NOT-FOUND     VALUE '23'.
               88  WS-FS-PERM-ERROR    VALUE '30'.

       01  WS-DB2-SQLCODE              PIC S9(09) COMP VALUE ZERO.
           88  DB2-SUCCESS             VALUE ZERO.
           88  DB2-NOT-FOUND           VALUE +100.
           88  DB2-DUPLICATE           VALUE -803.
           88  DB2-DEADLOCK            VALUE -911.

       01  WS-PROCESSING-COUNTERS.
           05  WS-RECORDS-READ         PIC 9(09) COMP VALUE ZERO.
           05  WS-RECORDS-WRITTEN      PIC 9(09) COMP VALUE ZERO.
           05  WS-RECORDS-UPDATED      PIC 9(09) COMP VALUE ZERO.
           05  WS-RECORDS-REJECTED     PIC 9(09) COMP VALUE ZERO.
           05  WS-RECORDS-SKIPPED      PIC 9(09) COMP VALUE ZERO.

       01  WS-ACCUMULATOR-FIELDS.
           05  WS-TOTAL-AMOUNT         PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-BATCH-TOTAL          PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05  WS-HASH-TOTAL           PIC S9(15) COMP-3
                                       VALUE ZERO.

       01  WS-DATE-VALIDATION-AREA.
           05  WS-VALID-DATE-SW        PIC X(01)  VALUE 'Y'.
               88  WS-DATE-VALID       VALUE 'Y'.
               88  WS-DATE-INVALID     VALUE 'N'.
           05  WS-CHECK-DATE           PIC 9(08).
           05  WS-CHECK-DATE-R REDEFINES WS-CHECK-DATE.
               10  WS-CHK-YYYY         PIC 9(04).
               10  WS-CHK-MM           PIC 9(02).
               10  WS-CHK-DD           PIC 9(02).

       01  WS-PAYER-CONSTANTS.
           05  WS-SYSTEM-PAYER-ID      PIC X(10)
                                       VALUE 'HCAS000001'.
           05  WS-TAX-ID-PAYER         PIC X(09)
                                       VALUE '123456789'.
           05  WS-PAYER-NAME           PIC X(35)
                                       VALUE 'NATIONAL HEALTH PARTNERS INC'.
           05  WS-NPI-PAYER            PIC X(10)
                                       VALUE '1234567890'.
