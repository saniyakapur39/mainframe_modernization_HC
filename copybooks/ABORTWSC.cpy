      *================================================================*
      * COPYBOOK: ABORTWSC                                             *
      * PURPOSE:  STANDARD ABEND AND ERROR HANDLING WORK AREAS         *
      * SYSTEM:   HEALTHCARE CLAIMS ADMINISTRATION SYSTEM (HCAS)       *
      *================================================================*

       01  WS-ABEND-HANDLING.
           05  WS-ABEND-PGM            PIC X(08).
           05  WS-ABEND-PARAGRAPH      PIC X(30).
           05  WS-ABEND-REASON         PIC X(80).
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-SQLCODE        PIC S9(09) COMP.
           05  WS-ABEND-SQLERRM        PIC X(80).
           05  WS-ABEND-CICS-RESP      PIC S9(08) COMP.
           05  WS-ABEND-CICS-RESP2     PIC S9(08) COMP.
           05  WS-ABEND-TIMESTAMP      PIC X(26).

       01  WS-ERROR-LOG-RECORD.
           05  ERR-LOG-TIMESTAMP       PIC X(26).
           05  ERR-LOG-PROGRAM         PIC X(08).
           05  ERR-LOG-SEVERITY        PIC X(01).
               88  ERR-SEVERITY-INFO   VALUE 'I'.
               88  ERR-SEVERITY-WARN   VALUE 'W'.
               88  ERR-SEVERITY-ERROR  VALUE 'E'.
               88  ERR-SEVERITY-FATAL  VALUE 'F'.
           05  ERR-LOG-CODE            PIC X(08).
           05  ERR-LOG-MESSAGE         PIC X(132).
           05  ERR-LOG-DATA-1          PIC X(50).
           05  ERR-LOG-DATA-2          PIC X(50).
