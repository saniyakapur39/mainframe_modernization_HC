# Healthcare Payer COBOL Demonstration Collection

## Overview

This collection contains **14 COBOL programs** and **7 shared copybooks** representing the core business processes found in a typical healthcare payer mainframe environment. These programs model the complete administrative lifecycle of a health insurance organization — from benefit plan configuration and member enrollment through claims adjudication, payment processing, and provider network management.

The programs are organized into three complexity tiers to demonstrate a conversion framework's capability across progressively challenging COBOL patterns. Each program is self-contained in terms of business logic, references shared copybooks for data structures, and uses realistic naming conventions, paragraph structures, and coding patterns consistent with production mainframe systems.

---

## Folder Structure

```
healthcare_cobol_demo/
├── GUIDE.md                    ← This document
├── copybooks/                  ← Shared data structure definitions
│   ├── HCCOMMON.cpy            ← Common fields, counters, constants
│   ├── CLMCOPY.cpy             ← Claims master and line item layouts
│   ├── MBRCOPY.cpy             ← Member enrollment and eligibility
│   ├── PRVCOPY.cpy             ← Provider master file structures
│   ├── PLNCOPY.cpy             ← Benefit plan configuration layouts
│   ├── EDI837CP.cpy            ← X12 837 claim with REDEFINES/OCCURS DEPENDING ON
│   └── ABORTWSC.cpy            ← Standard error/abend handling areas
├── simple/                     ← Entry-level programs (≈250–420 lines)
│   ├── MEMRPT01.cbl            ← Enrollment error report generator
│   ├── PRVLD001.cbl            ← Provider master file loader
│   └── ELIGINQ1.cbl            ← Real-time eligibility inquiry (CICS)
├── medium/                     ← Intermediate programs (≈360–600 lines)
│   ├── BENBLD01.cbl            ← Benefit plan builder (DB2 batch)
│   ├── EDI834IN.cbl            ← EDI 834 enrollment file parser
│   ├── MEMXREF1.cbl            ← Member identity cross-reference
│   └── PRVVAL02.cbl            ← Provider credentialing verification
└── complex/                    ← Advanced programs (≈680–1380 lines)
    ├── CLMADJ01.cbl            ← Claims adjudication engine (main driver)
    ├── CLMPRC01.cbl            ← Claims pricing engine (subroutine)
    ├── EDI835GN.cbl            ← EDI 835 remittance/EOB generator
    ├── CLMDTL00.cbl            ← Claims detail inquiry (CICS+DB2+VSAM)
    ├── EXTAPI01.cbl            ← External REST API pricing integration
    └── MEMMGR01.cbl            ← EDI batch processor (POINTER/memory mgmt)
```

---

## Program Inventory

### Simple Tier

| Program | Healthcare Domain | Description | Key COBOL Features |
|---------|------------------|-------------|-------------------|
| **MEMRPT01** | Member Enrollment | Reads the enrollment error log file produced by nightly EDI 834 batch processing and generates a formatted print report with severity tallies, transaction type breakdowns, and page-controlled output for enrollment operations staff. | Sequential file I/O, report writing with `WRITE AFTER ADVANCING`, carriage control (`C01 IS PAGE-TOP`), `EVALUATE` statements, formatted counters |
| **PRVLD001** | Provider Network | Loads new and updated provider records from a flat file into the Provider Master VSAM KSDS. Validates NPI format using a Luhn check-digit algorithm, verifies taxonomy codes, validates effective date ranges, and routes add/update/delete actions against the indexed file. | VSAM KSDS (`ORGANIZATION IS INDEXED`, `ACCESS MODE IS DYNAMIC`), `READ`/`WRITE`/`REWRITE`/`DELETE` on indexed files, `REDEFINES` for digit-level NPI parsing, `FUNCTION MOD`, input validation patterns |
| **ELIGINQ1** | Member Eligibility | CICS online transaction that accepts a member ID and date of service from a BMS map, queries DB2 enrollment and benefit plan tables via a multi-table join, and returns real-time eligibility status including remaining deductible, out-of-pocket amounts, and PCP assignment. | CICS commands (`SEND MAP`, `RECEIVE MAP`, `RETURN TRANSID`), embedded DB2 SQL with multi-table `INNER JOIN` and `LEFT JOIN`, `DFHCOMMAREA`/`EIBCALEN` pseudo-conversational pattern, `LINKAGE SECTION`, `STRING`/`COMPUTE` |

### Medium Tier

| Program | Healthcare Domain | Description | Key COBOL Features |
|---------|------------------|-------------|-------------------|
| **BENBLD01** | Plan & Benefit Setup | Batch program that reads plan configuration input and builds benefit plan records in DB2. Implements insert-only temporal logic — renewals terminate the prior row and insert a new effective-dated row; amendments mark existing rows as historical ('H') rather than overwriting. Preserves full audit trail for retroactive claims processing. | DB2 embedded SQL (`INSERT`, `UPDATE`, `SELECT`), `EXEC SQL COMMIT`/`ROLLBACK`, OCCURS table for service limits, effective-date temporal patterns, `EVALUATE` for action routing (New/Renew/Amend/Terminate) |
| **EDI834IN** | Member Enrollment | Parses inbound X12 834 Benefit Enrollment and Maintenance files. Strips ISA/GS/ST interchange envelopes, identifies segment types, uses `UNSTRING` to decompose element-delimited segments into working storage fields, and writes validated member records to an enrollment staging file. Handles INS, REF, DTP, NM1, N3, N4, DMG, and HD segments. | Variable-length records (`RECORDING MODE IS V`), `UNSTRING DELIMITED BY`, segment-driven `EVALUATE` parsing, EDI X12 envelope processing, multi-level control break logic, `INSPECT TALLYING` |
| **MEMXREF1** | Member Enrollment | Identity matching engine that reads parsed enrollment staging records and performs a three-tier matching strategy: (1) exact match by Member ID against VSAM, (2) SSN-based match with secondary verification scoring, and (3) demographic fuzzy match via DB2 query on last name, DOB, and first initial. Assigns new Member IDs from a DB2 sequence for unmatched enrollees and maintains the cross-reference table. | VSAM with alternate keys (`ALTERNATE RECORD KEY WITH DUPLICATES`), DB2 embedded SQL, `FUNCTION UPPER-CASE` for case-insensitive matching, scoring algorithm with threshold logic, DB2 sequence management, mixed VSAM/DB2 access |
| **PRVVAL02** | Provider Network | Subroutine called during claims adjudication to validate provider credentials, network participation, taxonomy codes, license state, and contract effective dates. Returns authorization status, edit codes, and fee schedule lookup results to the calling program via `LINKAGE SECTION` parameters. | `PROCEDURE DIVISION USING`, `LINKAGE SECTION` parameter passing, `GOBACK`, `REDEFINES` for taxonomy table, DB2 multi-table `LEFT JOIN`, provider edit code accumulation, `CALL`-compatible subroutine design |

### Complex Tier

| Program | Healthcare Domain | Description | Key COBOL Features |
|---------|------------------|-------------|-------------------|
| **CLMADJ01** | Claims Adjudication | Master claims adjudication driver that orchestrates a seven-phase pipeline: (1) data validation of header and line items, (2) eligibility verification via DB2, (3) provider validation via `CALL 'PRVVAL02'`, (4) duplicate claim checking, (5) pricing via `CALL 'CLMPRC01'`, (6) benefit application with deductible/coinsurance/OOP-max waterfall logic, and (7) claim finalization with DB2 inserts and accumulator updates. Uses DB2 cursors with `FOR UPDATE` for staged claim processing. | DB2 cursors (`DECLARE CURSOR`, `FETCH`, `CLOSE`), `FOR UPDATE OF` positioned updates, `CALL` verb for inter-program communication, multi-phase pipeline with switch-controlled flow, COMP-3 financial arithmetic, `OCCURS DEPENDING ON` line item processing |
| **CLMPRC01** | Claims Pricing | Pricing engine subroutine supporting four methodologies: fee-for-service with RBRVS/RVU calculation and geographic wage index adjustment, percent-of-charge, capitation, and DRG-based institutional pricing with operating/capital rate splitting and cost outlier threshold processing. Implements multi-procedure surgical discounting and lesser-of-charge logic. | `PROCEDURE DIVISION USING` with complex structures, RBRVS calculation (Work RVU + PE RVU + MP RVU × Conversion Factor × Wage Index), DRG weight-based payment with proportional line distribution, `COMPUTE ROUNDED` for actuarial precision, multi-procedure discount algorithm, OCCURS tables for line-level work areas |
| **EDI835GN** | Billing & Remittance | Generates outbound X12 835 Health Care Claim Payment/Advice files for provider remittance, produces Explanation of Benefits (EOB) flat file records for member mailing, and writes a check register report. Implements provider-level control breaks to group claims per payee, builds complete ISA/GS/ST/BPR/TRN/N1/CLP/NM1/SVC/CAS/SE/GE/IEA envelope structure, and handles EFT vs. check payment method routing. Includes precise COMP-3 to display formatting with zero-fill and space-fill per X12 5010 positional requirements. | Multi-file output (835 + EOB + report), DB2 cursors with nested cursor processing (claim-level + line-level), provider control break logic, X12 835 segment generation with `STRING`, COMP-3 to display field formatting, `INSPECT REPLACING LEADING`, financial accumulator hierarchies (line → claim → provider → grand), `WRITE AFTER ADVANCING PAGE` |
| **CLMDTL00** | Claims Inquiry | CICS online transaction demonstrating the "Monolithic Intersection" pattern — simultaneously uses DB2 cursors and VSAM file reads in a single program. Opens a DB2 cursor to fetch claim history for a member, then uses the Billing NPI from each DB2 row as a RIDFLD to read the VSAM Provider Master file, merging relational and indexed-sequential data into a unified claims detail response. Implements pseudo-conversational pattern with pagination, claim status/type decode tables, and financial summary accumulation. | CICS pseudo-conversational (`RETURN TRANSID`), DB2 cursors (`DECLARE/OPEN/FETCH/CLOSE`) for claim and line detail, `EXEC CICS READ FILE RIDFLD` for VSAM KSDS access driven by DB2 results, `DFHCOMMAREA`/`EIBCALEN`, CICS `HANDLE CONDITION`/`HANDLE ABEND`, `REDEFINES` for decode tables, `OCCURS` for response arrays, `WRITEQ TD` for error logging, null indicator handling |
| **EXTAPI01** | External Pricing | CICS program that calls an external REST API for specialty drug and out-of-network claim pricing using `EXEC CICS WEB CONVERSE`. Constructs JSON request payloads manually using `STRING WITH POINTER`, parses JSON responses using positional scanning, and returns results via CICS Channels and Containers (bypassing the 32KB COMMAREA limit). Implements HTTP response code handling, retry logic with exponential backoff for transient failures (408/429/503), and Bearer token authentication. | `EXEC CICS WEB OPEN/CONVERSE/CLOSE`, `EXEC CICS WEB WRITE HTTPHEADER`, CICS Channels and Containers (`GET CONTAINER`/`PUT CONTAINER`), manual JSON construction with `STRING WITH POINTER`, JSON parsing via positional scanning, HTTP status code `EVALUATE`, retry loop with `EXEC CICS DELAY`, `FUNCTION TRIM`/`FUNCTION NUMVAL`/`FUNCTION LENGTH` |
| **MEMMGR01** | EDI Batch Processing | EDI 837 batch processor demonstrating dynamic memory management with `USAGE IS POINTER` and `SET ADDRESS OF`. Receives large EDI payloads (up to 5MB) via CICS Containers, maps `LINKAGE SECTION` structures over raw container memory without data copy, and processes bulk X12 transactions by parsing ISA/GS/ST/CLM/NM1/SV1/SV2/HI/DTP segments. Inserts parsed claims into a DB2 staging table with periodic commits to avoid lock escalation. Detects element/segment delimiters dynamically from the ISA header. | `USAGE IS POINTER`, `SET ADDRESS OF` for memory mapping, `EXEC CICS GET CONTAINER SET(pointer)` for zero-copy access, `OCCURS DEPENDING ON` in `LINKAGE SECTION` for variable-length payload, pointer arithmetic (`SET ptr UP BY offset`), `UNSTRING DELIMITED BY` for segment parsing, DB2 `INSERT` with periodic `COMMIT`, `EXEC SQL ROLLBACK` in abend handler, ISA fixed-length overlay in `LINKAGE SECTION` |

---

### Advanced Modernization Challenges

The four programs above (CLMDTL00, EXTAPI01, MEMMGR01) and the EDI837CP copybook represent the **hardest modernization challenges** for automated COBOL-to-Java conversion frameworks:

- **CLMDTL00** — Tests the framework's ability to handle programs that simultaneously use CICS, DB2, and VSAM in the same transaction. The DB2-result-drives-VSAM-read pattern requires understanding that relational and indexed-sequential data access are interleaved.
- **EDI837CP** — Tests `REDEFINES` conversion (memory overlay with no Java equivalent) and `OCCURS DEPENDING ON` (variable-length records that change size at runtime). The framework must generate wrapper classes or discriminated unions.
- **EXTAPI01** — Tests CICS Web Support conversion. The manual JSON construction/parsing must be converted to modern HTTP client libraries (e.g., `RestTemplate`, `WebClient`) with proper serialization frameworks (Jackson/Gson).
- **MEMMGR01** — Tests pointer-based memory management conversion. `USAGE IS POINTER` and `SET ADDRESS OF` have no direct Java equivalent. The framework must translate pointer arithmetic into safe Java abstractions (ByteBuffer, byte arrays with offset tracking).

---

## Shared Copybooks

| Copybook | Purpose | Key Structures |
|----------|---------|----------------|
| **HCCOMMON.cpy** | System-wide common fields | Current date/time, program ID, return codes, file status codes with 88-level conditions, DB2 SQLCODE conditions, processing counters, financial accumulators, date validation area with `REDEFINES`, payer constants |
| **CLMCOPY.cpy** | Claims data record layouts | Claim master record with header/member/provider/diagnosis/financial groups, `OCCURS DEPENDING ON` for variable claim lines (up to 50), line-level detail with procedure codes, modifiers (4 per line), adjustment reasons (5 per line), NDC and DRG codes, claim adjustment record with CARC group codes |
| **MBRCOPY.cpy** | Member enrollment structures | Member master record with demographics, address, enrollment info (plan/group/product/coverage with 88-levels), benefit accumulators (deductible/OOP/copay/coinsurance), audit trail fields, eligibility response record for inquiry programs |
| **PRVCOPY.cpy** | Provider master file layout | Provider record with NPI key, demographics (individual + organizational), practice info (taxonomy, specialty, license, DEA, Medicare/Medicaid IDs), address, contract info (network, fee schedule, par status), credentialing status, payment routing (EFT/check with bank details), audit fields |
| **PLNCOPY.cpy** | Benefit plan configuration | Plan record with composite key (plan code + effective date), plan metadata (product type, LOB, metal level), cost-sharing structures (deductibles, OOP max, copays, coinsurance), network configuration, service limit table with `OCCURS 20 TIMES` (category, limit type, quantity, period, covered flag) |
| **EDI837CP.cpy** | X12 837 claim record with type discrimination | `CLAIM-MASTER-RECORD` with `REDEFINES` for institutional (UB-04) vs. professional (CMS-1500) claim detail areas occupying the same physical memory, `OCCURS DEPENDING ON` for variable-length diagnosis code arrays (1–12) and service line items (1–50), line-level `REDEFINES` for revenue code vs. procedure code detail, nested `OCCURS DEPENDING ON` for diagnosis pointers and adjustment reasons, 88-level conditions for claim status, type, admit type, and adjustment groups |
| **ABORTWSC.cpy** | Error handling work areas | Standard abend record (program, paragraph, reason, file status, SQLCODE, CICS RESP/RESP2), error log record with severity levels (Info/Warn/Error/Fatal via 88-levels) |

---

## COBOL Feature Coverage Matrix

| Feature | Programs Using It |
|---------|------------------|
| Sequential file I/O | MEMRPT01, PRVLD001, EDI834IN, MEMXREF1, EDI835GN |
| VSAM KSDS (Indexed) | PRVLD001, MEMXREF1, CLMDTL00 (via CICS FILE) |
| VSAM alternate keys | MEMXREF1 |
| DB2 embedded SQL | ELIGINQ1, BENBLD01, MEMXREF1, PRVVAL02, CLMADJ01, CLMPRC01, EDI835GN, CLMDTL00, MEMMGR01 |
| DB2 cursors (DECLARE/FETCH/CLOSE) | CLMADJ01, EDI835GN, CLMDTL00 |
| DB2 positioned UPDATE (FOR UPDATE) | CLMADJ01 |
| DB2 COMMIT/ROLLBACK | BENBLD01, CLMADJ01, EDI835GN, MEMMGR01 |
| CICS transaction processing | ELIGINQ1, CLMDTL00, EXTAPI01, MEMMGR01 |
| CICS BMS map SEND/RECEIVE | ELIGINQ1, CLMDTL00 |
| CICS pseudo-conversational | ELIGINQ1, CLMDTL00 |
| CICS Channels and Containers | EXTAPI01, MEMMGR01 |
| CICS WEB CONVERSE (HTTP/REST) | EXTAPI01 |
| CICS HANDLE CONDITION/ABEND | CLMDTL00, EXTAPI01, MEMMGR01 |
| CICS WRITEQ TD (transient data) | CLMDTL00, EXTAPI01, MEMMGR01 |
| CICS READ FILE (VSAM via CICS) | CLMDTL00 |
| USAGE IS POINTER | MEMMGR01 |
| SET ADDRESS OF | MEMMGR01 |
| CALL verb (inter-program) | CLMADJ01 → PRVVAL02, CLMADJ01 → CLMPRC01 |
| LINKAGE SECTION / USING | PRVVAL02, CLMPRC01, CLMDTL00, MEMMGR01 |
| GOBACK | PRVVAL02, CLMPRC01 |
| COPY statement (copybooks) | All programs |
| COMP-3 (packed decimal) | All programs via copybooks |
| OCCURS / OCCURS DEPENDING ON | CLMCOPY, PLNCOPY, EDI837CP, CLMADJ01, CLMPRC01, CLMDTL00, MEMMGR01 |
| REDEFINES | HCCOMMON, PRVLD001, PRVVAL02, EDI837CP, CLMDTL00 |
| 88-level condition names | All programs and copybooks |
| EVALUATE (case/switch) | All programs |
| STRING / UNSTRING | MEMRPT01, ELIGINQ1, EDI834IN, EDI835GN, CLMDTL00, EXTAPI01, MEMMGR01 |
| STRING WITH POINTER | EXTAPI01, MEMMGR01 |
| INSPECT TALLYING / REPLACING | EDI834IN, EDI835GN, EXTAPI01 |
| FUNCTION CURRENT-DATE | All batch programs |
| FUNCTION UPPER-CASE | MEMXREF1 |
| FUNCTION MOD | PRVLD001 |
| FUNCTION TRIM | EXTAPI01 |
| FUNCTION NUMVAL | EXTAPI01, MEMMGR01 |
| FUNCTION LENGTH | EXTAPI01 |
| COMPUTE ROUNDED | CLMPRC01 |
| Report writing (WRITE AFTER ADVANCING) | MEMRPT01, EDI835GN |
| Control break processing | EDI835GN |
| Multi-file output | EDI835GN (835 + EOB + check register) |
| Variable-length records | EDI834IN, EDI835GN, EDI837CP |
| JSON construction/parsing (manual) | EXTAPI01 |
| Pointer arithmetic | MEMMGR01 |
| CICS DELAY (retry/backoff) | EXTAPI01 |
| DB2-driven VSAM reads | CLMDTL00 |
| Null indicator handling | CLMDTL00 |

---

## Healthcare Payer Domain Coverage

| Business Domain | Programs |
|-----------------|----------|
| **Plan & Benefit Setup** | BENBLD01 (plan builder with temporal logic) |
| **Member Enrollment** | EDI834IN (834 parser), MEMXREF1 (identity matching), MEMRPT01 (error reporting) |
| **Eligibility Verification** | ELIGINQ1 (real-time CICS inquiry) |
| **Provider Network Management** | PRVLD001 (master file loader), PRVVAL02 (credentialing verification) |
| **Claims Adjudication** | CLMADJ01 (adjudication engine), CLMPRC01 (pricing engine), CLMDTL00 (claims detail inquiry) |
| **Claims Pricing** | CLMPRC01 (internal pricing), EXTAPI01 (external REST API pricing) |
| **Billing & Remittance** | EDI835GN (835 remittance + EOB generation) |
| **EDI Batch Processing** | MEMMGR01 (837 batch ingest with dynamic memory), EDI834IN (834 enrollment parsing) |
| **Premium Billing** | Cost-sharing logic embedded in BENBLD01 (plan configuration) and CLMADJ01 (benefit application waterfall) |

---

## Inter-Program Dependencies

```
CLMADJ01 (Claims Adjudication Engine)
  ├── CALL 'PRVVAL02'  →  Provider Credentialing Verification
  ├── CALL 'CLMPRC01'  →  Claims Pricing Engine (internal)
  └── May invoke EXTAPI01 → External REST API Pricing (via CICS LINK)

CLMDTL00 (Claims Detail Inquiry - CICS Online)
  ├── DB2 Cursor → HCAS.CLAIM_MASTER (claim history)
  └── VSAM READ  → PRVMAST (provider demographics via RIDFLD from DB2)

EXTAPI01 (External Pricing Integration - CICS)
  ├── CICS GET CONTAINER ← Receives pricing request
  ├── CICS WEB CONVERSE  → External REST API
  └── CICS PUT CONTAINER → Returns pricing response

MEMMGR01 (EDI 837 Batch Processor - CICS)
  ├── CICS GET CONTAINER SET(ptr) ← Receives raw EDI payload
  ├── SET ADDRESS OF → Maps LINKAGE SECTION over memory
  └── DB2 INSERT → HCAS.CLAIM_STAGING (feeds CLMADJ01)

EDI834IN (834 Enrollment Parser)
  └── Output → MEMXREF1 (Member Cross-Reference)
                 └── Error Output → MEMRPT01 (Error Report)

CLMADJ01 (Adjudication Output)
  └── Drives → EDI835GN (835 Remittance Generation)
```

---

## Notes for Conversion Framework Demonstration

- **No data files are included** — these programs reference DB2 tables, VSAM files, and sequential datasets that would exist in the mainframe environment. The focus is on the COBOL source code structure, business logic, and integration patterns.
- **Financial precision** is critical throughout. All monetary fields use `COMP-3` (packed decimal) to prevent floating-point drift. The Java conversion must use `BigDecimal` equivalents.
- **Effective-date temporal logic** in BENBLD01 uses insert-only patterns rather than UPDATE — the conversion must preserve this behavior rather than converting to standard CRUD.
- **The CALL chain** (CLMADJ01 → PRVVAL02/CLMPRC01) demonstrates a common mainframe pattern where monolithic batch programs delegate to specialized subroutines via `LINKAGE SECTION` parameter blocks. The Java conversion should demonstrate how these become service calls or method invocations.
- **EDI X12 processing** (834 inbound, 835 outbound) represents the most common integration format in healthcare. The string parsing (`UNSTRING`) and generation (`STRING`) patterns are core to demonstrating format handling.
- **REDEFINES memory overlays** (EDI837CP) have no direct Java equivalent. The conversion must use discriminated union patterns, abstract base classes with concrete subtypes, or a ByteBuffer wrapper approach to represent the same physical memory being interpreted differently based on claim type.
- **OCCURS DEPENDING ON** (EDI837CP, MEMMGR01) creates variable-length records whose size changes at runtime. Java `List<T>` or `ArrayList<T>` are the closest equivalents, but the framework must understand that COBOL computes record offsets dynamically.
- **CICS WEB CONVERSE** (EXTAPI01) represents the bridge between legacy CICS and modern REST APIs. The conversion should replace this with native Java HTTP clients (HttpClient, RestTemplate, WebClient) with proper JSON serialization via Jackson or Gson.
- **USAGE IS POINTER / SET ADDRESS OF** (MEMMGR01) represents direct memory address manipulation. Java does not support pointers. The conversion must use `ByteBuffer`, `byte[]` with offset tracking, or structured deserialization to achieve equivalent functionality safely.
- **DB2-driven VSAM reads** (CLMDTL00) demonstrate the common pattern where results from one data access method drive lookups in another. The Java conversion should merge these into unified database queries or orchestrate multiple repository calls within a single service method.
