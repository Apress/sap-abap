REPORT YCL_CH05_08_BILL_DOCS_LIST LINE-SIZE 135 LINE-COUNT 60
       NO STANDARD PAGE HEADING.

*********************************************************************
* List of Billing Documents of a Specified Company Code with Total **
*********************************************************************

* Billing Documents of XXXX/XXXXXXXXXXXXXXXXXXXXXXXXX   Curr: XXXXX
* ------------------------------------------------------------------
* Doc No. Doc Date Customer     N a m e        C i t y        Amount
*                    Code
* ------------------------------------------------------------------
*     SY-DBCNT      005 - 009  **
*     VBELN         011 - 020  **
*     FKDAT         022 - 031  **
*     KUNNR         033 - 042  **
*     NAME1         044 - 078  **
*     ORT01         080 - 114  **
*     NETWR         116 - 132  **
*********************************
*******************************************************************
* a database view with tables KNA1 (customer primary)            **
*                             VBRK (billing docs-header)         **
* view name: YCL_CH05_VBRKKNA                                    **
*                                                                **
* join condition:                                                **
*   VBRK                    KNA1    MANDT = VBRK    MANDT        **
*   KNA1                    KNA1    KUNNR = VBRK    KUNAG        **
*                                                                **
* view fields:                                                   **
* View field      Table    Field                                 **
*                                                                **
*  MANDT          KNA1     MANDT                                 **
*  KUNNR          KNA1     KUNNR                                 **
*  VBELN          VBRK     VBELN                                 **
*  NAME1          KNA1     NAME1                                 **
*  ORT01          KNA1     ORT01                                 **
*  PSTLZ          KNA1     PSTLZ                                 **
*  STRAS          KNA1     STRAS                                 **
*  FKDAT          VBRK     FKDAT                                 **
*  KURRF          VBRK     KURRF                                 **
*  BUKRS          VBRK     BUKRS                                 **
*  NETWR          VBRK     NETWR                                 **
*                                                                **
*******************************************************************
TABLES: YCL_CH05_VBRKKNA, " creates a local structure having all
                          " fields of view YCL_CH05_VBRKKNA
        T001.             " local structure with all fields of T001

DATA: STRNG     TYPE STRING,
      TOTAL     TYPE VBRK-NETWR.

PARAMETERS CCODE  TYPE VBRK-BUKRS DEFAULT 3000 VALUE CHECK.
                  " the clause VALUE CHECK will check values
                  " in the primary table of this foreign key field
*********************************************************************
TOP-OF-PAGE.

WRITE :/5 STRNG, 129(3) SY-PAGNO. " STRNG - refer CONCATENATE sttement in
                                  " START-OF-SELECTION
SKIP 1.
WRITE:/5(127) SY-ULINE.
WRITE:/5 TEXT-003,11 TEXT-004, 22 TEXT-005, 33 TEXT-006, 46 TEXT-007,
         83 TEXT-008, 126 TEXT-009.

WRITE:/6 TEXT-010, 35 TEXT-011.
WRITE:/5(127) SY-ULINE.

***********************************************************************
START-OF-SELECTION.

SELECT SINGLE * FROM T001 WHERE BUKRS = CCODE." fetches a single row, no
                                              " loop construct SELECT
                                              " ENDSELECT

CONCATENATE TEXT-001 CCODE '/' T001-BUTXT TEXT-002 T001-WAERS INTO STRNG.

SELECT * FROM YCL_CH05_VBRKKNA WHERE BUKRS = CCODE.
 YCL_CH05_VBRKKNA-NETWR = YCL_CH05_VBRKKNA-NETWR *
  YCL_CH05_VBRKKNA-KURRF.

 WRITE:/5(5) SY-DBCNT, YCL_CH05_VBRKKNA-VBELN, YCL_CH05_VBRKKNA-FKDAT,
             YCL_CH05_VBRKKNA-KUNNR, YCL_CH05_VBRKKNA-NAME1,
             YCL_CH05_VBRKKNA-ORT01, (17) YCL_CH05_VBRKKNA-NETWR.

 TOTAL = TOTAL + YCL_CH05_VBRKKNA-NETWR.

ENDSELECT.
********************************

END-OF-SELECTION.
SKIP 1.

WRITE: /(17) TOTAL UNDER YCL_CH05_VBRKKNA-NETWR. " the field TOTAL will
                                                 " be outputted
                                                 " UNDER NETWR. no need
                                                 " to count columns
                                                 " for alignment
