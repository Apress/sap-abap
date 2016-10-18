*&---------------------------------------------------------------------*
*& Report  YCL_CH12_02_ALV_FCAT_ETC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT YCL_CH12_02_ALV_FCAT_ETC.

********************************************
* Customer Wise Sales Summary in ALV Grid **
* Use Field Catalogue, Provide for Saving **
* of Layouts, Give Grid Title Etc.        **
********************************************

****************************************************************
* declare: (extra declarations over previous program)         **
*                                                             **
* (1) internal table for field catalogue. thru field          **
* catalogue, you will be able to control the properties of    **
* fields/columns output like suppression of execution of      **
* conversion routine, optimize column widths, impart color    **
* to columns, control the output format of  fields/columns    **
* output etc. (FCAT: DDIC type LVC_T_FCAT)                    **
*                                                             **
* (2) color control variable to impart colors to ALV fields   **
* /columns output. (CLR)                                      **
*                                                             **
* (3) a structure with fields whose values control overall    **
* appearance of ALV grid like grid title etc.                 **
* (LAYOUT: DDIC type LVC_S_LAYO)                              **
*                                                             **
* (4) a structure with fields whose values enable the saving &**
* maintenance of ALV layouts. (DVARIANT DDIC type DISVARIANT) **
*                                                             **
* create text symbols TEXT-001: 'Sales Summary-Company Code:' **
*                     TEXT-002: 'Curr.'                       **
*                                                             **
* in the PBO event:                                           **
*                                                             **
*               call function module LVC_FIELDCATALOG_MERGE.  **
*               function module accepts DDIC structure as     **
*               input parameter. returns default field        **
*               catalogue in FCAT[].                          **
*               adjust field catalogue FCAT[] for             **
*               suppression of execution of conversion exit   **
*               routine, different colors to fields/columns   **
*               output & field/column width optimization.     **
*                                                             **
*                assign value to field LAYOUT-GRID_TITLE.     **
*                assign value to field DVARIANT-REPORT.       **
*                                                             **
* when invoking the method SET_TABLE_FOR_FIRST_DISPLAY        **
* provide extra parameters:                                   **
*                  EXPORTING                                  **
*                    IS_VARIANT       = DVARIANT              **
*                    I_SAVE           = 'U'                   **
*                    IS_LAYOUT        = LAYOUT                **
*                    I_STRUCTURE_NAME comment/delete          **
*                  CHANGING                                   **
*                    IT_FIELDCATALOG  = FCAT[]                **
*                                                             **
****************************************************************

DATA: SALES_TAB  TYPE YCL_CH06_SALES_SUM_TAB,
      SALES_STRU LIKE LINE OF SALES_TAB,
      BUTXT      TYPE T001-BUTXT,
      WAERS      TYPE T001-WAERS,

      OK_CODE    TYPE SY-UCOMM,
      CCONTR     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       " reference variable for instance of custom container
      ALV_GRID   TYPE REF TO CL_GUI_ALV_GRID.
       " reference variable for instance of ALV grid

**** addition to prev. prog.

DATA: FCAT       TYPE LVC_T_FCAT " field catalogue
        WITH HEADER LINE,

      CLR(1)     TYPE N VALUE 1,

      LAYOUT     TYPE LVC_S_LAYO, " overall grid appearance
      DVARIANT   TYPE DISVARIANT. " for saving layouts

**********************************************
PARAMETERS: CCODE TYPE VBRK-BUKRS DEFAULT 3000.

**********************************************
START-OF-SELECTION.

SELECT SINGLE BUTXT WAERS FROM T001 INTO (BUTXT, WAERS) WHERE
       BUKRS = CCODE.

SELECT KUNNR NAME1 ORT01 NETWR KURRF FROM YCL_CH05_VBRKKNA INTO
  CORRESPONDING FIELDS OF SALES_STRU WHERE BUKRS = CCODE.

 SALES_STRU-NETWR =  SALES_STRU-NETWR * SALES_STRU-KURRF.
 SALES_STRU-KURRF = 0.
 COLLECT SALES_STRU INTO SALES_TAB.
ENDSELECT.

SORT SALES_TAB BY KUNNR.

CALL SCREEN 100. " screen 100 will be loaded

**************************************************************
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.  " PBO module
 SET PF-STATUS 'STAT1'.
 SET TITLEBAR 'TITLE1' WITH CCODE BUTXT WAERS.

 CREATE OBJECT CCONTR   " create instance of custom container
   EXPORTING
     CONTAINER_NAME              = 'CUST_CONTT' " name of custom control
   EXCEPTIONS
     CNTL_ERROR                  = 1
     CNTL_SYSTEM_ERROR           = 2
     CREATE_ERROR                = 3
     LIFETIME_ERROR              = 4
     LIFETIME_DYNPRO_DYNPRO_LINK = 5
     others                      = 6
     .
 IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 ENDIF.

 CREATE OBJECT ALV_GRID " create instance of ALV grid
   EXPORTING
     I_PARENT          = CCONTR " reference to instance of custom container
   EXCEPTIONS
     ERROR_CNTL_CREATE = 1
     ERROR_CNTL_INIT   = 2
     ERROR_CNTL_LINK   = 3
     ERROR_DP_CREATE   = 4
     others            = 5
     .
 IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 ENDIF.

**** addition to prev. prog. start *****

 CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
  EXPORTING
     I_STRUCTURE_NAME         = 'YCL_CH12_SALES_SUM_STRU'
   CHANGING
     CT_FIELDCAT              = FCAT[]
  EXCEPTIONS
    INCONSISTENT_INTERFACE    = 1
    PROGRAM_ERROR             = 2
    OTHERS                    = 3
           .
 IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 ENDIF.

 LOOP AT FCAT.
  CLR = CLR + 1.  " color codes 1 2 3 4 for fields KUNNR, NAME1, ORT01 & NETWR

  CONCATENATE 'C' CLR '00' INTO FCAT-EMPHASIZE.

  FCAT-COL_OPT = 'X'. " column width optimization

  IF FCAT-FIELDNAME = 'KUNNR'. " output leading zeroes in KUNNR
   FCAT-NO_CONVEXT = 'X'. " suppress execution of conversion exit routine
   FCAT-CONVEXIT   = ' '. " make conversion exit routine blank
   FCAT-EDIT_MASK  = ' '. " make edit mask blank

  ENDIF.
  MODIFY FCAT.

 ENDLOOP.
.

 CONCATENATE TEXT-001 CCODE '/' BUTXT '--'
   TEXT-002 WAERS INTO LAYOUT-GRID_TITLE.

 DVARIANT-REPORT = 'YCL_CH12_02_ALV_FCAT_ETC'.

**** addition to prev. prog. end *****


***** populate, display instance of ALV grid *****

 CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_VARIANT                  = DVARIANT " change, save layouts
      I_SAVE                      = 'U' " change, save layouts
      IS_LAYOUT                   = LAYOUT " overall grid appearance
*     I_STRUCTURE_NAME              = 'YCL_CH12_SALES_SUM_STRU'
    CHANGING
      IT_OUTTAB                   = SALES_TAB
      IT_FIELDCATALOG             = FCAT[]  " field catalogue
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      others                        = 4
         .

 IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT. " PAI module

IF OK_CODE = 'BACK'.
 SET SCREEN 0. " these two statements return
 LEAVE SCREEN. " control to PARAMETERS prompt
ENDIF.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
