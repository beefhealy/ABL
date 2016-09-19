&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: cohealy 

  Created: 
      
  Usage: 
        Run from an editor with ops (or whatever) database connected 
        using the following:
        
        DEFINE VARIABLE cDatabase AS CHARACTER NO-UNDO INIT "ops".
        CREATE ALIAS DICTDB FOR DATABASE VALUE(cDatabase).
        RUN xrefAnalysis.w(cDatabase).
        
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER ipcWorkingDB  AS CHARACTER NO-UNDO.

DEFINE STREAM strReport.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iRows AS INTEGER NO-UNDO.
DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.


DEFINE TEMP-TABLE ttXref NO-UNDO 
    FIELD parentFile    AS CHARACTER    FORMAT "X(30)" LABEL "Main Program"
    FIELD childFile     AS CHARACTER    FORMAT "X(30)" LABEL "Program/Include"
    FIELD dbTable       AS CHARACTER    FORMAT "X(30)" LABEL "DB Table"
    FIELD dbField       AS CHARACTER    FORMAT "X(30)" LABEL "DB Field"
    FIELD lineNumber    AS character    FORMAT "9999999" LABEL "Line #"
    FIELD refType       AS CHARACTER    FORMAT "X(20)" LABEL "Ref Type"
    FIELD misc          AS CHARACTER    FORMAT "X(30)" LABEL "misc"
    FIELD osType        AS CHARACTER    FORMAT "X(8)"   LABEL "OpSys"    
    INDEX parIndx parentFile
    INDEX chiIndx childFile
    INDEX tblIndex dbTable
    INDEX fldIndex dbField.
    
DEFINE TEMP-TABLE ttProgramUsage
    FIELD dbTable       AS CHARACTER
    FIELD parentFile    AS CHARACTER 
    FIELD childFile     AS CHARACTER 
    FIELD dbField       AS CHARACTER
    FIELD lineNumber    AS CHARACTER.

DEFINE TEMP-TABLE ttProgramPath
    FIELD parentFile    AS CHARACTER 
    FIELD parentPath    AS CHARACTER 
    FIELD childFile     AS CHARACTER 
    FIELD childPath     AS CHARACTER
    INDEX parentIndx IS PRIMARY UNIQUE parentFile childFile .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brXref

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttXref

/* Definitions for BROWSE brXref                                        */
&Scoped-define FIELDS-IN-QUERY-brXref ttXref.parentFile ttXref.childFile ttXref.dbTable ttXref.dbField ttXref.lineNumber ttXref.refType ttXref.misc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brXref   
&Scoped-define SELF-NAME brXref
&Scoped-define QUERY-STRING-brXref FOR EACH ttXref
&Scoped-define OPEN-QUERY-brXref OPEN QUERY {&SELF-NAME} FOR EACH ttXref.
&Scoped-define TABLES-IN-QUERY-brXref ttXref
&Scoped-define FIRST-TABLE-IN-QUERY-brXref ttXref


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brXref}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsTables2Use fiFile btnFile btnImport ~
btnReset btnExport fiMisc cmbParent cmbChild cmbTable cmbField brXref ~
btnProgramReport btnSubProgs btnTables btnFields btnLineNumbers ~
btnUnreferencedTables btnUnreferencedFields BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS rsTables2Use fiFile fiMisc cmbParent ~
cmbChild cmbTable cmbField 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnRemovePath C-Win 
FUNCTION fnRemovePath RETURNS CHARACTER
  ( ipcFullPath AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExport 
     LABEL "Export" 
     SIZE 8 BY 1.14.

DEFINE BUTTON btnFields 
     LABEL "Fields" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnFile 
     LABEL "..." 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnImport 
     LABEL "Import file" 
     SIZE 12.8 BY 1.14.

DEFINE BUTTON btnLineNumbers 
     LABEL "Line numbers" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnProgramReport 
     LABEL "Programs" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnReset 
     LABEL "Reset" 
     SIZE 13.4 BY 1.14.

DEFINE BUTTON btnSubProgs 
     LABEL "Sub Programs" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnTables 
     LABEL "Tables" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUnreferencedFields 
     LABEL "Unreferenced Fields" 
     SIZE 22 BY 1.14.

DEFINE BUTTON btnUnreferencedTables 
     LABEL "Unreferenced Tables" 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbChild AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cmbField AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cmbParent AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cmbTable AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE fiFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "XREF File" 
     VIEW-AS FILL-IN 
     SIZE 81 BY 1 NO-UNDO.

DEFINE VARIABLE fiMisc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Misc" 
     VIEW-AS FILL-IN 
     SIZE 53.4 BY 1 NO-UNDO.

DEFINE VARIABLE rsTables2Use AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All DB Tables", "DB",
"XREF Tables", "XREF",
"DB not in XREF", "DBNOXREF"
     SIZE 56.8 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brXref FOR 
      ttXref SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brXref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brXref C-Win _FREEFORM
  QUERY brXref DISPLAY
      ttXref.parentFile
    ttXref.childFile
    ttXref.dbTable
    ttXref.dbField
    ttXref.lineNumber
    ttXref.refType
    ttXref.misc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 206 BY 19.67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsTables2Use AT ROW 1.19 COL 14.2 NO-LABEL WIDGET-ID 28
     fiFile AT ROW 2.1 COL 12 COLON-ALIGNED WIDGET-ID 2
     btnFile AT ROW 2.1 COL 95 WIDGET-ID 6
     btnImport AT ROW 2.1 COL 100.2 WIDGET-ID 4
     btnReset AT ROW 2.1 COL 113.2 WIDGET-ID 16
     btnExport AT ROW 2.1 COL 127 WIDGET-ID 36
     fiMisc AT ROW 3.05 COL 153 COLON-ALIGNED WIDGET-ID 46
     cmbParent AT ROW 3.29 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     cmbChild AT ROW 3.29 COL 31.8 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cmbTable AT ROW 3.29 COL 62.8 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cmbField AT ROW 3.29 COL 93.6 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     brXref AT ROW 4.24 COL 3 WIDGET-ID 200
     btnProgramReport AT ROW 24.19 COL 3.2 WIDGET-ID 18
     btnSubProgs AT ROW 24.19 COL 19.4 WIDGET-ID 40
     btnTables AT ROW 24.19 COL 36 WIDGET-ID 20
     btnFields AT ROW 24.19 COL 52.2 WIDGET-ID 22
     btnLineNumbers AT ROW 24.19 COL 68 WIDGET-ID 38
     btnUnreferencedTables AT ROW 24.19 COL 85.2 WIDGET-ID 32
     btnUnreferencedFields AT ROW 24.19 COL 108.2 WIDGET-ID 34
     BUTTON-1 AT ROW 24.43 COL 136 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 215 BY 25.33 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "XREF Analysis"
         HEIGHT             = 25.33
         WIDTH              = 215
         MAX-HEIGHT         = 38.67
         MAX-WIDTH          = 215
         VIRTUAL-HEIGHT     = 38.67
         VIRTUAL-WIDTH      = 215
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brXref cmbField DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brXref
/* Query rebuild information for BROWSE brXref
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttXref.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brXref */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* XREF Analysis */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* XREF Analysis */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExport C-Win
ON CHOOSE OF btnExport IN FRAME DEFAULT-FRAME /* Export */
DO:
/*   DEFINE VARIABLE excelDump AS classes.BrowseDump.          */
/*   excelDump = NEW classes.BrowseDump(BROWSE brXref:HANDLE). */
/*   excelDump:DumpToExcel().                                  */
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFields C-Win
ON CHOOSE OF btnFields IN FRAME DEFAULT-FRAME /* Fields */
DO:    
    RUN pcClipboardData("fields").   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFile C-Win
ON CHOOSE OF btnFile IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cFileName   AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lProceed    AS LOGICAL NO-UNDO INITIAL TRUE. 
    
    SYSTEM-DIALOG GET-FILE cFileName TITLE "Choose XREF..." 
    FILTERS "Source Files (*.xref)" "*.xref", 
    "XREF Files (*.xref)" "*.xref" 
    MUST-EXIST 
    USE-FILENAME 
    UPDATE lProceed. 
    
    IF lProceed = TRUE THEN 
        ASSIGN fiFile = cFileName
               FILE-INFO:FILENAME = cFileName.
    DISPLAY fiFile WITH FRAME {&frame-name}.
    
    MESSAGE "Load this file? " SKIP
                FILE-INFO:FULL-PATHNAME 
            VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE lProceed.                     
            
        IF NOT lProceed THEN RETURN NO-APPLY.
        ELSE APPLY "CHOOSE" TO btnImport.
    APPLY "CHOOSE" TO btnReset.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImport C-Win
ON CHOOSE OF btnImport IN FRAME DEFAULT-FRAME /* Import file */
DO:
    DEFINE VARIABLE lLoad       AS LOGICAL      NO-UNDO INIT TRUE.
    DEFINE VARIABLE cLine       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cRefTypes   AS CHARACTER    NO-UNDO INIT "SORT-ACCESS,UPDATE,DELETE,CREATE,REFERENCE,ACCESS,SEARCH,RUN".
    DEFINE VARIABLE cDummy      AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttXref.
    
    DO WITH FRAME {&frame-name}:
        ASSIGN fiFile.
        IF fiFile = "" THEN RETURN NO-APPLY.
        FILE-INFO:FILE-NAME = fiFile.        
    END.
    
    SESSION:SET-WAIT-STATE ("general").
    INPUT FROM VALUE (fiFile).
    
    DO WHILE TRUE ON ENDKEY UNDO,leave:
        IMPORT UNFORMATTED cLine.
        
        IF NUM-ENTRIES(cLine," ") < 4 THEN DO:
            MESSAGE "Skipping invalid line: " cLine
                VIEW-AS ALERT-BOX.
            NEXT.
        END.
        
        IF CAN-DO(cRefTypes,ENTRY(4,cLine," ")) AND ENTRY(5,cLine," ") BEGINS(ipcWorkingDB + ".")  
        THEN DO:
            
            CREATE ttXref.
            ASSIGN ttXref.parentFile    = fnRemovePath(ENTRY(1,cLine," "))   
                   ttXref.childFile     = fnRemovePath(ENTRY(2,cLine," "))
                   ttXref.lineNumber    = ENTRY(3,cLine," ")
                   ttXref.refType       = ENTRY(4,cLine," ")
                   ttXref.dbTable       = REPLACE(ENTRY(5,cLine," "), ipcWorkingDB + ".","")
                   ttXref.dbField       = ENTRY(6,cLine," ") 
                   ttXref.misc          = ENTRY(7,cLine," ") NO-ERROR.
            iRows = iRows + 1.
            
            FIND ttProgramPath WHERE ttProgramPath.parentFile   = ttXref.parentFile
                                 AND ttProgramPath.childFile    = ttXref.childFile NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(ttProgramPath) 
            THEN DO:
                CREATE ttProgramPath.
                ASSIGN ttProgramPath.parentFile = ttXref.parentFile
                       ttProgramPath.parentPath = SEARCH(ttXref.parentFile)
                       ttProgramPath.childFile   = ttXref.childFile
                       ttProgramPath.childPath  = SEARCH(ttXref.childFile).
            END.
        END.
    END.
    INPUT CLOSE.
    
    MESSAGE iRows " rows imported"
        VIEW-AS ALERT-BOX.
    
    brXref:MAX-DATA-GUESS = iRows.
    
    cmbParent:LIST-ITEMS = ?.
    FOR EACH ttXref BREAK BY ttXref.parentFile:
        IF LAST-OF(ttXref.parentFile) 
        THEN cmbParent:ADD-LAST (ttXref.parentFile) NO-ERROR.                    
    END.
    cmbParent:ADD-FIRST("<Select>").
    
    cmbChild:LIST-ITEMS = ?.
    FOR EACH ttXref BREAK BY ttXref.childFile:
        IF LAST-OF(ttXref.childFile) 
        THEN cmbChild:ADD-LAST (ttXref.childFile) NO-ERROR.                    
    END.
    cmbChild:ADD-FIRST("<Select>").   
     
    
    SESSION:SET-WAIT-STATE ("").
    
    OPEN QUERY brXref FOR EACH ttXref.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLineNumbers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLineNumbers C-Win
ON CHOOSE OF btnLineNumbers IN FRAME DEFAULT-FRAME /* Line numbers */
DO:    
    RUN pcClipboardData("lines").   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProgramReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProgramReport C-Win
ON CHOOSE OF btnProgramReport IN FRAME DEFAULT-FRAME /* Programs */
DO:    
    RUN pcClipboardData("programs").   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME /* Reset */
DO:
  DO WITH FRAME {&frame-name}:
      ASSIGN cmbTable  = "<Select>"
             cmbField  = "".
             
      IF AVAILABLE ttXref THEN 
        ASSIGN cmbParent = "<Select>"
               cmbChild  = "<Select>".
             
      display cmbParent
              cmbChild
              cmbTable
              cmbField.
              
    OPEN QUERY brXref FOR EACH ttXref.    
    APPLY "ENTRY" TO cmbParent.          
             
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubProgs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubProgs C-Win
ON CHOOSE OF btnSubProgs IN FRAME DEFAULT-FRAME /* Sub Programs */
DO:    
    RUN pcClipboardData("subprograms").   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTables C-Win
ON CHOOSE OF btnTables IN FRAME DEFAULT-FRAME /* Tables */
DO:    
    RUN pcClipboardData("tables").   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUnreferencedFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUnreferencedFields C-Win
ON CHOOSE OF btnUnreferencedFields IN FRAME DEFAULT-FRAME /* Unreferenced Fields */
DO:
      
    OUTPUT TO VALUE("C:\temp\" + ipcWorkingDB + "_unreferencedFields.txt").
    DEFINE VARIABLE cUnrefFields AS CHARACTER NO-UNDO.
    
    FOR EACH DICTDB._field,
        EACH DICTDB._file OF DICTDB._field 
            WHERE DICTDB._file._hidden = NO 
              AND NOT CAN-FIND(FIRST ttXref WHERE ttXref.dbTable = DICTDB._file._file-name 
                                              AND ttXref.dbField = DICTDB._field._field-name NO-LOCK):        
         
         EXPORT DICTDB._file._file-name DICTDB._field._field-name.
        
    END.
    
    OUTPUT CLOSE.
    MESSAGE "Report dumped to " SKIP 
            "C:\temp\" + ipcWorkingDB + "_unreferencedFields.txt"
    VIEW-AS ALERT-BOX.
     
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUnreferencedTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUnreferencedTables C-Win
ON CHOOSE OF btnUnreferencedTables IN FRAME DEFAULT-FRAME /* Unreferenced Tables */
DO:
    DEFINE VARIABLE cUnrefTables AS CHARACTER NO-UNDO.
    
    FOR EACH DICTDB._file WHERE DICTDB._file._hidden = NO 
                            AND NOT CAN-FIND(FIRST ttXref WHERE ttXref.dbTable = DICTDB._file._file-name NO-LOCK):
        
        IF cUnrefTables = "" THEN 
            ASSIGN cUnrefTables = DICTDB._file._file-name.
        ELSE             
            ASSIGN cUnrefTables = cUnrefTables + "," + DICTDB._file._file-name.
    END.
    
    CLIPBOARD:VALUE = cUnrefTables.
    MESSAGE cUnrefTables SKIP  
            "Copied to clipboard"
        VIEW-AS ALERT-BOX.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  DEFINE VARIABLE cParentPath   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cChildPath    AS CHARACTER   NO-UNDO.
  
  FOR EACH ttXRef WHERE refType = "RUN":
      MESSAGE ttXref.refType
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbChild
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbChild C-Win
ON VALUE-CHANGED OF cmbChild IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&frame-name}:
      ASSIGN cmbChild.
      RUN pcApplyFilter.            
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbField C-Win
ON VALUE-CHANGED OF cmbField IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&frame-name}:
      ASSIGN cmbField.
      RUN pcApplyFilter.            
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbParent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbParent C-Win
ON VALUE-CHANGED OF cmbParent IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&frame-name}:
      ASSIGN cmbParent.
      RUN pcApplyFilter.            
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTable C-Win
ON VALUE-CHANGED OF cmbTable IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&frame-name}:
      ASSIGN cmbTable.
      cmbField:LIST-ITEMS = ?.
      IF cmbTable <> "" AND cmbTable <> "<Select>" THEN
      DO:
          
          FIND DICTDB._file WHERE DICTDB._file._file-name = cmbTable NO-LOCK.
          FOR EACH DICTDB._field OF DICTDB._file NO-LOCK:
              cmbField:add-last(DICTDB._field._field-name).
          END.
      END.
      RUN pcApplyFilter.                  
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiMisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMisc C-Win
ON VALUE-CHANGED OF fiMisc IN FRAME DEFAULT-FRAME /* Misc */
DO:
  RUN pcApplyFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsTables2Use
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsTables2Use C-Win
ON VALUE-CHANGED OF rsTables2Use IN FRAME DEFAULT-FRAME
DO:
  RUN pcInit.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brXref
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  ASSIGN {&WINDOW-NAME}:X = (SESSION:WIDTH-PIXELS - {&WINDOW-NAME}:WIDTH-PIXELS) / 2
         {&WINDOW-NAME}:Y = (SESSION:HEIGHT-PIXELS - {&WINDOW-NAME}:HEIGHT-PIXELS) / 2.
         
  RUN enable_UI.
  RUN pcInit.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY rsTables2Use fiFile fiMisc cmbParent cmbChild cmbTable cmbField 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rsTables2Use fiFile btnFile btnImport btnReset btnExport fiMisc 
         cmbParent cmbChild cmbTable cmbField brXref btnProgramReport 
         btnSubProgs btnTables btnFields btnLineNumbers btnUnreferencedTables 
         btnUnreferencedFields BUTTON-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pcApplyFilter C-Win 
PROCEDURE pcApplyFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    
    ASSIGN cWhere = "FOR EACH ttXref WHERE TRUE".
    
    DO WITH FRAME {&frame-name}:
        ASSIGN cmbTable cmbField cmbChild cmbParent fiMisc.        
    END.
    
    IF cmbTable <> "" AND cmbTable <> "<Select>" THEN 
        ASSIGN cWhere = cWhere + " AND ttXref.dbTable = " + QUOTER(cmbTable).
    
    IF cmbField <> "" AND cmbField <> "<Select>" THEN 
        ASSIGN cWhere = cWhere + " AND ttXref.dbField = " + QUOTER(cmbField).
    
    IF cmbParent <> "" AND cmbParent <> "<Select>" THEN 
        ASSIGN cWhere = cWhere + " AND ttXref.parentFile = " + QUOTER(cmbParent).
    
    IF cmbChild <> "" AND cmbChild <> "<Select>" THEN 
        ASSIGN cWhere = cWhere + " AND ttXref.childFile = " + QUOTER(cmbChild).
    
    IF fiMisc <> "" THEN
        ASSIGN cWhere = cWhere + " AND ttXref.misc BEGINS(" + QUOTER(fiMisc) + ")".

    BROWSE brXref:PRIVATE-DATA = "XREF Export: Table: " + cmbTable + " Field: " + cmbField + " Main program: " + cmbParent + " Sub-Program: " + cmbChild.           
    
    QUERY brXref:QUERY-PREPARE (cWhere).
    QUERY brXref:QUERY-OPEN ().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pcClipboardData C-Win 
PROCEDURE pcClipboardData :
DEFINE INPUT  PARAMETER cListType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cProgramList    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSubProgList    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLineNumList    AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttProgramUsage.
    
     
    DEFINE VARIABLE hQuery     AS HANDLE.
    CREATE QUERY hQuery.
    
    hQuery:SET-BUFFERS(BUFFER ttXref:HANDLE).
    
    hQuery:QUERY-PREPARE(cWhere).
    
    hQuery:QUERY-OPEN.
    
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        hQuery:GET-NEXT.    
        
        IF hQuery:QUERY-OFF-END THEN LEAVE.
        
        
        FIND ttProgramUsage WHERE ttProgramUsage.dbTable = ttXref.dbTable
                              AND ttProgramUsage.dbField = ttXref.dbField
/*                              AND ttProgramUsage.parentFile = ttXref.parentFile*/
                              AND ttProgramUsage.childFile =  ttXref.childFile
                              AND ttProgramUsage.lineNumber = ttXref.lineNumber NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttProgramUsage 
        THEN DO:
            CREATE ttProgramUsage.
            BUFFER-COPY ttXref TO ttProgramUsage. 
        END.    
        
        /*Program List based on what's in the browse*/
        IF ttXref.parentFile <> "" 
        THEN DO:
            IF cProgramList = "" THEN
                ASSIGN cProgramList = ttXref.parentFile.
            ELSE DO:
                IF INDEX(cProgramList,ttXref.parentFile) = 0
                THEN cProgramList = cProgramList + "," + ttXref.parentFile.            
            END.
        END.
        
        /*Program List based on what's in the browse*/
        IF ttXref.childFile <> "" 
        THEN DO:
            IF cSubProgList = "" THEN
                ASSIGN cSubProgList = ttXref.childFile.
            ELSE DO:
                IF INDEX(cSubProgList,ttXref.childFile) = 0
                THEN cSubProgList = cSubProgList + "," + ttXref.childFile.            
            END.
        END.
                 
        /*Table List based on what's in the browse*/
        IF ttXref.dbTable <> "" 
        THEN DO:
            IF cTableList = "" THEN
                ASSIGN cTableList = ttXref.dbTable.
            ELSE DO:
                IF INDEX(cTableList,ttXref.dbTable) = 0
                THEN cTableList = cTableList + "," + ttXref.dbTable.            
            END.        
        END.
        
        /*Field List based on what's in the browse*/
        IF ttXref.dbField <> "" 
        THEN DO:
            IF cFieldList = "" THEN
                ASSIGN cFieldList = ttXref.dbField.
            ELSE DO:
                IF INDEX(cFieldList,ttXref.dbField) = 0
                THEN cFieldList = cFieldList + "," + ttXref.dbField.            
            END.
        END.
        
        /*Field List based on what's in the browse*/
        IF ttXref.lineNumber <> "" 
        THEN DO:
            IF cLineNumList = "" THEN
                ASSIGN cLineNumList = ttXref.lineNumber.
            ELSE DO:
                IF INDEX(cLineNumList,ttXref.lineNumber) = 0
                THEN cLineNumList = cLineNumList + "," + ttXref.lineNumber.            
            END.
        END.
    END.
    hQuery:QUERY-CLOSE.
    
    /*tidy up*/
    DELETE OBJECT hQuery.
    SESSION:SET-WAIT-STATE("").
    
    CASE cListType:
        WHEN "programs" THEN 
            CLIPBOARD:VALUE = cProgramList.
        WHEN "subprograms" THEN 
            CLIPBOARD:VALUE = cSubProgList.
        WHEN "tables" THEN 
            CLIPBOARD:VALUE = cTableList.
        WHEN "fields" THEN 
            CLIPBOARD:VALUE = cFieldList.   
        WHEN "lines" THEN 
            CLIPBOARD:VALUE = cLineNumList. 
    END CASE.    
    
    MESSAGE NUM-ENTRIES(CLIPBOARD:VALUE) SKIP  
            CLIPBOARD:VALUE SKIP  "The above list has been copied to the clipboard"
        VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pcInit C-Win 
PROCEDURE pcInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&frame-name}:
        ASSIGN rsTables2Use.
        
        cmbTable:LIST-ITEMS = ?.
        
        IF rsTables2Use <> "DB" AND NOT CAN-FIND(FIRST ttXref) 
        THEN DO:
            MESSAGE "You must first load an XREF file"
                VIEW-AS ALERT-BOX.
            ASSIGN rsTables2Use = "DB".
            RETURN NO-APPLY.
        END.
            
        
        CASE rsTables2Use: 
            WHEN "DB"  
            THEN DO:
                FOR EACH DICTDB._file WHERE DICTDB._file._hidden = NO NO-LOCK:
                    cmbTable:ADD-LAST (DICTDB._file._file-name).
                END.
            END.
            WHEN "XREF" 
            THEN DO:
                FOR EACH ttXref NO-LOCK BREAK BY dbTable:
                    IF LAST-OF (dbTable) then
                        cmbTable:ADD-LAST (ttXref.dbTable).
                END.            
            END.
            WHEN "DBNOXREF" 
            THEN DO:
                FOR EACH DICTDB._file WHERE DICTDB._file._hidden = NO NO-LOCK:
                    FIND FIRST ttXref WHERE ttXref.dbTable = DICTDB._file._file-name NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE(ttXref) THEN 
                        cmbTable:ADD-LAST(DICTDB._file._file-name).
                END.            
            END.        
        END CASE. 
        cmbTable:ADD-FIRST ("<Select>").
        
                
        cmbField:LIST-ITEMS = ?.
        cmbField:ADD-FIRST ("<Select>").
        APPLY "VALUE-CHANGED" TO cmbTable.
        
        APPLY "CHOOSE" TO btnReset. 
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnRemovePath C-Win 
FUNCTION fnRemovePath RETURNS CHARACTER
  ( ipcFullPath AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFormatted AS CHARACTER NO-UNDO.
    
    /*strip out file paths*/
    IF INDEX(ipcFullPath,"/") > 0 THEN /* Linux */
        ASSIGN cFormatted = SUBSTRING(ipcFullPath,R-INDEX(ipcFullPath,"/") + 1).
    
    IF INDEX(ipcFullPath,"\") > 0 THEN  /* Windows */
        ASSIGN cFormatted = SUBSTRING(ipcFullPath,R-INDEX(ipcFullPath,"\") + 1).
    
    RETURN cFormatted.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

