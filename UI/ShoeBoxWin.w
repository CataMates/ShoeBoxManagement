&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          shoebox          PROGRESS
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

  Author: 

  Created: 

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

/* Local Variable Definitions ---                                       */

{dsTabShoeBox.i}
DEFINE VAR bShoeBox   AS beTabShoeBox.
DEFINE VAR hdsShoeBox AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-11

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TabShoeBox

/* Definitions for BROWSE BROWSE-11                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-11 TabShoeBox.ShoeBoxNum ~
TabShoeBox.Sex TabShoeBox.Age 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-11 
&Scoped-define QUERY-STRING-BROWSE-11 FOR EACH TabShoeBox NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-11 OPEN QUERY BROWSE-11 FOR EACH TabShoeBox NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-11 TabShoeBox
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-11 TabShoeBox


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-11}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-2 FILL-IN-1 COMBO-BOX-3 BUTTON-9 ~
BUTTON-10 BROWSE-11 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-2 FILL-IN-1 COMBO-BOX-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-10 
     LABEL "DELETE" 
     SIZE 19 BY 1.43.

DEFINE BUTTON BUTTON-9 
     LABEL "CREATE" 
     SIZE 17 BY 1.43.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "M","F" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0-2","2-4","4-6","6-8","8-10","10-12","12-14" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-11 FOR 
      TabShoeBox SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-11 C-Win _STRUCTURED
  QUERY BROWSE-11 NO-LOCK DISPLAY
      TabShoeBox.ShoeBoxNum FORMAT "->,>>>,>>9":U WIDTH 16.2
      TabShoeBox.Sex COLUMN-LABEL "Gender" FORMAT "x(1)":U WIDTH 11.4
      TabShoeBox.Age FORMAT "x(8)":U WIDTH 11.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 5.24
         FONT 9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     COMBO-BOX-2 AT ROW 2.67 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     FILL-IN-1 AT ROW 2.67 COL 49 COLON-ALIGNED WIDGET-ID 18
     COMBO-BOX-3 AT ROW 4.81 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     BUTTON-9 AT ROW 7.43 COL 10 WIDGET-ID 2
     BUTTON-10 AT ROW 7.43 COL 31 WIDGET-ID 4
     BROWSE-11 AT ROW 11.24 COL 6 WIDGET-ID 200
     "Gender" VIEW-AS TEXT
          SIZE 10 BY 1.19 AT ROW 2.67 COL 6 WIDGET-ID 14
     "Age" VIEW-AS TEXT
          SIZE 8 BY 1.19 AT ROW 4.57 COL 6 WIDGET-ID 16
     "ShoeBoxNum" VIEW-AS TEXT
          SIZE 16 BY 1.19 AT ROW 2.43 COL 34 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.6 BY 17
         FONT 9 WIDGET-ID 100.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 17
         WIDTH              = 62.6
         MAX-HEIGHT         = 17.33
         MAX-WIDTH          = 106.6
         VIRTUAL-HEIGHT     = 17.33
         VIRTUAL-WIDTH      = 106.6
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
/* BROWSE-TAB BROWSE-11 BUTTON-10 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-11
/* Query rebuild information for BROWSE BROWSE-11
     _TblList          = "ShoeBox.TabShoeBox"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ShoeBox.TabShoeBox.ShoeBoxNum
"ShoeBoxNum" ? ? "integer" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ShoeBox.TabShoeBox.Sex
"Sex" "Gender" ? "character" ? ? ? ? ? ? no ? no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ShoeBox.TabShoeBox.Age
"Age" ? ? "character" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-11 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

PROCEDURE addShoeBox:
    bShoeBox = NEW beTabShoeBox().
    bShoeBox:fetchdata().
    TEMP-TABLE ttTabShoeBox:TRACKING-CHANGES = TRUE.
    CREATE ttTabShoeBox.
    ASSIGN
        ttTabShoeBox.ShoeBoxNum = next-value(next_TabShoeBox).
        ttTabShoeBox.Sex = COMBO-BOX-2:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
        ttTabShoeBox.Age = COMBO-BOX-3:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
    TEMP-TABLE ttTabShoeBox:TRACKING-CHANGES = FALSE.
    bShoeBox:AddShoeBox(INPUT-OUTPUT DATASET dsTabShoeBox).
END.

PROCEDURE deleteShoeBox:
    
    DEFINE VAR tableHandle AS HANDLE.
    bShoeBox = NEW beTabShoeBox().
    bShoeBox:fetchdata().
   
        DEFINE VAR genericBufferHandle     AS HANDLE NO-UNDO.
        DEFINE VAR queryHandleGeneric      AS HANDLE NO-UNDO.
        
        ASSIGN hdsShoeBox = bShoeBox:getHandle().
    
        genericBufferHandle = hdsShoeBox:GET-BUFFER-HANDLE("ttTabShoeBox").

        
        CREATE QUERY queryHandleGeneric.
        queryHandleGeneric:SET-BUFFERS(genericBufferHandle).
        queryHandleGeneric:QUERY-PREPARE("FOR EACH ttTabShoeBox WHERE ttTabShoeBox.ShoeBoxNum = " + quoter(FILL-IN-1:SCREEN-VALUE IN FRAME DEFAULT-FRAME)).
        queryHandleGeneric:FORWARD-ONLY = TRUE.
        queryHandleGeneric:QUERY-OPEN() NO-ERROR.
        queryHandleGeneric:GET-FIRST().
        genericBufferHandle:BUFFER-DELETE ().

    bShoeBox:AddShoeBox2().
    
END.

ON CHOOSE OF BUTTON-9 DO:
    RUN addShoeBox.
    BROWSE BROWSE-11:REFRESH().
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

ON CHOOSE OF BUTTON-10 DO:
    RUN deleteShoeBox.
    BROWSE BROWSE-11:REFRESH().
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-11
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
  RUN enable_UI.
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
  DISPLAY COMBO-BOX-2 FILL-IN-1 COMBO-BOX-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE COMBO-BOX-2 FILL-IN-1 COMBO-BOX-3 BUTTON-9 BUTTON-10 BROWSE-11 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

