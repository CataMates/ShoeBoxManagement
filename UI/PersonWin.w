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

{dsPerson.i}
DEFINE VAR bPerson   AS bePerson.
DEFINE VAR hdsPerson AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-6

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Person

/* Definitions for BROWSE BROWSE-6                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-6 Person.PersonNum Person.Name ~
Person.Address Person.CI 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-6 
&Scoped-define QUERY-STRING-BROWSE-6 FOR EACH Person NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-6 OPEN QUERY BROWSE-6 FOR EACH Person NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-6 Person
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-6 Person


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-6}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-3 BUTTON-2 ~
BUTTON-3 BUTTON-4 BROWSE-6 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL "CREATE" 
     SIZE 15 BY 1.43.

DEFINE BUTTON BUTTON-3 
     LABEL "UPDATE" 
     SIZE 17 BY 1.43.

DEFINE BUTTON BUTTON-4 
     LABEL "DELETE" 
     SIZE 17 BY 1.43.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.19 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 2" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.19 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 3" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-6 FOR 
      Person SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-6 C-Win _STRUCTURED
  QUERY BROWSE-6 NO-LOCK DISPLAY
      Person.PersonNum FORMAT "->,>>>,>>9":U WIDTH 13.2
      Person.Name FORMAT "x(20)":U
      Person.Address FORMAT "x(30)":U
      Person.CI FORMAT "x(15)":U WIDTH 12.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 5.95
         FONT 9 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-1 AT ROW 1.95 COL 21 COLON-ALIGNED WIDGET-ID 12
     FILL-IN-2 AT ROW 3.86 COL 21 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-3 AT ROW 5.76 COL 21 COLON-ALIGNED WIDGET-ID 16
     BUTTON-2 AT ROW 8.14 COL 21 WIDGET-ID 18
     BUTTON-3 AT ROW 8.14 COL 41 WIDGET-ID 20
     BUTTON-4 AT ROW 8.14 COL 61 WIDGET-ID 22
     BROWSE-6 AT ROW 10.29 COL 13 WIDGET-ID 200
     "CUI:" VIEW-AS TEXT
          SIZE 11 BY 1.67 AT ROW 5.29 COL 11 WIDGET-ID 8
     "Name:" VIEW-AS TEXT
          SIZE 11 BY 1.19 AT ROW 1.71 COL 11 WIDGET-ID 4
     "Address:" VIEW-AS TEXT
          SIZE 11 BY 1.33 AT ROW 3.62 COL 11 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.2 BY 17.33
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
         HEIGHT             = 17.33
         WIDTH              = 106.2
         MAX-HEIGHT         = 17.33
         MAX-WIDTH          = 106.2
         VIRTUAL-HEIGHT     = 17.33
         VIRTUAL-WIDTH      = 106.2
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
                                                                        */
/* BROWSE-TAB BROWSE-6 BUTTON-4 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-6
/* Query rebuild information for BROWSE BROWSE-6
     _TblList          = "ShoeBox.Person"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ShoeBox.Person.PersonNum
"Person.PersonNum" ? ? "integer" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ShoeBox.Person.Name
     _FldNameList[3]   = ShoeBox.Person.Address
     _FldNameList[4]   > ShoeBox.Person.CI
"Person.CI" ? ? "character" ? ? ? ? ? ? no ? no no "12.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-6 */
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

/*PROCEDURE generateList:                                                                       */
/*    SELECT-4:LIST-ITEMS IN FRAME DEFAULT-FRAME = "".                                          */
/*                                                                                              */
/*    bPerson = new bePerson().                                                                 */
/*                                                                                              */
/*    bPerson:fetchData().                                                                      */
/*                                                                                              */
/*    hdsPerson = bPerson:getHandle().                                                          */
/*                                                                                              */
/*    DEFINE VAR personTableHandle     AS HANDLE NO-UNDO.                                       */
/*    DEFINE VAR queryHandleGeneric    AS HANDLE NO-UNDO.                                       */
/*    DEFINE VAR personName            AS CHARACTE NO-UNDO.                                     */
/*                                                                                              */
/*    personTableHandle = hdsPerson:GET-BUFFER-HANDLE("ttPerson").                              */
/*                                                                                              */
/*    CREATE QUERY queryHandleGeneric.                                                          */
/*        queryHandleGeneric:SET-BUFFERS(personTableHandle).                                    */
/*        queryHandleGeneric:QUERY-PREPARE("FOR EACH ttPerson" ).                               */
/*        queryHandleGeneric:FORWARD-ONLY = TRUE.                                               */
/*        queryHandleGeneric:QUERY-OPEN() NO-ERROR.                                             */
/*                                                                                              */
/*                                                                                              */
/*        REPEAT:                                                                               */
/*            queryHandleGeneric:GET-NEXT().                                                    */
/*            IF queryHandleGeneric:QUERY-OFF-END THEN                                          */
/*              LEAVE.                                                                          */
/*/*            IF SELECT-4:LIST-ITEMS IN FRAME DEFAULT-FRAME = ? THEN DO:                    */*/
/*/*                SELECT-4:LIST-ITEMS = personTableHandle::Name.                            */*/
/*/*            END.                                                                          */*/
/*/*            ELSE DO:                                                                      */*/
/*/*                SELECT-4:LIST-ITEMS = SELECT-4:LIST-ITEMS + "," + personTableHandle::Name.*/*/
/*/*            END.                                                                          */*/
/*              BROWSE-6:ITEMS-PER-ROW = personTableHandle.                                     */
/*        END.                                                                                  */
/*END.                                                                                          */

PROCEDURE addPerson:
    
    DEFINE VAR tableHandle AS HANDLE.
    bPerson = NEW bePerson().
    bPerson:fetchdata().
    TEMP-TABLE ttPerson:TRACKING-CHANGES = TRUE.
    CREATE ttPerson.
    ASSIGN
        ttPerson.PersonNum = next-value(next_person).
        ttPerson.Name = FILL-IN-1:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
        ttPerson.Address = FILL-IN-2:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
        ttPerson.CI = FILL-IN-3:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
    TEMP-TABLE ttPerson:TRACKING-CHANGES = FALSE.
    bPerson:AddPerson(INPUT-OUTPUT DATASET dsPerson). 
    
    

        
/*    APPLY "VALUE-CHANGED" TO BROWSE-6 .*/

END.

PROCEDURE updatePerson:
    DEFINE VAR tableHandle AS HANDLE.
    DEFINE VAR hdsPerson AS HANDLE.
    bPerson = NEW bePerson().
    bPerson:fetchdata().
    

        DEFINE VAR genericBufferHandle     AS HANDLE NO-UNDO.
        DEFINE VAR queryHandleGeneric      AS HANDLE NO-UNDO.
        
        ASSIGN hdsPerson = bPerson:getHandle().
    
        genericBufferHandle = hdsPerson:GET-BUFFER-HANDLE("ttPerson").
     .
        CREATE QUERY queryHandleGeneric.
        queryHandleGeneric:SET-BUFFERS(genericBufferHandle).
        queryHandleGeneric:QUERY-PREPARE("FOR EACH ttPerson WHERE ttPerson.Name = " + quoter(FILL-IN-1:SCREEN-VALUE IN FRAME DEFAULT-FRAME)).
        queryHandleGeneric:FORWARD-ONLY = TRUE.
        queryHandleGeneric:QUERY-OPEN() NO-ERROR.
        queryHandleGeneric:GET-FIRST().
        
        genericBufferHandle::Address = STRING(FILL-IN-2:SCREEN-VALUE IN FRAME DEFAULT-FRAME).
        genericBufferHandle::CI = STRING(FILL-IN-3:SCREEN-VALUE IN FRAME DEFAULT-FRAME).
        
    bPerson:AddPerson2().
    
/*    DEFINE VAR na AS CHAR.                                 */
/*    DEFINE VAR ad AS CHAR.                                 */
/*    DEFINE VAR ci AS CHAR.                                 */
/*                                                           */
/*    ASSIGN                                                 */
/*        na = FILL-IN-1:SCREEN-VALUE IN FRAME DEFAULT-FRAME.*/
/*        ad = FILL-IN-2:SCREEN-VALUE IN FRAME DEFAULT-FRAME.*/
/*        ci = FILL-IN-3:SCREEN-VALUE IN FRAME DEFAULT-FRAME */
/*                                                           */
/*    TEMP-TABLE ttPerson:TRACKING-CHANGES = TRUE.           */
/*                                                           */
/*    FIND FIRST ttPerson WHERE ttPerson.Name = na.          */
/*    ttPerson.Address = ad.                                 */
/*    ttPerson.CI = ci.                                      */
   
    
    
    

/*    ASSIGN                                                               */
/*        ttPerson.Address = FILL-IN-2:SCREEN-VALUE IN FRAME DEFAULT-FRAME.*/
/*        ttPerson.CI = FILL-IN-3:SCREEN-VALUE IN FRAME DEFAULT-FRAME.     */
/*    UPDATE ttPerson.                                                     */
/*    TEMP-TABLE ttPerson:TRACKING-CHANGES = FALSE.                        */

    
/*    bPerson:AddPerson(INPUT-OUTPUT DATASET dsPerson).*/
END.

PROCEDURE deletePerson:
    DEFINE VAR tableHandle AS HANDLE.
    bPerson = NEW bePerson().
    bPerson:fetchdata().
    

/*    TEMP-TABLE ttPerson:TRACKING-CHANGES = TRUE.*/
    
/*    BROWSE-6:DELETE-SELECTED-ROW(BROWSE-6:NUM-SELECTED-ROWS) IN FRAME DEFAULT-FRAME.*/
    
    
        DEFINE VAR genericBufferHandle     AS HANDLE NO-UNDO.
        DEFINE VAR queryHandleGeneric      AS HANDLE NO-UNDO.
        
        ASSIGN hdsPerson = bPerson:getHandle().
    
        genericBufferHandle = hdsPerson:GET-BUFFER-HANDLE("ttPerson").

        
        CREATE QUERY queryHandleGeneric.
        queryHandleGeneric:SET-BUFFERS(genericBufferHandle).
        queryHandleGeneric:QUERY-PREPARE("FOR EACH ttPerson WHERE ttPerson.Name = " + quoter(FILL-IN-1:SCREEN-VALUE IN FRAME DEFAULT-FRAME)).
        queryHandleGeneric:FORWARD-ONLY = TRUE.
        queryHandleGeneric:QUERY-OPEN() NO-ERROR.
        queryHandleGeneric:GET-FIRST().
        genericBufferHandle:BUFFER-DELETE ().

    bPerson:AddPerson2().
END.


ON CHOOSE OF BUTTON-2 DO:
    RUN addPerson.
    BROWSE BROWSE-6:REFRESH().
    {&OPEN-QUERY-{&BROWSE-NAME}}
   
END.
ON CHOOSE OF BUTTON-3 DO:
    RUN updatePerson.
    BROWSE BROWSE-6:REFRESH().
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

ON CHOOSE OF BUTTON-4 DO:
    RUN deletePerson.
    BROWSE BROWSE-6:REFRESH().
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-6
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
  //RUN generateList.
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
  DISPLAY FILL-IN-1 FILL-IN-2 FILL-IN-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FILL-IN-1 FILL-IN-2 FILL-IN-3 BUTTON-2 BUTTON-3 BUTTON-4 BROWSE-6 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

