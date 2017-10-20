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
{dsRequest.i}
DEFINE VAR bRequest AS beRequest.
DEFINE VAR hdsRequest AS HANDLE.

{dsPerson.i}
DEFINE VAR bPerson   AS bePerson.
DEFINE VAR hdsPerson AS HANDLE.

{dsTabShoeBox.i}
DEFINE VAR bShoeBox   AS beTabShoeBox.
DEFINE VAR hdsShoeBox AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-8

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Request

/* Definitions for BROWSE BROWSE-8                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-8 Request.RequestNum Request.ReqDate ~
Request.PersonNum Request.ShoeBoxNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-8 
&Scoped-define QUERY-STRING-BROWSE-8 FOR EACH Request NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-8 OPEN QUERY BROWSE-8 FOR EACH Request NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-8 Request
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-8 Request


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-8}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-1 COMBO-BOX-2 BROWSE-8 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-1 COMBO-BOX-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 2" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-8 FOR 
      Request SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-8 C-Win _STRUCTURED
  QUERY BROWSE-8 NO-LOCK DISPLAY
      Request.RequestNum FORMAT "->,>>>,>>9":U
      Request.ReqDate FORMAT "99/99/99":U WIDTH 14
      Request.PersonNum FORMAT "->,>>>,>>9":U WIDTH 14
      Request.ShoeBoxNum FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61 BY 5.24 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     COMBO-BOX-1 AT ROW 2.91 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     COMBO-BOX-2 AT ROW 4.81 COL 20 COLON-ALIGNED WIDGET-ID 8
     BROWSE-8 AT ROW 11.24 COL 22 WIDGET-ID 200
     "PERSON:" VIEW-AS TEXT
          SIZE 15 BY 1.43 AT ROW 2.67 COL 5 WIDGET-ID 10
     "ShoeBox Type" VIEW-AS TEXT
          SIZE 15 BY 1.43 AT ROW 4.57 COL 4 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.2 BY 17.19 WIDGET-ID 100.


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
         HEIGHT             = 17.19
         WIDTH              = 106.2
         MAX-HEIGHT         = 17.19
         MAX-WIDTH          = 106.2
         VIRTUAL-HEIGHT     = 17.19
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
/* BROWSE-TAB BROWSE-8 COMBO-BOX-2 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-8
/* Query rebuild information for BROWSE BROWSE-8
     _TblList          = "ShoeBox.Request"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = ShoeBox.Request.RequestNum
     _FldNameList[2]   > ShoeBox.Request.ReqDate
"Request.ReqDate" ? ? "date" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ShoeBox.Request.PersonNum
"Request.PersonNum" ? ? "integer" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = ShoeBox.Request.ShoeBoxNum
     _Query            is OPENED
*/  /* BROWSE BROWSE-8 */
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

PROCEDURE generateList:
    COMBO-BOX-1:LIST-ITEMS IN FRAME DEFAULT-FRAME = "".

    bPerson = new bePerson().

    bPerson:fetchData().

    hdsPerson = bPerson:getHandle().

    DEFINE VAR personTableHandle     AS HANDLE NO-UNDO.
    DEFINE VAR queryHandleGeneric    AS HANDLE NO-UNDO.
    DEFINE VAR personName            AS CHARACTE NO-UNDO.

    personTableHandle = hdsPerson:GET-BUFFER-HANDLE("ttPerson").

    CREATE QUERY queryHandleGeneric.
        queryHandleGeneric:SET-BUFFERS(personTableHandle).
        queryHandleGeneric:QUERY-PREPARE("FOR EACH ttPerson" ).
        queryHandleGeneric:FORWARD-ONLY = TRUE.
        queryHandleGeneric:QUERY-OPEN() NO-ERROR.


        REPEAT:
            queryHandleGeneric:GET-NEXT().
            IF queryHandleGeneric:QUERY-OFF-END THEN
              LEAVE.
           IF COMBO-BOX-1:LIST-ITEMS IN FRAME DEFAULT-FRAME = ? THEN DO:
                COMBO-BOX-1:LIST-ITEMS = personTableHandle::Name.
            END.
            ELSE DO:
                COMBO-BOX-1:LIST-ITEMS = COMBO-BOX-1:LIST-ITEMS + "," + personTableHandle::Name.
            END.
              //BROWSE-6:ITEMS-PER-ROW = personTableHandle.
        END.
END.

PROCEDURE generateList2:
    COMBO-BOX-2:LIST-ITEMS IN FRAME DEFAULT-FRAME = "".

    bShoeBox = new beTabShoeBox().

    bShoeBox:fetchData().

    hdsShoeBox = bShoeBox:getHandle().

    DEFINE VAR shoeBoxTableHandle     AS HANDLE NO-UNDO.
    DEFINE VAR queryHandleGeneric    AS HANDLE NO-UNDO.
    DEFINE VAR personName            AS CHARACTE NO-UNDO.

    shoeBoxTableHandle = hdsShoeBox:GET-BUFFER-HANDLE("ttTabShoeBox").

    CREATE QUERY queryHandleGeneric.
        queryHandleGeneric:SET-BUFFERS(shoeBoxTableHandle).
        queryHandleGeneric:QUERY-PREPARE("FOR EACH ttTabShoeBox" ).
        queryHandleGeneric:FORWARD-ONLY = TRUE.
        queryHandleGeneric:QUERY-OPEN() NO-ERROR.


        REPEAT:
            queryHandleGeneric:GET-NEXT().
            IF queryHandleGeneric:QUERY-OFF-END THEN
              LEAVE.
           IF COMBO-BOX-2:LIST-ITEMS IN FRAME DEFAULT-FRAME = ? THEN DO:
                COMBO-BOX-2:LIST-ITEMS = shoeBoxTableHandle::Sex + " " + shoeBoxTableHandle::Age.
            END.
            ELSE DO:
                COMBO-BOX-2:LIST-ITEMS = COMBO-BOX-2:LIST-ITEMS + "," + shoeBoxTableHandle::Sex + " " + shoeBoxTableHandle::Age .
            END.
              //BROWSE-6:ITEMS-PER-ROW = personTableHandle.
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-8
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
  RUN generateList.
  RUN generateList2.
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
  DISPLAY COMBO-BOX-1 COMBO-BOX-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE COMBO-BOX-1 COMBO-BOX-2 BROWSE-8 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

