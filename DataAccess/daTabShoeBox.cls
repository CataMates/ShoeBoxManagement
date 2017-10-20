 
 /*------------------------------------------------------------------------
    File        : daTabShoeBox
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Wed Oct 18 11:20:42 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS daTabShoeBox INHERITS dataAccess: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC daTabShoeBox (  ):
        SUPER ().
        
    END CONSTRUCTOR.
    
    {ttTabShoeBox.i}
    DEFINE PRIVATE DATASET dsTabShoeBox FOR ttTabShoeBox.
    DEFINE DATA-SOURCE srcTabShoeBox FOR TabShoeBox.
        
    
    METHOD PUBLIC VOID fetchData(INPUT-OUTPUT DATASET dsTabShoeBox, INPUT filterString AS CHAR):
        DEFINE VAR tabShoeBoxTableHandle     AS HANDLE NO-UNDO.
        DEFINE VAR queryHandleGeneric    AS HANDLE NO-UNDO.
        DEFINE VAR hdsTabShoeBox            AS HANDLE NO-UNDO.

        
  
        BUFFER ttTabShoeBox:ATTACH-DATA-SOURCE(DATA-SOURCE srcTabShoeBox:HANDLE).

        hdsTabShoeBox= DATASET dsTabShoeBox :HANDLE.
        SUPER:fetchData(hdsTabShoeBox).
        BUFFER ttTabShoeBox:DETACH-DATA-SOURCE().
        
        tabShoeBoxTableHandle = hdsTabShoeBox:GET-BUFFER-HANDLE("ttTabShoeBox").
        
    END METHOD.
    
    // save data
    METHOD PUBLIC LOGICAL store(INPUT-OUTPUT DATASET dsTabShoeBox):
        
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        //DEFINE DATA-SOURCE srcRequest FOR Request.
        BUFFER ttTabShoeBox:ATTACH-DATA-SOURCE (DATA-SOURCE srcTabShoeBox:HANDLE).
        FOR EACH bf-ttTabShoeBox:
            BUFFER bf-ttTabShoeBox:SAVE-ROW-CHANGES().
        END.
/*        DO TRANSACTION:                                                                     */
/*            FOR EACH bf-ttTabShoeBox:                                                       */
/*                                                                                            */
/*                lResult = BUFFER bf-ttTabShoeBox:SAVE-ROW-CHANGES(1, "ShoeBoxNum") NO-ERROR.*/
/*                IF NOT lResult THEN                                                         */
/*                DO:                                                                         */
/*                    BUFFER bf-ttTabShoeBox:ERROR = TRUE.                                    */
/*                    BUFFER bf-ttTabShoeBox:ERROR-STRING = ERROR-STATUS:GET-MESSAGE(1).      */
/*                    RETURN FALSE.                                                           */
/*                END.                                                                        */
/*                IF NOT BUFFER bf-ttTabShoeBox:ACCEPT-ROW-CHANGES() THEN                     */
/*                    RETURN FALSE.                                                           */
/*            END.                                                                            */
/*        END.                                                                                */
        BUFFER ttTabShoeBox:DETACH-DATA-SOURCE().
        RETURN TRUE.
        
    END METHOD.

    DESTRUCTOR PUBLIC daTabShoeBox ( ):

    END DESTRUCTOR.

END CLASS.