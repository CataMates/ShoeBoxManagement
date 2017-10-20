 
 /*------------------------------------------------------------------------
    File        : daRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Tue Oct 17 18:38:11 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS daRequest INHERITS dataAccess: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {ttRequest.i}
    DEFINE PRIVATE DATASET dsRequest FOR ttRequest.
    DEFINE DATA-SOURCE srcRequest FOR Request.
        
    CONSTRUCTOR PUBLIC daRequest (  ):
        SUPER ().
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID fetchData(INPUT-OUTPUT DATASET dsRequest , INPUT filterString AS CHAR):
        DEFINE VAR requestTableHandle     AS HANDLE NO-UNDO.
        DEFINE VAR queryHandleGeneric    AS HANDLE NO-UNDO.
        DEFINE VAR hdsRequest            AS HANDLE NO-UNDO.

        
  
        BUFFER ttRequest:ATTACH-DATA-SOURCE(DATA-SOURCE srcRequest:HANDLE).

        hdsRequest= DATASET dsRequest :HANDLE.
        SUPER:fetchData(hdsRequest).
        BUFFER ttRequest:DETACH-DATA-SOURCE().
        
        requestTableHandle = hdsRequest:GET-BUFFER-HANDLE("ttRequest").
        
    END METHOD.
    
    // save data
    METHOD PUBLIC LOGICAL saveRowChanges(INPUT-OUTPUT DATASET dsRequest):
        
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        //DEFINE DATA-SOURCE srcRequest FOR Request.
        BUFFER ttRequest:ATTACH-DATA-SOURCE (DATA-SOURCE srcRequest:HANDLE).
        
        DO TRANSACTION:
            FOR EACH bf-ttRequest:
                
                lResult = BUFFER bf-ttRequest:SAVE-ROW-CHANGES(1, "RequstNum") NO-ERROR.
                IF NOT lResult THEN
                DO:
                    BUFFER bf-ttRequest:ERROR = TRUE.
                    BUFFER bf-ttRequest:ERROR-STRING = ERROR-STATUS:GET-MESSAGE(1).
                    RETURN FALSE.
                END.
                IF NOT BUFFER bf-ttRequest:ACCEPT-ROW-CHANGES() THEN
                    RETURN FALSE.
            END.
        END.
        BUFFER ttRequest:DETACH-DATA-SOURCE().
        RETURN TRUE.
        
    END METHOD.
    

    DESTRUCTOR PUBLIC daRequest ( ):

    END DESTRUCTOR.

END CLASS.