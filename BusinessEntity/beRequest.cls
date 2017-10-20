 
 /*------------------------------------------------------------------------
    File        : baRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Tue Oct 17 18:55:20 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS beRequest: 
    DEFINE PRIVATE VARIABLE dataAccessRequest AS daRequest.
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {dsRequest.i}
    
    CONSTRUCTOR PUBLIC beRequest (  ):
        dataAccessRequest = NEW daRequest().
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID fetchData():
        DEFINE VAR hdsRequest AS HANDLE NO-UNDO.
        DEFINE VAR filterString AS CHAR INIT "" FORMAT 'X(50)' NO-UNDO.
        
        dataAccessRequest:fetchData(INPUT-OUTPUT DATASET dsRequest BY-REFERENCE, INPUT filterString).
        
    END METHOD.
    
    METHOD PUBLIC HANDLE getHandle():
        RETURN DATASET dsRequest:HANDLE.
    END METHOD.

    DESTRUCTOR PUBLIC beRequest ( ):

    END DESTRUCTOR.

END CLASS.