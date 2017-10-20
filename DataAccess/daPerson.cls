 
 /*------------------------------------------------------------------------
    File        : daPerson
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Tue Oct 17 16:32:34 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.



CLASS daPerson INHERITS dataAccess: 
    {ttPerson.i}
    DEFINE PRIVATE DATASET dsPerson FOR ttPerson.
    DEFINE DATA-SOURCE srcPerson FOR Person.
        
    CONSTRUCTOR PUBLIC daPerson (  ):
        SUPER ().
        
    END CONSTRUCTOR.
    
     METHOD PUBLIC VOID fetchData(INPUT-OUTPUT DATASET dsPerson , INPUT filterString AS CHAR):
        DEFINE VAR personTableHandel     AS HANDLE NO-UNDO.
        DEFINE VAR queryHandleGeneric    AS HANDLE NO-UNDO.
        DEFINE VAR hdsPerson             AS HANDLE NO-UNDO.
        

        
  
        BUFFER ttPerson:ATTACH-DATA-SOURCE(DATA-SOURCE srcPerson:HANDLE).

        hdsPerson= DATASET dsPerson:HANDLE.
        
        SUPER:fetchData(hdsPerson).
        BUFFER ttPerson:DETACH-DATA-SOURCE().
        
        personTableHandel = hdsPerson:GET-BUFFER-HANDLE("ttPerson"). 
        
    END METHOD.
    
    // save data
    METHOD PUBLIC LOGICAL store(INPUT-OUTPUT DATASET dsPerson):
        
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        //DEFINE DATA-SOURCE srcPerson FOR Person.
        BUFFER ttPerson:ATTACH-DATA-SOURCE (DATA-SOURCE srcPerson:HANDLE).
        
        FOR EACH bf-ttPerson:
            BUFFER bf-ttPerson:SAVE-ROW-CHANGES().
        END.
/*        DO TRANSACTION:                                                                */
/*            FOR EACH bf-ttPerson:                                                      */
/*                                                                                       */
/*                lResult = BUFFER bf-ttPerson:SAVE-ROW-CHANGES(1, "PersonNum") NO-ERROR.*/
/*                IF NOT lResult THEN                                                    */
/*                DO:                                                                    */
/*                    BUFFER bf-ttPerson:ERROR = TRUE.                                   */
/*                    BUFFER bf-ttPerson:ERROR-STRING = ERROR-STATUS:GET-MESSAGE(1).     */
/*                    RETURN FALSE.                                                      */
/*                END.                                                                   */
/*                IF NOT BUFFER bf-ttPerson:ACCEPT-ROW-CHANGES() THEN                    */
/*                    RETURN FALSE.                                                      */
/*            END.                                                                       */
/*        END.                                                                           */
        BUFFER ttPerson:DETACH-DATA-SOURCE().
        RETURN TRUE.
        
    END METHOD.

    DESTRUCTOR PUBLIC daPerson ( ):

    END DESTRUCTOR.

END CLASS.