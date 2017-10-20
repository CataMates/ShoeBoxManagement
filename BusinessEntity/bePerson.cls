 
 /*------------------------------------------------------------------------
    File        : bePerosn
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Wed Oct 18 12:39:32 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bePerson: 
    {dsPerson.i}
    DEFINE VAR dataAccessPerson AS daPerson NO-UNDO.
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC bePerson (  ):
        dataAccessPerson = NEW daPerson().
                
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID fetchData():
        DEFINE VAR hdsPerson AS HANDLE NO-UNDO.
        DEFINE VAR filterString AS CHAR INIT "" FORMAT 'X(50)' NO-UNDO.
        dataAccessPerson:fetchData(INPUT-OUTPUT DATASET dsPerson BY-REFERENCE, INPUT filterString).
        TEMP-TABLE ttPerson:TRACKING-CHANGES = TRUE.
    END METHOD.
    
    METHOD PUBLIC HANDLE getHandle():
        RETURN DATASET dsPerson:HANDLE.
    END METHOD.
    
    // add-delete-update booking method
    METHOD PUBLIC LOGICAL AddPerson(INPUT-OUTPUT DATASET dsPerson):

        dataAccessPerson =  NEW daPerson().
        dataAccessPerson:store(INPUT-OUTPUT DATASET dsPerson).
        RETURN TRUE.
        
        FINALLY:
            IF VALID-OBJECT (dataAccessPerson) THEN
            DELETE OBJECT dataAccessPerson.
        END FINALLY.
        
    END METHOD.
    
     METHOD PUBLIC LOGICAL AddPerson2():
        
        dataAccessPerson =  NEW daPerson().
        dataAccessPerson:store(INPUT-OUTPUT DATASET dsPerson).
        RETURN TRUE.
        
        FINALLY:
            IF VALID-OBJECT (dataAccessPerson) THEN
            DELETE OBJECT dataAccessPerson.
        END FINALLY.
        
    END METHOD.

    DESTRUCTOR PUBLIC bePerson ( ):

    END DESTRUCTOR.

END CLASS.