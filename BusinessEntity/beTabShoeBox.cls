 
 /*------------------------------------------------------------------------
    File        : beTabShoeBox
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Wed Oct 18 11:26:12 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS beTabShoeBox: 
    DEFINE PRIVATE VARIABLE dataAccessTabShoeBox AS daTabShoeBox.
    {dsTabShoeBox.i}
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC beTabShoeBox (  ):
        dataAccessTabShoeBox = NEW daTabShoeBox().
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID fetchData():
        DEFINE VAR hdsTabShoeBox AS HANDLE NO-UNDO.
        DEFINE VAR filterString AS CHAR INIT "" FORMAT 'X(50)' NO-UNDO.
        dataAccessTabShoeBox:fetchData(INPUT-OUTPUT DATASET dsTabShoeBox BY-REFERENCE, INPUT filterString).
        
        TEMP-TABLE ttTabShoeBox:TRACKING-CHANGES = TRUE.
        
    END METHOD.
    
    METHOD PUBLIC HANDLE getHandle():
        RETURN DATASET dsTabShoeBox:HANDLE.
    END METHOD.
    
    // add-delete-update booking method
    METHOD PUBLIC LOGICAL AddShoeBox(INPUT-OUTPUT DATASET dsTabShoeBox):

        dataAccessTabShoeBox =  NEW daTabShoeBox().
        dataAccessTabShoeBox:store(INPUT-OUTPUT DATASET dsTabShoeBox).
        RETURN TRUE.
        
        FINALLY:
            IF VALID-OBJECT (dataAccessTabShoeBox) THEN
            DELETE OBJECT dataAccessTabShoeBox.
        END FINALLY.
        
    END METHOD.
    
    METHOD PUBLIC LOGICAL AddShoeBox2():

        dataAccessTabShoeBox =  NEW daTabShoeBox().
        dataAccessTabShoeBox:store(INPUT-OUTPUT DATASET dsTabShoeBox).
        RETURN TRUE.
        
        FINALLY:
            IF VALID-OBJECT (dataAccessTabShoeBox) THEN
            DELETE OBJECT dataAccessTabShoeBox.
        END FINALLY.
        
    END METHOD.

    DESTRUCTOR PUBLIC beTabShoeBox ( ):

    END DESTRUCTOR.

END CLASS.