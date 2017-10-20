 
 /*------------------------------------------------------------------------
    File        : beDonation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Wed Oct 18 11:12:58 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS beDonation: 
    DEFINE PRIVATE VARIABLE dataAccessDonation AS daDonation.
    {dsDonation.i}
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC beDonation (  ):
        dataAccessDonation = NEW daDonation().
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID fetchData():
        DEFINE VAR hdsDonation AS HANDLE NO-UNDO.
        DEFINE VAR filterString AS CHAR INIT "" FORMAT 'X(50)' NO-UNDO.
        
        dataAccessDonation:fetchData(INPUT-OUTPUT DATASET dsDonation BY-REFERENCE, INPUT filterString).
        
    END METHOD.
    
    METHOD PUBLIC HANDLE getHandle():
        RETURN DATASET dsDonation:HANDLE.
    END METHOD.
    
    

    DESTRUCTOR PUBLIC beDonation ( ):

    END DESTRUCTOR.

END CLASS.