 
 /*------------------------------------------------------------------------
    File        : dataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Tue Oct 17 16:35:57 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS dataAccess ABSTRACT: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC dataAccess (  ):
        SUPER ().
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID fetchData(INPUT-OUTPUT hdsGeneric AS HANDLE):
        hdsGeneric:FILL().
    END METHOD.
    
    

    DESTRUCTOR PUBLIC dataAccess ( ):

    END DESTRUCTOR.

END CLASS.
 
