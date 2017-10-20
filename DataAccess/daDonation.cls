 
 /*------------------------------------------------------------------------
    File        : daDonation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Wed Oct 18 11:05:15 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS daDonation INHERITS dataAccess: 
    
    {ttDonation.i}
    DEFINE PRIVATE DATASET dsDonation FOR ttDonation.
    DEFINE DATA-SOURCE srcDonation FOR Donation.
        
    CONSTRUCTOR PUBLIC daDonation(  ):
        SUPER ().
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID fetchData(INPUT-OUTPUT DATASET dsDonation, INPUT filterString AS CHAR):
        DEFINE VAR donationTableHandle     AS HANDLE NO-UNDO.
        DEFINE VAR queryHandleGeneric    AS HANDLE NO-UNDO.
        DEFINE VAR hdsDonation            AS HANDLE NO-UNDO.

        
  
        BUFFER ttDonation:ATTACH-DATA-SOURCE(DATA-SOURCE srcDonation:HANDLE).

        hdsDonation= DATASET dsDonation :HANDLE.
        SUPER:fetchData(hdsDonation).
        BUFFER ttDonation:DETACH-DATA-SOURCE().
        
        donationTableHandle = hdsDonation:GET-BUFFER-HANDLE("ttDonation").
        
    END METHOD.
    
    // save data
    METHOD PUBLIC LOGICAL saveRowChanges(INPUT-OUTPUT DATASET dsDonation):
        
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        //DEFINE DATA-SOURCE srcRequest FOR Request.
        BUFFER ttDonation:ATTACH-DATA-SOURCE (DATA-SOURCE srcDonation:HANDLE).
        
        DO TRANSACTION:
            FOR EACH bf-ttDonation:
                
                lResult = BUFFER bf-ttDonation:SAVE-ROW-CHANGES(1, "DonationNum") NO-ERROR.
                IF NOT lResult THEN
                DO:
                    BUFFER bf-ttDonation:ERROR = TRUE.
                    BUFFER bf-ttDonation:ERROR-STRING = ERROR-STATUS:GET-MESSAGE(1).
                    RETURN FALSE.
                END.
                IF NOT BUFFER bf-ttDonation:ACCEPT-ROW-CHANGES() THEN
                    RETURN FALSE.
            END.
        END.
        BUFFER ttDonation:DETACH-DATA-SOURCE().
        RETURN TRUE.
        
    END METHOD.
    
    DESTRUCTOR PUBLIC daDonation ( ):

    END DESTRUCTOR.

END CLASS.