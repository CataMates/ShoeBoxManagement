
/*------------------------------------------------------------------------
    File        : createReceiver.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Oct 16 13:50:07 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE i AS INTEGER.


/*CREATE TabShoeBox.         */
/*ASSIGN                     */
/*    TabShoeBox.Sex = "F".  */
/*    TabShoeBox.Age = "4-6".*/
    
FOR EACH TabShoeBox:
    DISPLAY TabShoeBox.sex.
    DISPLAY TabShoeBox.Age.
END.




