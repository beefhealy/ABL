 
 /*------------------------------------------------------------------------
    File        : SVNLog
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : chealy
    Created     : Thu Nov 12 09:49:28 GMT 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS classes.SVNLog: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
	DEFINE PRIVATE VARIABLE cXMLPath AS CHARACTER.
	DEFINE PRIVATE VARIABLE cXSDPath AS CHARACTER.
	
	
    DEFINE TEMP-TABLE logentry NO-UNDO
        FIELD revision AS DECIMAL DECIMALS 0 XML-DATA-TYPE "unsignedInt" XML-NODE-TYPE "ATTRIBUTE" 
        FIELD author   AS CHARACTER 
        FIELD date     AS DATETIME-TZ 
        FIELD msg      AS CHARACTER 
        INDEX revKey IS UNIQUE PRIMARY revision.

    DEFINE TEMP-TABLE paths NO-UNDO
        FIELD logentry_id AS RECID XML-NODE-TYPE "HIDDEN" .

    DEFINE TEMP-TABLE path NO-UNDO
        FIELD action        AS CHARACTER XML-NODE-TYPE "ATTRIBUTE" 
        FIELD copyfrom-path AS CHARACTER XML-NODE-TYPE "ATTRIBUTE" 
        FIELD copyfrom-rev  AS DECIMAL DECIMALS 0 XML-DATA-TYPE "unsignedInt" XML-NODE-TYPE "ATTRIBUTE" 
        FIELD path_Text     AS CHARACTER XML-NODE-TYPE "TEXT" 
        FIELD paths_id      AS RECID XML-NODE-TYPE "HIDDEN" .

    DEFINE DATASET log  
        FOR logentry, paths, path
        PARENT-ID-RELATION RELATION1 FOR paths, path
        PARENT-ID-FIELD paths_id
        PARENT-ID-RELATION RELATION2 FOR logentry, paths
        PARENT-ID-FIELD logentry_id
        PARENT-FIELDS-BEFORE (author,date)
        PARENT-FIELDS-AFTER (msg).
		
	CONSTRUCTOR PUBLIC SVNLog ( INPUT ipcXMLPath AS CHARACTER, INPUT ipcXSDPath AS CHARACTER ):
		SUPER ().
		
		
		ASSIGN 
		    cXMLPath = ipcXMLPath
		    cXSDPath = ipcXSDPath.
		
	END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL ReadXML(  ):
		
		DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        ASSIGN lResult = DATASET LOG:READ-XML ("FILE", cXMLPath,"EMPTY",?,?,?).
        
		RETURN lResult.

	END METHOD.    
	
	
	METHOD PUBLIC LONGCHAR GetXML():
	    DEFINE VARIABLE lcXML AS LONGCHAR NO-UNDO.
	    
	    DATASET LOG:WRITE-XML ("LONGCHAR", lcXML).
	    
	    RETURN lcXML.
	END.
	
            
        
	DESTRUCTOR PUBLIC SVNLog ( ):

	END DESTRUCTOR.

END CLASS.