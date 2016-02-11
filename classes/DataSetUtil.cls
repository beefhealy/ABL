 
 /*------------------------------------------------------------------------
    File        : DataSetUtil
    Purpose     : 
    Syntax      : 
    Description : DataSet Utilities 
                  Create a dynamic dataset and add table buffers & relationships
                  using the methods provided.
                  The dataset can be returned as XML for fop processing via dsToXML
    Author(s)       : cohealy
    Created      : Fri Sep 7th 2012
    Notes        : 
    Instantiation:
                    DEFINE VARIABLE myDataSet AS comutils.libs.DataSetUtil.
                    myDataSet = NEW comutils.libs.DataSetUtil().    
  ----------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.

/**
 * @author      Cormac Healy 
 * @since       2012-09-27          
 */
CLASS classes.DataSetUtil:
    

    /*Handle to store document dataset - only accessible via available methods*/
    DEFINE PRIVATE VARIABLE hDataSet AS HANDLE NO-UNDO.    
    
	CONSTRUCTOR PUBLIC DataSetUtil (  ):
        /* may set initial defaults for a valid document such as encoding etc... here */
        CREATE DATASET hDataSet.
	END CONSTRUCTOR.
	
	CONSTRUCTOR PUBLIC DataSetUtil (ipcNodeName AS CHARACTER):
        /* may set initial defaults for a valid document such as encoding etc... here */
        CREATE DATASET hDataSet.
        ASSIGN hDataSet:XML-NODE-NAME = ipcNodeName.
    END CONSTRUCTOR.
	
	DESTRUCTOR PUBLIC DataSetUtil ():
	    IF VALID-HANDLE(hDataSet) /*just in case...*/ 
           THEN destroyDataSet().
    END DESTRUCTOR.

    
    /**
    * Add temp-table buffer handles to the data set.
    * @param iphBuffer - The temp-table buffer handle to be added
    * @return TRUE if the buffer was added successfully
    */    
	METHOD PUBLIC LOGICAL addToDataSet(iphBuffer AS HANDLE):
        RETURN hDataSet:ADD-BUFFER (iphBuffer).                
    END.
    
    /**
    * Create a relationship between tables/buffers in the dataset.
    * @param iphParentBuffer - The parent temp-table buffer handle to be added
    * @param iphChilduffer - The child temp-table buffer handle to be added
    * @param ipcRelatedFields - The related fields between the buffers
    * @param iplNested - Nest child records within parent
    * @return TRUE if the relationship was added successfully
    */
    METHOD PUBLIC LOGICAL addDataSetRelation(iphParentBuffer AS HANDLE,
                                             iphChildBUffer AS HANDLE,
                                             ipcRelatedFields AS CHARACTER,
                                             iplNested AS LOGICAL):        
        /*Check that we have a valid existing dataset*/
        IF NOT VALID-HANDLE(hDataSet) THEN 
            RETURN NO.
        
        /*Link the parent & child table using the related fields*/
        hDataSet:ADD-RELATION (iphParentBuffer,iphChildBuffer,ipcRelatedFields,YES,iplNested).
        RETURN YES.
    END.
    
    /**
    * Empty the current dataset
    * @return TRUE if the dataset was emptied successfully
    */  
    METHOD PUBLIC LOGICAL emptyDataSet():
        RETURN hDataSet:EMPTY-DATASET ().
    END.
    
    /**
    * Return a LONGCHAR XML version of the dataset 
    * @return LONGCHAR XML version of dataset
    */  
    METHOD PUBLIC LONGCHAR dsToXML():
        DEFINE VARIABLE lcReturnXml  AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE lOK         AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cErrorMsg   AS CHARACTER NO-UNDO.    
                       
        lOk = hDataSet:WRITE-XML ("longchar",lcReturnXml,true,?,?,false,false).
        IF NOT lOK THEN DO:
            cErrorMsg = "Unable to fill Dataset from XML file" + ERROR-STATUS:GET-MESSAGE (1).
            handleError(cErrorMsg).
            UNDO, THROW NEW Progress.Lang.AppError(cErrorMsg,1).
        END.
        ELSE 
            RETURN lcReturnXml.          
    END.    
    
    /**
    * Create a dataset from an XML file on disk (No validation)
    * @param ipcXMLFile - The filesystem location of the XML file
    * @return TRUE if successfully read XML file into dataset
    */  
    METHOD PUBLIC LOGICAL fillDataset (ipcXMLFile AS CHARACTER):
        DEFINE VARIABLE lOK         AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cErrorMsg   AS CHARACTER NO-UNDO.
        
        lOK = hDataSet:READ-XML ("file",ipcXMLFile,"empty","",?,?,?) NO-ERROR.
        IF NOT lOk 
        THEN DO: 
            cErrorMsg = "Unable to fill Dataset from XML file" + ERROR-STATUS:GET-MESSAGE (1).
            handleError(cErrorMsg).
            UNDO, THROW NEW Progress.Lang.AppError(cErrorMsg,1).
        END.                
    END.
    /**
    * Create a dataset from a LONGCHAR variable (No validation)
    * @param iplcXML - The LONGCHAR variable representation of the XML
    * @return TRUE if successfully read XML parameter into dataset
    */  
    METHOD PUBLIC LOGICAL fillDataset (iplcXML AS LONGCHAR):
        DEFINE VARIABLE lOK         AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cErrorMsg   AS CHARACTER NO-UNDO.
        
        lOK = hDataSet:READ-XML ("file",iplcXML,"empty","",?,?,?) NO-ERROR.
        IF NOT lOk 
        THEN DO: 
            cErrorMsg = "Unable to fill Dataset from longchar" + ERROR-STATUS:GET-MESSAGE (1).
            handleError(cErrorMsg).
            UNDO, THROW NEW Progress.Lang.AppError(cErrorMsg,1).
        END.                
    END.
    /*-----------------------------end fillDataSet------------------------------*/
    
    /**
    * Create a dataset from a LONGCHAR variable (No validation)
    * @param ipcFile - The location of the file to write
    * @return TRUE if successfully write dataset to file
    */  
    METHOD PUBLIC LOGICAL writeToFile (ipcFile AS CHARACTER):
        DEFINE VARIABLE lOK         AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cErrorMsg   AS CHARACTER NO-UNDO.
        
        lOK = hDataSet:WRITE-XML("file", 
                                 ipcFile, 
                                 TRUE, /*formatted*/ 
                                 ?, /*encoding*/
                                 ?, /*schema location*/ 
                                 FALSE, /*write schema*/ 
                                 FALSE) /*min schema */ NO-ERROR.
        IF NOT lOk 
        THEN DO: 
            cErrorMsg = "Unable to write Dataset to file." + ERROR-STATUS:GET-MESSAGE (1).
            handleError(cErrorMsg).
            UNDO, THROW NEW Progress.Lang.AppError(cErrorMsg,1).
        END.                
    END.
    /*-----------------------------end fillDataSet------------------------------*/
    
    /**
    * Log an error using the error handler in the service framework
    * @param ipcErrorText - error description
    */  
    METHOD PUBLIC VOID handleError(ipcErrorText AS CHARACTER):        
        /*Log the error*/
        MESSAGE ipcErrorText
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    
    /**
    * destroy the current dataset
    */  
    METHOD PUBLIC LOGICAL destroyDataSet():
        IF VALID-HANDLE(hDataSet) THEN 
            DELETE OBJECT hDataSet.
    END.   

    
END CLASS.
