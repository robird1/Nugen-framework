/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogEmwr
 * Brief: The class is the format to store the EMWR information.
 *
 * Create Date: 08/31/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogEmwr.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class LogEmwr extends LogContent
{
    
    /**
     * The low level error code of an EMWR event.
     */
    private String mErrorCode = null;

    /**
     * The detailed description of an EMWR event.
     */
    private String mErrorDescription = null;

    /**
     * The constructor of LogEmwr. Call parent's constructor to get the LogFile
     * instance and initialize the global variables.
     * 
     * @param errorCode The low level error code of an EMWR event.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param errorDescription The detailed description of an EMWR event.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @return None
     */
    public LogEmwr(final String errorCode, final String errorDescription)
    {
        // Call parent's constructor
        super();

        // Initialize mErrorCode to errorCode
        mErrorCode = errorCode;
        
        // Initialize mErrorDescription to errorDescription
        mErrorDescription = errorDescription;
    }

    /**
     * Write the EMWR information into the log file.
     * 
     * @param logFileWriter Use the LogFileWriter object to write a log.
     *       Range: Valid LogFileWriter object
     *       Unit: LogFileWriter
     *       Scaling: 1
     *       
     * @return The result of the writing process. If the return value is equal to 
     *       SafetyBoolean.TRUE, it means the writing process is successful.
     *       If the return value is equal to SafetyBoolean.FALSE, it means the
     *       writing process is failed.
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */
    @Override
    public final SafetyBoolean write(final LogFileWriter logFileWriter)
    {
        // Get the log data which contains the EMWR information
        final LogData logData = getLogString();
        SafetyBoolean isWriteResult = SafetyBoolean.FALSE;
        
        // Get the content of LogData object
        final String content = logData.getContent();

        // Is content equal to null?
        if (content != null)
        {
            // Append log data to log file
            isWriteResult = logFileWriter.writeSystemLog(logData, SafetyBoolean.TRUE);
        }

        // Return the writing result
        return isWriteResult;
    }

    /**
     * Get the LogData object which contains the EMWR information.
     * 
     * @param None
     * 
     * @return The result
     *       Range: Valid LogData object
     *       Unit: LogData
     *       Scaling: 1
     */
    @Override
    final LogData getLogString()
    {
        final LogData logData = new LogData();
        final LogFileXmlElement xmlString = new LogFileXmlElement();
        final long currentTime = System.currentTimeMillis();

        // Add the new tag "EMWR" into XmlSerializer
        xmlString.newTag(LogFileConstants.TAG_EMWR);
        
        // Add the attribute "Time" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile()
                .getTimeString(currentTime));
        
        // Add the attribute ErrorCode with mTestErrorCode to the XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_ERROR_CODE, mErrorCode);
        
        // Add the attribute ErrorDescription with mTestErrorDescription to the XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_ERROR_DESCRIPTION, mErrorDescription);
        
        // End the recent tag to the XmlSerializer
        xmlString.endTag(LogFileConstants.TAG_EMWR);
        
        // Get the string of the XmlSerializer, and set the content of log data to it
        logData.setContent(xmlString.getString());

        // Return the LogData object
        return logData;
    }
    
}
