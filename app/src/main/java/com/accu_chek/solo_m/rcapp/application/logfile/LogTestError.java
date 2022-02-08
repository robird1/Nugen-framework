/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogTestError
 * Brief: The class is the format to store the test error information
 *
 * Create Date: 08/30/2015
 * $Revision: 19842 $
 * $Author: AdamChen $
 * $Id: LogTestError.java 19842 2015-09-25 09:33:00Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class LogTestError extends LogContent
{
    
    /**
     * The low level error code of an test error event.
     */
    private String mTestErrorCode = null;

    /**
     * The detailed description of an test error event.
     */
    private String mTestErrorDescription = null;

    /**
     * The constructor of LogTestError. Call parent's constructor to get the LogFile
     * instance and initialize the global variables.
     * 
     * @param testErrorCode The low level error code of an test error event.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param testErrorDescription The detailed description of an test error event.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1 
     *       
     * @return None
     */
    public LogTestError(final String testErrorCode, final String testErrorDescription)
    {
        // Get LogFile instance
        super();

        // Initialize mTestErrorCode to testErrorCode
        mTestErrorCode = testErrorCode;
        
        // Initialize mTestErrorDescription to testErrorDescription
        mTestErrorDescription = testErrorDescription;
    }

    /**
     * Write the test error information into the log file.
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
        // Get the log data which contains the test error information
        final LogData logData = getLogString();
        SafetyBoolean isWriteResult = SafetyBoolean.FALSE;
        
        // Get the content of LogData object
        final String content = logData.getContent();

        // Is the content of the log data equal to null?
        if (content != null)
        {
            // Append the log data's content to the log file, and get the writing result
            isWriteResult = logFileWriter.writeSystemLog(logData, SafetyBoolean.TRUE);
        }

        // Return the writing result
        return isWriteResult;
    }

    /**
     * Get the LogData object which contains the test error information.
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
        
        // Get the current system time
        final long currentTime = System.currentTimeMillis();

        // Add the new tag TESTERROR to the XmlSerializer
        xmlString.newTag(LogFileConstants.TAG_TEST_ERROR);
        
        // Add the attribute Time with the current system time to the XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile()
                .getTimeString(currentTime));
        
        // Add the attribute "ErrorCode" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_ERROR_CODE, mTestErrorCode);
        
        // Add the attribute "ErrorDescription" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_ERROR_DESCRIPTION, mTestErrorDescription);
        
        // End the recent tag into XmlSerializer
        xmlString.endTag(LogFileConstants.TAG_TEST_ERROR);
        
        // Set the content of log data to the XML string
        logData.setContent(xmlString.getString());

        // Return the LogData object
        return logData;
    }
    
}
