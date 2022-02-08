/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogCgTest
 * Brief: The class is used to store the log content of the cG test information.
 *
 * Create Date: 08/31/2015
 * $Revision: 19842 $
 * $Author: AdamChen $
 * $Id: LogCgTest.java 19842 2015-09-25 09:33:00Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class LogCgTest extends LogContent
{
    
    /**
     * The initialize value of an integer value.
     */
    private static final int INT_FIELD_UNUSED = -1;    
    
    /**
     * The count of cG test.
     */
    private int mCount = INT_FIELD_UNUSED;
    
    /**
     * The value of cG test.
     */
    private int mValue = INT_FIELD_UNUSED;

    /**
     * The result of cG test.
     */
    private String mResult = null;

    /**
     * The constructor of LogCgTest. Call parent's constructor to get the LogFile
     * instance and initialize the global variables.
     * 
     * @param count The count of cG test.
     *       Range: -2^31 - (2^31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @param value The value of cG test.
     *       Range: -2^31 - (2^31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @param result The result of cG test.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @return None
     */
    public LogCgTest(final int count, final int value, final String result)
    {
        // Call parent's constructor
        super();
        
        // Initialize mCount to count
        mCount = count;

        // Initialize mValue to value
        mValue = value;
        
        // Initialize mResult to result
        mResult = result;
    }

    /**
     * Write the cG test information into the log file.
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
        // Get the log data which contains the cG test information
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
     * Get the LogData object which contains the cG test information.
     * 
     * @param None
     * 
     * @return The cG test result with XML format.
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
        
        // Add the new tag "CGRECORD" into XmlSerializer
        xmlString.newTag(LogFileConstants.TAG_CG_RECORD);
        
        // Add the attribute "Time" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile()
                .getTimeString(currentTime));
        
        // Add the attribute "cGValue" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_CG_COUNT, mCount);
        
        // Add the attribute "cGValue" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_CG_VALUE, mValue);
        
        // Add the attribute "cGResult" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_CG_RESULT, mResult);
        
        // End the recent tag into XmlSerializer
        xmlString.endTag(LogFileConstants.TAG_CG_RECORD);
        
        // Set the content of log data to the XML string
        logData.setContent(xmlString.getString());

        // Return the LogData object
        return logData;
    }
    
}
