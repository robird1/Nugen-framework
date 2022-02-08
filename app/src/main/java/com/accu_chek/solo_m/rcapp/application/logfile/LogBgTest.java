/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogBgTest
 * Brief: The class is used to store the log content of the bG test information.
 *
 * Create Date: 08/31/2015
 * $Revision: 19842 $
 * $Author: AdamChen $
 * $Id: LogBgTest.java 19842 2015-09-25 09:33:00Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class LogBgTest extends LogContent
{
    
    /**
     * The initialize value of an integer value.
     */
    private static final int INT_FIELD_UNUSED = -1;    
    
    /**
     * The count of bG test.
     */
    private int mCount = INT_FIELD_UNUSED;
    
    /**
     * The value of bG test.
     */
    private int mValue = INT_FIELD_UNUSED;

    /**
     * The result of bG test.
     */
    private String mResult = null;

    /**
     * The constructor of LogBgTest. Call parent's constructor to get the LogFile
     * instance and initialize the global variables.
     * 
     * @param count The count of the bG test.
     *       Range: -2^31 - (2^31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @param value The value of the bG test.
     *       Range: -2^31 - (2^31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @param result The result of the bG test.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @return None
     */
    public LogBgTest(final int count, final int value, final String result)
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
     * Write the bG test information into the log file.
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
        // Get log data of the bG test
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
     * Get the LogData object which contains the bG test information.
     * 
     * @param None
     * 
     * @return The bG test result with XML format.
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
        
        // Add the new tag "BGRECORD" into XmlSerializer
        xmlString.newTag(LogFileConstants.TAG_BG_RECORD);
        
        // Add the attribute "Time" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile()
                .getTimeString(currentTime));
        
        // Add the attribute "BGCount" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_BG_COUNT, mCount);
        
        // Add the attribute "BGValue" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_BG_VALUE, mValue);
        
        // Add the attribute "BGResult" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_BG_RESULT, mResult);
        
        // End the recent tag into XmlSerializer
        xmlString.endTag(LogFileConstants.TAG_BG_RECORD);
        
        // Set the content of log data to the XML string
        logData.setContent(xmlString.getString());

        // Return the LogData object
        return logData;
    }
    
}
