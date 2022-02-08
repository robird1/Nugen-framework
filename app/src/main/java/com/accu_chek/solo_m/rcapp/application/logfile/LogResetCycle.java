/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogResetCycle
 * Brief: The class is the format to store the count of the HW reset cycles
 *
 * Create Date: 08/31/2015
 * $Revision: 19842 $
 * $Author: AdamChen $
 * $Id: LogResetCycle.java 19842 2015-09-25 09:33:00Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class LogResetCycle extends LogContent
{
    
    /**
     * The initial value of an integer value.
     */
    private static final int INT_FIELD_UNUSED = -1;    
    
    /**
     * The count of HW reset cycles.
     */
    private int mCount = INT_FIELD_UNUSED;

    /**
     * The constructor of LogResetCycle. Call parent's constructor to get the LogFile
     * instance and initialize the global variables.
     * 
     * @param count The count of HW reset cycles.
     *       Range: -2^31 - (2^31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return None
     */
    public LogResetCycle(final int count)
    {
        // Get LogFile instance
        super();
        
        // Initialize mCount to count
        mCount = count;
    }

    /**
     * Write the count of HW reset cycles into the log file.
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
        // Get the log data which contains the count of hardware reset cycles
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
     * Get the LogData object which contains the count of HW reset cycles.
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
        
        // Add the new tag RESETCYCLE to the XmlSerializer
        xmlString.newTag(LogFileConstants.TAG_RESET_CYCLE);
        
        // Add the attribute Time with the current system time to the XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile().getTimeString(currentTime));
        
        // Add the attribute ResetCount with mCount to the XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_RESET_COUNT, mCount);
        
        // End the recent tag to the XmlSerializer
        xmlString.endTag(LogFileConstants.TAG_RESET_CYCLE);
        
        // Get the string of the XmlSerializer, and set the content of log data to it
        logData.setContent(xmlString.getString());

        // Return the LogData object
        return logData;
    }
    
}
