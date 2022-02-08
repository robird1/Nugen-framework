/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LineInitialFormat
 * Brief: The class is the format in order to initialize the log content.
 *
 * Create Date: 07/15/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LineInitialFormat.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

import java.io.File;
import java.io.RandomAccessFile;

public class LineInitialFormat implements LogFileFormat
{
    
    /**
     * The initial value of an integer value.
     */
    private static final int INT_INITIAL_VALUE = -1;

    /**
     * The initial value of a long value.
     */
    private static final long LONG_INITIAL_VALUE = -1L;

    /**
     * Set the position of a log in the log file.
     * 
     * @param logFile The File object of the log file.
     *       Range: Valid File object
     *       Unit: File
     *       Scaling: 1
     *       
     * @param randomAccessFile The RandomAccessFile object to access the log file.
     *       Range: Valid RandomAccessFile object
     *       Unit: RandomAccessFile
     *       Scaling: 1
     *       
     * @param endTag The SafetyString object of the log end tag.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @param postion The position number in HeadPosition.
     *       Range: 0 ~ (2 ^ 31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return The default position of the file pointer.
     *       Range: -1
     *       Unit: long
     *       Scaling: 1
     */
    @Override
    public final long setStartPosition(final File logFile, final RandomAccessFile randomAccessFile,
            final SafetyString endTag, final int... postion)
    {
        // Return initialized value
        return LONG_INITIAL_VALUE;
    }

    /**
     * Transfer a log into correct format.
     * The length of text is 1500 characters and left alignment.
     * 
     * @param log The content of a log.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The formatted content of a log.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    @Override
    public final SafetyString getContextwithFormat(final SafetyString log)
    {
        final StringBuffer stringBuffer = new StringBuffer();
        SafetyString contentLog = null;
        int contentLogCRC = INT_INITIAL_VALUE;

        // Get the formatted string
        stringBuffer.append(String.format(LogFileConstants.HEAD_FORMAT,
                log.getString()));

        // Generate the CRC16 of formatted string
        contentLogCRC = CRCTool.generateCRC16(stringBuffer.toString()
                .getBytes());
        
        // Transfer to a safety string
        contentLog = new SafetyString(stringBuffer.toString(), contentLogCRC);

        // Removes the characters in stringBuffer
        stringBuffer.delete(0, stringBuffer.length());

        // Return the result
        return contentLog;
    }

    /**
     * Get an end tag string with correct format.
     * 
     * @param endTag The SafetyString of end tag.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
      * @param newLine The SafetyString of auxiliary text.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    @Override
    public final SafetyString getEndwithFormat(final SafetyString endTag,
            final SafetyString... newLine)
    {
        final StringBuffer stringBuffer = new StringBuffer();
        SafetyString endLog = null;
        int endLogCRC = INT_INITIAL_VALUE;

        // Is newLine equal to null?
        if (newLine != null)
        {
            // Is the length of newLine larger than 0?
            if (newLine.length > 0)
            {
                // Get the newLine
                stringBuffer.append(newLine[0].getString());
                
                // Append the formatted string
                stringBuffer.append(String.format(LogFileConstants.LOG_FORMAT,
                        endTag.getString()));

                // Generate the CRC16 of the newLine and formatted string 
                endLogCRC = CRCTool.generateCRC16(stringBuffer.toString()
                        .getBytes());
                
                // Transfer to a safety string
                endLog = new SafetyString(stringBuffer.toString(), endLogCRC);
            }
        }

        // Removes the characters in stringBuffer
        stringBuffer.delete(0, stringBuffer.length());

        // Return the result
        return endLog;
    }
    
}
