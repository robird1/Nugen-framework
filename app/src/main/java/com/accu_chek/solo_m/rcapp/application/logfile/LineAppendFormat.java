/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LineAppendFormat
 * Brief: The class is the format in order to append the log content.
 *
 * Create Date: 07/15/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LineAppendFormat.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

public class LineAppendFormat implements LogFileFormat
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
     * The global variable is used to log the errors of the LineAppendFormat class.
     */
    private LogLogFileError mLogFileError = new LogLogFileError();

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
     * @return The result
     *       Range: 0 ~ (2 ^ 63)-1
     *       Unit: long
     *       Scaling: 1
     */
    @Override
    public final long setStartPosition(final File logFile, final RandomAccessFile randomAccessFile,
            final SafetyString endTag, final int... postion)
    {
        final long endtagByteLength = endTag.getString().getBytes().length;
        final long fileLength = logFile.length();
        long currentPosition = LONG_INITIAL_VALUE;

        try
        {
            /* 
             * Is the length of log file larger than the encrypted end tag of
             * "Log" with CRC?
             */
            if (fileLength > endtagByteLength)
            {
                // Calculate the start position
                final long startPosition = fileLength
                        - String.format(LogFileConstants.LOG_FORMAT,
                                endTag.getString()).getBytes().length;

                // Sets the length of randomAccessFile to newLength
                randomAccessFile.setLength(startPosition);
            }

            // Moves randomAccessFile's file pointer to a new position
            randomAccessFile.seek(logFile.length());
            
            // Get the current position within randomAccessFile
            currentPosition = randomAccessFile.getFilePointer();
        }
        // Does IOException occur?
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(endTag.getString(), exception);
        }
        finally
        {
            // Apply to the coding standard
        }

        // Return the result
        return currentPosition;
    }

    /**
     * Transfer a log into correct format.
     * 
     * @param log The SafetyString of a log.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    @Override
    public final SafetyString getContextwithFormat(final SafetyString log)
    {
        final StringBuffer content = new StringBuffer();
        SafetyString contentlog = null;
        int contentLogCRC = INT_INITIAL_VALUE;

        // Get the formatted string
        content.append(String.format(LogFileConstants.LOG_FORMAT, 
                log.getString()));

        // Generate the CRC16 of formatted string
        contentLogCRC = CRCTool.generateCRC16(content.toString().getBytes());
        
        // Transfer to a safety string
        contentlog = new SafetyString(content.toString(), contentLogCRC);

        // Removes the characters in stringBuffer
        content.delete(0, content.length());

        // Return the result
        return contentlog;
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
