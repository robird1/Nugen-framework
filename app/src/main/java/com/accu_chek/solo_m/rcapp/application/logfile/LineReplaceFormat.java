/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LineReplaceFormat
 * Brief: The class is the format in order to replace the log content.
 *
 * Create Date: 07/15/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LineReplaceFormat.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

public class LineReplaceFormat implements LogFileFormat
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
     * The string of "The position of the file pointer is incorrect.". 
     * The definition is used to describe the error position information.
     */
    private static final String STRING_INCORRECT_POSITION = "The position of the file pointer is incorrect.";
    
    /**
     * The global variable is used to log the errors of the LineReplaceFormat class.
     */
    private LogLogFileError mLogFileError = new LogLogFileError();
    
    /**
     * Set the position of a log in the log file. If log file
     * is not empty, set the writing position to
     * position. If the length of the log file is less than
     * position, set the file length to position.
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
            final SafetyString endTag, final int... position)
    {
        long currentPosition = LONG_INITIAL_VALUE;

        try
        {
            // Is position equal to null?
            if (position != null)
            {
                // Is position larger than 0?
                if (position.length > 0)
                {
                    final byte byteOfIsCorrectValue = LogFile
                            .checkHeadPositionRange(position[0]).getByte();
                    final byte byteOfIsTrueBoolean = SafetyBoolean.TRUE.getByte();

                    // Is position correct?
                    if (byteOfIsCorrectValue == byteOfIsTrueBoolean)
                    {
                        final long lengthOfLogFile = logFile.length();

                        // Is position larger than the length of log file?
                        if (position[0] > lengthOfLogFile)
                        {
                            // Set the length of randomAccessFile to position
                            randomAccessFile.setLength(position[0]);
                        }

                        /* 
                         * Moves randomAccessFile's file pointer to 
                         * a new position
                         */
                        randomAccessFile.seek(position[0]);

                        // Get the current position of randomAccessFile
                        currentPosition = randomAccessFile.getFilePointer();
                    }
                    else
                    {
                        // Write the incorrect position error into a file
                        final Exception exception = new Exception(STRING_INCORRECT_POSITION);
                        mLogFileError.write(endTag.getString(), exception);  
                    }
                }
            }
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

        // Return the start position of RC app log or RC device log
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
     * @return The formatted content of a log.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    @Override
    public final SafetyString getContextwithFormat(final SafetyString log)
    {
        final StringBuffer content = new StringBuffer();
        SafetyString contentLog = null;
        int contentLogCRC = INT_INITIAL_VALUE;

        // Get the formatted string
        content.append(String.format(LogFileConstants.LOG_FORMAT,
                log.getString()));

        // Generate the CRC16 of the formatted string
        contentLogCRC = CRCTool.generateCRC16(content.toString().getBytes());
        
        // Transfer to a safety string
        contentLog = new SafetyString(content.toString(), contentLogCRC);

        // Removes the characters in stringBuffer
        content.delete(0, content.length());

        // Return the content with specified format of RC app log or RC device log
        return contentLog;
    }

    /**
     * Get an end tag string with correct format. 
     * The function does nothing in LineReplaceFormat.
     * 
     * @param endTag The SafetyString of end tag.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
      * @param necessaryChar The SafetyString of auxiliary text.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Null
     *       Unit: SafetyString
     *       Scaling: 1
     */
    @Override
    public final SafetyString getEndwithFormat(final SafetyString endTag,
            final SafetyString... necessaryChar)
    {
        /* 
         * The class is used to replace the RC application log and RC device
         * log. The tow logs are existing logs, the class just needs to know the
         * start position of the logs, the end position is processed by other class.
         * So the function return null. 
         */
        return null;
    }
    
}
