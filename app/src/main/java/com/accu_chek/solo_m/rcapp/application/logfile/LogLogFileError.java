/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogLogFileError
 * Brief: Log errors when write error log file.
 *
 * Create Date: 10/01/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import android.annotation.SuppressLint;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.TimeZone;

public class LogLogFileError
{
    
    /**
     * The folder path of the log error file which contains the LogFile internal errors.
     */
    private static final String ERRORFILELOG_FOLDER = LogFileConstants.LOG_PATH;

    /**
     * The file name of the log error file which contains the LogFile internal errors.
     */
    private static final String FILE_NAME = "log_error";
    
    /**
     * The time zone of the calendar for the log file.
     */
    private static final String TIME_ZONE_UTC = "UTC";
    
    /**
     * The underscore symbol.
     */ 
    private static final String UNDERSCORE = "_";
    
    /**
     * The string "Write". The definition is used to display the error information.
     */ 
    private static final String STRING_WRITE = "Write ";
    
    /**
     * The string "fail". The definition is used to display the error information.
     */ 
    private static final String STRING_FAIL = " fail!";
    
    
    /**
     * The string ""Time="". The definition is the tag name which is used to display the time of the LogFile internal errors.
     */ 
    private static final String ATTRIBUTE_TIME = "Time=";
    
    /**
     * The blank symbol
     */ 
    private static final String STRING_BLANK = " ";

    /**
     * Log LogFile module internal error.
     * 
     * @param errorDescription The detailed error description of the LogFile module.
     *       Range: Null, Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param exception The occurred exception of the LogFile module. 
     *       Range: Valid Exception object
     *       Unit: Exception
     *       Scaling: 1
     *       
     * @return None
     */
    final <T extends Exception> void write(final String errorDescription,
            final T exception)
    {
        
        final StringBuffer stringBuffer = new StringBuffer();
        final long currentTime = System.currentTimeMillis();
        final SimpleDateFormat timeFormat = new SimpleDateFormat(LogFileConstants.TIME_FORMAT);
        final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW47305);

        // Set the time zone of the time format to UTC
        timeFormat.setTimeZone(TimeZone.getTimeZone(TIME_ZONE_UTC));
        
        // Append the current time to the string buffer
        stringBuffer.append(ATTRIBUTE_TIME + '"' + timeFormat.format(currentTime) + '"');
        stringBuffer.append(STRING_BLANK);

        // Is description equal to null?
        if (errorDescription != null)
        {
            // Append the description of the log file's error to the string buffer
            stringBuffer.append(LogFileConstants.ATTR_ANDROID_EXCEPTION_DESCRIPTION);
            stringBuffer.append('=');
            stringBuffer.append('"');
            stringBuffer.append(STRING_WRITE);
            stringBuffer.append(errorDescription);
            stringBuffer.append(STRING_FAIL);
            stringBuffer.append('"');
            stringBuffer.append(STRING_BLANK);
        }
        
        if (exception != null)
        {
            final Writer stringWriter = new StringWriter();
            final PrintWriter printWriter = new PrintWriter(stringWriter);
            final String exceptionReason = exception.getMessage();

            // Write mException to print writer 
            exception.printStackTrace(printWriter);

            //Is the message of mException equal to null?
            if (exceptionReason != null)
            {
                // Append the exception message to the string buffer
                stringBuffer.append(LogFileConstants.ATTR_ANDROID_EXCEPTION);
                stringBuffer.append('=');
                stringBuffer.append('"');
                stringBuffer.append(exceptionReason);
                stringBuffer.append('"');
                stringBuffer.append(STRING_BLANK);
            }
            
            // Append the stack trace of the exception to the string buffer
            stringBuffer.append(LogFileConstants.ATTR_ANDROID_EXCEPTION_CODE);
            stringBuffer.append('=');
            stringBuffer.append('"');
            stringBuffer.append(stringWriter.toString());
            stringBuffer.append('"');
        }

        // Write the error into a file
        writeLogFileErrorToFile(stringBuffer.toString());
        
        // Clear string buffer
        stringBuffer.delete(0, stringBuffer.length());
        
        // Show EMWR
        NotifyProxy.showEMWR(emwrMessage);
    }   
    
    /**
     * The function creates a log error file, and uses the file number and the current time as
     * the name of the log error file.
     * 
     * FileOutputStream object in Android API is used to write the error, and
     * there is no FileLock control because a new file is created when a new log
     * is written.
     * 
     * @param errorInfo The information of the LogFile error.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @return None
     */
    @SuppressLint("SimpleDateFormat")
    private void writeLogFileErrorToFile(final String errorInfo)
    {
        String filepath = null;
        long fileindex = 0;
        FileOutputStream fileOutputStream = null;
        final SimpleDateFormat timeFormat = new SimpleDateFormat(LogFileConstants.LOG_ERROR_TIME);

        // Set the time zone of the time format to UTC
        timeFormat.setTimeZone(TimeZone.getTimeZone(TIME_ZONE_UTC));

        //Get the current system time
        fileindex = System.currentTimeMillis();
        
        //Use the current system time to set the file path of the log error file
        filepath = ERRORFILELOG_FOLDER.concat(File.separator).concat(FILE_NAME)
                .concat(UNDERSCORE).concat(timeFormat.format(fileindex)).concat(UNDERSCORE)
                .concat(String.valueOf(fileindex));

        // Is errorInfo equal to null?
        if (errorInfo != null)
        {
            final StringBuffer stringBuffer = new StringBuffer();

            try
            {
                fileOutputStream = new FileOutputStream(filepath, true);

                // Append the content of the log to the StringBuffer instance
                stringBuffer.append(String.format(Locale.US, LogFileConstants.LOG_FORMAT,
                        errorInfo));
                
                // Append a new line to the StringBuffer instance
                stringBuffer.append(LogFileConstants.NEW_LINE_CHARACTORS);

                // Write the content to the log error file
                fileOutputStream.write(stringBuffer.toString().getBytes());

                // Clear the string buffer
                stringBuffer.delete(0, stringBuffer.length());
            }
            catch (FileNotFoundException exception)
            {
                // Print the stack trace of the exception
                exception.printStackTrace();
            }
            catch (IOException exception)
            {
                // Print the stack trace of the exception
                exception.printStackTrace();
            }
            catch (Exception exception)
            {
                // Print the stack trace of the exception
                exception.printStackTrace();
            }
            finally
            {
                // Clear the string buffer
                stringBuffer.delete(0, stringBuffer.length());

                // Is the FileOutputStream instance equal to null?
                if (fileOutputStream != null)
                {
                    try
                    {
                        // Close the FileOutputStream instance
                        fileOutputStream.close();
                    }
                    catch (IOException exception)
                    {
                        // Print the stack trace of the exception
                        exception.printStackTrace();
                    }
                    finally
                    {
                        // Apply to the coding standard
                    }
                }
            }
        }
    }
    
}
