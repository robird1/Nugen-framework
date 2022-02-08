/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogException
 * Brief: The class is the format to store the exception.
 *
 * Create Date: 08/26/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogException.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;

public class LogException extends LogContent
{
    
    /**
     * The description of an exception.
     */
    private String mDescription = null;

    /**
     * The Exception object of LogException.
     */
    private Exception mException = null;

    /**
     * The constructor of LogException. Call parent's constructor to get the LogFile
     * instance and initialize the global variables.
     * 
     * @param description The description of an exception.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param exception The Exception object that occurred during the execution of programs.
     *       Range: Valid Exception object
     *       Unit: Exception
     *       Scaling: 1
     *       
     * @return None
     */
    public LogException(final String description, final Exception exception)
    {
        // Call parent's constructor
        super();

        // Initialize mDescription to description
        mDescription = description;
        
        // Initialize mException to exception
        mException = exception;
    }

    /**
     * Write the exception information into the log file.
     * 
     * @param writer Use the LogFileWriter object to write a log.
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
        // Get the log data which contains the exception information
        final LogData logData = getLogString();

        SafetyBoolean isWriteResultOK = SafetyBoolean.FALSE;

        // Get the content of LogData object
        final String content = logData.getContent();

        //Is the content of LogData object equal to null?
        if (content != null)
        {
            // Write the log data into log file
            isWriteResultOK = logFileWriter.writeSystemLog(logData, SafetyBoolean.TRUE);
        }

        // Return the writing result
        return isWriteResultOK;
    }

    /**
     * Get the LogData object which contains the exception information.
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
        // Get the current time
        final long currentTime = System.currentTimeMillis();
        final LogData logData = new LogData();
        final LogFileXmlElement xmlString = new LogFileXmlElement();

        // Add the new tag "EXCEPTION" into XmlSerializer
        xmlString.newTag(LogFileConstants.TAG_EXCEPTION);

        // Add the attribute "Time" into XmlSerializer
        xmlString.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile()
                .getTimeString(currentTime));

        // Is mDescription equal to null?
        if (mDescription != null)
        {
            // Add the attribute "AndroidExceptionDescription" into XmlSerializer
            xmlString.addTagAttribute(
                    LogFileConstants.ATTR_ANDROID_EXCEPTION_DESCRIPTION,
                    mDescription);
        }

        // Is mException equal to null?
        if (mException != null)
        {
            final Writer stringWriter = new StringWriter();
            final PrintWriter printWriter = new PrintWriter(stringWriter);
            final String exceptionReason = mException.getMessage();

            // Write mException to print writer 
            mException.printStackTrace(printWriter);

            //Is the message of mException equal to null?
            if (exceptionReason != null)
            {
                // Add the attribute "AndroidExceptionEvent" into XmlSerializer
                xmlString.addTagAttribute(
                        LogFileConstants.ATTR_ANDROID_EXCEPTION,
                        mException.getMessage());
            }

            // Add the attribute "AndroidExceptionCode" into XmlSerializer
            xmlString.addTagAttribute(
                    LogFileConstants.ATTR_ANDROID_EXCEPTION_CODE,
                    stringWriter.toString());
        }

        // End the recent tag into XmlSerializer
        xmlString.endTag(LogFileConstants.TAG_EXCEPTION);
        
        // Set the content of log data to the XML string
        logData.setContent(xmlString.getString());

        // Return the LogData object
        return logData;
    }
    
}
