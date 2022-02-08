/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogContent
 * Brief: The class is used to store the log content.
 *
 * Create Date: 08/26/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogContent.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public abstract class LogContent
{
    
    /**
     * The string of "Please initialize LogFile.". It is used to describe the invalid 
     * LogFile error.
     */
    private static final String STRING_INVALID_LOGFILE = "Please initialize LogFile.";
    
    /**
     * The LogFile object of LogContent.
     */
    private LogFile mLogFile = null;
    
    /**
     * The global variable is used to log the errors of the LogContent class.
     */
    private LogLogFileError mLogFileError = new LogLogFileError();

    /**
     * The constructor of LogContent. Initialize mLogFile variable.
     * 
     * If mLogFile can't be initialized, throw RuntimeException.
     * 
     * @param None
     * 
     * @return None
     */
    LogContent()
    {
        // Get LogFile instance
        mLogFile = LogFile.getInstance();

        // Is mLogFile initialized?
        if (mLogFile == null)
        {
            // Write the error of the log file into a file
            final Exception exception = new RuntimeException(STRING_INVALID_LOGFILE);
            mLogFileError.write(null, exception);  
        }
    }

    /**
     * The constructor of LogContent. Initialize mLogFile variable. If
     * LogFile.getInstance() returns null, give LogFile context and create
     * it. If initializing mLogFile fails, throw RuntimeException.
     * 
     * @param context The context of caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @return None
     */
    LogContent(final Context context)
    {
        // Get LogFile instance
        final LogFile logfile = LogFile.getInstance();

        // Is context equal to null and logfile equal to null?
        if ((context != null) && (logfile == null))
        {
            // Create LogFile instance with context
            mLogFile = LogFile.getInstance(context);
        }
        else
        {
            // Get LogFile instance and set the mLogFile to it
            mLogFile = LogFile.getInstance();
        }

        // Is mLogFile equal to null?
        if (mLogFile == null)
        {
            // Write the invalid LogFile error into a file
            final Exception exception = new RuntimeException(STRING_INVALID_LOGFILE);
            mLogFileError.write(null, exception);  
        }
    }

    /**
     * Get LogFile object in LogContent.
     * 
     * @param None
     * 
     * @return The LogFile instance of LogContent class
     *       Range: Valid LogFile object
     *       Unit: LogFile
     *       Scaling: 1
     */
    final LogFile getLogFile()
    {
        // Return mLogFile
        return mLogFile;
    }

    /**
     * The function should be implemented if the child wants to write his own
     * log text.
     * 
     * @param logFileWriter Use the LogFileWriter object to write a log.
     *       Range: Valid LogFileWriter object
     *       Unit: LogFileWriter
     *       Scaling: 1
     *       
     * @return The result
     *       Range: SafetyBoolean.FALSE(failed)
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */
    abstract SafetyBoolean write(final LogFileWriter logFileWriter);

    /**
     * Get the log string.
     * 
     * @param None
     * 
     * @return The result
     *       Range: null
     *       Unit: LogData
     *       Scaling: 1
     */
    abstract LogData getLogString();
    
}
