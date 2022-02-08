/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogFile
 * Brief: The class is the interface to provide log file functionality.
 *
 * Create Date: 07/15/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogFile.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.TimeZone;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

final public class LogFile
{
    
    /**
     * The time zone of the calendar for the log file.
     */
    private static final String TIME_ZONE_UTC = "UTC";
    
    /**
     * The thread number in thread pool.
     */
    private static final int THREAD_NUMBER = 1;
    
    /**
     * The LogFile instance in this class.
     */
    private static LogFile mLogFile = null;

    /**
     * The switch of log file function.
     */
    private static LogFileSwitch mLogFileSwitch = LogFileSwitch.ENABLE;   

    /**
     * The time showing format of the log file.
     */
    private SimpleDateFormat mTimeFormat = new SimpleDateFormat(LogFileConstants.TIME_FORMAT);

    /**
     * The LogFileWriter instance of LogFile.
     */
    private LogFileWriter mLogFileWriter = null;

    /**
     * The context of the LogFile class.
     */
    private Context mContext = null;

    /**
     * The synchronized lock instance of LogFile.
     */
    private final Object mLock = new Object(); 
    
    /**
     * The constructor of LogFile. Initialize the global variables used in LogFile.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid LogFile object
     *       Unit: LogFile
     *       Scaling: 1
     */
    private LogFile(final Context context)
    {
        // Set the time zone of mTimeFormat to UTC
        mTimeFormat.setTimeZone(TimeZone.getTimeZone(TIME_ZONE_UTC));

        // Set mContext to context
        mContext = context.getApplicationContext();
        
        /*
         * Create a singleton LogFileWriter, and set mLogFileWriter to the
         * singleton LogFileWriter
         */
        mLogFileWriter = LogFileWriter.getInstance(mContext);
    }

    /**
     * Log information into the log file.
     * The function will post a work into thread to write a log of LogContent.
     * 
     * @param logObject The LogContent object which contains log information.
     *       Range: Null, valid LogContent object
     *       Unit: LogContent
     *       Scaling: 1
     *       
     * @return None
     * 
     */
    public void log(final LogContent logContent)
    {
        final byte byteOfIsTrueBoolean = SafetyBoolean.TRUE.getByte();
        final byte byteOfIsLogEnable = isLogEnable().getByte();
        
        // Is the log function enabled? And Is logContent equal to null?
        if ((byteOfIsTrueBoolean == byteOfIsLogEnable) && (null != logContent))
        {
            // Creates a thread pool that reuses a 1 thread
            final ExecutorService executor = Executors.newFixedThreadPool(THREAD_NUMBER);
            
            /*
             *  Execute a new thread to create the log file and write default 
             *  information into the log file
             */
            executor.execute(new Runnable()
            {
                @Override
                public void run()
                {
                    // write information into log file
                    writeContentIntoFile(logContent);
                }
            });
            
            // Shutdown executor service
            executor.shutdown();
        }
    }
    
    /**
     * Get the time/date string with time/date format which is set in the LogFile.
     * 
     * @param timeStamp The time stamp.
     *       Range: 0 ~ (2 ^ 64)-1
     *       Unit: long
     *       Scaling: 1
     *       
     * @return The formatted string of the current system time.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     */
    String getTimeString(final long timeStamp)
    {
        // Set the time zone of mTimeFormat to UTC
        mTimeFormat.setTimeZone(TimeZone.getTimeZone(TIME_ZONE_UTC));

        // Return the formatted time stamp string
        return mTimeFormat.format(timeStamp);
    }
    
    /**
     * Write a log into the log file.
     * 
     * @param logObject The LogContent object which contains log information.
     *       Range: Valid LogContent object
     *       Unit: LogContent
     *       Scaling: 1
     *       
     * @return None
     * 
     */
    private void writeContentIntoFile(final LogContent logContent)
    {
        synchronized (mLock)
        {
            final File logFile = mLogFileWriter.getLogFile();

            // Is logFile equal to null?
            if (logFile == null)
            {
                // Set initial flag of mLogFileWriter
                mLogFileWriter.setInitialFlag(SafetyBoolean.FALSE);

                /*
                 * Check whether the error log file is created.
                 * And create new file if it does not exist.
                 */
                mLogFileWriter.initFromDic();
            }
            else
            {
                final boolean isExistLogFile = logFile.exists();

                // Does the log file exist? 
                if (isExistLogFile == false)
                {
                    // Set initial flag of mLogFileWriter
                    mLogFileWriter
                            .setInitialFlag(SafetyBoolean.FALSE);

                    /*
                     * Check whether the error log file is created.
                     * And create new file if it does not exist.
                     */
                    mLogFileWriter.initFromDic();
                }
            }

            // Write log into log file with mLogFileWriter
            logContent.write(mLogFileWriter);
        }        
    }
    
    /**
     * The singleton for LogFile. Create and get the LogFile instance.
     * 
     * @param context The context of caller.
     *       Range: valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid object
     *       Unit: LogFile
     *       Scaling: 1
     */
    public static synchronized LogFile getInstance(final Context context)
    {
        // Is mLogFile equal to null?
        if (mLogFile == null)
        {
            // Create a new log file with context
            mLogFile = new LogFile(context);
        }
            
        // Return the log file
        return mLogFile;
    }

    /**
     * The singleton for LogFile. Get the LogFile instance.
     * 
     * @param None
     * 
     * @return The result
     *       Range: Valid object
     *       Unit: LogFile
     *       Scaling: 1
     */
    public static synchronized LogFile getInstance()
    {
        // Return the log file
        return mLogFile;
    }
    
    /**
     * Check whether the log file function is enabled.
     * 
     * @param None
     * 
     * @return The result
     *       Range: SafetyBoolean.TRUE(OK), SafetyBoolean.FALSE(Failed)
     *       Unit: SafetyBoolean
     *       Scaling: 1
     * 
     */
    public static SafetyBoolean isLogEnable()
    {
        SafetyBoolean isLogEnable = SafetyBoolean.TRUE;
        final byte byteOfIsFALSEBoolean = SafetyBoolean.FALSE.getByte();
        final byte byteOfSwitchSetting = mLogFileSwitch.mSwitchSetting.getByte();

        // Is mSwitchsetting equal to SafetyBoolean.FALSE?
        if (byteOfSwitchSetting == byteOfIsFALSEBoolean)
        {
            // Set the result to SafetyBoolean.FALSE
            isLogEnable = SafetyBoolean.FALSE;
        }

        // Return the result
        return isLogEnable;
    }    

    /**
     * Check whether the integer index of head position is legal.
     * 
     * @param position The position in HeadPosition.
     *       Range: 0 ~ (2 ^ 32)-1
     *       Unit: int
     *       Scaling: 1
     * 
     * @return The result
     *       Range: SafetyBoolean.TRUE(OK), SafetyBoolean.FALSE(Failed)
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */
    static SafetyBoolean checkHeadPositionRange(final int position)
    {
        SafetyBoolean isCorrectValue = SafetyBoolean.FALSE;

        switch (position)
        {
        case HeadPosition.APPLICATION_INFORMATION_POSITION :
            /* Falls through */
        case HeadPosition.DEVICE_INFORMATION_POSITION :
            // Set the result to SafetyBoolean.TRUE
            isCorrectValue = SafetyBoolean.TRUE;
            break;

        default :
            // Set the result to SafetyBoolean.FALSE
            isCorrectValue = SafetyBoolean.FALSE;
            break;
        }

        // Return the result
        return isCorrectValue;
    }
    
    /**
     * Switch enum of LogFile
     */
    public enum LogFileSwitch
    {
        // Enable the log file function
        ENABLE(SafetyBoolean.TRUE),
        
        // Disable the log file function
        DISABLE(SafetyBoolean.FALSE);

        
        /**
         * The switch of log file function.
         */
        private final SafetyBoolean mSwitchSetting;

        
        /**
         * The constructor of LogFileSwitch. Initialize the variable mSwitchsetting.
         * 
         * @param inSwitchsetting The switch setting of the log file function.
         *       Range: Valid SafetyBoolean object
         *       Unit: SafetyBoolean
         *       Scaling: 1
         * 
         * @return None
         */
        private LogFileSwitch(final SafetyBoolean inSwitchSetting)
        {
            mSwitchSetting = inSwitchSetting;
        }
    }
    
    
    /**
     * The log's start position in saved space
     */
    interface HeadPosition
    {
        // The position of application information at log file
        int APPLICATION_INFORMATION_POSITION = 6008;
        
        
        // The position of device information at log file
        int DEVICE_INFORMATION_POSITION = 7510;
    }
    
}
