/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogAppInfo
 * Brief: The class is used to store the Log contnet of the RC application information.
 *
 * Create Date: 08/27/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogAppInfo.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import android.content.Context;
import android.content.pm.PackageManager.NameNotFoundException;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.logfile.LogFile.HeadPosition;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;

public class LogAppInfo extends LogContent
{
    
    /**
     * Use the package to get the RC Launcher information.
     */
    private static final String RCLAUNCHER_INFO_PACKAGE = "com.accu_chek.solo_m.rcapp.presentation";     
    
    /**
     * The context of the LogTransmitterInfo class.
     */
    private Context mContext = null;
    
    /**
     * The global variable is used to log the errors of the LogAppInfo class.
     */
    private LogLogFileError mLogFileError = new LogLogFileError();
    
    /**
     * The constructor of LogAppInfo. Call parent's constructor to get the LogFile
     * instance and initialize the global variable.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @return None
     */
    public LogAppInfo(final Context context)
    {
        // Call parent's constructor
        super(context);
        
        // Set mContext to context
        mContext = context;        
    }

    /**
     * Write the RC application information into the log file.
     * 
     * The log is written on the position,
     * HeadPosition.APPLICATION_INFORMATION_POSITION.
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
        // Get the log data
        final LogData logData = getLogString();
        SafetyBoolean isWriteResult = SafetyBoolean.FALSE;
        String content = null;

        // Get the content of log data
        content = logData.getContent();

        // Is the content of log data equal to null?
        if (content != null)
        {
            // Write the log data into log file
            isWriteResult = logFileWriter.writeSystemLog(logData, SafetyBoolean.FALSE,
                    HeadPosition.APPLICATION_INFORMATION_POSITION);        
        }

        // Return the writing result
        return isWriteResult;
    }

    /**
     * Get the LogData object which contains the RC Application information.
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

        try
        {
            final LogFileXmlElement xmlString = new LogFileXmlElement();
            SafetyString rfSwVersion = new SafetyString();
            SafetyString bgmSwVersion = new SafetyString();
            String rcLauncherVersion = null;       
            
            // Get the current time
            final long currentTime = System.currentTimeMillis();

            // Add the new tag "RCAPP" into XmlSerializer
            xmlString.newTag(LogFileConstants.TAG_APP);
            
            // Add the attribute "Time" into XmlSerializer
            xmlString.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile()
                    .getTimeString(currentTime));
            
            // Get the RCLauncher version number
            rcLauncherVersion = mContext.getPackageManager().getPackageInfo(
                    RCLAUNCHER_INFO_PACKAGE, 0).versionName;
            
            // Add the attribute MainProcessorFWVersion into XmlSerializer
            xmlString.addTagAttribute(LogFileConstants.ATTR_MAINPROCESSOR_VER,
                    rcLauncherVersion);
            
            // Get the RF SW version
            rfSwVersion = NugenGeneralModel.getString(mContext, 
                   NugenFrameworkConstants.BTLEConstants.KEY_BLEVERSION);
            
            // Add the attribute RFSWVersion into XmlSerializer
            xmlString.addTagAttribute(LogFileConstants.ATTR_RF_SW_VER,
                    rfSwVersion.getString());
            
            // Get BGM SW version
            bgmSwVersion = NugenGeneralModel.getString(mContext, 
                    NugenFrameworkConstants.BGMConstants.KEY_BG_VERSION);

            // Add the attribute BGMSWVerion into XmlSerializer
            xmlString.addTagAttribute(LogFileConstants.ATTR_BGM_SW_VER,
                    bgmSwVersion.getString());
            
            // End the recent tag into XmlSerializer
            xmlString.endTag(LogFileConstants.TAG_APP);
            
            // Set the content of log data to the XML string
            logData.setContent(xmlString.getString());
        }
        catch (NameNotFoundException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        finally
        {
            // Apply to the coding standard
        }

        // Return the log data
        return logData;
    }
    
}
