/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogDeviceInfo
 * Brief: The class is the format to store the RC device information.
 *
 * Create Date: 08/31/2015
 * $Revision: 19842 $
 * $Author: AdamChen $
 * $Id: LogDeviceInfo.java 19842 2015-09-25 09:33:00Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.logfile.LogFile.HeadPosition;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

public class LogDeviceInfo extends LogContent
{
    
    /**
     * The context of the LogDeviceInfo class.
     */
    private Context mContext = null;

    /**
     * The constructor of LogDeviceInfo. Call parent's constructor to get
     * the LogFile instance and initialize variables.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @return None
     */
    public LogDeviceInfo(final Context context)
    {
        // Call parent's constructor
        super(context);

        // Set mContext to context
        mContext = context;
    }

    /**
     * Write the device information into the log file.
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
        final LogData logData = getLogString();
        SafetyBoolean isWriteResultOK = SafetyBoolean.FALSE;
        String content = null;

        // Get the content of log data
        content = logData.getContent();

        // Is the content of log data equal to null?
        if (content != null)
        {
            // Write the log data into log file
            isWriteResultOK = logFileWriter.writeSystemLog(logData, SafetyBoolean.FALSE,
                    HeadPosition.DEVICE_INFORMATION_POSITION);
        }

        // Return the writing result
        return isWriteResultOK;
    }

    /**
     * Get the LogData object which contains the device information.
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
        final LogFileXmlElement xml = new LogFileXmlElement();
        SafetyString serialNumber = new SafetyString();
        SafetyString manufacturerDate = new SafetyString();
        SafetyString hwVersion = new SafetyString();
       
        // Get the current time
        final long currentTime = System.currentTimeMillis();

        // Add the new tag "RC" into XmlSerializer
        xml.newTag(LogFileConstants.TAG_RC);
        
        // Add the attribute "Time" into XmlSerializer
        xml.addTagAttribute(LogFileConstants.ATTR_TIME, getLogFile()
                .getTimeString(currentTime));
        
        // Get serial number
        serialNumber = NugenProductionModel.getString(mContext, 
                NugenFrameworkConstants.ProductionConstants.KEY_METER_SERIAL_NUMBER);
        
        // Add the attribute "SN" into XmlSerializer
        xml.addTagAttribute(LogFileConstants.ATTR_SN, serialNumber.getString());
        
        // Get HW version number
        hwVersion = NugenProductionModel.getString(mContext, 
                NugenFrameworkConstants.ProductionConstants.KEY_HARDWAREVERSION);

        // Add the attribute "HWVERSION" into XmlSerializer
        xml.addTagAttribute(LogFileConstants.ATTR_HW_VER, hwVersion.getString());
        
        // Get manufacture date
        manufacturerDate = NugenProductionModel.getString(mContext, 
                NugenFrameworkConstants.ProductionConstants.KEY_DATE_DEFAULT);
        
        // Add the attribute "ManufacturerDate" into XmlSerializer
        xml.addTagAttribute(LogFileConstants.ATTR_MANUFACTURE_DATE, 
                manufacturerDate.getString());

        // End the recent tag into XmlSerializer
        xml.endTag(LogFileConstants.TAG_RC);
        
        // Set the content of log data to the XML string
        logData.setContent(xml.getString());

        // Return the log data object
        return logData;
    }
    
}
