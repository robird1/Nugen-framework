/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogFileConstants
 * Brief: The interface is the constants for LogFile module.
 *
 * Create Date: 07/15/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogFileConstants.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

public interface LogFileConstants
{
    
    // The folder path of the log file.
    public static final String LOG_PATH = "/data/nugen";
    
    // The time format of the log file.
    public static final String LOG_ERROR_TIME = "yyyy-MM-dd'T'";
    
    // The time format of logs.
    public static final String TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
    
    // The head format of logs.
    public static final String HEAD_FORMAT = "%-1500s";
    
    // The log format of logs.
    public static final String LOG_FORMAT = "%s";

    // The symbol of new line
    public static final String NEW_LINE_CHARACTORS = "\r\n";

    // The string of log tag.
    public static final String TAG_LOG = "Log";
    
    // The string of head tag.
    public static final String TAG_HEAD = "Head";
    
    // The string of RC application tag.
    public static final String TAG_APP = "RCAPP";
    
    // The string of RC tag.
    public static final String TAG_RC = "RC";
    
    // The string of EMWR tag.
    public static final String TAG_EMWR = "EMWR";
    
    // The string of test error tag.
    public static final String TAG_TEST_ERROR = "TESTERROR";  
    
    // The string of exception tag.
    public static final String TAG_EXCEPTION = "EXCEPTION";
   
    // The string of bG record tag.
    public static final String TAG_BG_RECORD = "BGRECORD";
    
    // The string of cG record tag.
    public static final String TAG_CG_RECORD = "CGRECORD";
    
    // The string of power on cycle tag.
    public static final String TAG_POWER_ON_CYCLE = "POWERONCYCLE";
    
    // The string of reset cycle tag.
    public static final String TAG_RESET_CYCLE = "RESETCYCLE";  

    // The string of end tag.
    public static final String END_TAG_LOG = "</Log>";

    // The string of time attribute.
    public static final String ATTR_TIME = "Time";
    
    // The string of main processor version number attribute.
    public static final String ATTR_MAINPROCESSOR_VER = "MainProcessorFWVersion";
    
    // The string of RF SW version number attribute.
    public static final String ATTR_RF_SW_VER = "RFSWVersion";
    
    // The string of bGM SW version number attribute.
    public static final String ATTR_BGM_SW_VER = "BGMSWVersion";
    
    // The string of serial number attribute.
    public static final String ATTR_SN = "SN";
    
    // The string of HW version number attribute.
    public static final String ATTR_HW_VER = "HWVersion";
    
    // The string of manufactured date attribute.
    public static final String ATTR_MANUFACTURE_DATE = "ManufactureDate";
    
    // The string of error code attribute.
    public static final String ATTR_ERROR_CODE = "ErrorCode";
    
    // The string of error description attribute.
    public static final String ATTR_ERROR_DESCRIPTION = "ErrorDescription";
    
    // The string of bG count attribute.
    public static final String ATTR_BG_COUNT = "BGCount"; 
    
    // The string of bG value attribute.
    public static final String ATTR_BG_VALUE = "BGValue"; 
    
    // The string of bG result attribute.
    public static final String ATTR_BG_RESULT = "BGResult";   

    // The string of cG count attribute.
    public static final String ATTR_CG_COUNT = "CGCount"; 
    
    // The string of cG value attribute.
    public static final String ATTR_CG_VALUE = "CGValue"; 
    
    // The string of cG result attribute.
    public static final String ATTR_CG_RESULT = "CGResult";
    
    // The string of power on count attribute.
    public static final String ATTR_POWER_ON_COUNT = "PowerOnCount";
    
    // The string of reset count attribute.
    public static final String ATTR_RESET_COUNT = "ResetCount"; 
    
    // The string of exception description attribute.
    public static final String ATTR_ANDROID_EXCEPTION_DESCRIPTION = 
            "AndroidExceptionDescription";
    
    // The string of exception event attribute.
    public static final String ATTR_ANDROID_EXCEPTION = 
            "AndroidExceptionEvent";
    
    // The string of exception code attribute.
    public static final String ATTR_ANDROID_EXCEPTION_CODE = 
            "AndroidExceptionCode";
    
}
