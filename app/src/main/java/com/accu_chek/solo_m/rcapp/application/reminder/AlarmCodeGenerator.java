/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AlarmRequestCodeGenerator
 * Brief: 
 *
 * Create Date: 11/6/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.setting.generalsetting.SettingUtils;

public class AlarmCodeGenerator
{

    private static final int DEFAULT_ALARM_CODE = 0;
    private static final String ALARM_REQUEST_CODE = "alarm_request_code";
    
    public static int getCode(Context context)
    {
        SafetyNumber<Integer> defaultValue = SettingUtils.convertSafeInt(DEFAULT_ALARM_CODE);
        
        return NugenGeneralModel.getInt(context, ALARM_REQUEST_CODE, defaultValue).get();
    }
    
    public static synchronized int add(Context context)
    {
        int code = getCode(context) + 1;
        
        NugenGeneralModel.setInt(context, ALARM_REQUEST_CODE, SettingUtils.convertSafeInt(code));
        
        return code;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Reminder] fix the bug of canceling alarm
// [Settings] move SettingUtils.class to FrameworkLibrary package
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
