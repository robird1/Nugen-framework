/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.presentation.basal.BasalDeliveryBase
 * Brief: Data processing base class of basal delivery 
 *
 * Create Date: 06/03/2015
 * $Revision: 18338 $
 * $Author: JacksonHuang $
 * $Id: BasalDeliveryBase.java 18338 2015-09-18 06:06:06Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.setting;

import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel.IUserSettingsModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;

public class UserSettingsModel implements IUserSettingsModel
{
    private static final Uri SETTINGS_URI = Uri.withAppendedPath(
            NugenFrameworkConstants.SP_URI, NugenFrameworkConstants.SETTING_DESTINATION);

    private static final SharedPreferenceModel mModel = 
            new SharedPreferenceModel(SETTINGS_URI);
    
    private static volatile UserSettingsModel mInstance = new UserSettingsModel();
    
    public static UserSettingsModel getInstance()
    {
        return mInstance;
    }
    
    private UserSettingsModel()
    {
        // Avoid multiple instance.
    }
    
    public static SafetyBoolean deleteAll(Context context)
    {
        return mModel.deleteAll(context);
    }
    
    @Override
    public void setValue(Context context, UserSettingsKey key,
            SafetyString value)
    {
        mModel.setString(context, key.getUserSettingKey(), value);
    }

    @Override
    public SafetyString getValue(Context context, UserSettingsKey key)
    {        
        return mModel.getString(context, key.getUserSettingKey(), null);
    }
}
