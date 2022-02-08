/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bolus.database.BolusAdviceModel
 * Brief: 
 *
 * Create Date: 2015¦~7¤ë31¤é
 * $Revision: 20521 $
 * $Author: DWYang $
 * $Id: BolusAdviceModel.java 20521 2015-10-01 11:09:05Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.bolus.database;

import java.util.HashMap;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.bolus.database.general.BolusDatabaseModel;
import com.accu_chek.solo_m.rcapp.application.bolus.tool.BolusGenericHelper;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.data.nugendata.UserSettingTable;

public class BolusAdviceOptionDatabaseModel extends BolusDatabaseModel
{
    private static BolusAdviceOptionDatabaseModel mInstance = null;
    private BolusAdviceOptionDatabaseModel(){}
    
    public static BolusAdviceOptionDatabaseModel getInstance()
    {   
        if(null == mInstance)
        {
            mInstance = new BolusAdviceOptionDatabaseModel();
        }
        
        return mInstance;
    }
    
    public SafetyNumber<Integer> createAdviceOption(final Context context, final SafetyNumber<Integer> actingTime, final SafetyNumber<Integer> offsetTime, final SafetyFloat mealRize, final SafetyFloat snackSize)
    {
        SafetyNumber<Integer> id = null;        
        HashMap<String, Object> insertValue = new HashMap<String, Object>();
        
        SafetyString actingTimeForDB = BolusGenericHelper.getInstance().convertSafetyIntegerToSafetyInsertionString(actingTime);
        SafetyString offsetTimeForDB = BolusGenericHelper.getInstance().convertSafetyIntegerToSafetyInsertionString(offsetTime);
        SafetyString mealRizeForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(mealRize); 
        SafetyString snackRizeForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(snackSize); 
        
        insertValue.put(UserSettingTable.COLUMN_ACTING_TIME, actingTimeForDB); 
        insertValue.put(UserSettingTable.COLUMN_OFFSET_TIME, offsetTimeForDB);       
        insertValue.put(UserSettingTable.COLUMN_MEAL_RISE, mealRizeForDB);       
        insertValue.put(UserSettingTable.COLUMN_SNACK_SIZE, snackRizeForDB);
        
        id = insertUserSetting(context, insertValue);

        return id;
    }
    
    public UserSettingTable readLastUserSetting(Context context)
    {
        return queryLastUserSetting(context);
    }
    
    public UserSettingTable readUserSettingById(Context context, SafetyNumber<Integer> id)
    {
        return queryUserSetting(context, id);
    }
    
    public SafetyNumber<Integer> removeUserSettingById(Context context, SafetyNumber<Integer> id)
    {
        return deleteUserSetting(context, id);
    }
    
    public SafetyNumber<Integer> removeAllUserSetting(Context context)
    {
        return deleteAllUserSetting(context);
    }
}
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889]
// 1. Add accessing time-block database table APIs
// 2. Remove useless files
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889]
// 1. Add accessing time-block database table APIs
// 2. Remove useless files
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Rearrange packages
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Update Bolus Database APIs
