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
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTimeBlockTable;

public class BolusTimeBlockDatabaseModel extends BolusDatabaseModel
{
    private static BolusTimeBlockDatabaseModel mInstance = null;
    private BolusTimeBlockDatabaseModel(){}
    
    public static BolusTimeBlockDatabaseModel getInstance()
    {   
        if(null == mInstance)
        {
            mInstance = new BolusTimeBlockDatabaseModel();
        }
        
        return mInstance;
    }
    
    public SafetyNumber<Integer> createTimeBlock(final Context context, final SafetyString startTime, final SafetyString endTime)
    {
        SafetyNumber<Integer> id = null;        
        HashMap<String, Object> insertValue = new HashMap<String, Object>();
        
        SafetyString timeFormat = BolusGenericHelper.getInstance().convertStringToSafety("hh:mm");
        SafetyString startTimeForDB = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyInsertionString(startTime, timeFormat);
        SafetyString endTimeForDB = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyInsertionString(endTime, timeFormat); 
        
        insertValue.put(PatientRecordTimeBlockTable.COLUMN_START_TIME, startTimeForDB);        
        insertValue.put(PatientRecordTimeBlockTable.COLUMN_END_TIME, endTimeForDB);
        
        id = insertTimeBlock(context, insertValue);

        return id;
    }
    
    public void updateTimeBlockStartEnd(final Context context, final SafetyNumber<Integer> timeBlockId, final SafetyString startTime, final SafetyString endTime)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        SafetyString timeFormat = BolusGenericHelper.getInstance().convertStringToSafety("hh:mm");
        SafetyString startTimeForDB = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyInsertionString(startTime, timeFormat);
        SafetyString endTimeForDB = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyInsertionString(endTime, timeFormat); 
        
        updateValues.put(PatientRecordTimeBlockTable.COLUMN_START_TIME, startTimeForDB);        
        updateValues.put(PatientRecordTimeBlockTable.COLUMN_END_TIME, endTimeForDB);
        
        updateTimeBlock(context, timeBlockId, updateValues);
    }
    
    public void updateTimeBlockCarbRatio(Context context, SafetyNumber<Integer> timeBlockId, SafetyFloat insulin, SafetyFloat carbs)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString insulinForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(insulin);
        SafetyString carbsForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(carbs);
        
        updateValues.put(PatientRecordTimeBlockTable.COLUMN_CARB_RATIO_INSULIN, insulinForDB);
        updateValues.put(PatientRecordTimeBlockTable.COLUMN_CARB_RATIO_CARBS, carbsForDB);
        
        updateTimeBlock(context, timeBlockId, updateValues);
    }

    public void updateTimeBlockInsulingSensitivity(Context context, SafetyNumber<Integer> timeBlockId, SafetyFloat insulin, SafetyFloat bg)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();       
        
        SafetyString insulinForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(insulin);
        SafetyString bgForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(bg);
        
        updateValues.put(PatientRecordTimeBlockTable.COLUMN_INSULIN_SENSITIVITY_INSULIN, insulinForDB);
        updateValues.put(PatientRecordTimeBlockTable.COLUMN_INSULIN_SENSITIVITY_BG, bgForDB);
        
        updateTimeBlock(context, timeBlockId, updateValues);
    }
    
    public void updateTimeBlockTarget(Context context, SafetyNumber<Integer> timeBlockId, SafetyFloat upper, SafetyFloat lower)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString upperForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(upper);
        SafetyString lowerForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(lower);

        updateValues.put(PatientRecordTimeBlockTable.COLUMN_BG_UPPER_TARGET, upperForDB);
        updateValues.put(PatientRecordTimeBlockTable.COLUMN_BG_LOWER_TARGET, lowerForDB);

        updateTimeBlock(context, timeBlockId, updateValues);
    }
    
    public PatientRecordTimeBlockTable readTimeBlockById(Context context, SafetyNumber<Integer> id)
    {
        return queryTimeBlock(context, id);
    }
    
    public SafetyNumber<Integer> removeTimeBlockById(Context context, SafetyNumber<Integer> id)
    {
        return deleteTimeBlock(context, id);
    }
    
    public SafetyNumber<Integer> removeAllTimeBlocks(Context context)
    {
        return deleteAllTimeBlocks(context);
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
