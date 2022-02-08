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
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTable;

public class BolusPatientRecordDatabaseModel extends BolusDatabaseModel
{
    private static BolusPatientRecordDatabaseModel mInstance = null;
    private BolusPatientRecordDatabaseModel(){}
    
    public static BolusPatientRecordDatabaseModel getInstance()
    {   
        if(null == mInstance)
        {
            mInstance = new BolusPatientRecordDatabaseModel();
        }
        
        return mInstance;
    }
    
    public SafetyNumber<Integer> createPatientRecord(final Context context, final SafetyString timeStamp)
    {
        SafetyNumber<Integer> id = null;        
        HashMap<String, Object> insertValue = new HashMap<String, Object>();
        SafetyString timeFormat = BolusGenericHelper.getInstance().convertStringToSafety("yyyy-MM-dd HH:mm:ss");
        SafetyString timeStampForDB = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyInsertionString(timeStamp, timeFormat);
        SafetyNumber<Long> timeStampLong = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyLong(timeStamp, timeFormat);
        SafetyString timeStampDBForDB = BolusGenericHelper.getInstance().convertStringToSafety(timeStampLong.get().toString());
        
        SafetyString segmentID = DatabaseUtil.toSafeInsertionString(0);
        
        insertValue.put(PatientRecordTable.COLUMN_ABSOLUTE_TIME_STAMP, timeStampForDB); 
        insertValue.put(PatientRecordTable.COLUMN_ABSOLUTE_TIME_STAMP_DB, timeStampDBForDB);        
        insertValue.put(PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP, timeStampForDB);
        insertValue.put(PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP_DB, timeStampDBForDB);        
        insertValue.put(PatientRecordTable.COLUMN_SEGMENT_ID, segmentID);
        
        id = insertPatientRecrod(context, insertValue);

        return id;
    }
    
    private HashMap<String, Object> getLinkIdUpdateContent(final SafetyNumber<Integer> linkId, SafetyString idKey, SafetyString idDBKey)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        SafetyString idForDB = BolusGenericHelper.getInstance().convertSafetyIntegerToSafetyInsertionString(linkId);
        SafetyString idDBForDB = BolusGenericHelper.getInstance().convertStringToSafety(linkId.get().toString());
        
        updateValues.put(idKey.getString(), idForDB);
        updateValues.put(idDBKey.getString(), idDBForDB);
        
        return updateValues;
    }
    
    public void updatePatientRecordBgId(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyNumber<Integer> bgId)
    {
        SafetyString idKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_BG_ID);
        SafetyString idDBKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_BG_ID_DB);
        
        updatePatientRecrod(context, patientRecordId, getLinkIdUpdateContent(bgId, idKey, idDBKey));
    }
    
    public void updatePatientRecordLogBookId(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyNumber<Integer> logBookId)
    {
        SafetyString idKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_LOGBOOK_ID);
        SafetyString idDBKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_LOGBOOK_ID_DB);
        
        updatePatientRecrod(context, patientRecordId, getLinkIdUpdateContent(logBookId, idKey, idDBKey));
    }
    
    public void updatePatientRecordFlags(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyString adviceFlag, final SafetyString testFlag, final SafetyString recordContent)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        updateValues.put(PatientRecordTable.COLUMN_BOLUS_ADVICE_FLAGS, adviceFlag);
        updateValues.put(PatientRecordTable.COLUMN_TEST_FLAGS, testFlag);
        updateValues.put(PatientRecordTable.COLUMN_RECORD_CONTENTS, recordContent);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordAdviceFlags(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyString adviceFlag)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        updateValues.put(PatientRecordTable.COLUMN_BOLUS_ADVICE_FLAGS, adviceFlag);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordContent(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyString recordContent)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();

        updateValues.put(PatientRecordTable.COLUMN_RECORD_CONTENTS, recordContent);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordRelativeTimeStamp(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyString timeStamp)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        SafetyString timeFormat = BolusGenericHelper.getInstance().convertStringToSafety("yyyy-MM-dd HH:mm:ss");
        SafetyString timeStampForDB = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyInsertionString(timeStamp, timeFormat);
        SafetyNumber<Long> timeStampLong = BolusGenericHelper.getInstance().convertSafetyTimeStringToSafetyLong(timeStamp, timeFormat);
        SafetyString timeStampDBForDB = BolusGenericHelper.getInstance().convertStringToSafety(timeStampLong.get().toString());
                
        updateValues.put(PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP, timeStampForDB);
        updateValues.put(PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP_DB, timeStampDBForDB); 
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
        
    public void updatePatientRecordConcentrationAndCarb(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat concentration, SafetyFloat carb)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(concentration);
        SafetyString data2ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(carb);
        
        updateValues.put(PatientRecordTable.COLUMN_CONCENTRATION, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_CARB_AMOUNT, data2ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordConcentration(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat concentration)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString dataForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(concentration);
        
        updateValues.put(PatientRecordTable.COLUMN_CONCENTRATION, dataForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordCarb(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat carb)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();

        SafetyString dataForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(carb);

        updateValues.put(PatientRecordTable.COLUMN_CARB_AMOUNT, dataForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordHealthPercentage(Context context, SafetyNumber<Integer> patientRecordId, SafetyNumber<Integer> healthPercentage)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = DatabaseUtil.toSafeInsertionString(healthPercentage.get().intValue());
        
        updateValues.put(PatientRecordTable.COLUMN_HEALTH_PERCENTAGE, data1ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordUserSettingId(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyNumber<Integer> userSettingId)
    {
        SafetyString idKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_USER_SETTING_ID);
        SafetyString idDBKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_USER_SETTING_ID_DB);
        
        updatePatientRecrod(context, patientRecordId, getLinkIdUpdateContent(userSettingId, idKey, idDBKey));
    }
    
    public void updatePatientRecordTimeBlockId(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyNumber<Integer> timeBlockId)
    {
        SafetyString idKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_TIME_BLOCK_ID);
        SafetyString idDBKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_TIME_BLOCK_ID_DB);
        
        updatePatientRecrod(context, patientRecordId, getLinkIdUpdateContent(timeBlockId, idKey, idDBKey));
    }
    
    public void updatePatientRecordUserSelectedInsulin(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat correctInsulin, SafetyFloat mealInsulin, SafetyFloat totalInsulin)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(correctInsulin);
        SafetyString data2ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(mealInsulin);
        SafetyString data3ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(totalInsulin);
        
        updateValues.put(PatientRecordTable.COLUMN_USER_SELECT_CORRECTION_BOLUS, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_USER_SELECT_MEAL_BOLUS, data2ForDB);
        updateValues.put(PatientRecordTable.COLUMN_USER_SELECT_TOTAL_BOLUS, data3ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordConfirmedInsulin(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat correctInsulin, SafetyFloat mealInsulin, SafetyFloat totalInsulin)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(correctInsulin);
        SafetyString data2ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(mealInsulin);
        SafetyString data3ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(totalInsulin);
        
        updateValues.put(PatientRecordTable.COLUMN_CONFIRM_CORRECTION_BOLUS, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_CONFIRM_MEAL_BOLUS, data2ForDB);
        updateValues.put(PatientRecordTable.COLUMN_CONFIRM_TOTAL_BOLUS, data3ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordRecommendedInsulin(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat correctInsulin, SafetyFloat mealInsulin, SafetyFloat totalInsulin)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(correctInsulin);
        SafetyString data2ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(mealInsulin);
        SafetyString data3ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(totalInsulin);
        
        updateValues.put(PatientRecordTable.COLUMN_RECOMMEND_CORRECTION_BOLUS, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_RECOMMEND_MEAL_BOLUS, data2ForDB);
        updateValues.put(PatientRecordTable.COLUMN_RECOMMEND_TOTAL_BOLUS, data3ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordCarbSuggestion(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat carbSuggestion)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(carbSuggestion);
        
        updateValues.put(PatientRecordTable.COLUMN_CARB_SUGGESTION, data1ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordAdviceDetails(Context context, SafetyNumber<Integer> patientRecordId, 
            SafetyFloat currentTarget, SafetyFloat correctedMealRise, SafetyFloat correctedDeltaBG, 
            SafetyFloat currentDeltaBG, SafetyFloat currentAllowedBG, SafetyFloat maxAllowedBG)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();

        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(currentTarget);
        SafetyString data2ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(correctedMealRise);
        SafetyString data3ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(correctedDeltaBG);
        SafetyString data4ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(currentDeltaBG);
        SafetyString data5ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(currentAllowedBG);
        SafetyString data6ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(maxAllowedBG);

        updateValues.put(PatientRecordTable.COLUMN_CURRENT_TARGET, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_CORRECTION_MEAL_INCREASE, data2ForDB);
        updateValues.put(PatientRecordTable.COLUMN_CORRECTION_DELTA_BG, data3ForDB);
        updateValues.put(PatientRecordTable.COLUMN_CURRENT_DELTA_BG, data4ForDB);
        updateValues.put(PatientRecordTable.COLUMN_CURRENT_ALLOWED_BG, data5ForDB);
        updateValues.put(PatientRecordTable.COLUMN_MAX_ALLOWED_BG, data6ForDB);

        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordActiveInsulin(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat activeInsulin)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(activeInsulin);
        
        updateValues.put(PatientRecordTable.COLUMN_ACTIVE_INSULIN, data1ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordBolusId(final Context context, final SafetyNumber<Integer> patientRecordId, final SafetyNumber<Integer> bolusId)
    {
        SafetyString idKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_BOLUS_ID);
        SafetyString idDBKey = BolusGenericHelper.getInstance().convertStringToSafety(PatientRecordTable.COLUMN_BOLUS_ID_DB);
        
        updatePatientRecrod(context, patientRecordId, getLinkIdUpdateContent(bolusId, idKey, idDBKey));
    }
    
    public void updatePatientRecordBolusType(Context context, SafetyNumber<Integer> patientRecordId, SafetyNumber<Integer> activationType, SafetyNumber<Integer> bolusType)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = DatabaseUtil.toSafeInsertionString(activationType.get().intValue());
        SafetyString data2ForDB = DatabaseUtil.toSafeInsertionString(bolusType.get().intValue());
        
        updateValues.put(PatientRecordTable.COLUMN_BOLUS_ACTIVATION_TYPE, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_BOLUS_DELIVERY_TYPE, data2ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordBolusAmount(Context context, SafetyNumber<Integer> patientRecordId, SafetyFloat immediateAmount, SafetyFloat delayedAmout)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(immediateAmount);
        SafetyString data2ForDB = BolusGenericHelper.getInstance().convertSafetyFloatToSafetyInsertionString(delayedAmout);
        
        updateValues.put(PatientRecordTable.COLUMN_IMMEDIATE_INSULIN, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_DELAYED_INSULIN, data2ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    public void updatePatientRecordBolusTime(Context context, SafetyNumber<Integer> patientRecordId, SafetyNumber<Integer> duration, SafetyNumber<Integer> lagTime)
    {
        HashMap<String, Object> updateValues = new HashMap<String, Object>();
        
        SafetyString data1ForDB = DatabaseUtil.toSafeInsertionString(duration.get().intValue());
        SafetyString data2ForDB = DatabaseUtil.toSafeInsertionString(lagTime.get().intValue());
        
        updateValues.put(PatientRecordTable.COLUMN_BOLUS_DURATION, data1ForDB);
        updateValues.put(PatientRecordTable.COLUMN_LAG_TIME, data2ForDB);
        
        updatePatientRecrod(context, patientRecordId, updateValues);
    }
    
    
    public PatientRecordTable readPatientRecordById(Context context, SafetyNumber<Integer> id)
    {
        return queryPatientRecrod(context, id);
    }
    
    public PatientRecordTable readLastPatientRecord(Context context)
    {
        return queryLastPatientRecrod(context);
    }
    
    public SafetyNumber<Integer> removePatientRecordById(Context context, SafetyNumber<Integer> id)
    {
        return deletePatientRecrod(context, id);
    }
    
    public SafetyNumber<Integer> removeAllPatientRecords(Context context)
    {
        return deleteAllPatientRecrod(context);
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
// [NSM-2889] Add accessing Patient Record database table APIs
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Update Bolus Database APIs
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Apply Patient Record storing to UI
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Apply Patient Record storing to UI
