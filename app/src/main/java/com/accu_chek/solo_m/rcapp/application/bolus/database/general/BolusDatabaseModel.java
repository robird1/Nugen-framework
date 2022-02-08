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

package com.accu_chek.solo_m.rcapp.application.bolus.database.general;

import java.util.ArrayList;
import java.util.HashMap;

import android.content.ContentUris;
import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.bolus.tool.BolusGenericHelper;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTimeBlockTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.UserSettingTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.SelectionType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public abstract class BolusDatabaseModel
{
    protected final SafetyNumber<Integer> insert(Context context, DatabaseModel model, HashMap<String, Object> insertValue)
    {
        Uri uri = model.insertData(context, insertValue);
        SafetyNumber<Integer> id = BolusGenericHelper.getInstance().convertIntegerToSafety((int) ContentUris.parseId(uri));

        return id;
    }
    
    protected final ArrayList<IDBData> query(Context context, DatabaseModel model, SafetyNumber<Integer> id, SafetyString select)
    {
        BolusQueryType type = new BolusQueryType();
        type.setSelection(select.getString());
        type.setSelectionArgs(id.get().longValue());
        
        return model.queryData(context, type);
    }
    
    protected final IDBData queryLastRecord(Context context, DatabaseModel model)
    {
        return model.getLastRecord(context);
    }
    
    protected final void update(Context context, DatabaseModel model, SafetyNumber<Integer> id, HashMap<String, Object> updateValues, SafetyString select)
    {
        BolusUpdateType type = new BolusUpdateType();
        
        type.setSelection(select.getString());
        type.setSelectionArgs(id.get().longValue());
        
        model.updateData(context, updateValues, type);
    }  
    
    protected final SafetyNumber<Integer> delete(Context context, DatabaseModel model, SafetyNumber<Integer> id, SafetyString select)
    {
        BolusDeleteType type = new BolusDeleteType();
        
        type.setSelection(select.getString());
        type.setSelectionArgs(id.get().longValue());
        
        return BolusGenericHelper.getInstance().convertIntegerToSafety(model.deleteData(context, type));
    }  
    
    protected final SafetyNumber<Integer> insertTimeBlock(Context context, HashMap<String, Object> insertValue)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordTimeBlockUri);
        SafetyNumber<Integer> id = insert(context, model, insertValue);

        return id;
    }
    
    protected final PatientRecordTimeBlockTable queryTimeBlock(Context context, SafetyNumber<Integer> id)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordTimeBlockUri);
        String command = PatientRecordTimeBlockTable.COLUMN_TIME_BLOCK_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        return (PatientRecordTimeBlockTable) query(context, model, id, select).get(0);
    }
    
    protected PatientRecordTimeBlockTable queryLastTimeBlock(Context context)
    {
        return (PatientRecordTimeBlockTable) queryLastRecord(context, new DatabaseModel(UrlType.patientRecordTimeBlockUri));
    }
    
    protected final void updateTimeBlock(Context context, SafetyNumber<Integer> id, HashMap<String, Object> updateValues)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordTimeBlockUri);
        String command = PatientRecordTimeBlockTable.COLUMN_TIME_BLOCK_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        update(context, model, id, updateValues, select);
    }  
    
    protected final SafetyNumber<Integer> deleteTimeBlock(Context context, SafetyNumber<Integer> id)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordTimeBlockUri);
        String command = PatientRecordTimeBlockTable.COLUMN_TIME_BLOCK_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        return delete(context, model, id, select);
    }
    
    protected final SafetyNumber<Integer> deleteAllTimeBlocks(Context context)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordTimeBlockUri);
        
        return BolusGenericHelper.getInstance().convertIntegerToSafety(model.deleteData(context, null));
    }
    
    protected final SafetyNumber<Integer> insertPatientRecrod(Context context, HashMap<String, Object> insertValue)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordUri);
        SafetyNumber<Integer> id = insert(context, model, insertValue);

        return id;
    }
    
    protected PatientRecordTable queryPatientRecrod(Context context, SafetyNumber<Integer> id)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordUri);
        String command = PatientRecordTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        return (PatientRecordTable) query(context, model, id, select).get(0);
    }
    
    protected PatientRecordTable queryLastPatientRecrod(Context context)
    {
        return (PatientRecordTable) queryLastRecord(context, new DatabaseModel(UrlType.patientRecordUri));
    }
    
    protected void updatePatientRecrod(Context context, SafetyNumber<Integer> id, HashMap<String, Object> updateValues)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordUri);
        String command = PatientRecordTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        update(context, model, id, updateValues, select);
    }  
    
    protected SafetyNumber<Integer> deletePatientRecrod(Context context, SafetyNumber<Integer> id)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordUri);
        String command = PatientRecordTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        return delete(context, model, id, select);
    }
    
    protected SafetyNumber<Integer> deleteAllPatientRecrod(Context context)
    {
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordUri);
        
        return BolusGenericHelper.getInstance().convertIntegerToSafety(model.deleteData(context, null));
    }
    
    protected final SafetyNumber<Integer> insertUserSetting(Context context, HashMap<String, Object> insertValue)
    {
        DatabaseModel model = new DatabaseModel(UrlType.userSettingUri);
        SafetyNumber<Integer> id = insert(context, model, insertValue);

        return id;
    }
    
    protected UserSettingTable queryUserSetting(Context context, SafetyNumber<Integer> id)
    {
        DatabaseModel model = new DatabaseModel(UrlType.userSettingUri);
        String command = UserSettingTable.COLUMN_USER_SETTING_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        return (UserSettingTable) query(context, model, id, select).get(0);
    }
    
    protected UserSettingTable queryLastUserSetting(Context context)
    {
        return (UserSettingTable) queryLastRecord(context, new DatabaseModel(UrlType.userSettingUri));
    }
    
    protected void updateUserSetting(Context context, SafetyNumber<Integer> id, HashMap<String, Object> updateValues)
    {
        DatabaseModel model = new DatabaseModel(UrlType.userSettingUri);
        String command = UserSettingTable.COLUMN_USER_SETTING_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        update(context, model, id, updateValues, select);
    }  
    
    protected SafetyNumber<Integer> deleteUserSetting(Context context, SafetyNumber<Integer> id)
    {
        DatabaseModel model = new DatabaseModel(UrlType.userSettingUri);
        String command = UserSettingTable.COLUMN_USER_SETTING_ID + SelectionType.ASSIGNVALUE;
        SafetyString select = BolusGenericHelper.getInstance().convertStringToSafety(command);
        
        return delete(context, model, id, select);
    }
    
    protected SafetyNumber<Integer> deleteAllUserSetting(Context context)
    {
        DatabaseModel model = new DatabaseModel(UrlType.userSettingUri);
        
        return BolusGenericHelper.getInstance().convertIntegerToSafety(model.deleteData(context, null));
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
