/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AlarmData
 * Brief: 
 *
 * Create Date: 10/30/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import java.util.HashMap;
import java.util.concurrent.TimeUnit;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public class AlarmData implements Parcelable
{

    public static final String KEY_DATA = "alarm_data";
    private static final String TAG = AlarmData.class.getSimpleName();
    private long mTime = 0;
    private boolean mIsRepeat = false;
    private long mInterval = TimeUnit.DAYS.toMillis(1);
    private int mEMWRCode = EMWRList.EMW41001.getCodeId();
    private int mRequestCode = 0;
    
    public static final Parcelable.Creator<AlarmData> CREATOR = new Creator<AlarmData>()
    {

        @Override
        public AlarmData createFromParcel(Parcel source)
        {
            AlarmData data = new AlarmData();
            data.setTime(source.readLong());
            data.setRepeatStatus(source.readByte() == 1);
            data.setRepeatInterval(source.readLong());
            data.setEMWRCode(source.readInt());
            data.setRequestCode(source.readInt());
            
            return data;
        }

        @Override
        public AlarmData[] newArray(int size)
        {
            return new AlarmData[size];
        }
        
    };
    
    @Override
    public int describeContents()
    {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags)
    {
        dest.writeLong(mTime);
        dest.writeByte((byte) (mIsRepeat ? 1 : 0));
        dest.writeLong(mInterval);
        dest.writeInt(mEMWRCode);
        dest.writeInt(mRequestCode);
    }
    
    public void setTime(long time)
    {
        mTime = time;
    }

    public long getTime()
    {
        return mTime;
    }

    public void setRepeatStatus(boolean isRepeat)
    {
        mIsRepeat = isRepeat;
    }
    
    public boolean getRepeatStatus()
    {
        return mIsRepeat;
    }
    
    public void setRepeatInterval(long interval)
    {
        mInterval = interval;
    }

    public long getRepeatInterval()
    {
        return mInterval;
    }

    public void setEMWRCode(int emwrCode)
    {
        mEMWRCode = emwrCode;
    }
    
    public int getEMWRCode()
    {
        return mEMWRCode;
    }

    public void setRequestCode(int requestCode)
    {
        mRequestCode = requestCode;
    }
    
    public int getRequestCode()
    {
        return mRequestCode;
    }

    public void store(Context context)
    {
        DatabaseModel model = new DatabaseModel(UrlType.reminderAlarmListUri);
        HashMap<String, Object> values = new HashMap<String, Object>();
        int repeatCode = getRepeatCode(getRepeatStatus());
            
        SafetyString time = DatabaseUtil.toSafeInsertionString(getTime());
        SafetyString repeatStatus = DatabaseUtil.toSafeInsertionString(repeatCode);
        SafetyString emwrCode = DatabaseUtil.toSafeInsertionString(getEMWRCode());
        SafetyString requestCode = DatabaseUtil.toSafeInsertionString(getRequestCode());

        values.put(ReminderAlarmListTable.COLUMN_TIME, time);
        values.put(ReminderAlarmListTable.COLUMN_REPEAT_STATUS, repeatStatus);
        values.put(ReminderAlarmListTable.COLUMN_EMWR_CODE, emwrCode);
        values.put(ReminderAlarmListTable.COLUMN_ALARM_REQUEST_CODE, requestCode);

        model.insertData(context, values);
    }

    public void delete(Context context)
    {
        DatabaseModel model = new DatabaseModel(UrlType.reminderAlarmListUri);
        int deleteCount = model.deleteData(context, new DBTools.DeleteByAlarmCode(getRequestCode()));
        
        Debug.printI(TAG, "[Enter] AlarmData.delete()");
        Debug.printI(TAG, "requestCode: " + getRequestCode());
        Debug.printI(TAG, "deletedCount: " + deleteCount);
    }

    public void update(Context context, long time)
    {
        int updateCount = 0;
        DatabaseModel model = new DatabaseModel(UrlType.reminderAlarmListUri);
        HashMap<String, Object> values = new HashMap<String, Object>();
        
        setTime(time);
        values.put(ReminderAlarmListTable.COLUMN_TIME, DatabaseUtil.toSafeInsertionString(getTime()));
        updateCount = model.updateData(context, values, new DBTools.UpdateByAlarmCode(getRequestCode()));
        
        Debug.printI(TAG, "[Enter] AlarmData.update()");
        Debug.printI(TAG, "requestCode: " + getRequestCode());
        Debug.printI(TAG, "updateCount: " + updateCount);
    }
    
    private int getRepeatCode(boolean isRepeat)
    {
        int code = 0;
        
        if (isRepeat)
        {
            code = 1;
        }
        
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
// [Reminder] add Reminder module
// [Reminder] add Reminder module
// [Reminder] add Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
