/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ReminderAlarmClockTable
 * Brief: 
 *
 * Create Date: 11/20/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import java.util.ArrayList;

import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.DBHelper;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.operationhandler.AbstractReminderTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

public class ReminderAlarmClockTable extends AbstractReminderTable
{

    // Table column name. This column value will be generated automatically by
    // Android.
    public static final String COLUMN_RECORD_ID = "record_id";

    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_NAME = "custom_name";

    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_STATE = "reminder_state";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_TIME = "reminder_time";

    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_TONE = "reminder_tone";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_REPEAT_STATUS = "reminder_repeat_status";

    // The ID of the current queried record generated by Android.
    private int mRecordId = -1;
    
    // Table column value encoded in SafetyString type. 
    private String mName = null;
    
    // Table column value encoded in JSON string type. Original type is Hamming value.
    private String mState = null;
    
    // Table column value encoded in SafetyString type. 
    private String mTime = null;
    
    // Table column value encoded in SafetyString type.
    private String mTone = null;

    // Table column value encoded in JSON string type. Original type is Hamming value.
    private String mRepeatStatus = null;

    // Table column value encoded in JSON string type. Original type is integer value.
    private String mAlarmRequestCode = null;

    
    @Override
    public SafetyChannel<Integer> getRecordId()
    {
        SafetyChannel<Integer> channel = CommonUtils
                .getSafetyChannel(mRecordId);
        
        return channel;
    }
    
    @Override
    public SafetyString getName()
    {
        SafetyString value = new SafetyString();
        value.set(mName, CRCTool.generateCRC16(mName.getBytes()));

        return value;
    }

    @Override
    public SafetyChannel<Integer> getState()
    {
        int[] channelValue = DatabaseUtil.restoreChannelIntValue(mState);
        SafetyChannel<Integer> channel = new SafetyChannel<Integer>(
                channelValue[0], channelValue[1]);
        
        return channel;
    }

    @Override
    public SafetyString getTime()
    {
        SafetyString value = new SafetyString();
        value.set(mTime, CRCTool.generateCRC16(mTime.getBytes()));

        return value;
    }

    @Override
    public SafetyString getTone()
    {
        SafetyString value = new SafetyString();
        value.set(mTone, CRCTool.generateCRC16(mTone.getBytes()));

        return value;
    }

    @Override
    public SafetyChannel<Integer> getRepeatStatus()
    {
        int[] channelValue = DatabaseUtil.restoreChannelIntValue(mRepeatStatus);
        SafetyChannel<Integer> channel = new SafetyChannel<Integer>(
                channelValue[0], channelValue[1]);
        
        return channel;
    }

    @Override
    public SafetyChannel<Integer> getAlarmRequestCode()
    {
        int[] channelValue = DatabaseUtil.restoreChannelIntValue(mAlarmRequestCode);
        SafetyChannel<Integer> channel = new SafetyChannel<Integer>(
                channelValue[0], channelValue[1]);
        
        return channel;
    }

    /**
     * 
     *
     * @param values
     */

    @Override
    protected void getDataFromContentValue(ContentValues values)
    {
        CommonUtils.objectCheck(values);

        mName = DatabaseUtil.getStringValue(values, COLUMN_NAME,
                mName);
        mState = DatabaseUtil.getStringValue(values, COLUMN_STATE, mState);
        mTime = DatabaseUtil.getStringValue(values, COLUMN_TIME,
                mTime);
        mTone = DatabaseUtil.getStringValue(values, COLUMN_TONE,
                mTone);
        mRepeatStatus = DatabaseUtil.getStringValue(values, 
                COLUMN_REPEAT_STATUS, mRepeatStatus);
        mAlarmRequestCode = DatabaseUtil.getStringValue(values,
                COLUMN_ALARM_REQUEST_CODE, mAlarmRequestCode);
    }

    /**
     * 
     *
     * @return
     */

    @Override
    public Uri onUri()
    {
        return UrlType.reminderAlarmClockUri;
    }

    /**
     * 
     *
     * @param cursor
     * @return
     */

    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {
        ReminderAlarmClockTable model = null;
        
        CommonUtils.objectCheck(cursor);
        
        int recordIdIndex = cursor.getColumnIndex(COLUMN_RECORD_ID);
        int nameIndex = cursor.getColumnIndex(COLUMN_NAME);
        int stateIndex = cursor.getColumnIndex(COLUMN_STATE);
        int timeIndex = cursor.getColumnIndex(COLUMN_TIME);
        int toneIndex = cursor.getColumnIndex(COLUMN_TONE);
        int repeatStatusIndex = cursor.getColumnIndex(COLUMN_REPEAT_STATUS);
        int requestCodeIndex = cursor.getColumnIndex(COLUMN_ALARM_REQUEST_CODE);
        int crcIndex = cursor.getColumnIndex(COLUMN_CRC);

        model = new ReminderAlarmClockTable();

        model.mRecordId = cursor.getInt(recordIdIndex);
        model.mName = DatabaseUtil.getStringValue(cursor, nameIndex);
        model.mState = DatabaseUtil.getStringValue(cursor, stateIndex);
        model.mTime = DatabaseUtil.getStringValue(cursor, timeIndex);
        model.mTone = DatabaseUtil.getStringValue(cursor, toneIndex);
        model.mRepeatStatus = DatabaseUtil.getStringValue(cursor, repeatStatusIndex);
        model.mAlarmRequestCode = DatabaseUtil.getStringValue(cursor, requestCodeIndex);
        model.setCRC(DatabaseUtil.getIntValue(cursor, crcIndex));
        
        return model;
    }

    /**
     * 
     *
     * @return
     */

    @Override
    public int generateCRC()
    {
        int nCRC = -1;
        ArrayList<String> list = new ArrayList<String>();

        list.add(mName);
        list.add(mState);
        list.add(mTime);
        list.add(mTone);
        list.add(mRepeatStatus);
        list.add(mAlarmRequestCode);

        nCRC = DatabaseUtil.generateCRC(list);

        return nCRC;
    }

    /**
     * 
     *
     * @return
     */

    @Override
    public String getPrimaryKeyName()
    {
        return ReminderAlarmClockTable.COLUMN_RECORD_ID;
    }
    
    @Override
    protected String getTableName()
    {
        return DBHelper.REMINDER_ALARM_CLOCK_TABLE;
    }


}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Reminder] add Alarm Clock / Custom reminders
