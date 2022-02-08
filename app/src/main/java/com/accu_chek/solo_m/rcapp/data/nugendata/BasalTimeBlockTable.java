package com.accu_chek.solo_m.rcapp.data.nugendata;

import java.util.ArrayList;

import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.operationhandler.AbstractTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

public class BasalTimeBlockTable extends AbstractTable
{
    // Table column name. This column value will be generated automatically by
    // Android.
    public static final String COLUMN_BLOCK_ID = "block_id";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_PROFILE_ID = "profile_id";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_END_TIME = "end_time";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_BASAL_RATE = "basal_rate";
    
    // The ID of the current queried record generated by Android.
    private int mRecordId = -1;
    
    // Table column value encoded in JSON string type.
    private String mProfileId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mEndTime = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBasalRate = EMPTY_COLUMN_VALUE;
    
    /**
     * Obtain column value.
     * 
     * @return Returns an object which stores the values of channel 1 and
     *         channel 2.
     *         Range: valid object
     *         Unit: SafetyChannel
     *         Scaling: 1
     * 
     * @see mRecordId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getRecordId()
    {
        SafetyChannel<Integer> channel = CommonUtils
                .getSafetyChannel(mRecordId);
        
        return channel;
    }

    /**
     * Obtain column value.
     * 
     * @return Returns an object which stores the values of channel 1 and
     *         channel 2.
     *         Range: valid object
     *         Unit: SafetyChannel
     *         Scaling: 1
     * 
     * @see mProfileId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getProfileId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mProfileId))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mProfileId);
            channel = new SafetyChannel<Integer>(channelValue[0],
                    channelValue[1]);
        }

        return channel;
    }

    /**
     * Obtain column value.
     * 
     * @return Returns an object which stores the values of channel 1 and
     *         channel 2.
     *         Range: valid object
     *         Unit: SafetyChannel
     *         Scaling: 1
     * 
     * @see mEndTime: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Long> getEndTime()
    {
        SafetyChannel<Long> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mEndTime))
        {
            long[] channelValue = DatabaseUtil
                    .restoreChannelLongValue(mEndTime);
            channel = new SafetyChannel<Long>(channelValue[0], channelValue[1]);
        }

        return channel;
    }

    /**
     * Obtain column value.
     * 
     * @return Returns an object which stores the values of channel 1 and
     *         channel 2.
     *         Range: valid object
     *         Unit: SafetyChannel
     *         Scaling: 1
     * 
     * @see mBasalRate: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBasalRate()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mBasalRate))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mBasalRate);
            channel = new SafetyChannel<Integer>(channelValue[0],
                    channelValue[1]);
        }

        return channel;
    }

    /**
     * Obtain table column values from the values set by user to initialize the
     * global variables for database operation (the insert or update operation).
     * 
     * @param values : the values set by user from the insert or update
     *            operation
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mProfileId: Use this global variable for storing column value.
     * @see mEndTime: Use this global variable for storing column value.
     * @see mBasalRate: Use this global variable for storing column value.
     */
    @Override
    protected void getDataFromContentValue(ContentValues values)
    {
        CommonUtils.objectCheck(values);
        
        mProfileId = DatabaseUtil.getStringValue(values, COLUMN_PROFILE_ID,
                mProfileId);
        mEndTime = DatabaseUtil.getStringValue(values, COLUMN_END_TIME,
                mEndTime);
        mBasalRate = DatabaseUtil.getStringValue(values, COLUMN_BASAL_RATE,
                mBasalRate);
        
    }

    /**
     * Generate record CRC value according to the values of record data.
     * 
     * @return the generated CRC value
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @see mProfileId: Use this global variable for storing column value.
     * @see mEndTime: Use this global variable for storing column value.
     * @see mBasalRate: Use this global variable for storing column value.
     */
    @Override
    public int generateCRC()
    {
        int nCRC = -1;
        ArrayList<String> list = new ArrayList<String>();

        list.add(mProfileId);
        list.add(mEndTime);
        list.add(mBasalRate);
        
        nCRC = DatabaseUtil.generateCRC(list);
        Debug.printI("QueryCommand", "genCRC() in BasalTimeBlockTable. nCRC: "
                + nCRC);

        return nCRC;
    }

    /**
     * Invoke this method to obtain the URI path of a certain table.
     * 
     * @return the URI reference of a certain table
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     */
    @Override
    public Uri onUri()
    {
        return UrlType.basalTimeBlockUri;
    }

    /**
     * Obtain the query record by cursor.
     * 
     * @param cursor : position to the entry of database table
     *            Range: valid object
     *            Unit: Cursor
     *            Scaling: 1
     * 
     * @return the query record
     *         Range: valid object
     *         Unit: IDBData
     *         Scaling: 1
     * 
     * @see mProfileId: Use this global variable for storing column value.
     * @see mEndTime: Use this global variable for storing column value.
     * @see mBasalRate: Use this global variable for storing column value.
     * @see mCRC: Use this global variable for storing calculated CRC value.
     */
    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {
        BasalTimeBlockTable model = null;
        
        CommonUtils.objectCheck(cursor);

        int recordIdIndex = cursor.getColumnIndex(COLUMN_BLOCK_ID);
        int profileIdIndex = cursor.getColumnIndex(COLUMN_PROFILE_ID);
        int endTimeIndex = cursor.getColumnIndex(COLUMN_END_TIME);
        int basalrateIndex = cursor.getColumnIndex(COLUMN_BASAL_RATE);
        int crcIndex = cursor.getColumnIndex(COLUMN_CRC);

        model = new BasalTimeBlockTable();

        model.mRecordId = cursor.getInt(recordIdIndex);
        model.mProfileId = DatabaseUtil.getStringValue(cursor, profileIdIndex);
        model.mEndTime = DatabaseUtil.getStringValue(cursor, endTimeIndex);
        model.mBasalRate = DatabaseUtil.getStringValue(cursor, basalrateIndex);
        model.setCRC(DatabaseUtil.getIntValue(cursor, crcIndex));
        
        return model;
    }

    /**
     * Obtain the primary key name of a certain table for the update operation.
     * 
     * @return the primary key name of a certain table
     *         Range: valid object
     *         Unit: String
     *         Scaling: 1
     */
    @Override
    public String getPrimaryKeyName()
    {
        return BasalTimeBlockTable.COLUMN_BLOCK_ID;
    }

    /**
     * Obtain the name of a certain table for the database operation.
     * 
     * @return the name of a certain table
     *         Range: valid object
     *         Unit: String
     *         Scaling: 1
     */
    @Override
    protected String getTableName()
    {
        return DBHelper.BASAL_TIME_BLOCK_TABLE;
    }

}
