package com.accu_chek.solo_m.rcapp.data.nugendata;

import java.util.ArrayList;

import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.operationhandler.AbstractTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

public class BasalProfileTable extends AbstractTable
{
    // Table column name. This column value will be generated automatically by
    // Android.
    public static final String COLUMN_PROFILE_ID = "profile_id";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_PROFILE_NAME = "profile_name";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_TOTAL_BASAL = "total_basal";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_IS_PROFILE_ACTIVE = "is_profile_active";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_ORDERING_INFO = "ordering_info";
    
    // The ID of the current queried record generated by Android.
    private int mRecordId = -1;
    
    // Table column value in String type.
    private String mProfileName = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mTotalBasal = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mIsProfileActive = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mOrderInfo = EMPTY_COLUMN_VALUE;

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
     * @return The string wrapped in SafetyString type.
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @see mProfileName: Use this global variable for storing column value.
     * 
     */
    public SafetyString getProfileName()
    {
        SafetyString sResult = new SafetyString(mProfileName,
                CRCTool.generateCRC16(mProfileName.getBytes()));

        return sResult;
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
     * @see mTotalBasal: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getTotalBasal()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mTotalBasal))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mTotalBasal);
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
     * @see mIsProfileActive: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getIsProfileActive()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mIsProfileActive))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mIsProfileActive);
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
     * @see mOrderInfo: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getOrderInfo()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mOrderInfo))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mOrderInfo);
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
     * @see mProfileName: Use this global variable for storing column value.
     * @see mTotalBasal: Use this global variable for storing column value.
     * @see mIsProfileActive: Use this global variable for storing column value.
     * @see mOrderInfo: Use this global variable for storing column value.
     */
    @Override
    protected void getDataFromContentValue(ContentValues values)
    {
        CommonUtils.objectCheck(values);
        
        mProfileName = DatabaseUtil.getStringValue(values, COLUMN_PROFILE_NAME,
                mProfileName);
        mTotalBasal = DatabaseUtil.getStringValue(values, COLUMN_TOTAL_BASAL,
                mTotalBasal);
        mIsProfileActive = DatabaseUtil.getStringValue(values,
                COLUMN_IS_PROFILE_ACTIVE, mIsProfileActive);
        mOrderInfo = DatabaseUtil.getStringValue(values, COLUMN_ORDERING_INFO,
                mOrderInfo);

    }

    /**
     * Generate record CRC value according to the values of record data.
     * 
     * @return the generated CRC value
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @see mProfileName: Use this global variable for storing column value.
     * @see mTotalBasal: Use this global variable for storing column value.
     * @see mIsProfileActive: Use this global variable for storing column value.
     * @see mOrderInfo: Use this global variable for storing column value.
     */
    @Override
    public int generateCRC()
    {
        int nCRC = -1;
        ArrayList<String> list = new ArrayList<String>();
        
        list.add(mProfileName);
        list.add(mTotalBasal);
        list.add(mIsProfileActive);
        list.add(mOrderInfo);
        
        nCRC = DatabaseUtil.generateCRC(list);
        Debug.printI("QueryCommand", "genCRC() in BasalProfileTable. nCRC: "
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
        return UrlType.basalProfileUri;
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
     * @see mProfileName: Use this global variable for storing column value.
     * @see mTotalBasal: Use this global variable for storing column value.
     * @see mIsProfileActive: Use this global variable for storing column value.
     * @see mOrderInfo: Use this global variable for storing column value.
     * @see mCRC: Use this global variable for storing calculated CRC value.
     */
    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {
        BasalProfileTable model = null;
        
        CommonUtils.objectCheck(cursor);

        int recordIdIndex = cursor.getColumnIndex(COLUMN_PROFILE_ID);
        int profileNameIndex = cursor.getColumnIndex(COLUMN_PROFILE_NAME);
        int totalBasalIndex = cursor.getColumnIndex(COLUMN_TOTAL_BASAL);
        int profileActiveIndex = cursor
                .getColumnIndex(COLUMN_IS_PROFILE_ACTIVE);
        int orderingInfoIndex = cursor.getColumnIndex(COLUMN_ORDERING_INFO);
        int crcIndex = cursor.getColumnIndex(COLUMN_CRC);

        model = new BasalProfileTable();

        model.mRecordId = cursor.getInt(recordIdIndex);
        model.mProfileName = DatabaseUtil.getStringValue(cursor,
                profileNameIndex);
        model.mTotalBasal = DatabaseUtil
                .getStringValue(cursor, totalBasalIndex);
        model.mIsProfileActive = DatabaseUtil.getStringValue(cursor,
                profileActiveIndex);
        model.mOrderInfo = DatabaseUtil.getStringValue(cursor,
                orderingInfoIndex);
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
        return BasalProfileTable.COLUMN_PROFILE_ID;
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
        return DBHelper.BASAL_PROFILE_TABLE;
    }
    

}
