package com.accu_chek.solo_m.rcapp.data.nugendata;

import java.util.ArrayList;

import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.operationhandler.AbstractTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

public class LogBookTable extends AbstractTable
{
    // Table column name. This column value will be generated automatically by
    // Android.
    public static final String COLUMN_LOG_BOOK_ID = "log_book_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BG_ID = "bg_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BG_ID_DB = "bg_id_db";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ID = "bolus_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ID_DB = "bolus_id_db";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_NOTE = "note";

    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_SEGMENT_ID = "segment_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_MEAL_TIME = "meal_time";

    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_CARB_VALUE = "carb_value";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BASAL_INSULIN_MDI = "basal_insulin_mdi";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_TIMESTAMP = "timestamp";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_TIMESTAMP_DB = "timestamp_db";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_HEALTH_EVENT_FALGS = "health_event_flags";

    // The ID of the current queried record generated by Android.
    private int mRecordId = -1;
    
    // Table column value encoded in JSON string type.
    private String mBgId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBolusId = EMPTY_COLUMN_VALUE;
    
    // Table column value in String type.
    private String mNote = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mSegmentId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mMealtime = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCarbValue = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBasalInsulinMDI = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mTimestamp = EMPTY_COLUMN_VALUE;
    
    // Table column value in String type.
    private String mHealthEventFlag = EMPTY_COLUMN_VALUE;

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
     * @see mBgId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBgId()
    {
        int[] channelValue = null;
        SafetyChannel<Integer> channel = null;

        if (!mBgId.isEmpty())
        {
            channelValue = DatabaseUtil.restoreChannelIntValue(mBgId);
        }
        // If the bGId is empty string then return 0
        else
        {
            channelValue = new int[2];
            channelValue[0] = CommonUtils.encodeCH1Value(0);
            channelValue[1] = CommonUtils.encodeCH2Value(0);
        }
        channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);

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
     * @see mBolusId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBolusId()
    {
        int[] channelValue = null;
        SafetyChannel<Integer> channel = null;

        if (!mBolusId.isEmpty())
        {
            channelValue = DatabaseUtil.restoreChannelIntValue(mBolusId);
        }
        // If the Bolus Id is empty string then return 0
        else
        {
            channelValue = new int[2];
            channelValue[0] = CommonUtils.encodeCH1Value(0);
            channelValue[1] = CommonUtils.encodeCH2Value(0);
        }
        channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);

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
     * @see mNote: Use this global variable for storing column value.
     * 
     */
    public SafetyString getNote()
    {
        SafetyString sResult = new SafetyString(mNote,
                CRCTool.generateCRC16(mNote.getBytes()));

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
     * @see mSegmentId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getSegmentId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mSegmentId))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mSegmentId);
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
     * @see mMealtime: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getMealtime()
    {
        int[] channelValue = null;
        SafetyChannel<Integer> channel = null;

        if (!mMealtime.isEmpty())
        {
            channelValue = DatabaseUtil.restoreChannelIntValue(mMealtime);
        }
        // If the Meal Time code is empty string then return default (No entry)
        else
        {
            channelValue = new int[2];
            channelValue[0] = CommonUtils
                    .encodeCH1Value(CommonConstants.MEAL_TIME_NO_ENTRY);
            channelValue[1] = CommonUtils
                    .encodeCH2Value(CommonConstants.MEAL_TIME_NO_ENTRY);
        }
        channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);

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
     * @see mCarbValue: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getCarbValue()
    {
        int[] channelValue = null;
        SafetyChannel<Integer> channel = null;

        if (!mCarbValue.isEmpty())
        {
            channelValue = DatabaseUtil.restoreChannelIntValue(mCarbValue);
        }
        // If the Carbs value is empty string then return default (No entry)
        else
        {
            channelValue = new int[2];
            channelValue[0] = CommonUtils.encodeCH1Value(0);
            channelValue[1] = CommonUtils.encodeCH2Value(0);
        }
        channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);

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
     * @see mBasalInsulinMDI: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBasalInsulinMDI()
    {
        int[] channelValue = null;
        SafetyChannel<Integer> channel = null;

        if (!mBasalInsulinMDI.isEmpty())
        {
            channelValue = DatabaseUtil
                    .restoreChannelIntValue(mBasalInsulinMDI);
        }
        // If the Carbs value is empty string then return default (No entry)
        else
        {
            channelValue = new int[2];
            channelValue[0] = CommonUtils.encodeCH1Value(0);
            channelValue[1] = CommonUtils.encodeCH2Value(0);
        }
        channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);

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
     * @see mTimestamp: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Long> getTimestamp()
    {
        if (mTimestamp.isEmpty())
        {
            throw new DataIntegrityException("Empty timestamp!");
        }
        long[] channelValue = DatabaseUtil.restoreChannelLongValue(mTimestamp);
        SafetyChannel<Long> channel = new SafetyChannel<Long>(channelValue[0],
                channelValue[1]);

        return channel;
    }

    /**
     * Obtain column value.
     * 
     * @return the string wrapped in SafetyString type 
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @see mHealthEventFlag: Use this global variable for storing column value.
     * 
     */
    public SafetyString getHealthEventFlag()
    {
        SafetyString sResult = new SafetyString(mHealthEventFlag,
                CRCTool.generateCRC16(mHealthEventFlag.getBytes()));

        return sResult;
    }

    /**
     * Call this API for getting the int array object of the Health
     * event code list from logbook table.
     *
     * @return int[] [out] List of the Health Event code
     * Range: Valid int array list object
     * Unit: int[]
     * Scaling: 1
     */
    public int[] getHealthEventCodeList()
    {
    	int[] nCodeList = null;
        String[] sItem = null;
        SafetyString sResult = new SafetyString(mHealthEventFlag,
                CRCTool.generateCRC16(mHealthEventFlag.getBytes()));

        if( sResult != null && sResult.getString() != null ){
        	sItem = sResult.getString().split(",");
        	if( sItem != null ){
	        	nCodeList = new int[sItem.length];
	        	for (int ni = 0; ni < sItem.length; ni++)
	        	{
	        		try
	        		{
	        			nCodeList[ni] = Integer.parseInt(sItem[ni]);
	        		}
	        		catch (Exception e)
	        		{
	        			nCodeList[ni] = CommonConstants.HEALTH_EVENT_NO_VALUE;
	        		}
	        	}
        	}
        }
        return nCodeList;
    }
    
    
    
    
    
    /**
     * Obtain table column values from the pump history data or user setting
     * data to initialize the global variables for database operation
     * (the insert or update operation).
     * 
     * @param values : the values from the pump history data or user setting
     *            data by the insert or update operation
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mBgId: Use this global variable for storing column value.
     * @see mBolusId: Use this global variable for storing column value.
     * @see mNote: Use this global variable for storing column value.
     * @see mSegmentId: Use this global variable for storing column value.
     * @see mMealtime: Use this global variable for storing column value.
     * @see mCarbValue: Use this global variable for storing column value.
     * @see mBasalInsulinMDI: Use this global variable for storing column value.
     * @see mTimestamp: Use this global variable for storing column value.
     * @see mHealthEventFlag: Use this global variable for storing column value.
     */
    @Override
    protected void getDataFromContentValue(ContentValues values)
    {
        CommonUtils.objectCheck(values);

        mBgId = DatabaseUtil.getStringValue(values, COLUMN_BG_ID, mBgId);
        mBolusId = DatabaseUtil.getStringValue(values, COLUMN_BOLUS_ID,
                mBolusId);
        mNote = DatabaseUtil.getStringValue(values, COLUMN_NOTE, mNote);
        mSegmentId = DatabaseUtil.getStringValue(values, COLUMN_SEGMENT_ID,
                mSegmentId);
        mMealtime = DatabaseUtil.getStringValue(values, COLUMN_MEAL_TIME,
                mMealtime);
        mCarbValue = DatabaseUtil.getStringValue(values, COLUMN_CARB_VALUE,
                mCarbValue);
        mBasalInsulinMDI = DatabaseUtil.getStringValue(values,
                COLUMN_BASAL_INSULIN_MDI, mBasalInsulinMDI);
        mTimestamp = DatabaseUtil.getStringValue(values, COLUMN_TIMESTAMP,
                mTimestamp);
        mHealthEventFlag = DatabaseUtil.getStringValue(values,
                COLUMN_HEALTH_EVENT_FALGS, mHealthEventFlag);
    }

    /**
     * Generate record CRC value according to the values of record data.
     * 
     * @return the generated CRC value
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mBgId: Use this global variable for storing column value.
     * @see mBolusId: Use this global variable for storing column value.
     * @see mNote: Use this global variable for storing column value.
     * @see mSegmentId: Use this global variable for storing column value.
     * @see mMealtime: Use this global variable for storing column value.
     * @see mCarbValue: Use this global variable for storing column value.
     * @see mBasalInsulinMDI: Use this global variable for storing column value.
     * @see mTimestamp: Use this global variable for storing column value.
     * @see mHealthEventFlag: Use this global variable for storing column value.
     */
    @Override
    public int generateCRC()
    {
        int nCRC = -1;
        ArrayList<String> list = new ArrayList<String>();

        list.add(mBgId);
        list.add(mBolusId);
        list.add(mNote);
        list.add(mSegmentId);
        list.add(mMealtime);
        list.add(mCarbValue);
        list.add(mBasalInsulinMDI);
        list.add(mTimestamp);
        list.add(mHealthEventFlag);

        nCRC = DatabaseUtil.generateCRC(list);
        Debug.printI("QueryCommand", "genCRC() in LogBookTable. nCRC: " + nCRC);

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
        return UrlType.logBookUri;
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
     * @see mBgId: Use this global variable for storing column value.
     * @see mBolusId: Use this global variable for storing column value.
     * @see mNote: Use this global variable for storing column value.
     * @see mSegmentId: Use this global variable for storing column value.
     * @see mMealtime: Use this global variable for storing column value.
     * @see mCarbValue: Use this global variable for storing column value.
     * @see mBasalInsulinMDI: Use this global variable for storing column value.
     * @see mTimestamp: Use this global variable for storing column value.
     * @see mHealthEventFlag: Use this global variable for storing column value.
     * @see mCRC: Use this global variable for storing calculated CRC value.
     */
    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {
        LogBookTable model = null;

        CommonUtils.objectCheck(cursor);
        int recordIdIndex = cursor.getColumnIndex(COLUMN_LOG_BOOK_ID);
        int bGIdIndex = cursor.getColumnIndex(COLUMN_BG_ID);
        int bolusIdIndex = cursor.getColumnIndex(COLUMN_BOLUS_ID);
        int noteIndex = cursor.getColumnIndex(COLUMN_NOTE);
        int segmentIdIndex = cursor.getColumnIndex(COLUMN_SEGMENT_ID);
        int mealTimeIndex = cursor.getColumnIndex(COLUMN_MEAL_TIME);
        int carbValueIndex = cursor.getColumnIndex(COLUMN_CARB_VALUE);
        int basalInsulinIndex = cursor.getColumnIndex(COLUMN_BASAL_INSULIN_MDI);
        int timestampIndex = cursor.getColumnIndex(COLUMN_TIMESTAMP);
        int healthEventFlagIndex = cursor
                .getColumnIndex(COLUMN_HEALTH_EVENT_FALGS);
        int crcIndex = cursor.getColumnIndex(COLUMN_CRC);

        model = new LogBookTable();

        model.mRecordId = cursor.getInt(recordIdIndex);
        model.mBgId = DatabaseUtil.getStringValue(cursor, bGIdIndex);
        model.mBolusId = DatabaseUtil.getStringValue(cursor, bolusIdIndex);
        model.mNote = DatabaseUtil.getStringValue(cursor, noteIndex);
        model.mSegmentId = DatabaseUtil.getStringValue(cursor, segmentIdIndex);
        model.mMealtime = DatabaseUtil.getStringValue(cursor, mealTimeIndex);
        model.mCarbValue = DatabaseUtil.getStringValue(cursor, carbValueIndex);
        model.mBasalInsulinMDI = DatabaseUtil.getStringValue(cursor,
                basalInsulinIndex);
        model.mTimestamp = DatabaseUtil.getStringValue(cursor, timestampIndex);
        model.mHealthEventFlag = DatabaseUtil.getStringValue(cursor,
                healthEventFlagIndex);
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
        return LogBookTable.COLUMN_LOG_BOOK_ID;
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
        return DBHelper.LOG_BOOK_TABLE;
    }

}
