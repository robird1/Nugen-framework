package com.accu_chek.solo_m.rcapp.data.nugendata;

import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.operationhandler.AbstractTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

public class LogBookView extends AbstractTable
{
    // Table column name. This column value will be generated automatically by
    // Android.
    public static final String COLUMN_LOG_BOOK_ID = "log_book_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BG_ID = "bg_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BG_VALUE = "bg_value";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BG_ID_DB = "bg_id_db";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ID = "bolus_id";

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

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONFIRM_TOTAL_BOLUS = "confirm_total_bolus";
    
 // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ADVICE_FLAGS = "bolus_advice_flags";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_ABSOLUTE_TIME_STAMP = 
            "absolute_time_stamp";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_RELATIVE_TIME_STAMP = 
            "relative_time_stamp";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_TEST_FLAGS = "test_flags";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_RECORD_CONTENTS = "record_contents";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ACTIVATION_TYPE = 
            "bolus_activation_type";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONFIRM_CORRECTION_BOLUS = 
            "confirm_correction_bolus";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONFIRM_MEAL_BOLUS = 
            "confirm_meal_bolus";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CARB_SUGGESTION = "carb_suggestion";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_ACTIVE_INSULIN = "active_insulin";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CURRENT_ALLOWED_BG = "current_allowed_bg";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CURRENT_TARGET = "current_target";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CORRECTION_MEAL_INCREASE = 
            "correction_meal_increase";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CORRECTION_DELTA_BG = 
            "correction_delta_bg";
   
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CURRENT_DELTA_BG = "current_delta_bg";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CARB_AMOUNT = "carb_amount";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_HEALTH_PERCENTAGE = "health_percentage";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_CORRECTION_BOLUS = 
            "user_correction_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_SELECT_MEAL_BOLUS = 
            "user_select_meal_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_SELECT_TOTAL_BOLUS = 
            "user_select_total_bolus";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_RECOMMEND_CORRECTION_BOLUS = 
            "recommend_correction_bolus";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_RECOMMEND_MEAL_BOLUS = 
            "recommend_meal_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_RECOMMEND_TOTAL_BOLUS = 
            "recommend_total_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_DELIVERY_TYPE = 
            "bolus_delivery_type";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_IMMEDIATE_INSULIN = "immediate_insulin";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_DELAYED_INSULIN = "delayed_insulin";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_DELAYED_DURATION = "delayed_duration";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_LAG_TIME = "lag_time";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_DURATION = "bolus_duration";
    
    // This column value will be generated automatically.
    public static final String COLUMN_CRC = "crc";

    // Table column value encoded in JSON string type.
    private String mBgValue = null;
    
    // The ID of the current queried record generated by Android.
    private int mRecordId = -1;
    
    // Table column value encoded in JSON string type.
    private String mBgId = null;
    
    // Table column value encoded in JSON string type.
    private String mBolusId = null;
    
    // Table column value in String type.
    private String mNote = null;
    
    // Table column value encoded in JSON string type.
    private String mMealtime = null;
    
    // Table column value encoded in JSON string type.
    private String mCarbValue = null;
    
    // Table column value encoded in JSON string type.
    private String mBasalInsulinMDI = null;
    
    // Table column value encoded in JSON string type.
    private String mTimestamp = null;
    
    // Table column value in String type.
    private String mHealthEventFlag = null;

    // Table column value encoded in JSON string type.
    private String mConfirmTotalBolus = null;
    
 // Table column value encoded in JSON string type.
    private String mAdviceFlag = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mAbsoluteTime = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRelativeTime = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mTestFlag = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRecordContent = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mActivationType = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mConfirmCorrectionBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mConfirmMealBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCarbSuggestion = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mActiveInsulin = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCurrentAllowedBg = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCurrentTarget = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCorrectionMealIncrease = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCorrectionDeltaBg = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCurrentDeltaBg = EMPTY_COLUMN_VALUE;
    
 // Table column value encoded in JSON string type.
    private String mCarbAmount = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mHealthPercentage = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mUserCorrectionBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mUserSelectMealBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mUserSelectTotalBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRecommendCorrectionBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRecommendMealBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRecommendTotalBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBolusDeliverType = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mImmediateInsulin = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mDelayedInsulin = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mDelayedDuration = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mLagTime = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBolusDuration = EMPTY_COLUMN_VALUE;
    
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
     * @see mBgValue: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBgValue()
    {
    	if( mBgValue == null || mBgValue.equalsIgnoreCase("") ){
    		return null;
    	}
    	else{
    		int[] channelValue = DatabaseUtil.restoreChannelIntValue(mBgValue);
    		SafetyChannel<Integer> channel = new SafetyChannel<Integer>(
                channelValue[0], channelValue[1]);
    		return channel;
    	}
    }
    
    public SafetyChannel<Integer> getConfirmTotalBolus()
    {
    	if( mConfirmTotalBolus == null || mConfirmTotalBolus.equalsIgnoreCase("") ){
    		return null;
    	}
    	else{
    		int[] channelValue = DatabaseUtil.restoreChannelIntValue(mConfirmTotalBolus);
    		SafetyChannel<Integer> channel = new SafetyChannel<Integer>(
                channelValue[0], channelValue[1]);
    		return channel;
    	}
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
     * Obtain column value.
     * 
     * @return The string wrapped in SafetyString type.
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @see mAdviceFlag: Use this global variable for storing column value.
     * 
     */
    public SafetyString getAdviceFlag()
    {
        SafetyString sResult = new SafetyString(mAdviceFlag,
                CRCTool.generateCRC16(mAdviceFlag.getBytes()));

        return sResult;
    }

    /**
     * Obtain column value.
     * 
     * @return The string wrapped in SafetyString type.
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @see mTestFlag: Use this global variable for storing column value.
     * 
     */
    public SafetyString getTestFlag()
    {
        SafetyString sResult = new SafetyString(mTestFlag,
                CRCTool.generateCRC16(mTestFlag.getBytes()));

        return sResult;
    }

    /**
     * Obtain column value.
     * 
     * @return The string wrapped in SafetyString type.
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @see mRecordContent: Use this global variable for storing column value.
     * 
     */
    public SafetyString getRecordContent()
    {
        SafetyString sResult = new SafetyString(mRecordContent,
                CRCTool.generateCRC16(mRecordContent.getBytes()));

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
     * @see mAbsoluteTime: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Long> getAbsoluteTime()
    {
        SafetyChannel<Long> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mAbsoluteTime))
        {
            long[] channelValue = DatabaseUtil.restoreChannelLongValue(mAbsoluteTime);
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
     * @see mRelativeTime: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Long> getRelativeTime()
    {
        SafetyChannel<Long> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mRelativeTime))
        {
            long[] channelValue = DatabaseUtil.restoreChannelLongValue(mRelativeTime);
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
     * @see mActivationType: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getActivationType()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mActivationType))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mActivationType);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mConfirmCorrectionBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getConfirmCorrectionBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mConfirmCorrectionBolus))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mConfirmCorrectionBolus);
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
     * @see mConfirmMealBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getConfirmMealBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mConfirmMealBolus))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mConfirmMealBolus);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mCarbSuggestion: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getCarbSuggestion()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mCarbSuggestion))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mCarbSuggestion);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mActiveInsulin: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getActiveInsulin()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mActiveInsulin))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mActiveInsulin);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mCurrentAllowedBg: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getCurrentAllowedBg()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mCurrentAllowedBg))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mCurrentAllowedBg);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mCurrentTarget: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getCurrentTarget()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mCurrentTarget))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mCurrentTarget);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mCorrectionMealIncrease: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getCorrectionMealIncrease()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mCorrectionMealIncrease))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mCorrectionMealIncrease);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mCorrectionDeltaBg: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getCorrectionDeltaBg()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mCorrectionDeltaBg))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mCorrectionDeltaBg);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mCurrentDeltaBg: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getCurrentDeltaBg()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mCurrentDeltaBg))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mCurrentDeltaBg);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mCarbAmount: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getCarbAmount()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mCarbAmount))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mCarbAmount);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mHealthPercentage: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getHealthPercentage()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mHealthPercentage))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mHealthPercentage);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
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
     * @see mUserCorrectionBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getUserCorrectionBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mUserCorrectionBolus))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mUserCorrectionBolus);
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
     * @see mUserSelectMealBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getUserSelectMealBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mUserSelectMealBolus))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mUserSelectMealBolus);
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
     * @see mUserSelectTotalBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getUserSelectTotalBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mUserSelectTotalBolus))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mUserSelectTotalBolus);
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
     * @see mRecommendCorrectionBolus: Use this global variable for storing
     *      column value.
     * 
     */
    public SafetyChannel<Integer> getRecommendCorrectionBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mRecommendCorrectionBolus))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mRecommendCorrectionBolus);
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
     * @see mRecommendMealBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getRecommendMealBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mRecommendMealBolus))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mRecommendMealBolus);
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
     * @see mRecommendTotalBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getRecommendTotalBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mRecommendTotalBolus))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mRecommendTotalBolus);
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
     * @see mBolusDeliverType: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getBolusDeliverType()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mBolusDeliverType))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mBolusDeliverType);
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
     * @see mImmediateInsulin: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getImmediateInsulin()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mImmediateInsulin))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mImmediateInsulin);
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
     * @see mDelayedInsulin: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getDelayedInsulin()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mDelayedInsulin))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mDelayedInsulin);
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
     * @see mDelayedDuration: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getDelayedDuration()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mDelayedDuration))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mDelayedDuration);
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
     * @see mLagTime: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getLagTime()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mLagTime))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mLagTime);
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
     * @see mBolusDuration: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBolusDuration()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mBolusDuration))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mBolusDuration);
            channel = new SafetyChannel<Integer>(channelValue[0],
                    channelValue[1]);
        }

        return channel;
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
        
        mBgValue = DatabaseUtil.getStringValue(values, COLUMN_BG_VALUE,
                mBgValue);
        mBolusId = DatabaseUtil.getStringValue(values, COLUMN_BOLUS_ID,
                mBolusId);
        mNote = DatabaseUtil.getStringValue(values, COLUMN_NOTE, mNote);
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
        
        mConfirmTotalBolus = DatabaseUtil.getStringValue(values, COLUMN_CONFIRM_TOTAL_BOLUS,
        		mConfirmTotalBolus);
        
        mAdviceFlag = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_BOLUS_ADVICE_FLAGS, mAdviceFlag);
        mAbsoluteTime = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_ABSOLUTE_TIME_STAMP, mAbsoluteTime);
        mRelativeTime = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP, mRelativeTime);
        mTestFlag = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_TEST_FLAGS, mTestFlag);
        mRecordContent = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_RECORD_CONTENTS, mRecordContent);
        mActivationType = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_BOLUS_ACTIVATION_TYPE, mActivationType);
        mConfirmCorrectionBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CONFIRM_CORRECTION_BOLUS, mConfirmCorrectionBolus);
        mConfirmMealBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CONFIRM_MEAL_BOLUS, mConfirmMealBolus);
        mConfirmTotalBolus = DatabaseUtil.getStringValue(values, COLUMN_CONFIRM_TOTAL_BOLUS, mConfirmTotalBolus);
        mCarbSuggestion = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CARB_SUGGESTION, mCarbSuggestion);
        mActiveInsulin = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_ACTIVE_INSULIN, mActiveInsulin);
        mCurrentAllowedBg = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CURRENT_ALLOWED_BG, mCurrentAllowedBg);
        mCurrentTarget = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CURRENT_TARGET, mCurrentTarget);
        mCorrectionMealIncrease = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CORRECTION_MEAL_INCREASE, mCorrectionMealIncrease);
        mCorrectionDeltaBg = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CORRECTION_DELTA_BG, mCorrectionDeltaBg);
        mCurrentDeltaBg = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CURRENT_DELTA_BG, mCurrentDeltaBg);
        mCarbAmount = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_CARB_AMOUNT, mCarbAmount);
        mHealthPercentage = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_HEALTH_PERCENTAGE, mHealthPercentage);
        mUserCorrectionBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_USER_SELECT_CORRECTION_BOLUS, mUserCorrectionBolus);
        mUserSelectMealBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_USER_SELECT_MEAL_BOLUS, mUserSelectMealBolus);
        mUserSelectTotalBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_USER_SELECT_TOTAL_BOLUS, mUserSelectTotalBolus);
        mRecommendCorrectionBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_RECOMMEND_CORRECTION_BOLUS, mRecommendCorrectionBolus);
        mRecommendMealBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_RECOMMEND_MEAL_BOLUS, mRecommendMealBolus);
        mRecommendTotalBolus = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_RECOMMEND_TOTAL_BOLUS, mRecommendTotalBolus);
        mBolusDeliverType = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_BOLUS_DELIVERY_TYPE, mBolusDeliverType);
        mImmediateInsulin = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_IMMEDIATE_INSULIN, mImmediateInsulin);
        mDelayedInsulin = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_DELAYED_INSULIN, mDelayedInsulin);
        mDelayedDuration = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_DELAYED_DURATION, mDelayedDuration);
        mLagTime = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_LAG_TIME, mLagTime);
        mBolusDuration = DatabaseUtil.getStringValue(values, PatientRecordTable.COLUMN_BOLUS_DURATION, mBolusDuration);
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
    	return 0;
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
        return UrlType.logBookViewUri;
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
        LogBookView model = null;

        CommonUtils.objectCheck(cursor);
        int recordIdIndex = cursor.getColumnIndex(COLUMN_LOG_BOOK_ID);
        int bgValueIndex = cursor.getColumnIndex(COLUMN_BG_VALUE);
        int bolusIdIndex = cursor.getColumnIndex(COLUMN_BOLUS_ID);
        int noteIndex = cursor.getColumnIndex(COLUMN_NOTE);
        int mealTimeIndex = cursor.getColumnIndex(COLUMN_MEAL_TIME);
        int carbValueIndex = cursor.getColumnIndex(COLUMN_CARB_VALUE);
        int basalInsulinIndex = cursor.getColumnIndex(COLUMN_BASAL_INSULIN_MDI);
        int timestampIndex = cursor.getColumnIndex(COLUMN_TIMESTAMP);
        int healthEventFlagIndex = cursor
                .getColumnIndex(COLUMN_HEALTH_EVENT_FALGS);
        int confirmTotalBolusIndex = cursor.getColumnIndex(COLUMN_CONFIRM_TOTAL_BOLUS);
        int absoluteTimeIndex = cursor.getColumnIndex(COLUMN_ABSOLUTE_TIME_STAMP);
        int activeInsulinIndex = cursor.getColumnIndex(COLUMN_ACTIVE_INSULIN);
        int activationTypeIndex = cursor.getColumnIndex(COLUMN_BOLUS_ACTIVATION_TYPE);
        int adviceIndex = cursor.getColumnIndex(COLUMN_BOLUS_ADVICE_FLAGS);
        int deliverTypeIndex = cursor.getColumnIndex(COLUMN_BOLUS_DELIVERY_TYPE);
        int bolusDurationIndex = cursor.getColumnIndex(COLUMN_BOLUS_DURATION);
        int carbAmountIndex = cursor.getColumnIndex(COLUMN_CARB_AMOUNT);
        int carbSuggestionIndex = cursor.getColumnIndex(COLUMN_CARB_SUGGESTION);
        int confirmCorrectionBolusIndex = cursor.getColumnIndex(COLUMN_CONFIRM_CORRECTION_BOLUS);
        int confirmMealBolusIndex = cursor.getColumnIndex(COLUMN_CONFIRM_MEAL_BOLUS);
        int correctionDeltaBgIndex = cursor.getColumnIndex(COLUMN_CORRECTION_DELTA_BG);
        int correctionMealIncreaseIndex = cursor.getColumnIndex(COLUMN_CORRECTION_MEAL_INCREASE);
        int currentAllowedBgIndex = cursor.getColumnIndex(COLUMN_CURRENT_ALLOWED_BG);
        int currentDeltaBgIndex = cursor.getColumnIndex(COLUMN_CURRENT_DELTA_BG);
        int currentTargetIndex = cursor.getColumnIndex(COLUMN_CURRENT_TARGET);
        int delayedDurationIndex = cursor.getColumnIndex(COLUMN_DELAYED_DURATION);
        int dalayedInsulinIndex = cursor.getColumnIndex(COLUMN_DELAYED_INSULIN);
        int healthPercentageIndex = cursor.getColumnIndex(COLUMN_HEALTH_PERCENTAGE);
        int immediateInsulinIndex = cursor.getColumnIndex(COLUMN_IMMEDIATE_INSULIN);
        int lagTimeIndex = cursor.getColumnIndex(COLUMN_LAG_TIME);
        int recommendCorrectionBolusIndex = cursor.getColumnIndex(COLUMN_RECOMMEND_CORRECTION_BOLUS);
        int recommendMealBolusIndex = cursor.getColumnIndex(COLUMN_RECOMMEND_MEAL_BOLUS);
        int recommendTotalBolusIndex = cursor.getColumnIndex(COLUMN_RECOMMEND_TOTAL_BOLUS);
        int recordContentIndex = cursor.getColumnIndex(COLUMN_RECORD_CONTENTS);
        int relativeTimeIndex = cursor.getColumnIndex(COLUMN_RELATIVE_TIME_STAMP);
        int testFlagIndex = cursor.getColumnIndex(COLUMN_TEST_FLAGS);
        int userCorrectionBolusIndex = cursor.getColumnIndex(COLUMN_USER_CORRECTION_BOLUS);
        int userSelectMealBolusIndex = cursor.getColumnIndex(COLUMN_USER_SELECT_MEAL_BOLUS);
        int userSelectTotalBolusIndex = cursor.getColumnIndex(COLUMN_USER_SELECT_TOTAL_BOLUS);        
        
        model = new LogBookView();

        model.mRecordId = cursor.getInt(recordIdIndex);
        model.mBgValue = DatabaseUtil.getStringValue(cursor, bgValueIndex);
        model.mBolusId = DatabaseUtil.getStringValue(cursor, bolusIdIndex);
        model.mNote = DatabaseUtil.getStringValue(cursor, noteIndex);
        model.mMealtime = DatabaseUtil.getStringValue(cursor, mealTimeIndex);
        model.mCarbValue = DatabaseUtil.getStringValue(cursor, carbValueIndex);
        model.mBasalInsulinMDI = DatabaseUtil.getStringValue(cursor,
                basalInsulinIndex);
        model.mTimestamp = DatabaseUtil.getStringValue(cursor, timestampIndex);
        model.mHealthEventFlag = DatabaseUtil.getStringValue(cursor,
                healthEventFlagIndex);
        model.mConfirmTotalBolus = DatabaseUtil.getStringValue(cursor,
        		confirmTotalBolusIndex);
        model.mAbsoluteTime = DatabaseUtil.getStringValue(cursor, absoluteTimeIndex);
        model.mActiveInsulin = DatabaseUtil.getStringValue(cursor, activeInsulinIndex);
        model.mActivationType = DatabaseUtil.getStringValue(cursor, activationTypeIndex);
        model.mAdviceFlag = DatabaseUtil.getStringValue(cursor, adviceIndex);
        model.mBolusDeliverType = DatabaseUtil.getStringValue(cursor, deliverTypeIndex);
        model.mBolusDuration = DatabaseUtil.getStringValue(cursor, bolusDurationIndex);
        model.mBolusId = DatabaseUtil.getStringValue(cursor, bolusIdIndex);
        model.mCarbAmount = DatabaseUtil.getStringValue(cursor, carbAmountIndex);
        model.mCarbSuggestion = DatabaseUtil.getStringValue(cursor, carbSuggestionIndex);
        model.mConfirmCorrectionBolus = DatabaseUtil.getStringValue(cursor, confirmCorrectionBolusIndex);
        model.mConfirmMealBolus = DatabaseUtil.getStringValue(cursor, confirmMealBolusIndex);
        model.mConfirmTotalBolus = DatabaseUtil.getStringValue(cursor, confirmTotalBolusIndex);
        model.mCorrectionDeltaBg = DatabaseUtil.getStringValue(cursor, correctionDeltaBgIndex);
        model.mCorrectionMealIncrease = DatabaseUtil.getStringValue(cursor, correctionMealIncreaseIndex);
        model.mCurrentAllowedBg = DatabaseUtil.getStringValue(cursor, currentAllowedBgIndex);
        model.mCurrentDeltaBg = DatabaseUtil.getStringValue(cursor, currentDeltaBgIndex);
        model.mCurrentTarget = DatabaseUtil.getStringValue(cursor, currentTargetIndex);
        model.mDelayedDuration = DatabaseUtil.getStringValue(cursor, delayedDurationIndex);
        model.mDelayedInsulin = DatabaseUtil.getStringValue(cursor, dalayedInsulinIndex);
        model.mHealthPercentage = DatabaseUtil.getStringValue(cursor, healthPercentageIndex);
        model.mImmediateInsulin = DatabaseUtil.getStringValue(cursor, immediateInsulinIndex);
        model.mLagTime = DatabaseUtil.getStringValue(cursor, lagTimeIndex);
        model.mRecommendCorrectionBolus = DatabaseUtil.getStringValue(cursor, recommendCorrectionBolusIndex);
        model.mRecommendMealBolus = DatabaseUtil.getStringValue(cursor, recommendMealBolusIndex);
        model.mRecommendTotalBolus = DatabaseUtil.getStringValue(cursor, recommendTotalBolusIndex);
        model.mRecordContent = DatabaseUtil.getStringValue(cursor, recordContentIndex);
        model.mRelativeTime = DatabaseUtil.getStringValue(cursor, relativeTimeIndex);
        model.mTestFlag = DatabaseUtil.getStringValue(cursor, testFlagIndex);
        model.mUserCorrectionBolus = DatabaseUtil.getStringValue(cursor, userCorrectionBolusIndex);
        model.mUserSelectMealBolus = DatabaseUtil.getStringValue(cursor, userSelectMealBolusIndex);
        model.mUserSelectTotalBolus = DatabaseUtil.getStringValue(cursor, userSelectTotalBolusIndex);
        
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
        return LogBookView.COLUMN_LOG_BOOK_ID;
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
        return DBHelper.LOG_BOOK_VIEW;
    }
}
// [NSM-2889] Update Bolus Database APIs
