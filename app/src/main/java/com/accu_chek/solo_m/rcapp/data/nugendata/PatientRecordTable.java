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

public class PatientRecordTable extends AbstractTable
{
    // Table column name. This column value will be generated automatically by
    // Android.
    public static final String COLUMN_RECORD_ID = "record_id";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BG_ID = "bg_id";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BG_ID_DB = "bg_id_db";   

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_LOGBOOK_ID = "logbook_id";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_LOGBOOK_ID_DB = "logbook_id_db";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_SEGMENT_ID = "segment_id";
    

    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ADVICE_FLAGS = "bolus_advice_flags";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_ABSOLUTE_TIME_STAMP = 
            "absolute_time_stamp";
    
    public static final String COLUMN_ABSOLUTE_TIME_STAMP_DB = 
            "absolute_time_stamp_DB";
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_RELATIVE_TIME_STAMP = 
            "relative_time_stamp";
    
    public static final String COLUMN_RELATIVE_TIME_STAMP_DB = 
            "relative_time_stamp_db";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_TEST_FLAGS = "test_flags";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_RECORD_CONTENTS = "record_contents";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONCENTRATION = "concentration";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CARB_AMOUNT = "carb_amount";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_HEALTH_PERCENTAGE = "health_percentage";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_SETTING_ID = "user_setting_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_SETTING_ID_DB = "user_setting_id_db";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_TIME_BLOCK_ID = "time_block_id";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_TIME_BLOCK_ID_DB = "time_block_id_db";
    
    
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_SELECT_CORRECTION_BOLUS = 
            "user_correction_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_SELECT_MEAL_BOLUS = 
            "user_select_meal_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_USER_SELECT_TOTAL_BOLUS = 
            "user_select_total_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONFIRM_CORRECTION_BOLUS = 
            "confirm_correction_bolus";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONFIRM_MEAL_BOLUS = 
            "confirm_meal_bolus";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONFIRM_TOTAL_BOLUS = 
            "confirm_total_bolus";
    
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
    public static final String COLUMN_CARB_SUGGESTION = "carb_suggestion";
    

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
    public static final String COLUMN_CURRENT_ALLOWED_BG = "current_allowed_bg";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_MAX_ALLOWED_BG = "max_allowed_bg";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_ACTIVE_INSULIN = "active_insulin";    
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ID = "bolus_id";

    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ID_DB = "bolus_id_db";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_ACTIVATION_TYPE = 
            "bolus_activation_type";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_DELIVERY_TYPE = 
            "bolus_delivery_type";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_IMMEDIATE_INSULIN = "immediate_insulin";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_DELAYED_INSULIN = "delayed_insulin";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_BOLUS_DURATION = "bolus_duration";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_LAG_TIME = "lag_time";
    
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_DELAYED_DURATION = "delayed_duration";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_CONTROL_SOLUTION = "control_solution";
    
    // Table column name. This column value can be null when inserting.
    public static final String COLUMN_TIMESTAMP_INVALID = "timestamp_invalid";
    
    // The ID of the current queried record generated by Android.
    private int mRecordId = -1;
    
    // Table column value encoded in JSON string type.
    private String mBgId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mLogBookId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mSegmentId = EMPTY_COLUMN_VALUE;
    
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
    private String mConcentration = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCarbAmount = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mHealthPercentage = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mUserSettingId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mTimeBlockId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mUserSelectCorrectionBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mUserSelectMealBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mUserSelectTotalBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mConfirmCorrectionBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mConfirmMealBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mConfirmTotalBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRecommendCorrectionBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRecommendMealBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mRecommendTotalBolus = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCarbSuggestion = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCurrentTarget = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCorrectionMealIncrease = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCorrectionDeltaBg = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCurrentDeltaBg = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mCurrentAllowedBg = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mMaxAllowedBg = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mActiveInsulin = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBolusId = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mActivationType = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBolusDeliverType = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mImmediateInsulin = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mDelayedInsulin = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mBolusDuration = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mLagTime = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mDelayedDuration = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mControlSolution = EMPTY_COLUMN_VALUE;
    
    // Table column value encoded in JSON string type.
    private String mTimestampInvalid = EMPTY_COLUMN_VALUE;
    
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
     * @see mBolusId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBgId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mBgId))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mBgId);
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
     * @see mBolusId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getLogBookId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mLogBookId))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mLogBookId);
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
     * @see mSegmentId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getSegmentId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mSegmentId))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mSegmentId);
            channel = new SafetyChannel<Integer>(channelValue[0], channelValue[1]);
        }

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
     * @see mBolusId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getConcentration()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mConcentration))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mConcentration);
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
     * @see mUserSettingId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getUserSettingId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mUserSettingId))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mUserSettingId);
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
     * @see mTimeBlockId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getTimeBlockId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mTimeBlockId))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mTimeBlockId);
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
     * @see mUserCorrectionBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getUserSelectCorrectionBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mUserSelectCorrectionBolus))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mUserSelectCorrectionBolus);
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
     * @see mConfirmTotalBolus: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getConfirmTotalBolus()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mConfirmTotalBolus))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mConfirmTotalBolus);
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
     * @see mCurrentAllowedBg: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getMaxAllowedBg()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mMaxAllowedBg))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mMaxAllowedBg);
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
     * @see mBolusId: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getBolusId()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mBolusId))
        {
            int[] channelValue = DatabaseUtil.restoreChannelIntValue(mBolusId);
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
     * @see mControlSolution: Use this global variable for storing column value.
     * 
     */
    public SafetyChannel<Integer> getControlSolution()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mControlSolution))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mControlSolution);
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
     * @see mTimestampInvalid: Use this global variable for storing column
     *      value.
     * 
     */
    public SafetyChannel<Integer> getTimestampInvalid()
    {
        SafetyChannel<Integer> channel = null;

        if (!EMPTY_COLUMN_VALUE.equals(mTimestampInvalid))
        {
            int[] channelValue = DatabaseUtil
                    .restoreChannelIntValue(mTimestampInvalid);
            channel = new SafetyChannel<Integer>(channelValue[0],
                    channelValue[1]);
        }

        return channel;
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
        return UrlType.patientRecordUri;
    }

    /**
     * Obtain table column values from the patient record data to initialize the
     * global variables for database operation (the insert or update operation).
     * 
     * @param values : the values from the patient record data by the insert or
     *            update operation
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mBolusId: Use this global variable for storing column value.
     * @see mAdviceFlag: Use this global variable for storing column value.
     * @see mAbsoluteTime: Use this global variable for storing column value.
     * @see mRelativeTime: Use this global variable for storing column value.
     * @see mTestFlag: Use this global variable for storing column value.
     * @see mRecordContent: Use this global variable for storing column value.
     * @see mActivationType: Use this global variable for storing column value.
     * @see mConfirmCorrectionBolus: Use this global variable for storing column
     *      value.
     * @see mConfirmMealBolus: Use this global variable for storing column
     *      value.
     * @see mConfirmTotalBolus: Use this global variable for storing column
     *      value.
     * @see mCarbSuggestion: Use this global variable for storing column value.
     * @see mActiveInsulin: Use this global variable for storing column value.
     * @see mCurrentAllowedBg: Use this global variable for storing column
     *      value.
     * @see mCurrentTarget: Use this global variable for storing column value.
     * @see mCorrectionMealIncrease: Use this global variable for storing column
     *      value.
     * @see mCorrectionDeltaBg: Use this global variable for storing column
     *      value.
     * @see mSegmentId: Use this global variable for storing column value.
     * @see mCarbAmount: Use this global variable for storing column value.
     * @see mHealthPercentage: Use this global variable for storing column
     *      value.
     * @see mUserCorrectionBolus: Use this global variable for storing column
     *      value.
     * @see mUserSelectMealBolus: Use this global variable for storing column
     *      value.
     * @see mUserSelectTotalBolus: Use this global variable for storing column
     *      value.
     * @see mRecommendCorrectionBolus: Use this global variable for storing
     *      column value.
     * @see mRecommendMealBolus: Use this global variable for storing column
     *      value.
     * @see mRecommendTotalBolus: Use this global variable for storing column
     *      value.
     * @see mBolusDeliverType: Use this global variable for storing column
     *      value.
     * @see mImmediateInsulin: Use this global variable for storing column
     *      value.
     * @see mDelayedInsulin: Use this global variable for storing column value.
     * @see mDelayedDuration: Use this global variable for storing column value.
     * @see mLagTime: Use this global variable for storing column value.
     * @see mBolusDuration: Use this global variable for storing column value.
     * @see mUserSettingId: Use this global variable for storing column value.
     * @see mTimeBlockId: Use this global variable for storing column value.
     * @see mControlSolution: Use this global variable for storing column value.
     * @see mTimestampInvalid: Use this global variable for storing column
     *      value.
     */
    @Override
    protected void getDataFromContentValue(ContentValues values)
    {
        CommonUtils.objectCheck(values);

        
        mBgId = DatabaseUtil.getStringValue(values, COLUMN_BG_ID, mBgId);
        mLogBookId = DatabaseUtil.getStringValue(values, COLUMN_LOGBOOK_ID, mLogBookId);
        mSegmentId = DatabaseUtil.getStringValue(values, COLUMN_SEGMENT_ID, mSegmentId);
        mAdviceFlag = DatabaseUtil.getStringValue(values, COLUMN_BOLUS_ADVICE_FLAGS, mAdviceFlag);
        mAbsoluteTime = DatabaseUtil.getStringValue(values, COLUMN_ABSOLUTE_TIME_STAMP, mAbsoluteTime);
        mRelativeTime = DatabaseUtil.getStringValue(values, COLUMN_RELATIVE_TIME_STAMP, mRelativeTime);
        mTestFlag = DatabaseUtil.getStringValue(values, COLUMN_TEST_FLAGS, mTestFlag);
        mRecordContent = DatabaseUtil.getStringValue(values, COLUMN_RECORD_CONTENTS, mRecordContent);
        mConcentration = DatabaseUtil.getStringValue(values, COLUMN_CONCENTRATION, mConcentration);
        mCarbAmount = DatabaseUtil.getStringValue(values, COLUMN_CARB_AMOUNT, mCarbAmount);
        mHealthPercentage = DatabaseUtil.getStringValue(values, COLUMN_HEALTH_PERCENTAGE, mHealthPercentage);
        mUserSettingId = DatabaseUtil.getStringValue(values, COLUMN_USER_SETTING_ID, mUserSettingId);
        mTimeBlockId = DatabaseUtil.getStringValue(values, COLUMN_TIME_BLOCK_ID, mTimeBlockId);
        mControlSolution = DatabaseUtil.getStringValue(values, COLUMN_CONTROL_SOLUTION, mControlSolution);
        mUserSelectCorrectionBolus = DatabaseUtil.getStringValue(values, COLUMN_USER_SELECT_CORRECTION_BOLUS, mUserSelectCorrectionBolus);
        mUserSelectMealBolus = DatabaseUtil.getStringValue(values, COLUMN_USER_SELECT_MEAL_BOLUS, mUserSelectMealBolus);
        mUserSelectTotalBolus = DatabaseUtil.getStringValue(values, COLUMN_USER_SELECT_TOTAL_BOLUS, mUserSelectTotalBolus);
        mConfirmCorrectionBolus = DatabaseUtil.getStringValue(values, COLUMN_CONFIRM_CORRECTION_BOLUS, mConfirmCorrectionBolus);
        mConfirmMealBolus = DatabaseUtil.getStringValue(values, COLUMN_CONFIRM_MEAL_BOLUS, mConfirmMealBolus);
        mConfirmTotalBolus = DatabaseUtil.getStringValue(values, COLUMN_CONFIRM_TOTAL_BOLUS, mConfirmTotalBolus);
        mRecommendCorrectionBolus = DatabaseUtil.getStringValue(values, COLUMN_RECOMMEND_CORRECTION_BOLUS, mRecommendCorrectionBolus);
        mRecommendMealBolus = DatabaseUtil.getStringValue(values, COLUMN_RECOMMEND_MEAL_BOLUS, mRecommendMealBolus);
        mRecommendTotalBolus = DatabaseUtil.getStringValue(values, COLUMN_RECOMMEND_TOTAL_BOLUS, mRecommendTotalBolus);
        mCarbSuggestion = DatabaseUtil.getStringValue(values, COLUMN_CARB_SUGGESTION, mCarbSuggestion);
        mCurrentTarget = DatabaseUtil.getStringValue(values, COLUMN_CURRENT_TARGET, mCurrentTarget);
        mCurrentAllowedBg = DatabaseUtil.getStringValue(values, COLUMN_CURRENT_ALLOWED_BG, mCurrentAllowedBg);
        mCorrectionMealIncrease = DatabaseUtil.getStringValue(values, COLUMN_CORRECTION_MEAL_INCREASE, mCorrectionMealIncrease);
        mCorrectionDeltaBg = DatabaseUtil.getStringValue(values, COLUMN_CORRECTION_DELTA_BG, mCorrectionDeltaBg);
        mCurrentDeltaBg = DatabaseUtil.getStringValue(values, COLUMN_CURRENT_DELTA_BG, mCurrentDeltaBg);
        mMaxAllowedBg = DatabaseUtil.getStringValue(values, COLUMN_MAX_ALLOWED_BG, mMaxAllowedBg);
        mActiveInsulin = DatabaseUtil.getStringValue(values, COLUMN_ACTIVE_INSULIN, mActiveInsulin);
        mBolusId = DatabaseUtil.getStringValue(values, COLUMN_BOLUS_ID, mBolusId);
        mActivationType = DatabaseUtil.getStringValue(values, COLUMN_BOLUS_ACTIVATION_TYPE, mActivationType);
        mBolusDeliverType = DatabaseUtil.getStringValue(values, COLUMN_BOLUS_DELIVERY_TYPE, mBolusDeliverType);
        mImmediateInsulin = DatabaseUtil.getStringValue(values, COLUMN_IMMEDIATE_INSULIN, mImmediateInsulin);
        mDelayedInsulin = DatabaseUtil.getStringValue(values, COLUMN_DELAYED_INSULIN, mDelayedInsulin);
        mBolusDuration = DatabaseUtil.getStringValue(values, COLUMN_BOLUS_DURATION, mBolusDuration);
        mLagTime = DatabaseUtil.getStringValue(values, COLUMN_LAG_TIME, mLagTime);
        mControlSolution = DatabaseUtil.getStringValue(values, COLUMN_CONTROL_SOLUTION, mControlSolution);
        mDelayedDuration = DatabaseUtil.getStringValue(values, COLUMN_DELAYED_DURATION, mDelayedDuration);
        mTimestampInvalid = DatabaseUtil.getStringValue(values, COLUMN_TIMESTAMP_INVALID, mTimestampInvalid);
    }

    /**
     * Generate record CRC value according to the values of record data.
     * 
     * @return the generated CRC value
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mBolusId: Use this global variable for storing column value.
     * @see mAdviceFlag: Use this global variable for storing column value.
     * @see mAbsoluteTime: Use this global variable for storing column value.
     * @see mRelativeTime: Use this global variable for storing column value.
     * @see mTestFlag: Use this global variable for storing column value.
     * @see mRecordContent: Use this global variable for storing column value.
     * @see mActivationType: Use this global variable for storing column value.
     * @see mConfirmCorrectionBolus: Use this global variable for storing column
     *      value.
     * @see mConfirmMealBolus: Use this global variable for storing column
     *      value.
     * @see mConfirmTotalBolus: Use this global variable for storing column
     *      value.
     * @see mCarbSuggestion: Use this global variable for storing column value.
     * @see mActiveInsulin: Use this global variable for storing column value.
     * @see mCurrentAllowedBg: Use this global variable for storing column
     *      value.
     * @see mCurrentTarget: Use this global variable for storing column value.
     * @see mCorrectionMealIncrease: Use this global variable for storing column
     *      value.
     * @see mCorrectionDeltaBg: Use this global variable for storing column
     *      value.
     * @see mSegmentId: Use this global variable for storing column value.
     * @see mCarbAmount: Use this global variable for storing column value.
     * @see mHealthPercentage: Use this global variable for storing column
     *      value.
     * @see mUserCorrectionBolus: Use this global variable for storing column
     *      value.
     * @see mUserSelectMealBolus: Use this global variable for storing column
     *      value.
     * @see mUserSelectTotalBolus: Use this global variable for storing column
     *      value.
     * @see mRecommendCorrectionBolus: Use this global variable for storing
     *      column value.
     * @see mRecommendMealBolus: Use this global variable for storing column
     *      value.
     * @see mRecommendTotalBolus: Use this global variable for storing column
     *      value.
     * @see mBolusDeliverType: Use this global variable for storing column
     *      value.
     * @see mImmediateInsulin: Use this global variable for storing column
     *      value.
     * @see mDelayedInsulin: Use this global variable for storing column value.
     * @see mDelayedDuration: Use this global variable for storing column value.
     * @see mLagTime: Use this global variable for storing column value.
     * @see mBolusDuration: Use this global variable for storing column value.
     * @see mUserSettingId: Use this global variable for storing column value.
     * @see mTimeBlockId: Use this global variable for storing column value.
     * @see mControlSolution: Use this global variable for storing column value.
     * @see mTimestampInvalid: Use this global variable for storing column
     *      value.
     */
    @Override
    public int generateCRC()
    {
        int nCRC = -1;
        ArrayList<String> list = new ArrayList<String>();

        list.add(mBgId);                        
        list.add(mLogBookId);                   
        list.add(mSegmentId);                   
        list.add(mAdviceFlag);                  
        list.add(mAbsoluteTime);                
        list.add(mRelativeTime);                
        list.add(mTestFlag);                    
        list.add(mRecordContent);               
        list.add(mConcentration);               
        list.add(mCarbAmount);                  
        list.add(mHealthPercentage);            
        list.add(mUserSettingId);               
        list.add(mTimeBlockId);                 
        list.add(mControlSolution);             
        list.add(mUserSelectCorrectionBolus);   
        list.add(mUserSelectMealBolus);         
        list.add(mUserSelectTotalBolus);        
        list.add(mConfirmCorrectionBolus);      
        list.add(mConfirmMealBolus);            
        list.add(mConfirmTotalBolus);           
        list.add(mRecommendCorrectionBolus);    
        list.add(mRecommendMealBolus);          
        list.add(mRecommendTotalBolus);         
        list.add(mCarbSuggestion);              
        list.add(mCurrentTarget);               
        list.add(mCurrentAllowedBg);            
        list.add(mCorrectionMealIncrease);      
        list.add(mCorrectionDeltaBg);           
        list.add(mCurrentDeltaBg);              
        list.add(mMaxAllowedBg);                
        list.add(mActiveInsulin);               
        list.add(mBolusId);                     
        list.add(mActivationType);              
        list.add(mBolusDeliverType);            
        list.add(mImmediateInsulin);            
        list.add(mDelayedInsulin);              
        list.add(mBolusDuration);               
        list.add(mLagTime);                     
        list.add(mControlSolution);             
        list.add(mDelayedDuration);             
        list.add(mTimestampInvalid);    
        
        nCRC = DatabaseUtil.generateCRC(list);
        
        Debug.printI("QueryCommand", "genCRC() in PatientRecordTable. nCRC: "
                + nCRC);

        return nCRC;
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
     * @see mBolusId: Use this global variable for storing column value.
     * @see mAdviceFlag: Use this global variable for storing column value.
     * @see mAbsoluteTime: Use this global variable for storing column value.
     * @see mRelativeTime: Use this global variable for storing column value.
     * @see mTestFlag: Use this global variable for storing column value.
     * @see mRecordContent: Use this global variable for storing column value.
     * @see mActivationType: Use this global variable for storing column value.
     * @see mConfirmCorrectionBolus: Use this global variable for storing column value.
     * @see mConfirmMealBolus: Use this global variable for storing column value.
     * @see mConfirmTotalBolus: Use this global variable for storing column value.
     * @see mCarbSuggestion: Use this global variable for storing column value.
     * @see mActiveInsulin: Use this global variable for storing column value.
     * @see mCurrentAllowedBg: Use this global variable for storing column value.
     * @see mCurrentTarget: Use this global variable for storing column value.
     * @see mCorrectionMealIncrease: Use this global variable for storing column value.
     * @see mCorrectionDeltaBg: Use this global variable for storing column value.
     * @see mSegmentId: Use this global variable for storing column value.
     * @see mCarbAmount: Use this global variable for storing column value.
     * @see mHealthPercentage: Use this global variable for storing column value.
     * @see mUserCorrectionBolus: Use this global variable for storing column value.
     * @see mUserSelectMealBolus: Use this global variable for storing column value.
     * @see mUserSelectTotalBolus: Use this global variable for storing column value.
     * @see mRecommendCorrectionBolus: Use this global variable for storing column value.
     * @see mRecommendMealBolus: Use this global variable for storing column value.
     * @see mRecommendTotalBolus: Use this global variable for storing column value.
     * @see mBolusDeliverType: Use this global variable for storing column value.
     * @see mImmediateInsulin: Use this global variable for storing column value.
     * @see mDelayedInsulin: Use this global variable for storing column value.
     * @see mDelayedDuration: Use this global variable for storing column value.
     * @see mLagTime: Use this global variable for storing column value.
     * @see mBolusDuration: Use this global variable for storing column value.
     * @see mUserSettingId: Use this global variable for storing column value.
     * @see mTimeBlockId: Use this global variable for storing column value.
     * @see mControlSolution: Use this global variable for storing column value.
     * @see mTimestampInvalid: Use this global variable for storing column value.
     * @see mCRC: Use this global variable for storing calculated CRC value.
     */
    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {
        PatientRecordTable model = new PatientRecordTable();
        
        CommonUtils.objectCheck(cursor);

        model.mRecordId = cursor.getInt(cursor.getColumnIndex(COLUMN_RECORD_ID));
        model.mBgId = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_BG_ID));        
        model.mLogBookId = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_LOGBOOK_ID));
        model.mSegmentId = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_SEGMENT_ID));
        model.mAdviceFlag = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_BOLUS_ADVICE_FLAGS));
        model.mAbsoluteTime = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_ABSOLUTE_TIME_STAMP));
        model.mRelativeTime = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_RELATIVE_TIME_STAMP));
        model.mTestFlag = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_TEST_FLAGS));
        model.mRecordContent = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_RECORD_CONTENTS));
        model.mConcentration = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CONCENTRATION));
        model.mCarbAmount = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CARB_AMOUNT));
        model.mHealthPercentage = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_HEALTH_PERCENTAGE));
        model.mUserSettingId = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_USER_SETTING_ID));
        model.mTimeBlockId = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_TIME_BLOCK_ID));
        model.mUserSelectCorrectionBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_USER_SELECT_CORRECTION_BOLUS));
        model.mUserSelectMealBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_USER_SELECT_MEAL_BOLUS));
        model.mUserSelectTotalBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_USER_SELECT_TOTAL_BOLUS));
        model.mConfirmCorrectionBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CONFIRM_CORRECTION_BOLUS));
        model.mConfirmMealBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CONFIRM_MEAL_BOLUS));
        model.mConfirmTotalBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CONFIRM_TOTAL_BOLUS));
        model.mRecommendCorrectionBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_RECOMMEND_CORRECTION_BOLUS));
        model.mRecommendMealBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_RECOMMEND_MEAL_BOLUS));
        model.mRecommendTotalBolus = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_RECOMMEND_TOTAL_BOLUS));
        model.mCarbSuggestion = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CARB_SUGGESTION));
        model.mCurrentTarget = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CURRENT_TARGET));
        model.mCorrectionMealIncrease = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CORRECTION_MEAL_INCREASE));
        model.mCorrectionDeltaBg = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CORRECTION_DELTA_BG));
        model.mCurrentDeltaBg = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CURRENT_DELTA_BG));
        model.mCurrentAllowedBg = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CURRENT_ALLOWED_BG));
        model.mMaxAllowedBg = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_MAX_ALLOWED_BG));
        model.mActiveInsulin = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_ACTIVE_INSULIN));
        model.mBolusId = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_BOLUS_ID));
        model.mActivationType = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_BOLUS_ACTIVATION_TYPE));
        model.mBolusDeliverType = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_BOLUS_DELIVERY_TYPE));
        model.mImmediateInsulin = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_IMMEDIATE_INSULIN));
        model.mDelayedInsulin = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_DELAYED_INSULIN));
        model.mLagTime = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_LAG_TIME));
        model.mBolusDuration = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_BOLUS_DURATION));
        model.mControlSolution = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_CONTROL_SOLUTION));
        model.mDelayedDuration = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_DELAYED_DURATION));
        model.mTimestampInvalid = DatabaseUtil.getStringValue(cursor, cursor.getColumnIndex(COLUMN_TIMESTAMP_INVALID));
        model.setCRC(DatabaseUtil.getIntValue(cursor, cursor.getColumnIndex(COLUMN_CRC)));

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
        return COLUMN_RECORD_ID;
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
        return DBHelper.PATIENT_RECORD_TABLE;
    }

}
// [NSM-2889] Update Bolus Database APIs
