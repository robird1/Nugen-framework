/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: TimeManagementModel
 * Brief: Provide functions about TimeSyncListener interface and TimeSegment ID
 * 
 * Create Date: 05/01/2015
 * $$Revision: 23865 $$
 * $$Author: TerryHsieh $$
 * $$Id: TimeManagementModel.java 23865 2015-11-11 09:15:04Z TerryHsieh $$
 */
package com.accu_chek.solo_m.rcapp.application.timemanagement;

import android.content.Context;

import java.util.ArrayList;
import java.util.List;

import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

import android.util.Log;

/**
 * This class provides TimeSegment id restore, create and add/remove TimeSyncListener for time sensitive activities.
 */
public class TimeManagementModel
{
    
    // Tag for debugging
    static final String TAG = "[TM]TimeManagementModel";

    // Flag for debugging
    private static final boolean isDEBUG = true;

    // GlobalContainer op code
    final static String TIME_SEGMENT_ID_NAME = "TimeSegmentID";
    
    // key post-fix for accessing 2 channels data in non-volatile memory 
    final static String KEY2_POSTFIX = "_2";
    
    // Time segment id initial value
    final static Integer TIME_SEGMENT_ID_INIT_VAL = 1;
    
    // Activity context
    Context mContext = null;
    
    // Time sync listener list
    List<TimeSyncListener> mTimeSyncListeners = new ArrayList<TimeSyncListener>();

    /**
     * Constructor of TimeManagement Model
     * 
     * @param Context [in] activity context
     *          Range: valid Context object
     *          Unit: Context
     *          Scaling: 1
     * 
     * @return void [out]
     */
    public TimeManagementModel(Context context)
    {
        String key = null;
        SafetyChannel<Integer> scID = null;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);
        mContext = context;
        key = TIME_SEGMENT_ID_NAME;
        scID = run_NugenGeneralModel_getInt(mContext, key);
        if(null == scID)
        {
            SafetyChannel<Integer> scVal = new SafetyChannel<Integer>(
                    CommonUtils.encodeCH1Value(TIME_SEGMENT_ID_INIT_VAL),
                    CommonUtils.encodeCH2Value(TIME_SEGMENT_ID_INIT_VAL));
            run_NugenGeneralModel_setInt(mContext, TIME_SEGMENT_ID_NAME, scVal);
        }
    }

    /**
     * A function to return current time segment id which store in share
     * preference.
     * 
     * @param void [in]
     * 
     * @return SafetyChannel<Integer> [out] TimeSegment id
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */
    public SafetyChannel<Integer> restoreTimeSegmentID()
    {
        // mContext's null check has been done in constructor, do don't need to
        // do again
        String key = TIME_SEGMENT_ID_NAME;
        SafetyChannel<Integer> scID = run_NugenGeneralModel_getInt(mContext,
                key);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scID);
        return scID;
    }

    /**
     * A function to new a TimeSegment id, the id will be the key to
     * search TimeSegment object in database.
     * 
     * @param void [in]
     * 
     * @return SafetyChannel<Integer> [out] TimeSegment id
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */
    public SafetyChannel<Integer> newTimeSegmentID()
    {
        // mContext's null check has been done in constructor, do don't need to
        // do again
        Integer id = 0;
        SafetyChannel<Integer> scID = null;
        scID = run_NugenGeneralModel_getInt(mContext, TIME_SEGMENT_ID_NAME);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scID);
        // Get original value of SafetyChannel
        id = CommonUtils.getOriginValue(scID.getValueCH1(), scID.getValueCH2()) + 1;
        if(id >= Integer.MAX_VALUE)
        {
            id = TIME_SEGMENT_ID_INIT_VAL;
        }
        //New ID
        scID = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(id),
                CommonUtils.encodeCH2Value(id));
        run_NugenGeneralModel_setInt(mContext, TIME_SEGMENT_ID_NAME, scID);
        return scID;
    }

    /**
     * A function for some time sensitive activities to add time sync listener
     * to TimeManagementModel.
     * 
     * @param TimeSyncListener [in] listener
     *            TimeSyncListener to listen time sync event for running
     *            pre-process and post-process of time sync.
     * 
     *            Range:valid TimeSyncListener object
     *            Unit: TimeSyncListener
     *            Scale: 1
     * 
     * @return SafetyBoolean [out] Result of addTimeSyncListener
     *          
     *          Range: valid SafetyBoolean object
     *          Unit: SafetyBoolean
     *          Scaling: 1
     */
    public SafetyBoolean addTimeSyncListener(TimeSyncListener listener)
            throws DataIntegrityException
    {
        boolean isSucess = false;
        SafetyBoolean isSBSucess = null;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(listener);
        isSucess = mTimeSyncListeners.add(listener);
        if (isSucess)
        {
            isSBSucess = SafetyBoolean.TRUE;
        }
        else
        {
            isSBSucess = SafetyBoolean.FALSE;
        }
        return isSBSucess;
    }

    /**
     * A function for some time sensitive activities to remove time sync
     * listener from TimeManagementModel.
     * 
     * @param TimeSyncListener [in] listener
     *            TimeSyncListener to listen time sync event for running
     *            pre-process and post-process of time sync.
     * 
     *            Range: valid TimeSyncListener object
     *            Unit: TimeSyncListener
     *            Scaling: 1
     * 
     * @return SafetyBoolean [out] Result of removeTimeSyncListener
     *              Range: valid SafetyBoolean object
     *              Unit: SafetyBoolean
     *              Scaling: 1
     */
    public SafetyBoolean removeTimeSyncListener(TimeSyncListener listener)
    {
        boolean isSucess = false;
        SafetyBoolean isSBSucess = null;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(listener);
        isSucess = mTimeSyncListeners.remove(listener);
        if (isSucess)
        {
            isSBSucess = SafetyBoolean.TRUE;
        }
        else
        {
            isSBSucess = SafetyBoolean.FALSE;
        }
        return isSBSucess;
    }

    /**
     * A function to trigger TimeSyncListener call back before system time
     * change.
     * 
     * @param void [in]
     * 
     * @return void [out]
     */
    public void triggerBeforeTimeSyncListener() throws DataIntegrityException
    {
        int count = mTimeSyncListeners.size();

        for (int i = 0; i < count; i++)
        {
            // List<TimeSyncListener> mTimeSyncListeners is a primitive type
            // list, we do not need to do unit test
            TimeSyncListener listener = mTimeSyncListeners.get(i);
            // Check whether the parameter is null or not
            CommonUtils.objectCheck(listener);
            listener.onBeforeTimeSyncEvent();
        }
    }

    /**
     * A function to trigger TimeSyncListener call back after system time
     * change.
     * 
     * @param void [in]
     * 
     * @return void [out]
     */
    public void triggerAfterTimeSyncListener() throws DataIntegrityException
    {
        int count = mTimeSyncListeners.size();

        for (int i = 0; i < count; i++)
        {
            // List<TimeSyncListener> mTimeSyncListeners is a primitive type
            // list, we do not need to do unit test
            TimeSyncListener listener = mTimeSyncListeners.get(i);
            // Check whether the parameter is null or not
            CommonUtils.objectCheck(listener);
            listener.onAfterTimeSyncEvent();
        }
    }

    // VVVVVVVVVVVVVVVVVVVV Design for test VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

    /**
     * calling NugenGeneralModel to set a
     * long value to share preference
     * 
     * @param Context [in] context
     *            Activity context
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling 1
     * @param String [in] key
     *            Searching key
     *            Range: valid String object
     *            Unit: String
     *            Scaling 1
     * @param SafetyChannel<Long> [in]scVal
     *            A safety long value
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scaling 1
     * 
     * @return void [out]
     */
    public void run_NugenGeneralModel_setInt(Context context, String key,
            SafetyChannel<Integer> scVal)
    {
        // context's null check has been done in constructor, do don't need to
        // do again
        SafetyNumber<Integer> snval = null;

        snval = new SafetyNumber<Integer>(scVal.getValueCH1(),
                -scVal.getValueCH1());
        NugenGeneralModel.setInt(context, key, snval);
        snval = new SafetyNumber<Integer>(scVal.getValueCH2(),
                -scVal.getValueCH2());
        NugenGeneralModel.setInt(context, key + KEY2_POSTFIX, snval);
    }

    /**
     * calling NugenGeneralModel to set an
     * int value to share preference
     * 
     * @param Context [in] context
     *            Activity context
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling 1
     * @param String [in] key
     *            Searching key
     *            Range: valid String object
     *            Unit: String
     *            Scaling 1
     * @param SafetyChannel<Integer> [in]scVal
     *            A safety int value
     *            Range: valid SafetyChannel<Integer> object
     *            Unit: SafetyChannel<Integer>
     *            Scaling 1
     * @throws N/A
     */
    public SafetyChannel<Integer> run_NugenGeneralModel_getInt(Context context,
            String key)
    {
        // context's null check has been done in constructor, do don't need to
        // do again
        int orgValue = 0;
        SafetyNumber<Integer> snCh1Value = null;
        SafetyNumber<Integer> snCh2Value = null;
        SafetyChannel<Integer> scRetVal = null;
        // The unit test of the following statements will be done by
        // NugenGeneralModel
        snCh1Value = NugenGeneralModel.getInt(context, key);
        snCh2Value = NugenGeneralModel.getInt(context, key + KEY2_POSTFIX);
        // Get original value of SafetyChannel
        if ((snCh1Value != null) && (snCh2Value != null))
        {
            int ch1Val = snCh1Value.get();
            int ch2Val = snCh2Value.get();
            orgValue = CommonUtils.getOriginValue(ch1Val, ch2Val);
            scRetVal = new SafetyChannel<Integer>(
                    CommonUtils.encodeCH1Value(orgValue),
                    CommonUtils.encodeCH2Value(orgValue));
        }
        else
        {
            // Apply to the coding standard
        }
        return scRetVal;
    }

    /**
     * 
     * Function for test to print message to Logcat or log file
     *
     * @param String [in] str
     *          Range: valid String object
     *          Unit: String
     *          Scaling: 1
     * @return void [out] 
     */
    @SuppressWarnings("unused")
    private static void printI(String str)
    {
        if (isDEBUG)
        {
            Log.i(TAG, str);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
