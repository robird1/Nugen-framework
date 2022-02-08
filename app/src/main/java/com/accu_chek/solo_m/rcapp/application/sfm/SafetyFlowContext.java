/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SafetyFlowContext
 * Brief: Record current monitoring sequence state and do run-time test or not.
 *
 * Create Date: 07/02/2015
 * $Revision: 25273 $
 * $Author: JamesLee $
 * $Id: SafetyFlowContext.java 25273 2015-12-01 12:55:41Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.sfm;

import android.content.Context;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.selftest.RunTimeTest;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class SafetyFlowContext
{
    // Define debug tag
    private static final String TAG = "SafetyFlowContext";

    // Define debug tag
    private static final String TAG_SFMID = "SFMID";

    // Not find out index from Map of safety related activities
    private static final int INDEX_NO_FIND_OUT = 255;

    // Initial index from Map of safety related activities
    private static final int INDEX_INIT = -1;

    // Point to object of RunTimeTest
    private RunTimeTest mRuntimeTest = null;

    // Record all safety related activities in the Map
    private Map<String, ArrayList<MONITOR>> mAcitvityFlowMap = null;

    // Point to context of application or activity in android.
    private Context mContext = null;

    // Record current monitor group
    private MonitorGroup mCurrentMonitorGroup = null;

    // Record the current state of current monitor sequence
    private ISafetyFlowMonitorState mCurrentState = null;

    // Record the current state of current monitor sequence
    private ISafetyFlowMonitorState mPreviousState = null;
    
    private boolean mIsBackwardStep = false;

    private int mCurrentIndex = 0;

    /**
     * The constructor that will initialize the sequence flow map.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param groupId [in] the specific id of monitor group that want to
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     */
    public SafetyFlowContext(final Context context, final MonitorGroup groupId)
    {
        initialize(context, groupId);
    }

    /**
     * Initialize the sequence flow map.
     * Create a map to load all the order of execution of safety related
     * activities.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param groupId [in] the specific id of monitor group that want to
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     */
    private void initialize(final Context context, final MonitorGroup groupId)
    {
        // Valid object
        CommonUtils.objectCheck(context, groupId);

        // Initialize variable.
        mContext = context;
        mCurrentMonitorGroup = groupId;
        mCurrentState = groupId.getHeadMonitorId(context);
        mPreviousState =groupId.getHeadMonitorId(context);
        mIsBackwardStep = false;
        mCurrentIndex = 0;
        mCurrentMonitorGroup.getHeadMonitorId(context);

        Debug.printI("SFMCtx", "initialize : mCurrentMonitor : "
                + mCurrentState.getMonitorId());

        // Load all monitoring sequences to flow map table .
        if (null == mAcitvityFlowMap)
        {
            // Load flow map from table.
            mAcitvityFlowMap = new HashMap<String, ArrayList<MONITOR>>();
            loadFlowMap(mAcitvityFlowMap);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * To compare the monitor state with the current monitor sequences to find
     * out the index.
     * The index is point to array list map of relative activities
     * 
     * @param monitoringList [in] the non-null of the current monitor sequences
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @param monitorstate
     *            [in] the monitoring sequence object of ISafetyFlowMonitorState
     *            Range: valid ISafetyFlowMonitorState object
     *            Unit: ISafetyFlowMonitorState
     *            Scaling: 1
     * 
     * 
     * @return int [out] find out the index from monitors of array list
     *         sequence
     *         Range: 0 - 2^31-1
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mContext[in]
     */

    protected int getMonitorIndex(final ArrayList<MONITOR> monitoringList,
            final ISafetyFlowMonitorState monitorstate)
    {
        int index = INDEX_INIT;
        int expectedId = 0;
        int searchId = 0;
        boolean isResultOK = false;

        // Search and get expected index.
        for (MONITOR item : monitoringList)
        {
            // Get next index.
            index = index + 1;

            // Get item id
            searchId = item.getState(mContext).getMonitorId();
            expectedId = monitorstate.getMonitorId();

            // Compare id
            if (searchId == expectedId)
            {
                // Find out index.
                isResultOK = true;
                break;
            }
            else
            {
                // Apply to the coding standard
            }
        }

        if (isResultOK == false)
        {
            index = INDEX_NO_FIND_OUT;
        }

        return index;
    }

    /**
     * The Safety Flow Monitor uses the delivered sequence of ID to validate
     * whether next flow is the right sequence or not. After safety related
     * activities
     * is corresponded with the right sequence, check whether the run time test
     * is
     * executed or not.
     * 
     * 
     * @param expectedSequence [in] the non-null next flow state of specific
     *            monitor
     *            sequence
     *            Range: valid ISafetyFlowMonitorState object
     *            Unit: ISafetyFlowMonitorState
     *            Scaling: 1
     * 
     * @return SafetyBoolean [out] return true if next flow is validate.
     *         otherwise return false.
     *         Range : valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean validate(final MONITOR nExpectedId)
    {
        int targetIndex = 0;
        int expectedIndex = 0;
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;
        SafetyBoolean safetyDoRunTime = SafetyBoolean.FALSE;
        ArrayList<MONITOR> monitors = null;
        ISafetyFlowMonitorState expectedState;

        // Valid object.
        CommonUtils.objectCheck(mCurrentState, mAcitvityFlowMap);

        // Get this group all monitor id by key-vaules.
        monitors = mAcitvityFlowMap.get(mCurrentMonitorGroup.name());

        expectedState = nExpectedId.getState(mContext);

        // Valid object.
        if (monitors != null)
        {
            // Get next sequence index
            expectedIndex = getMonitorIndex(monitors, expectedState);
            targetIndex = getTargetIndex(monitors);

            mPreviousState= mCurrentState;
            
            // Do compare between each other.
            if (targetIndex == expectedIndex)
            {
                mCurrentState = expectedState;
                mCurrentIndex = mCurrentIndex + 1;
           
                // After
                SafetyFlowMonitoring
                        .setWaitForActivitiesState(SafetyBoolean.FALSE);

                // Check whether do runtime or not.
                safetyDoRunTime = monitors.get(expectedIndex)
                        .getIsRunTimeExecutedState();

                isResultOK = executeRuntimeTest(safetyDoRunTime);

            }
            else
            {
                isResultOK = SafetyBoolean.FALSE;
            }
            
            Debug.printI(TAG_SFMID, "[expect]" + mCurrentMonitorGroup.name()
                    + "[" + expectedIndex + "]" + monitors.get(expectedIndex));

        }
        else
        {
            isResultOK = SafetyBoolean.FALSE;
        }

        // Debug message
        if (isResultOK.equals(SafetyBoolean.FALSE))
        {
            Debug.printI(
                    TAG_SFMID,
                    "[validate]Fail Info target:" + targetIndex + ",expect:"
                            + expectedIndex + ", DoRunTest:" + safetyDoRunTime
                            + ",mCurrentMonitorId:"
                            + mCurrentState.getMonitorId()
                            + ",expectedSequence : "
                            + expectedState.getMonitorId());
        }

        return isResultOK;
    }

    public SafetyBoolean validateContext(
            final MONITOR nExpectedId)
    {
        int targetIndex = 0;
        int expectedIndex = 0;
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;
        SafetyBoolean safetyDoRunTime = SafetyBoolean.FALSE;
        ArrayList<MONITOR> monitors = null;
        ISafetyFlowMonitorState expectedSequence;

//        Debug.printI(TAG_SFMID, "validateContext..<<<......>>>.........<<<<<<<..........>>>");

        // Valid object.
        CommonUtils.objectCheck(mCurrentState, mAcitvityFlowMap);
        
        // Get this group all monitor id by key-vaules.
        monitors = mAcitvityFlowMap.get(mCurrentMonitorGroup.name());
      
        expectedSequence = nExpectedId.getState(mContext);
       
        // Valid object.
        if (monitors != null)
        {
          
            // Get next sequence index
            expectedIndex = getMonitorIndex(monitors, expectedSequence);
           
           if(expectedIndex > mCurrentIndex)
               targetIndex = (getMonitorIndex(monitors, mCurrentState))+1;
           else
            // Use current sequence to calculate next sequence index.
            targetIndex = (getMonitorIndex(monitors, mCurrentState)) ;
           
//            Debug.printI(TAG_SFMID, "[expect]" + mCurrentMonitorGroup.name()
//                    + "[" + expectedIndex + "]" + monitors.get(expectedIndex));

            // Do compare between each other.
            if (targetIndex == expectedIndex)
            {
                isResultOK = SafetyBoolean.TRUE;
            
//                Debug.printI(TAG_SFMID, "validate "+mCurrentMonitorGroup.name()+" Context Flow is OKay!!!!!");
            }
            else
            {
                isResultOK = SafetyBoolean.FALSE;
                Debug.printI(TAG_SFMID, "validate "+mCurrentMonitorGroup.name()+" Context Flow is Fail!!!!!");
            
            }

        }
        else
        {
            isResultOK = SafetyBoolean.FALSE;
        }

        return isResultOK;
    }

    private int getTargetIndex(ArrayList<MONITOR> monitors)
    {
        int targetIndex = 0;

        // Valid object
        CommonUtils.objectCheck(monitors, mCurrentState);

        if (mIsBackwardStep == true)
        {
            targetIndex = (getMonitorIndex(monitors, mCurrentState));
            mIsBackwardStep = false;
        }
        else
        {
            // Use current sequence to calculate next sequence index.
            targetIndex = (getMonitorIndex(monitors, mCurrentState)) + 1;
        }

        // Debug Message
        if (targetIndex == 1)
        {
            Debug.printI("SFMM", "monitors table " + monitors);
        }

        return targetIndex;
    }

    /**
     * The Safety Flow Monitor uses the delivered sequence of ID to validate
     * whether next flow is the right sequence or not. After safety related
     * activities
     * is corresponded with the right sequence, check whether the run time test
     * is
     * executed or not.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param groupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @param nAssignFlow [in] Set specific monitoring sequence
     *            Range: valid MONITOR object
     *            Unit: MONITOR
     *            Scaling: 1
     * @return
     * 
     */
    public SafetyBoolean backwardStep(Context context,
            final MonitorGroup groupId, final MONITOR nAssignFlow)
    {
        // Get state id
        int backwardStateId = nAssignFlow.getState(context).getMonitorId();
        int headMonitorStateId = groupId.getHeadMonitorId(context)
                .getMonitorId();

        mCurrentState = nAssignFlow.getState(context);

        Debug.printI("SFMID", "backwardStateId : " + backwardStateId);
        Debug.printI("SFMID", "headMonitorStateId : " + headMonitorStateId);
        if (backwardStateId == headMonitorStateId)
        {
            mIsBackwardStep = false;
            Debug.printI("SFMID", "mIsBackwardStep : " + mIsBackwardStep);
        }
        else
        {
            mIsBackwardStep = true;
        }

        return SafetyBoolean.TRUE;
    }

    /**
     * To do run time test.
     * The main processor shall send its safety flow monitoring status flag to
     * the communications processor for evaluation by run time test.
     * The run time test will send request for challenge to communications
     * processor.
     * and start the challenge-response scheme.
     * 
     * @param safetyDoRunTime [in] if safetyDoRunTime.TRUE start to run time
     *            test
     *            sequence
     *            Range: valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @return SafetyBoolean [out] return run time test result
     *         Range : SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    protected SafetyBoolean executeRuntimeTest(
            final SafetyBoolean safetyDoRunTime)
    {
        SafetyBoolean isResultOK = SafetyBoolean.TRUE;

        if (safetyDoRunTime.equals(SafetyBoolean.TRUE))
        {
            SafetyFlowMonitoring.setRunTimeTest(SafetyBoolean.TRUE);

            Log.d("SFM_Context", "[Check point] execute runTimeTest ");

            // Need do at here
            mRuntimeTest = getRunTimeTestInstance();

            isResultOK = mRuntimeTest.runTest();

            Log.d("SFM_Context", "[Check Point] RuntimeTest result "
                    + isResultOK);
            SafetyFlowMonitoring.setRunTimeTest(SafetyBoolean.FALSE);

            SafetyFlowMonitoring.setWaitForActivitiesState(SafetyBoolean.TRUE);

        }
        else
        {
            // Apply to the coding standard
            isResultOK = SafetyBoolean.TRUE;
        }

        return isResultOK;
    }

    /**
     * To get the instance of run time test
     * 
     * @return RunTimeTest [out] Return the non-null run time test object
     *         Range : valid RunTimeTest object
     *         Unit: RunTimeTest
     *         Scaling: 1
     */
    protected RunTimeTest getRunTimeTestInstance()
    {
        // Check whether the object is null.
        if (null == mRuntimeTest)
        {
            mRuntimeTest = RunTimeTest.getInstance(mContext);
            mRuntimeTest.hook();

        }
        else
        {
            // Apply to the coding standard
        }

        return mRuntimeTest;
    }

    /**
     * To load all sequence of safety related activities
     * 
     * @param flowmap [in] the non-null flow map to record relative safety
     *            activities sequences
     *            Range: valid Map<String, ArrayList<MONITOR>> object
     *            Unit: Map<String, ArrayList<MONITOR>>
     *            Scaling: 1
     * 
     * @see mAcitvityFlowMap [in]
     * 
     **/
    private void loadFlowMap(final Map<String, ArrayList<MONITOR>> flowmap)
    {
        ArrayList<MONITOR> monitoringList = null;

        int length = 0;

        // Load activity all monitor sequence.
        for (MonitorGroup group : MonitorGroup.values())
        {
            length = group.getMonitorIdLength();

            monitoringList = new ArrayList<MONITOR>();
            MONITOR[] monitors = new MONITOR[length];
            group.getMonitorId(monitors);

            // Load every monitor sequences
            for (MONITOR monitor : monitors)
            {
                monitoringList.add(monitor);
            }

            // put all safety relative activities
            flowmap.put(group.name(), monitoringList);

            Debug.printI(TAG, "key[" + group.name() + "] and Monitor List"
                    + monitoringList);
        }
    }

    public SafetyBoolean isStopFlowContext(ISafetyFlowMonitorState state)
    {
        ArrayList<MONITOR> monitors = null;
        ISafetyFlowMonitorState expectedSequence;
        SafetyBoolean isStop = SafetyBoolean.FALSE;

        // Valid object.
        CommonUtils.objectCheck(mCurrentState, mAcitvityFlowMap);

        // Get this group all monitor id by key-vaules.
        monitors = mAcitvityFlowMap.get(mCurrentMonitorGroup.name());

        int expectedIndex = getMonitorIndex(monitors, state);

        if (expectedIndex == INDEX_NO_FIND_OUT)
        {
            isStop = SafetyBoolean.TRUE;
        }
        else
        {
            isStop = SafetyBoolean.FALSE;
        }

        return isStop;
    }

    public void getPreviousMonitorID(MONITOR nMonitorId)
    {
        int length = 0;
        length = mCurrentMonitorGroup.getMonitorIdLength();

        MONITOR[] monitors = new MONITOR[length];
        mCurrentMonitorGroup.getMonitorId(monitors);

        nMonitorId = monitors[mCurrentIndex - 1];
    }

    // debug
    public MonitorGroup getMonitorGroup()
    {
        return mCurrentMonitorGroup;
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */