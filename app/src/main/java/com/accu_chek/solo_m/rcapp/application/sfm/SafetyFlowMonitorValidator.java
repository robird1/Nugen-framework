/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SafetyFlowMonitorValidator
 * Brief: Build flow map and validate critical path of activates.
 *
 * Create Date: 07/02/2015
 * $Revision: 25273 $
 * $Author: JamesLee $
 * $Id: SafetyFlowMonitorValidator.java 25273 2015-12-01 12:55:41Z JamesLee $
 */
package com.accu_chek.solo_m.rcapp.application.sfm;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class SafetyFlowMonitorValidator
{
    // Define debug tag
    private static final String TAG = "SFM";

    // Define debug tag
    private static final String TAG_SFMID = "SFMID";

    // The byte value of SafetyBoolean TRUE
    private static byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // The byte value of SafetyBoolean FALSE
    private static byte mByteSafetyFALSE = SafetyBoolean.FALSE.getByte();

    // Define the instance of SafetyFlowMonitor
    private static volatile SafetyFlowMonitorValidator mInstance = null;

    // Storing monitoring sequences of safety related activities.
    private static Map<String, SafetyFlowContext> mFlowMap = new HashMap<String, SafetyFlowContext>();

    // Storing flow key of safety related activities.
    private ArrayList<String> mFlowKey = new ArrayList<String>();

    /**
     * Return the singleton instance of SafetyFlowMonitor
     * 
     * @see mInstance[out]
     * 
     * @return the [out] single instance of SafetyFlowMonitor
     *         Range: valid SafetyFlowMonitor object
     *         Unit: SafetyFlowMonitor
     *         Scaling: 1
     */
    public static synchronized SafetyFlowMonitorValidator getInstance()
    {
        if (null == mInstance)
        {
            mInstance = new SafetyFlowMonitorValidator();
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }

    /**
     * Start to monitor the specific sequence flow. The initial state of
     * sequence will be sequence start.
     * Use a map capacity to save needed monitoring sequences of safety flow.
     * This capacity uses a unique key to record every monitoring sequences.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param flowpath [in] the non-null specific flow path
     *            Range : valid FlowPath object
     *            Unit: FlowPath
     *            Scaling: 1
     * @param nGroupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     */
    public synchronized void startMonitorFlow(Context context,
            FlowPath flowpath, MonitorGroup nGroupId)
    {
        String key = null;
        int length = 0;
        MONITOR[] monitors = null;

        // Valid input object
        CommonUtils.objectCheck(context, flowpath);

        key = generateKey(flowpath, nGroupId.getHammingId());

        length = nGroupId.getMonitorIdLength();
        monitors = new MONITOR[length];
        nGroupId.getMonitorId(monitors);

        Debug.printI(TAG_SFMID,
                "[ start]" + nGroupId + "[0]" + monitors[0].toString());

        mFlowMap.put(key, new SafetyFlowContext(context, nGroupId));
        mFlowKey.add(key);
    }

    /**
     * Stop monitoring the specific sequence flow. It will report sequence end
     * state to check whether it can end sequence from current state.
     * If end flow state inputs the parameter which is ENDFLOW id , it will skip
     * to
     * monitor current
     * sequence to directly finish end flow.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param flowpath
     *            [in] the non-null specific flow path
     *            Range : valid FlowPath object
     *            Unit: FlowPath
     *            Scaling: 1
     * @param nGroupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * 
     * @param stopFlow [in] the non-null next flow state of specific monitor
     *            sequence
     *            Range: valid ISafetyFlowMonitorState object
     *            Unit: ISafetyFlowMonitorState
     *            Scaling: 1
     */
    public synchronized SafetyBoolean stopMonitorFlow(final Context context,
            final FlowPath flowpath, final MonitorGroup nGroupId,
            final MONITOR nStopId)
    {
        int expectedStopId = 0;
        int defaultStopId = 0;
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;
        String key = null;
        ISafetyFlowMonitorState stopFlow = nStopId.getState(context);

        // Valid object
        CommonUtils.objectCheck(context, flowpath);

        // Generate key of critical flow path.
        key = generateKey(flowpath, nGroupId.getHammingId());

        // Get stop Id
        expectedStopId = stopFlow.getMonitorId();
        defaultStopId = MONITOR.ENDFLOW.getHammingId();

        // Check the expected-id is equal to default stop-id.
        if (expectedStopId == defaultStopId)
        {
            isResultOK = SafetyBoolean.TRUE;

            Debug.printI(TAG_SFMID,
                    "[stop]" + nGroupId + " " + MONITOR.ENDFLOW.name());
        }
        else
        {
            isResultOK = monitorState(context, flowpath, nGroupId, nStopId);
            Debug.printI(TAG_SFMID, "[stop]" + nGroupId);
        }

        // Remove the context from flow map.
        mFlowMap.remove(key);

        // Restore internal state
        SafetyFlowMonitoring.setWaitForActivitiesState(SafetyBoolean.TRUE);

        return isResultOK;
    }

    /**
     * Report the current state of specific sequence to Safety Flow Monitor.
     * The Safety Flow Monitor uses the delivered sequence of ID to validate
     * whether next flow is the right sequence or not.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param flowpath [in] the non-null specific flow path
     *            Range : valid FlowPath object
     *            Unit: FlowPath
     *            Scaling: 1
     * @param nGroupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @param nExpectId [in] the non-null next flow state of specific
     *            monitor
     *            sequence
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * 
     * @see mMonitoringSequence [in]
     * 
     **/
    public synchronized SafetyBoolean monitorState(Context context,
            FlowPath flowpath, MonitorGroup nGroupId, MONITOR nExpectId)
    {
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;
        String key = generateKey(flowpath, nGroupId.getHammingId());
        SafetyFlowContext flowContext = null;

        // Valid object
        CommonUtils.objectCheck(context, flowpath, nGroupId, nExpectId);

        // Get context of critical path.
        flowContext = mFlowMap.get(key);

        if (flowContext != null)
        {
            isResultOK = flowContext.validate(nExpectId);

            Debug.printI(TAG, "Execute [monitorState] "
                    + flowContext.getMonitorGroup().name() + ",isResultOK:"
                    + isResultOK);

            // Remove non-relative flow context.
            if (isResultOK.equals(SafetyBoolean.TRUE))
            {
                removeOtherFlow(flowpath, nGroupId, nExpectId);
            }
            else
            {
                // Apply to the coding standard

                // debug message
                Debug.printI(TAG, "[monitorState] Fail Flow!!  nExpectId :"
                        + nExpectId.name() + ",isRemovedOK:" + isResultOK);
            }
        }
        else
        {
            isResultOK = SafetyBoolean.FALSE;
        }

        return isResultOK;
    }

    /**
     * Report the current state of specific sequence to Safety Flow Monitor.
     * The Safety Flow Monitor uses the delivered sequence of ID to validate
     * whether next flow is the right sequence or not.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param flowpath [in] the non-null specific flow path
     *            Range : valid FlowPath object
     *            Unit: FlowPath
     *            Scaling: 1
     * @param nGroupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @param nAssignFlow [in] Set specific monitoring sequence
     *            Range: valid MONITOR object
     *            Unit: MONITOR
     *            Scaling: 1
     * 
     * @see mMonitoringSequence [in]
     * 
     **/
    public synchronized SafetyBoolean backwardStep(final Context context,
            final FlowPath flowpath, final MonitorGroup nGroupId,
            final MONITOR nAssignFlow)
    {
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;
        String key = generateKey(flowpath, nGroupId.getHammingId());
        SafetyFlowContext flowContext = null;

        // Valid object
        CommonUtils.objectCheck(context, flowpath, nGroupId, nAssignFlow);

        // Get context of flow
        flowContext = mFlowMap.get(key);

        Debug.printI(TAG_SFMID, "[backwardStep] To " + nAssignFlow.name());

        // Valid object
        if (null != flowContext)
        {
            isResultOK = flowContext.backwardStep(context, nGroupId,
                    nAssignFlow);
        }
        else
        {
            // Can't find id in monitoring flow sequence.
            isResultOK = SafetyBoolean.FALSE;
        }

        // debug
        if (isResultOK.equals(SafetyBoolean.FALSE))
        {
            Debug.printI(TAG_SFMID,
                    "[monitoring] Can't find id in monitoring flow");
        }

        return isResultOK;
    }

    /**
     * Check whether the specify monitor sequence is monitoring. It will
     * check whether the FlowContext is exist in the map If
     * FlowContext is existed, it will return SafetyBoolean.TRUE. Otherwise it
     * will return SafetyBoolean.FALSE
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param flowpath [in] the non-null specific flow path
     *            Range : valid FlowPath object
     *            Unit: FlowPath
     *            Scaling: 1
     * @param nGroupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @param nAssignFlow
     *
     * @return SafetyBoolean [out] Return SafetyBoolean.TRUE if the specify
     *         monitor sequence is monitoring. Otherwise return
     *         SafetyBoolean.FALSE
     *         Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean isMonitoring(Context context, FlowPath flowpath,
            MonitorGroup nGroupId, MONITOR nAssignFlow)
    {
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;
        String key = null;
        SafetyFlowContext flowContext = null;

        CommonUtils.objectCheck(context, flowpath);

        key = generateKey(flowpath, nGroupId.getHammingId());

        flowContext = mFlowMap.get(key);

        if (null != flowContext)
        {
            isResultOK = SafetyBoolean.TRUE;
        }
        else
        {
            isResultOK = SafetyBoolean.FALSE;

            // debug
//            Debug.printI(TAG, "Monitor array :" + mFlowMap.toString() + ",size"
//                    + mFlowMap.size() + ",nGroupId:" + nGroupId
//                    + ",nAssignFlow:" + nAssignFlow);
        }

        return isResultOK;
    }

    /**
     * Generate the unique key with the format flow path name "_" monitor group
     * id
     * 
     * @param flowpath [in] the non-null specific flow path
     *            Range : valid FlowPath object
     *            Unit: FlowPath
     *            Scaling: 1
     * @param nGroupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @return String [out] the string with format is flow path name_id
     *         Range: valid String object
     *         Unit: String
     *         Scaling: 1
     */
    protected String generateKey(FlowPath flowpath, int nGroupId)
    {
        String key = null;

        CommonUtils.objectCheck(flowpath, nGroupId);

        key = String.format("%s_%d", flowpath.name(), nGroupId);

        return key;
    }

    public SafetyBoolean isMonitoringExisted(final MONITOR nExpectedId)
    {
        byte isFlowOKByte = mByteSafetyTRUE;
        SafetyFlowContext flowContext = null;
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;

        CommonUtils.objectCheck(nExpectedId);

        for (String key : mFlowKey)
        {
            flowContext = mFlowMap.get(key);

            // Valid object
            if (flowContext != null)
            {
                isResultOK = isFlowValidated(flowContext, nExpectedId);

                isFlowOKByte = isResultOK.getByte();

                // This monitoring state exists in the flow map .
                if (isFlowOKByte == mByteSafetyTRUE)
                {
                    Debug.printI(TAG, "This monitoring state exist in "
                            + flowContext.getMonitorGroup().name() + " Context");

                    break;
                }
            }
            else
            {
                isResultOK = SafetyBoolean.FALSE;
            }
        }

        return isResultOK;
    }

    protected SafetyBoolean removeOtherFlow(final FlowPath flowpath,
            final MonitorGroup nGroupId, final MONITOR nExpectedId)
    {
        byte isFlowOKByte = mByteSafetyTRUE;
        SafetyFlowContext flowContext = null;
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;
        String currentKey = generateKey(flowpath, nGroupId.getHammingId());
        SafetyFlowContext currentFlowContext = mFlowMap.get(currentKey);
        ArrayList<String> removeMap = new ArrayList<String>();

        CommonUtils.objectCheck(nGroupId, nExpectedId);

        for (String key : mFlowKey)
        {
            flowContext = mFlowMap.get(key);

            // Valid object
            if (flowContext != null)
            {
                // Skip current context flow.
                if (flowContext.equals(currentFlowContext))
                {
                    continue;
                }

                isFlowOKByte = isFlowValidated(flowContext, nExpectedId)
                        .getByte();

                if (isFlowOKByte == mByteSafetyFALSE)
                {
                    Debug.printI(TAG, "Prepare remove "
                            + flowContext.getMonitorGroup().name() + " Context");

                    removeMap.add(key);
                }
            }
            else
            {
                isResultOK = SafetyBoolean.FALSE;
            }
        }

        // Check size .
        if (removeMap.size() > 0)
        {
            removeMap(removeMap);
        }
        else
        {
            removeMap.clear();
        }

        return isResultOK;
    }

    protected void removeMap(ArrayList<String> removelist)
    {
        // Remove flow context from flow map.
        for (String item : removelist)
        {
            mFlowMap.remove(item);
            mFlowKey.remove(item);

        }

        // For debug
        for (String key : mFlowKey)
        {
            SafetyFlowContext flowContext = mFlowMap.get(key);

            // Valid object
            if (flowContext != null)
            {
                Debug.printI(TAG, "Remain flow :"
                        + flowContext.getMonitorGroup().name());
            }
        }
    }

    protected SafetyBoolean isFlowValidated(SafetyFlowContext flowContext,
            MONITOR nExpectedId)
    {
        SafetyBoolean isResultOK = SafetyBoolean.TRUE;

        if (flowContext != null)
        {
            isResultOK = flowContext.validateContext(nExpectedId);
        }
        else
        {
            isResultOK = SafetyBoolean.FALSE;
        }

        return isResultOK;
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
