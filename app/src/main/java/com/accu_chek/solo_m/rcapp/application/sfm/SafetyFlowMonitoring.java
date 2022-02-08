/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SafetyFlowMonitoring
 * Brief: Provide methods as "start","stop" for safety related activities.
 *
 * Create Date: 07/02/2015
 * $Revision: 25273 $
 * $Author: JamesLee $
 * $Id: SafetyFlowMonitoring.java 25273 2015-12-01 12:55:41Z JamesLee $
 */
package com.accu_chek.solo_m.rcapp.application.sfm;

import java.util.HashMap;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SafetyFlowMonitoring
{

    // Define debug tag
    private static final String TAG = "SFM";

    // Define debug tag
    private static final String TAG_SFMID = "SFMID";

    // Define the instance of SafetyFlowMonitoring
    private static volatile SafetyFlowMonitoring mInstance = null;

    // The byte value of SafetyBoolean TRUE
    private static byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // The byte value of SafetyBoolean FALSE
    private static byte mByteSafetyFALSE = SafetyBoolean.FALSE.getByte();

    // Record whether the error is occurred within the safety flow monitor
    private static SafetyBoolean mIsErrorOccurred = SafetyBoolean.FALSE;

    // Define the object of NotifyMessage to do safe state
    private static NotifyMessage mNotifyMessage = null;

    // Define to check whether the flag is doing the run time test of self-test
    // or not.
    private static SafetyBoolean mIsRunTimeTestDoing = SafetyBoolean.FALSE;

    // Define internal state to perform safety relative activities
    private static SafetyBoolean mIsWaitingForActivities = SafetyBoolean.TRUE;

    // private static HashMap<K, V>
    private EMWRList mEMWRId = EMWRList.EMW41501;

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
    public static synchronized SafetyFlowMonitoring getInstance()
    {
        if (null == mInstance)
        {
            mInstance = new SafetyFlowMonitoring();
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }

    /**
     * Start to monitor the specific sequence flow.
     * Only allow one flow monitor group in flow monitor sequence.
     * 
     * @param context
     *            [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param nGroupId
     *            [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * 
     * @see mContext[out]
     * 
     * 
     */
    public void start(final Context context, final MonitorGroup nGroupId)
    {
        final FlowPath flowpath = FlowPath.ACTIVITY_CRITICAL_PATH;
        SafetyBoolean isMonitorInlife = SafetyBoolean.TRUE;
        byte isMonitorInlifeByte = mByteSafetyFALSE;

        SafetyFlowMonitorValidator sfmValidator = SafetyFlowMonitorValidator
                .getInstance();

        isMonitorInlife = isMonitoringActivity(context, nGroupId, null);

        isMonitorInlifeByte = isMonitorInlife.getByte();

        // Current flow monitoring sequence is life in SFM.
        if (isMonitorInlifeByte == mByteSafetyFALSE)
        {
            sfmValidator.startMonitorFlow(context, flowpath, nGroupId);

            // test Get emwr id of SFM component.
            mEMWRId = getEMWRListID(nGroupId);
        }
        else
        {
            // Apply to the coding standard
        }

    }

    /**
     * Stop monitor the specific activity-critical-path sequence flow.
     * To do safe state check through the result after end flow procedures.
     * 
     * @param context
     *            [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param nGroupId
     *            [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @param stopFlow
     *            [in] the non-null next flow state of specific monitor
     *            sequence
     *            Range: valid ISafetyFlowMonitorState object
     *            Unit: ISafetyFlowMonitorState
     *            Scaling: 1
     */

    public void stop(final Context context, final MonitorGroup nGroupId,
            final MONITOR expectedFlow)
    {
        final FlowPath activityFlowPath = FlowPath.ACTIVITY_CRITICAL_PATH;
        SafetyBoolean isResultOK = SafetyBoolean.TRUE;
        SafetyBoolean isMonitorInlive = SafetyBoolean.FALSE;
        byte isMonitorInlifeByte = mByteSafetyFALSE;

        ISafetyFlowMonitorState stopFlow = expectedFlow.getState(context);

        CommonUtils.objectCheck(context, nGroupId, stopFlow);

        // Debug.printI(TAG, "[stop]" + expectedFlow.name());

        isMonitorInlive = isMonitoringActivity(context, nGroupId, expectedFlow);
        isMonitorInlifeByte = isMonitorInlive.getByte();

        if (isMonitorInlifeByte == mByteSafetyTRUE)
        {
            isResultOK = SafetyFlowMonitorValidator.getInstance()
                    .stopMonitorFlow(context, activityFlowPath, nGroupId,
                            expectedFlow);
        }
        else
        {
            isResultOK = SafetyBoolean.TRUE;
        }

        doSafeStateCheck(nGroupId, isResultOK);

        // debug
        if (isResultOK.equals(SafetyBoolean.FALSE))
        {
            Debug.printI(TAG_SFMID, "[stop] SFM Fail start EMWR !! gID:"
                    + nGroupId + ",mID:" + expectedFlow.name());
        }
    }

    /**
     * Report the current state of specific sequence to Safety Flow Monitor
     * if the current state of specific sequence is delivered to Safety Flow
     * Monitor and when get verified result is fail to enter safe state
     * 
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param nGroupId [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @param nextFlow [in] the non-null next flow state of specific monitor
     *            sequence
     *            Range: valid ISafetyFlowMonitorState object
     *            Unit: ISafetyFlowMonitorState
     *            Scaling: 1
     * 
     */
    public void monitoring(final Context context, final MonitorGroup nGroupId,
            final MONITOR nNextFlow)
    {
        final FlowPath activityFlowPath = FlowPath.ACTIVITY_CRITICAL_PATH;
        SafetyBoolean isResultOK = SafetyBoolean.TRUE;
        SafetyBoolean isMonitorInlive = SafetyBoolean.FALSE;
        byte isMonitorInlifeByte = mByteSafetyFALSE;

        CommonUtils.objectCheck(context, activityFlowPath);

        isMonitorInlive = isMonitoringActivity(context, nGroupId, nNextFlow);
        isMonitorInlifeByte = isMonitorInlive.getByte();

        if (isMonitorInlifeByte == mByteSafetyTRUE)
        {
            isResultOK = SafetyFlowMonitorValidator.getInstance().monitorState(
                    context, activityFlowPath, nGroupId, nNextFlow);
        }
        else
        {
            isResultOK = SafetyFlowMonitorValidator.getInstance()
                    .isMonitoringExisted(nNextFlow);
        }

        doSafeStateCheck(nGroupId, isResultOK);

        // debug
        if (isResultOK.equals(SafetyBoolean.FALSE))
        {
            Debug.printI(TAG_SFMID, "[monitoring] SFM Fail start EMWR !! gID:"
                    + nGroupId + ",mID:" + nNextFlow.name());
        }
    }

    /**
     * To check the flow monitor sequence of the current state is registered in
     * Safety Flow Monitor flow context array list.
     * 
     * @param context
     *            [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param nGroupId
     *            [in] the specific id of monitor group that requires to be
     *            monitored
     *            Range: valid MonitorGroup object
     *            Unit: MonitorGroup
     *            Scaling: 1
     * @param nAssignFlow
     * 
     */
    protected SafetyBoolean isMonitoringActivity(final Context context,
            final MonitorGroup nGroupId, MONITOR nAssignFlow)
    {
        SafetyBoolean isMontoring = SafetyBoolean.FALSE;
        FlowPath activityFlowPath = FlowPath.ACTIVITY_CRITICAL_PATH;
        SafetyFlowMonitorValidator sfmValidator = SafetyFlowMonitorValidator
                .getInstance();

        CommonUtils.objectCheck(context, activityFlowPath);

        isMontoring = sfmValidator.isMonitoring(context, activityFlowPath,
                nGroupId, nAssignFlow);

//        if (isMontoring.equals(SafetyBoolean.FALSE))
//        {
//            Debug.printI(TAG, "[Not exist]  [gID]: "
//                    + nGroupId.name());
//        }
        return isMontoring;

    }

    /**
     * Report the error state to Safety Flow Monitor.
     * During safety flow monitor gets the result is not OK.
     * The main processor will create a thread to enter safe state procedures.
     * 
     * @param nGroupId
     * 
     * @param errorState [in] set error state to Safety Flow Monitor
     *            SafetyBoolean.TRUE means error occurred
     *            Range : valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @see mIsErrorOccurred[out]
     *      mNotifyEMRWThread[out]
     * 
     * 
     */
    public void reportErrorState(final SafetyBoolean errorState)
    {
        NotifyMessage notifyMessage = null;

        mIsErrorOccurred = errorState;

        // Create a service thread to handle EMWR
        if (mIsErrorOccurred.equals(SafetyBoolean.TRUE))
        {
            // EMWR

            // notifyMessage = new NotifyMessage(mEMWRId);
            // NotifyProxy.showEMWR(notifyMessage);

        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Report the safety flow monitor status to communication processor within
     * the response to the ui-challenge.
     * This status includes the internal status of the flag
     * "waiting for activities".
     * 
     * 
     * @see mWaitingForActivities[in],
     *      mIsRunTimeTestDoing[in],
     *      mIsErrorOccurred[in]
     * 
     * 
     * 
     * @return result [out] get error state of Safety Flow Monitor
     *         Range : valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean getStatus()
    {
        SafetyBoolean isErrorOccurred = SafetyBoolean.FALSE;
        byte isErrorOccurredByte = mByteSafetyFALSE;
        byte isRunTimeTestDoingByte = mByteSafetyFALSE;
        byte isWaitingForActivitiesByte = mByteSafetyFALSE;

        isErrorOccurredByte = mIsErrorOccurred.getByte();
        isRunTimeTestDoingByte = mIsRunTimeTestDoing.getByte();
        isWaitingForActivitiesByte = mIsWaitingForActivities.getByte();

        if (isErrorOccurredByte == mByteSafetyTRUE)
        {
            isErrorOccurred = SafetyBoolean.TRUE;
        }

        // The meter is at standy mode and received challenge
        // in regulator cycle from communication procssor.
        else if (isRunTimeTestDoingByte == mByteSafetyTRUE)
        {
            if (isWaitingForActivitiesByte == mByteSafetyTRUE)
            {
                isErrorOccurred = SafetyBoolean.TRUE;
                Debug.printI(TAG, "[Check Point] internal state sfmstatus:"
                        + isErrorOccurred);
            }
            else
            {
                isErrorOccurred = SafetyBoolean.FALSE;
            }
        }
        else
        {
            isErrorOccurred = SafetyBoolean.FALSE;
        }

        return isErrorOccurred;
    }

    /**
     * Set internal state waiting for activities flag.
     * When this flag is set to true , the Communications process will execute
     * main processor error handling
     * within the responses to the "challenge".
     * 
     * @param waitforactivity [in] Set internal flag state
     *            Range : valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @see mWaitingForActivities[out]
     * 
     */
    public static void setWaitForActivitiesState(
            final SafetyBoolean waitforactivity)
    {
        mIsWaitingForActivities = waitforactivity;
    }

    /**
     * Set the flag to record whether the run time test is doing or not
     * when deliver the ID of sequence with tags to do runtime test.
     * To finish the runtime test, shall set the flag to false.
     * 
     * 
     * @param isRunTimeTestDoing
     *            [in] record the run time test is running
     *            Range : valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @see isRunTimeTestDoing[out]
     * 
     **/
    public static void setRunTimeTest(final SafetyBoolean isRunTimeTestDoing)
    {
        mIsRunTimeTestDoing = isRunTimeTestDoing;
        Debug.printI(TAG, "[Check Point] isRunTimeTestDoing :  "
                + isRunTimeTestDoing);
    }

    /**
     * Back to assign monitoring step in safety flow monitoring.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
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
     */
    public SafetyBoolean backwardStep(final Context context,
            final MonitorGroup nGroupId, final MONITOR nAssignFlow)
    {
        final FlowPath activityFlowPath = FlowPath.ACTIVITY_CRITICAL_PATH;
        SafetyBoolean isResultOK = SafetyBoolean.TRUE;
        SafetyBoolean isMonitorInlive = SafetyBoolean.FALSE;
        byte isMonitorInlifeByte = mByteSafetyFALSE;

        // Valid object
        CommonUtils.objectCheck(context);

        // Check whether the flow monitoring is build.
        isMonitorInlive = isMonitoringActivity(context, nGroupId, nAssignFlow);
        isMonitorInlifeByte = isMonitorInlive.getByte();

        if (isMonitorInlifeByte == mByteSafetyTRUE)
        {
            isResultOK = SafetyFlowMonitorValidator.getInstance().backwardStep(
                    context, activityFlowPath, nGroupId, nAssignFlow);
        }
        else
        {
            // Create flow monitoring sequence.
            start(context, nGroupId);

            isResultOK = SafetyFlowMonitorValidator.getInstance().backwardStep(
                    context, activityFlowPath, nGroupId, nAssignFlow);
        }

        return isResultOK;
    }

    /**
     * Start to do EMWR safe state or not by the result of monitor sequence.
     * 
     * @param nGroupId
     * 
     * @param isResultOK [in] set the result OK of safety flow monitor
     *            Range : valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     */
    protected void doSafeStateCheck(MonitorGroup nGroupId,
            final SafetyBoolean isResultOK)
    {
        // show EMWR message.
        if (isResultOK.equals(SafetyBoolean.FALSE))
        {
            Debug.printI(TAG_SFMID, "");

            reportErrorState(SafetyBoolean.TRUE);

            // Get emwr id of SFM component.
            mEMWRId = getEMWRListID(nGroupId);
        }
        else
        {
            reportErrorState(SafetyBoolean.FALSE);
        }
    }

    protected EMWRList getEMWRListID(final MonitorGroup nGroupId)
    {

        EMWRList emwrid = nGroupId.getMonitorFlow();

//        Debug.printI(TAG,
//                "[getEMWRListID]" + nGroupId.name() + "," + emwrid.getCodeId());

        return emwrid;
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */