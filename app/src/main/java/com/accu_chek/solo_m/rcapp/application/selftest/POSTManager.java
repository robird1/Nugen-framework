/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: POSTManager
 * Brief: POSTManager is executed by framework service started.
 *
 * Create Date: 10/06/2015
 * $Revision: 24584 $
 * $Author: JamesLee $
 * $Id: POSTManager.java 24584 2015-11-23 03:31:53Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.selftest;

import android.content.Context;
import android.os.ConditionVariable;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.sfm.FlowPath;
import com.accu_chek.solo_m.rcapp.application.sfm.ISafetyFlowMonitorState;
import com.accu_chek.solo_m.rcapp.application.sfm.MONITOR;
import com.accu_chek.solo_m.rcapp.application.sfm.MonitorGroup;
import com.accu_chek.solo_m.rcapp.application.sfm.SafetyFlowMonitoring;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public final class POSTManager extends IPOSTManager.Stub
{

    // static final String TAG = POSTManager.class.getSimpleName();
    static final String TAG = "POSTManager";

    // The byte value of SafetyBoolean TRUE
    private static byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // The byte value of SafetyBoolean FALSE
    private static byte mByteSafetyFALSE = SafetyBoolean.FALSE.getByte();

    // Point to the object of POST
    private static POST mPOSTInstance = null;

    // Point to context activity in android.
    private Context mContext = null;

    // Point to the object of SafetyFlowMonitor
    private SafetyFlowMonitoring mSFM = null;

    // Define the object of the interface IPOSTResultListener
    private IPOSTResultListener mPostResult = null;

    // Handler needs uses with self-looper
    private RunSelfTestThread mRunTestThread = null;

    // For synchronized mPostResult data
    private final ConditionVariable mPOSTResultCBExist = new ConditionVariable();

    /**
     * constructor
     * 
     * @param context
     *            [in] : context of activity in android.
     *            Range :valid context object
     *            Unit: context
     *            Scaling: 1
     * 
     * @return None
     */
    public POSTManager(final Context context)
    {
        mContext = context;
    }

    /**
     * Initialize the POSTManager that needs the objects
     * 
     * @see mContext [out]
     *      mSFM [in]
     *      post [out]
     * 
     * @return None
     * 
     * @throws RemoteException [out]
     */
    @Override
    public void initial() throws RemoteException
    {
        final MonitorGroup nGroupId = MonitorGroup.POST_MONITOR;

        // Get SFM's instance and start flow monitor
        // the main processor CPU test will be executed after the POST
        // as a run-time test.
        mSFM = SafetyFlowMonitoring.getInstance();
        mSFM.start(mContext, nGroupId);

        mPOSTInstance = POST.getInstance(mContext);

    }

    /**
     * Provide the interface to set the ready state of communication processor.
     * 
     * @param a_CommsReadyResult [in] : point to IPOSTResulCallBack object
     *            Range :-2^31 - 2^31-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @see mContext[in]
     *      mPOSTInstance [out]
     * 
     * @return None
     * 
     * @throws RemoteException
     */
    @Override
    public void setCommsReadyState(int a_CommsReadyResult)
            throws RemoteException
    {
        mPOSTInstance = POST.getInstance(mContext);
        mPOSTInstance.setCommsPostResult(a_CommsReadyResult);
    }

    /**
     * Create a thread to do power on self-test items
     * 
     * @see mRunTestThread [out]
     * 
     * @return None
     */
    @Override
    public void runTest()
    {
        final ExecutorService service = Executors.newFixedThreadPool(1);

        Debug.printI(TAG, "[Run Process] execute runTest");

        // Start thread to do power on self-test items
        mRunTestThread = new RunSelfTestThread();
        service.execute(mRunTestThread);
    }

    /**
     * Provide the interface to register POST's result call-back function.
     * Register Post result call back.
     *
     * @param resultCB [in] : refer to IPOSTResultListener object
     *            Range :valid IPOSTResulCallBack object
     *            Unit: context
     *            Scaling: 1
     * 
     * @see mPostResult [out]
     *      mPOSTResultCBExist[in]
     * 
     * @return None
     * 
     * @throws RemoteException
     */
    @Override
    public void registerPOSTResultCB(IPOSTResultListener resultCB)
            throws RemoteException
    {
        // Valid object
        CommonUtils.objectCheck(resultCB);

        mPostResult = resultCB;

        // unlock synchronized signal
        // ensure the callback instance is initialized
        mPOSTResultCBExist.open();
    }

    /**
     * Need to do power on self-test in the thread.
     * If it doesn't use thread during power on self-test, the main thread will
     * be block.
     * Caused by using event to do synchronized signal within power on self-test
     */
    protected class RunSelfTestThread implements Runnable
    {

        /**
         * Do power on self-test all items.
         * 
         * @see mPOSTInstance[in]
         *      mContext[in]
         *      mPOSTResultCBExist[in]
         *      mPostResult[in]
         *      mSFM[in]
         * 
         * @return None
         */
        @Override
        public void run()
        {
            byte isResultOKByte = mByteSafetyFALSE;

            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            MonitorGroup nGroupId;

            ISafetyFlowMonitorState endFlow;

            // Valid object
            CommonUtils.objectCheck(mPOSTInstance);

            // start to do power on self-test
            isResultOK = mPOSTInstance.runTest();

            isResultOKByte = isResultOK.getByte();

            // Record the result to setting model.
            NugenSettingModel.setSafetyBoolean(mContext,
                    NugenFrameworkConstants.KEY_POST_RESULT_STATE, isResultOK);

            try
            {
                Debug.printI(TAG, "[Run Process] callback onFinish ");

                // Lock and wait synchronized signal
                // wait the callback instance is initialized
                mPOSTResultCBExist.block();

                // call finish post's flow
                mPostResult.onFinish();

                // reset lock
                mPOSTResultCBExist.close();
            }
            catch (RemoteException e)
            {
                isResultOKByte = mByteSafetyFALSE;
            }
            finally
            {
                // Apply to the coding standard
            }

            // An error occurred
            if (isResultOKByte == mByteSafetyFALSE)
            {
                SafetyFlowMonitoring.getInstance().reportErrorState(
                        SafetyBoolean.TRUE);
            }
            else
            {
                // stop flow monitor and do run-time test
                SafetyFlowMonitoring.getInstance().reportErrorState(
                        SafetyBoolean.FALSE);

                mSFM.stop(mContext, MonitorGroup.POST_MONITOR, MONITOR.POST_END);
            }
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