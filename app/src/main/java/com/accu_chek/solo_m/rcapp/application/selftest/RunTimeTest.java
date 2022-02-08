/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: RunTimeTest
 * Brief: Implement run-time self-test items.
 *
 * Create Date: 07/02/2015
 * $Revision: 21782 $
 * $Author: JamesLee $
 * $Id: RunTimeTest.java 21782 2015-10-19 02:10:51Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.selftest;

import android.content.Context;

import android.os.ConditionVariable;

import com.accu_chek.solo_m.rcapp.application.challenge.Challenge;
import com.accu_chek.solo_m.rcapp.application.challenge.Challenge.State;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class RunTimeTest extends Selftest implements Challenge.IChallenge
{
    
    private static final String TAG = "RunTimeTest";

    // Define the instance of SafetyFlowMonitor
    private static RunTimeTest mInstance = null;

    // The object of UI-Challenge
    private static Challenge mChallengeCtrl = null;

    // The byte value of SafetyBoolean TRUE
    private static byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // Point to context of application in android.
    private Context mContext = null;

    // For synchronized mPostResult data
    private final ConditionVariable mUIChallengeCBExist = new ConditionVariable();

    // For synchronized initialization ready signal of ui-challenge
    private final ConditionVariable mUIChallengeInitialReady = new ConditionVariable();

    // Record the ui-challenge's initialization ready flag.
    private SafetyBoolean mIsInitChallenge = SafetyBoolean.FALSE;

    /**
     * constructor
     * 
     * @param context
     *            [in] : context of application or activity in android.
     *            Range : context is equal to valid context object
     *            Unit: context
     *            Scaling: 1
     * 
     * @see mContext [in]
     * 
     * @return None
     */
    public RunTimeTest(final Context context)
    {
        super(context);

        // initial run-time test
        initial(context);
    }

    /**
     * Return the singleton instance of RunTimeTest
     * 
     * 
     * @see mInstance[out]
     * 
     * @return the [out] single instance of RunTimeTest
     *         Range: valid RunTimeTest object
     *         Unit: RunTimeTest
     *         Scaling: 1
     */
    public static synchronized RunTimeTest getInstance(final Context context)
    {
        if (null == mInstance)
        {
            mInstance = new RunTimeTest(context);
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }

    /**
     * Initialize the run-time test that needs the objects
     * 
     * @param context
     *            [in] : the context of activity in android.
     *            Range :valid context object
     *            Unit: context
     *            Scaling: 1
     * 
     * @see mContext [out]
     *      challengeCtrl [out]
     * 
     * @return None
     */
    private void initial(final Context context)
    {
        mContext = context.getApplicationContext();

        if (null == mChallengeCtrl)
        {
            // Initialized UI-Challenge
            mChallengeCtrl = new Challenge(mContext);
            mChallengeCtrl.setChallengeListen(RunTimeTest.this);
            mChallengeCtrl.initial();
            // wait ui-challenge until initialization finish.
            mIsInitChallenge = isInitialChallengeOK();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Add all registered self-test items by interface ISelfTestListener in
     * run-time test sequence
     * 
     */
    public void hook()
    {
        // Define SelfTest_UIChallenge's object is one of self-test items
        final SelftestUIChallenge requestChallenge = new SelftestUIChallenge();

        super.addListener(requestChallenge);
    }

    /** 
     * Implement interface callback to do ALU test
     *
     * @param add[in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * @param sub [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * @param mul [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * @return SafetyChannel<Integer> [out] Return calculated result of ALU
     *         Range: -2^31-1 ~ 2^31-1
     *         Unit: SafetyChannel<Integer>
     *         Scaling:1
     */
    @Override
    public SafetyChannel<Integer> doALUTest(SafetyChannel<Integer> add,
            SafetyChannel<Integer> sub, SafetyChannel<Integer> mul)
    {
        SafetyChannel<Integer> result = null;

        // call ALU calculated from native
        result = nativeALUcalcuated(add, sub, mul);

        return result;
    }
    
    /**
     * Get UI-Challenge's flow state
     * 
     * @param state
     *            [in] : object of State
     *            Range : valid State object.
     *            Unit: State
     *            Scaling:1
     * 
     * @see mUIChallengeCBExist[in]
     *      mUIChallengeInitialReady[in]
     * 
     * @return None
     */
    @Override
    public void stateChange(State state)
    {
        Debug.printI(TAG, "[CallBack] stateChange : " + state);

        // Finish UI-Challeng cycle
        if (state.equals(State.STATE_END))
        {
            Debug.printI(TAG, "[sync. mStateChange] <<<< unLock notify ");
            // unlock synchronized signal
            mUIChallengeCBExist.open();
        }
        else if (state.equals(State.STATE_INITIAL_READY))
        {
            // unlock synchronized signal
            mUIChallengeInitialReady.open();
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * Call native JNI to implement ALU test
     *
     * @param add [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * @param sub [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * @param mul [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * @return SafetyChannel<Integer> [out] Return calculated result of ALU
     *         Range: -2^31-1 ~ 2^31-1
     *         Unit: SafetyChannel<Integer>
     *         Scaling:1
     */
    private SafetyChannel<Integer> nativeALUcalcuated(
            SafetyChannel<Integer> add, SafetyChannel<Integer> sub,
            SafetyChannel<Integer> mul)
    {
        int paraAdd = 0;

        int paraSub = 0;

        int paraMul = 0;

        int calcuteResult = 0;

        // Valid object
        CommonUtils.objectCheck(add, sub, mul);

        // Get parameter data
        paraAdd = CommonUtils.getOriginValue((Integer) add.getValueCH1(),
                (Integer) add.getValueCH2());
        paraSub = CommonUtils.getOriginValue((Integer) sub.getValueCH1(),
                (Integer) sub.getValueCH2());
        paraMul = CommonUtils.getOriginValue((Integer) mul.getValueCH1(),
                (Integer) mul.getValueCH2());

        // Calculated ALU test result
        calcuteResult = (paraAdd + paraSub) * paraMul;

        // SelftestNative.getInstance();
        // calcuteResult = SelftestNative.cpuTest(para_Add, para_Sub, para_Mul);

        Debug.printI(TAG, "callback doALUTest - calcuteResult:" + calcuteResult);

        // apply safety channel 
        final SafetyChannel<Integer> safetyCalcResult = CommonUtils
                .getSafetyChannel(calcuteResult);

        return safetyCalcResult;
    }

    /**
     * Wait UI-Challenge until initialization finish
     * 
     * @param N/A
     * 
     * @see mUIChallengeInitialReady[in]
     * 
     * @return SafetyBoolean
     *         [out] : return the result of initialization
     *         Range :valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    private SafetyBoolean isInitialChallengeOK()
    {
        final SafetyBoolean isResultOK = SafetyBoolean.TRUE;

        // wait UI Challenge initial ready
        mUIChallengeInitialReady.block();

        // end synchronized signal
        mUIChallengeInitialReady.close();

        return isResultOK;
    }

    /**
     * one of power on seft-test items - configuration
     */
    class SelftestUIChallenge implements ISelftestListener
    {
        
        private TimeoutTimer mCheckTimeOutTimer = null;

        /**
         * Status: Coding
         * 
         * Implement the action of the assigned self-test item here
         * send request for challenge to communication processor.
         * 
         * @see challengeCtrl[in]
         *      mCheckTimeOutTimer[in]
         *      mUIChallengeCBExist[in]
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range :valid SafetyBoolean object
         *         Unit:SafetyBoolean
         *         Scaling: 1
         * 
         * */
        @Override
        public SafetyBoolean doAction()
        {
            byte isTimeoutOccurred = 0;

            byte initChallengeByte = 0;

            SafetyBoolean isResultOK = SafetyBoolean.TRUE;

            // Valid object
            CommonUtils.objectCheck(mChallengeCtrl, mUIChallengeCBExist);

            isResultOK = mIsInitChallenge;
            initChallengeByte = mIsInitChallenge.getByte();

            // Until ui-challenge initialization ready to trigger ui challenge
            if (initChallengeByte == mByteSafetyTRUE)
            {
                // start request-challenge scheme between meter and
                // communication processor.
                mChallengeCtrl.runRequestForChallenge();

                // Asynchronous needs a timer-out counter
                mCheckTimeOutTimer = new TimeoutTimer(mContext)
                {   
                    @Override
                    public void onFinish()
                    {
                        // time-out is occurred.
                        mUIChallengeCBExist.open();                     
                    }           
                };

                // start time out
                mCheckTimeOutTimer.start();

                // lock signal wait synchronized ready
                mUIChallengeCBExist.block();

                // check whether the time out is occurred
                isTimeoutOccurred = mCheckTimeOutTimer.getResult().getByte();

                // check timeout is occurred.
                if (isTimeoutOccurred == mByteSafetyTRUE)
                {                    
                    isResultOK = SafetyBoolean.FALSE;                   
                }
                else
                {
                    // Apply to the coding standard
                }

                // release time-out object
                mCheckTimeOutTimer.cancel();
                mCheckTimeOutTimer = null;

                Debug.printI(TAG, "[sync. mStateChange] >> unLock     ");

                // close synchronized signal
                mUIChallengeCBExist.close();
            }

            Debug.printI(TAG, "[isResultOK] checkRequestForChallenge  ? "
                    + isResultOK);

            return isResultOK;
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