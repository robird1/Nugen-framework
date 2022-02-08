/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: TimeoutTimer
 * Brief: Implement the count down time-out timer
 *
 * Create Date: 10/06/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.selftest;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * Set a time out timer for synchronized signal of thread.
 */
public class TimeoutTimer implements Runnable
{

    // Debug tag
    private static final String TAG = "TimeoutTimer";

    // Define no responded timeout timer setting.
    private static final long DEFAULT_TIME_OUT_SEC = 10000L;

    // Define interval tick time
    private static final long DEFAULT_INTERVAL_TICK_TIME = 100L;

    // The byte value of SafetyBoolean TRUE
    private static byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // The byte value of SafetyBoolean FALSE
    private static byte mByteSafetyFALSE = SafetyBoolean.FALSE.getByte();

    // Define the flag to record whether the time out is occurred or not.
    private SafetyBoolean mIsTimeoutOccurred = SafetyBoolean.FALSE;

    // Point to context activity in android.
    private Context mContext = null;

    // Point to a message loop is through the Handler class.
    private Looper mLooper = null;

    // Handler message for posting delayed
    private Handler mHandler = null;

    // Stop count down timer.
    private byte mCancel = mByteSafetyFALSE;

    // Define the flag to record the function is done, when the time-out is
    // occurred.
    private byte mFinish = mByteSafetyFALSE;

    // Define the count-down count
    private long mTimeCount = 0L;

    // Callback to timeout listener .
    private TimeoutListener mListener;

    /**
     * constructor
     * 
     * @param context
     *            [in] : context of application or activity in android.
     *            Range : context is equal to valid context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @see mContext [in]
     * 
     * @return None
     * 
     */
    public TimeoutTimer(final Context context)
    {
        initial(context);
    }

    /**
     * Initialization data.
     * 
     * @param context
     *            [in] : context of application or activity in android.
     *            Range : context is equal to valid context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @see mCancel[out]
     *      mFinish[out]
     *      mContext[out]
     *      mLooper[out]
     *      handler[out]
     *      mTimeCount[out]
     * 
     * @return None
     * 
     */
    private void initial(final Context context)
    {
        // initial object
        mCancel = mByteSafetyFALSE;
        mFinish = mByteSafetyFALSE;

        // get looper of main thread and handler of android
        mContext = context.getApplicationContext();
        mLooper = mContext.getMainLooper();
        mHandler = new Handler(mLooper);

        // reset timer count
        mTimeCount = 0;
    }

    /**
     * start time-out timer
     * 
     * 
     * @see handler[in]
     *      mTimeOutObject[out]
     * 
     * @return None
     * 
     */
    public void start()
    {
        Debug.printI(TAG, "new start method");

        // Valid object
        CommonUtils.objectCheck(mHandler);
        Debug.printI(TAG, "postDelayed : " + DEFAULT_TIME_OUT_SEC);
        mHandler.postDelayed(this, 0);
    }

    /**
     * Cancel time-out timer
     * 
     * @see mTimeOutObject[in]
     *      handler[in]
     *      mCancel[out]
     * 
     * @return None
     */
    public void cancel()
    {
        // Cancel time-out timer flag.
        mCancel = mByteSafetyTRUE;

        // Remove timeout callback
        Debug.printI(TAG, "[cancel] : Remove timeout callback");

    }

    /**
     * Get the status which the time-out is occurred.
     * 
     * @return SafetyBoolean
     *         [out] : return if the time-out is occurred.
     *         Range :valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     * 
     */
    public SafetyBoolean getResult()
    {
        return mIsTimeoutOccurred;
    }

    /**
     * Check whether the countdown timer is the end.
     * Do the tick-event during the countdown.
     * Do the finish-event when the time-out is occurred.
     * 
     * @see mCancel[in]
     *      mTimeCount[in]
     *      handler[in]
     *      mTimeOutObject[out]
     *      mFinish[out]
     * 
     * @return None
     */
    @Override
    public void run()
    {
        Debug.printI(TAG, "run()  " + mTimeCount);

        // check timeout count limitation
        if (mTimeCount < DEFAULT_TIME_OUT_SEC)
        {
            mTimeCount = mTimeCount + DEFAULT_INTERVAL_TICK_TIME;

            if (mCancel == mByteSafetyFALSE)
            {
                // Do event during on-tick.
                onTick();
                onTickListener();

                // Check timeout count everytime after an interval time.
                Debug.printI(TAG,
                        "Check timeout count everytime after an interval time.");

                if (null != mHandler)
                {
                    mHandler.postDelayed(this, DEFAULT_INTERVAL_TICK_TIME);
                }

            }
            else
            {
                // Apply to the coding standard
            }

        }
        // The timeout timer is cancelled.
        else if (mCancel == mByteSafetyFALSE)
        {
            Debug.printI(TAG, "[Timeout is occurred]");

            // Timeout is occurred.
            mIsTimeoutOccurred = SafetyBoolean.TRUE;
            onFinish();
            onFinishListener();
        }
        else
        {
            mFinish = mByteSafetyFALSE;
        }
    }

    /**
     * If time-out is occurred, do this function.
     * 
     * @see mFinish[out]
     * 
     * @return None
     */
    public void onFinish()
    {
        mFinish = mByteSafetyTRUE;
    }

    /**
     * Callback fired on regular interval.
     * 
     * @return None
     * 
     */
    public void onTick()
    {
        // This function is implemented by the self-test item
    }

    /**
     * Release all object and restore to unknown status.
     * 
     * @see mCancel[out]
     *      mFinish[out]
     *      mTimeCount[out]
     *      mContext[out]
     *      mLooper[out]
     *      handler[out]
     * 
     * @return None
     */
    public void release()
    {
        // release object
        mCancel = mByteSafetyTRUE;
        mFinish = mByteSafetyFALSE;
        mTimeCount = 0;
//        mHandler.removeCallbacks(this);
        mContext = null;
        mLooper = null;
        mHandler = null;
    }

    /**
     * Set timeout count callback function.
     * 
     * @param listener [in] set the object of TimeoutListener.
     *            Range :valid TimeoutListener object
     *            Unit: TimeoutListener
     *            Scaling: 1
     * 
     * @see mListener[out]
     * 
     * @return None
     */
    public void setListener(TimeoutListener listener)
    {
        // Valid object
        CommonUtils.objectCheck(listener);

        // Set object.
        mListener = listener;
    }

    /**
     * Callback to listener call the function "onFinish" if the object of
     * TimeoutListener is not null.
     * 
     * @see mListener[in]
     * 
     * @return None
     */
    private void onFinishListener()
    {
        if (mListener != null)
        {
            mListener.onFinish();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Callback to listener call the function "onTick" if the object of
     * TimeoutListener is not null.
     * 
     * @see mListener[in]
     * 
     * @return None
     */
    private void onTickListener()
    {
        if (mListener != null)
        {
            mListener.onTick();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    // Define a interface during count down timer.
    public interface TimeoutListener
    {
        /**
         * If time-out is occurred, do this function.
         * 
         * @see mFinish[out]
         * 
         * @return None
         */
        public void onFinish();

        /**
         * Callback fired on regular interval.
         * 
         * @return None
         */
        public void onTick();
    }

}