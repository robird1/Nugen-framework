package com.accu_chek.solo_m.rcapp.application.util;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * Set a time out timer to check if the main processor does not receive an
 * answer from Comms Proc. within a specified time
 */
// Start the main activity thread
public abstract class AbstractTimeoutTask
{
    /**
     * 
     */
    private static final long ONE_SECOND = 1000L;

    // No responsed timeout timer setting.
    public static final long DEFAULT_TIME_OUT_SEC = 10000L;

    // The byte value of SafetyBoolean TRUE
    private static final byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // The byte value of SafetyBoolean FALSE
    private static final byte mByteSafetyFALSE = SafetyBoolean.FALSE.getByte();

    private String TAG = "TimeoutTask";

    Handler mHandler = null;

    private long mTimeCount = 0;
    private long mDelayTime = 0L;
    private RunnableTask mTask = new RunnableTask();

    /**
     * 
     */
    private class RunnableTask implements Runnable
    {

        /**
         * Status: Coding
         * 
         * If time out is occurred , call back to do this.
         * 
         * @see mCancel[in]
         *      mByteSafetyFALSE[in]
         *      mIsTimeoutOccurred[out]
         *      mFinish[out]
         * 
         */
        @Override
        public void run()
        {

            Debug.printI(TAG, "[run] enter");
            if (mTimeCount < mDelayTime)
            {
                Debug.printI(TAG, "[run] count");
                mTimeCount = mTimeCount + ONE_SECOND;
                mHandler.postDelayed(this, ONE_SECOND);
            }
            else
            {
                Debug.printI(TAG, "[run] finish");
                mTimeCount = 0L;
                onFinish();
            }

        }

    }

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
     */
    public AbstractTimeoutTask(Context context)
    {
        initial(context);
    }

    /**
     * Status: Coding
     * 
     * Initial data.
     * 
     * @param context
     *            [in] : context of application or activity in android.
     *            Range : context is equal to valid context object
     *            Unit: Context
     *            Scaling: 1
     * 
     */
    private void initial(final Context context)
    {
        Debug.printI(TAG, "[initial] enter");
        Looper looper = context.getMainLooper();

        mHandler = new Handler(looper);
        mTimeCount = 0L;
    }

    /**
     * Status: Coding
     * 
     * start time-out timer
     * 
     * @param timeoutObject
     *            [in] : point to the object of TimeoutTimer
     *            Range :valid TimeoutTimer object
     *            Unit: TimeoutTimer
     *            Scaling: 1
     * 
     */
    public void start(long delayTime)
    {
        Debug.printI(TAG, "[start] enter");

        // check valid delay time
        if (delayTime < 0L)
        {
            delayTime = DEFAULT_TIME_OUT_SEC;
        }

        mDelayTime = delayTime;

        // start delay process
        mHandler.postDelayed(mTask, 0L);
 
    }

    /**
     * Status: Coding
     * 
     * Cancel time-out timer
     * 
     * @param timeoutObject
     *            [in] : point to the object of TimeoutTimer
     *            Range :valid TimeoutTimer object
     *            Unit: TimeoutTimer
     *            Scaling: 1
     * 
     * @see mCancel[out]
     * 
     */
    public void cancel()
    {
        Debug.printI(TAG, "[cancel] enter");

        // Remove timeout callback
        mHandler.removeCallbacks(mTask);

        release();
    }

    /**
     * Status: Coding
     * 
     * If time-out is occurred , do this function.
     * 
     * @see mFinish[out]
     * 
     */
    public abstract void onFinish();

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void release()
    {
        // release object
        mTimeCount = 0L;
        mHandler = null;

    }

}