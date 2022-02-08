/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IDispatchCommand
 * Brief: Implement flight mode command via BLEController interface.
 *
 * Create Date: 10/06/2015
 * $Revision: 21298 $
 * $Author: JamesLee $
 * $Id: ISelftestListener.java 21298 2015-10-12 03:04:06Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.flight;

import android.content.Context;
import android.os.ConditionVariable;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.selftest.TimeoutTimer;
import com.accu_chek.solo_m.rcapp.application.selftest.TimeoutTimer.TimeoutListener;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.util.HashMap;

public class FlightCommand implements ResponseCallback,TimeoutListener
{

    // Defines debug tag.
    private static final String TAG = "FlightMode";

    // Define the instance of FlightCommand
    private static FlightCommand mFlightInstance = null;

    // Synchronized for command dispatch process.
    private final ConditionVariable mSyncCommand = new ConditionVariable();

    // Define the flag to record whether the time out is occurred or not.
    private SafetyBoolean mIsTimeoutOccurred = SafetyBoolean.FALSE;

    // Define dispatch command result.
    private SafetyBoolean mIsResultOK = SafetyBoolean.FALSE;

    // Point to context activity in android.
    private Context mContext = null;

    // Define a timeout timer object.
    private TimeoutTimer mCheckTimeOutTimer = null;

    // This HashMap is used to store the received intent value
    private HashMap<Integer, SetCommand> mCommandMap = null;

    /**
     * constructor
     * 
     * Initialize the flight mode that needs the objects
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return None
     */
    public FlightCommand(final Context context)
    {
        initialize(context);
    }

    /**
     * Activate or deactivate the meter to flight mode via BLE controller
     * interface.
     * 
     * @param hammingId[in] : Set command hamming id value.
     *            Range : HammingDistance.SAFETY_NUMBER_VALUE_0001,
     *            HammingDistance.SAFETY_NUMBER_VALUE_0002,
     *            HammingDistance.SAFETY_NUMBER_VALUE_0003
     *            Unit: Integer
     *            Scaling:1
     * @param isActivated [in] : If set to true to activated
     *            flight mode,otherwise set to false.
     *            Range: Valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling:1
     * 
     * @see mContext[in]
     * 
     * @return None
     * 
     */
    public synchronized void setCommand(final Integer hammingId,
            final SafetyBoolean isActivated)
    {
        // Get instance of BLEController
        final BLEController instance = BLEController.getInstance(mContext);

        // Validate object
        if (instance != null)
        {
            // Pump has confirmed within a specific timeout
            startTimeoutTimer();

            // Start send command
            mCommandMap.get(hammingId).actionCommand(instance, isActivated);

            Debug.printI(TAG, "[Step_3] setCommand block command  ");

            // Wait callback function finish or timeout is occurred.
            mSyncCommand.block();
        }
        else
        {
            // set result of flight mode
            setResult(SafetyBoolean.FALSE);
        }

        // Cancel and release time-out object
        mCheckTimeOutTimer.cancel();
        // mCheckTimeOutTimer.release();

        // Reset synchronous signal to do synchronize next process
        mSyncCommand.close();

    }

    /**
     * Indicate that the request was completed.
     * 
     * @param isResultOK [in] set flight mode
     *            Range: valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling:1
     * 
     * @see mSyncCommand[in]
     *      mCheckTimeOutTimer[in]
     * 
     * @return None
     * 
     */
    @Override
    public void onRequestCompleted(SafetyBoolean isResultOK)
    {
        // Release the thread is blocked on starting dispatch command.
        mSyncCommand.open();

        // set result of flight mode
        setResult(isResultOK);

        // Cancel and release time-out object
        mCheckTimeOutTimer.cancel();

        Debug.printI(TAG, "[Step_4] unlock synchronized signal : " + isResultOK);
    }

    /**
     * Get a result after dispatched command.
     * 
     * @see mIsTimeoutOccurred[in]
     * 
     * @return SafetyBoolean[out] Return the result.
     *         Range : Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     * 
     */
    public SafetyBoolean isResultOK()
    {
        SafetyBoolean isResultOK = mIsResultOK;

        if (mIsTimeoutOccurred == SafetyBoolean.TRUE)
        {
            isResultOK = SafetyBoolean.FALSE;
        }
        else
        {
            // Apply to the coding standard
        }

        return isResultOK;
    }


    /**
     * Set a result after dispatched command.
     * 
     * @see mIsResultOK[out]
     * 
     * @return SafetyBoolean[out] set the result okay or not.
     *         Range : Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean getResult()
    {
        return mIsResultOK;
    }
    
    /**
     * Initialize the flight mode that needs the objects
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @see mContext [in]
     *      mIsTimeoutOccurred[out]
     *      mIsResultOK[out]
     *      mCheckTimeOutTimer[out]
     *      mCommandMap[out]
     *      mFlightInstance[out]
     * 
     * @return None
     */
    protected void initialize(final Context context)
    {
        mContext = context;

        mIsTimeoutOccurred = SafetyBoolean.FALSE;

        mIsResultOK = SafetyBoolean.TRUE;

        mFlightInstance = FlightCommand.this;

        // A timer-out counter shall be build for synchronous communication.
        mCheckTimeOutTimer = new TimeoutTimer(mContext);
        mCheckTimeOutTimer.setListener(this);
    
        // Initialize command hash map.
        mCommandMap = new HashMap<Integer, SetCommand>();
        {
            // push command id to hashmap
            mCommandMap.put(HammingDistance.SAFETY_NUMBER_VALUE_0001,
                    SetCommand.setFlightMode);
            mCommandMap.put(HammingDistance.SAFETY_NUMBER_VALUE_0002,
                    SetCommand.setMPFlightMode);
            mCommandMap.put(HammingDistance.SAFETY_NUMBER_VALUE_0003,
                    SetCommand.syncSystemData);
        }
    }

    /**
     * Set a result after dispatched command.
     * 
     * @param isResultOK[in] set the result okay or not.
     *            Range : Valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @see mIsResultOK[out]
     * 
     * @return None
     */
    protected void setResult(final SafetyBoolean isResultOK)
    {
        mIsResultOK = isResultOK;
    }


    /**
     * Record a timeout is occurred after dispatched command.
     * 
     * @param isTimeoutOccurred[in] Set the time-out is occurred or not.
     *            Range : Valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @see mIsTimeoutOccurred[out]
     * 
     * @return None
     */
    protected void setTimeoutFlag(final SafetyBoolean isTimeoutOccurred)
    {
        mIsTimeoutOccurred = isTimeoutOccurred;
    }

    /**
     * Start a timeout timer.
     * If the system has no any response after dispatched command.
     * 
     * @see mCheckTimeOutTimer[in]
     * 
     * @return None
     */
    protected void startTimeoutTimer()
    {
        // Start count down timer
        mCheckTimeOutTimer.start();

        Debug.printI(TAG, "[Step_2] startTimeoutTimer ");
    }

    // Provide flight command id sets send command via BLEController.
    public enum SetCommand
    {
        
        // Set flight mode command id on meter .
        setFlightMode(HammingDistance.SAFETY_NUMBER_VALUE_0001)
        {
            /**
             * Implement set set flight mode command on meter.
             * 
             * @param bleInstance[in] : set instance of BLEController.
             *            Range: Valid BLEController object
             *            Unit: BLEController
             *            Scaling:1
             * 
             * @param isActivated [in] : If set to true to activated
             *            command,otherwise set to false.
             *            Range: Valid SafetyBoolean object
             *            Unit: SafetyBoolean
             *            Scaling:1
             * 
             * @return None
             */
            @Override
            public void actionCommand(final BLEController instance,
                    SafetyBoolean isActivate)
            {
                Debug.printI(TAG, "[setCommand] setFlightMode : "
                        + mFlightInstance);

                // Valid object
                CommonUtils.objectCheck(instance);

                // Start send command
                instance.setFlightMode(isActivate, mFlightInstance);
            }
        },
        // Set flight mode command id on micropump .
        setMPFlightMode(HammingDistance.SAFETY_NUMBER_VALUE_0002)
        {
            /**
             * Implement set set flight mode command on micropump.
             * 
             * @param bleInstance[in] : set instance of BLEController.
             *            Range: Valid BLEController object
             *            Unit: BLEController
             *            Scaling:1
             * 
             * @param isActivated [in] : If set to true to activated
             *            command,otherwise set to false.
             *            Range: Valid SafetyBoolean object
             *            Unit: SafetyBoolean
             *            Scaling:1
             * 
             * @return None
             */
            @Override
            public void actionCommand(final BLEController instance,
                    SafetyBoolean isActivate)
            {
                Debug.printI(TAG, "[setCommand] setMPFlightMode : "
                        + mFlightInstance);
                // Valid object
                CommonUtils.objectCheck(instance);

                // Start send command to activate flight mode on micropump
                instance.setMPFlightMode(mFlightInstance);
            }
        },
        // Set synchronizing system data mode commmand id.
        syncSystemData(HammingDistance.SAFETY_NUMBER_VALUE_0003)
        {
            /**
             * Implement set system synchronized command.
             * 
             * @param bleInstance[in] : set instance of BLEController.
             *            Range: Valid BLEController object
             *            Unit: BLEController
             *            Scaling:1
             * 
             * @param isActivated [in] : If set to true to activated
             *            command,otherwise set to false.
             *            Range: Valid SafetyBoolean object
             *            Unit: SafetyBoolean
             *            Scaling:1
             * 
             * @return None
             */
            @Override
            public void actionCommand(final BLEController instance,
                    SafetyBoolean isActivate)
            {
                Debug.printI(TAG, "[setCommand] setSystemSync : "
                        + mFlightInstance);
                
                // Valid object
                CommonUtils.objectCheck(instance);
                
                // Start send command
                instance.setSystemSync(mFlightInstance);
            }
        };

        // Apply hamming distance value to command id
        private Integer mHammingId = 0;

        /**
         * constructor
         * 
         * Initialize the flight mode that needs the objects
         * 
         * @param nHammingId [in] set hamming distance command id
         *            Range : HammingDistance.SAFETY_NUMBER_VALUE_0001,
         *            HammingDistance.SAFETY_NUMBER_VALUE_0002,
         *            HammingDistance.SAFETY_NUMBER_VALUE_0003
         *            Unit: Integer
         *            Scaling: 1
         * 
         * @return None
         */
        private SetCommand(Integer nHammingId)
        {
            mHammingId = nHammingId;
        }

        /**
         * Get hamming distance value of command id
         * 
         * Initialize the flight mode that needs the objects
         * 
         * @return Integer [out] Return hamming distance command id
         *         Range : HammingDistance.SAFETY_NUMBER_VALUE_0001,
         *         HammingDistance.SAFETY_NUMBER_VALUE_0002,
         *         HammingDistance.SAFETY_NUMBER_VALUE_0003
         *         Unit: Integer
         *         Scaling: 1
         */
        public Integer getHammingId()
        {
            return mHammingId;
        }

        /**
         * Provide a interface to implement action command.
         * 
         * @param bleInstance[in] : set instance of BLEController.
         *            Range: Valid BLEController object
         *            Unit: BLEController
         *            Scaling:1
         * 
         * @param isActivated [in] : If set to true to activated
         *            command,otherwise set to false.
         *            Range: Valid SafetyBoolean object
         *            Unit: SafetyBoolean
         *            Scaling:1
         * 
         * @return None
         */
        public void actionCommand(final BLEController bleInstance,
                final SafetyBoolean isActivate)
        {
            // This function is implemented by
            // the component of the enum SetCommand
        }

    }

    /**
     * If timeout is occurred , callback to do this function.
     * 
     * @see mContext[in]
     *      mSyncCommand[in]
     *      mCheckTimeOutTimer[out]
     * 
     * @return None
     */
    @Override
    public void onFinish()
    {
        // Time-out is occurred.
        setTimeoutFlag(SafetyBoolean.TRUE);

        // Set result of flight mode
        setResult(SafetyBoolean.FALSE);

        // Release the thread is blocked on starting dispatch command.
        mSyncCommand.open();
        Debug.printI(TAG, "[Step_5] timeout is occurred ");
    }

    @Override
    public void onTick()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */        
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Commit for setting the SVN keyword
// Add file header and footer comment block.
