/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: FlightController
 * Brief: Record flight state and request command dispatcher.
 *
 * Create Date: 10/08/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.flight;

import android.content.Context;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.flight.FlightCommand.SetCommand;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class FlightController
{

    // Defines debug tag.
    private static final String TAG = "FlightMode";

    // The byte value of SafetyBoolean TRUE
    private static byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // Define the singleton instance
    private static volatile FlightController mInstance = null;

    // Define activate mode set SafetyBoolean TRUE ,otherwise set to FALSE.
    private static SafetyBoolean mIsFlightActivated = SafetyBoolean.FALSE;

    // Point to context activity in android.
    private Context mContext = null;

    // Define whether synchronized system data.
    private SafetyBoolean mIsSynchReady = SafetyBoolean.FALSE;

    /**
     * constructor
     * 
     * Initialize the flight mode that needs the objects
     * 
     * @param context [in] the non-null context of application
     *            Range : Valid Context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @see mContext [in]
     * 
     * @return None
     */
    public FlightController(final Context context)
    {
        mContext = context;

        // Load flight mode status from the setting.
        mIsFlightActivated = NugenGeneralModel.getSafetyBoolean(context,
                NugenFrameworkConstants.KEY_FLIGHT_MODE_STATUS,
                SafetyBoolean.FALSE);
    }

    /**
     * Get the instance object of the FlightController
     *
     * @param context[in] : the context of activity in android.
     *            Range: Valid context object
     *            Unit: context
     *            Scaling: 1
     * 
     * @see mInstance [out]
     * 
     * @return FlightController [out] the instance object.
     *         Range: Valid FlightController object
     *         Unit: FlightController
     *         Scaling: 1
     */
    public static synchronized FlightController getInstance(
            final Context context)
    {
        // Check whether the mInstance is null or not.
        if (null == mInstance)
        {
            mInstance = new FlightController(context);
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }

    /**
     * Check whether the meter is paired to micropump or not.
     * 
     * @return SafetyBoolean [out] Return the status of pump running.
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling:1
     */
    public final SafetyBoolean isPairedtoMP()
    {
        // Get instance of BLEController
        final BLEController instance = BLEController.getInstance();

        // The meter is paired with micropump set to true, otherwise set to
        // false.
        SafetyBoolean isPaired = SafetyBoolean.FALSE;

        byte isBond = (byte) 0X00;

        if (null != instance)
        {
            // Check is bonded and connected of micropump.
            isBond = instance.isBonded().getByte();

            // Get micropump connect or bonded status..
            if ((isBond == mByteSafetyTRUE))
            {
                // The micro-pump is connected and bonded.
                isPaired = SafetyBoolean.TRUE;
            }
            else
            {
                // The micro-pump is not connected and bonded.
                isPaired = SafetyBoolean.FALSE;
            }

            Debug.printI(TAG, "[Step_0] isPumpRunning : " + isPaired);
        }

        return isPaired;
    }

    /**
     * Check whether the pump is running or not.
     * 
     * @return SafetyBoolean [out] Return the status of pump running.
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling:1
     */
    public final SafetyBoolean isConnected()
    {
        // Get instance of BLEController
        final BLEController instance = BLEController.getInstance();

        byte isConnectByte = (byte) 0X00;

        // The meter is connected with micropump set to true, otherwise set to
        // false.
        SafetyBoolean isConnected = SafetyBoolean.FALSE;

        if (null != instance)
        {
            // Check is bonded and connected of micro-pump.
            isConnectByte = instance.isConnected().getByte();

            // Get micropump connect or bonded status..
            if ((isConnectByte == mByteSafetyTRUE))
            {
                // The micro-pump is connected and bonded.
                isConnected = SafetyBoolean.TRUE;
            }
            else
            {
                // The micro-pump is not connected and bonded.
                isConnected = SafetyBoolean.FALSE;
            }

            Debug.printI(TAG, "[Step_1] mIsConnected : " + isConnected);
        }

        return isConnected;
    }

    /**
     * Set activate or deactivate flight mode status on meter.
     * 
     * @param isActivated [in] : If set to true to activate flight mode
     *            ,otherwise set to false.
     *            Range: Valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling:1
     * 
     * @see mContext[in]
     *      mIsFlightActivated[out]
     * @return None
     */
    public final void setFlightStatusOnMeter(final SafetyBoolean isActivated)
    {
        // Set flight mode status on meter
        mIsFlightActivated = isActivated;

        Debug.printI(TAG, "[setFlightStatusOnMeter] mIsFlightActivated : "
                + isActivated);

        // Record the result to setting model.
        NugenSettingModel.setSafetyBoolean(mContext,
                NugenFrameworkConstants.KEY_FLIGHT_MODE_STATUS, isActivated);
    }

    /**
     * Get flight mode status on meter.
     * 
     * @return SafetyBoolean [out] Return flight mode status.
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling:1
     */
    public final SafetyBoolean getFlightStatusOnMeter()
    {
        Log.d("FlightMode", " Is Flight Activated : " + mIsFlightActivated);

        return mIsFlightActivated;
    }

    /**
     * After power on meter every time, enter flight mode setting need to
     * synchronized system data with micropump.
     * 
     * @param isSynchReady [in] Set synchronized ready signal.
     *            Range: Valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling:1
     * 
     * @return None.
     */
    public final void setSyncDataReady(SafetyBoolean isSynchReady)
    {
        Log.d("FlightMode", " setSyncDataReady mIsSynchReady: " + isSynchReady);

        mIsSynchReady = isSynchReady;

    }

    /**
     * Return synchronized system data status .
     * 
     * @return SafetyBoolean [out] Return synchronized system status.
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling:1
     */
    public final SafetyBoolean getSyncReadyStatus()
    {
        Log.d("FlightMode", " Is mIsSynchReady  : " + mIsSynchReady);

        return mIsSynchReady;
    }

    /**
     * The method is provided to activate flight mode on micropump.
     * 
     * @param listener[in]: Set the object of interface IDispatchCommand
     *            Range: Valid IDispatchCommand object
     *            Unit: IDispatchCommand
     *            Scaling: 1
     * 
     * @return None
     */
    public synchronized void activateMPFlightMode(IDispatchCommand listener)
    {
        final ExecutorService service = Executors.newFixedThreadPool(1);

        // Activated micro pump to flight mode.
        final SafetyBoolean isMicroPumpActivated = SafetyBoolean.TRUE;

        // Set dispatch activated flight command id
        final Integer mHammingCommandId = SetCommand.setMPFlightMode
                .getHammingId();

        DispatchCommand commandThread = null;

        // Valid object.
        CommonUtils.objectCheck(listener);

        // New a thread to dispatch activated flight command on micropump
        commandThread = new DispatchCommand(mHammingCommandId,
                isMicroPumpActivated, listener);

        // If don't use a thread, it will block ui thread.
        // Start thread to dispatch activated flight command.
        service.execute(commandThread);

    }

    /**
     * The method is provided to activate or deactivate flight mode on meter.
     * 
     * @param isMeterActivated [in] activate or deactivate flight mode.
     *            if set true , activate flight mode, otherwise deactivate
     *            flight mode.
     * @param listener[in]: Set the object of interface IDispatchCommand
     *            Range: Valid IDispatchCommand object
     *            Unit: IDispatchCommand
     *            Scaling: 1
     * 
     * @return None
     */
    public synchronized void activateFlightModeOnMeter(
            SafetyBoolean isMeterActivated, IDispatchCommand listener)
    {
        final ExecutorService service = Executors.newFixedThreadPool(1);
        // Set dispatch activated flight command id
        final Integer mHammingCommandId = SetCommand.setFlightMode
                .getHammingId();

        DispatchCommand commandThread = null;

        // Valid object.
        CommonUtils.objectCheck(listener);

        // New a thread to dispatch activated flight command on micropump
        commandThread = new DispatchCommand(mHammingCommandId,
                isMeterActivated, listener);

        // If don't use a thread, it will block ui thread.
        // Start thread to dispatch activated flight command.
        service.execute(commandThread);

    }

    /**
     * The purpose of this method is after micropuming is connected shall
     * synchronize system data includes the bolus and basal rate.
     * 
     * @param listener[in]: Set the object of interface IDispatchCommand
     *            Range: Valid IDispatchCommand object
     *            Unit: IDispatchCommand
     *            Scaling: 1
     * 
     * @see mContext[in]
     * 
     * @return None
     */
    public synchronized void startSynchronizeData(IDispatchCommand listener)
    {
        final ExecutorService service = Executors.newFixedThreadPool(1);

        // Get flight mode instance
        final FlightController instance = FlightController
                .getInstance(mContext);

        // Activate synchronized data.
        final SafetyBoolean isActivated = SafetyBoolean.FALSE;

        DispatchCommand commandThread = null;

        // Set dispatch synchronized system data command id
        Integer mHammingCommandId = SetCommand.syncSystemData.getHammingId();

        // New a command thread.
        commandThread = instance.new DispatchCommand(mHammingCommandId,
                isActivated, listener);

        // If don't use a thread, it will block ui thread.
        // Start thread to dispatch activated flight command.
        service.execute(commandThread);

    }

    /**
     * Create a runnable to dispatch command via BLEController interface.
     * Includes sync system data command , set flight mode on meter and on
     * micropump.
     **/
    protected class DispatchCommand implements Runnable
    {

        // Point to context activity in android.
        private SafetyBoolean mIsActivated = null;

        // private IDispatchFlightCommand mDispatchInstance = null;
        private IDispatchCommand mDispatchcommand = null;

        // Get flight command instance.
        private final FlightCommand mFlightInstance = new FlightCommand(
                mContext);

        // Dispatch command hamming distance id.
        private Integer mHammingCommandId = 0;;

        /**
         * Constructor
         * This constructor shall initialize IDispatchCommand and
         * the activated mode status.
         * 
         * @param hammingId [in] set hamming distance command id
         *            Range : HammingDistance.SAFETY_NUMBER_VALUE_0001,
         *            HammingDistance.SAFETY_NUMBER_VALUE_0002,
         *            HammingDistance.SAFETY_NUMBER_VALUE_0003
         *            Unit: Integer
         *            Scaling: 1
         * @param isActivated [in] : If set to true to activated
         *            flight mode,
         *            otherwise set to false.
         *            Range: Valid SafetyBoolean object
         *            Unit: SafetyBoolean
         *            Scaling:1
         * @param dispatchCommand [in] : It is a IDispatchCommand object.
         *            Range: Valid IDispatchCommand object
         *            Unit: IDispatchCommand
         *            Scaling:1
         * 
         * @return SafetyBoolean [out] Set activated command state.
         *         Range: Valid SafetyBoolean object
         *         Unit: SafetyBoolean
         *         Scaling:1
         */
        protected DispatchCommand(
                final Integer hammingId, final SafetyBoolean isActivated,
                final IDispatchCommand dispatchCommand)
        {
            // command hammming distance value.
            mHammingCommandId = hammingId;

            // The interface object.
            mDispatchcommand = dispatchCommand;

            // Switch on-off
            mIsActivated = isActivated;
        }

        /**
         * This method is called when build a dispatch command thread is
         * started.
         * 
         * @return None
         */
        @Override
        public void run()
        {
            // Action command result
            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            // Start dispatch flight command.
            mFlightInstance.setCommand(mHammingCommandId, mIsActivated);

            // Valid the object.
            if (null != mDispatchcommand)
            {
                // Get the result after dispatching command
                isResultOK = mFlightInstance.getResult();
            }
            else
            {
                isResultOK = SafetyBoolean.FALSE;
            }

            // Execute action onComplete
            mDispatchcommand.onCompleted(isResultOK);
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
// Commit for setting the SVN keyword
// Add file header and footer comment block.

