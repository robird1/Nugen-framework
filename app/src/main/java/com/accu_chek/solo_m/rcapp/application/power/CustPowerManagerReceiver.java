/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: CustPowerMangerReceiver
 * Brief: This class receives intents from android platform.
 *
 * Create Date: 05/28/2015
 * $Revision: 23075 $
 * $Author: StanleyWu $
 * $Id: CustPowerManagerReceiver.java 23075 2015-11-03 06:28:57Z StanleyWu $
 */

package com.accu_chek.solo_m.rcapp.application.power;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.BatteryManager;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager.LEDTYPE;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.util.HashMap;

import org.json.JSONException;
import org.json.JSONObject;

public class CustPowerManagerReceiver extends BroadcastReceiver
{

    /**
     * String for screen is on
     */
    public static final String SCREEN_ON = "Screen.is.on";

    /**
     * String for screen is off
     */
    public static final String SCREEN_OFF = "Screen.is.off";

    /**
     * Define the context object
     */
    private static Context mContext = null;

    private static final String TAG = "CustPowerManagerReceiver";

    /**
     * Define PowerManagerStateMachine object
     */
    private static PowerManagerStateMachine pmSM = null;

    /**
     * This HashMap is used to store the received intent value
     */
    private HashMap<String, ReceiveState> mStateMap = new HashMap<String, ReceiveState>();

    {
        mStateMap.put(Intent.ACTION_SCREEN_ON, PowerManagerStatus.ON);
        mStateMap.put(Intent.ACTION_SCREEN_OFF, PowerManagerStatus.OFF);
        mStateMap.put(Intent.ACTION_BATTERY_CHANGED,
                PowerManagerStatus.BATTERY_INFO);
    }

    /**
     * Call PowerManagerStateMachine makeState to start State Machine.
     */
    public final void startPowerManagerStatemachine(final Context context)
    {
        CommonUtils.objectCheck(context);
        pmSM = PowerManagerStateMachine.makeState(context);
    }

    /**
     * This method is called when the BroadcastReceiver receives an Intent
     * broadcast.
     *
     * @param context [in] The Context in which the receiver is running.
     *            Range: Valid object
     *            Unit: Context
     *            Scaling: 1
     *            
     * @param intent [in] The Intent being received.
     *            Range: Valid object
     *            Unit: Intent
     *            Scaling: 1
     *            
     * return void [out] None
     */
    @Override
    public final void onReceive(final Context context, final Intent intent)
    {
        // Check input parameters
        CommonUtils.objectCheck(context, intent);

        final ReceiveState state = mStateMap.get(intent.getAction());
        Debug.printI(TAG, "intent.getAction() :" + intent.getAction());
        mContext = context;

        if (state != null)
        {
            state.doAction(intent);
        }
    }

    /**
     * Interface of receive state.
     */
    interface ReceiveState
    {
        /**
         * 
         * Interface of receive state.
         *
         * @param intent The received intent.
         * return void [out] None
         */
        void doAction(Intent intent);
    }

    /**
     * Interface of Power Manager status.
     */
    interface PowerManagerStatus
    {
        ScreenOn ON = new ScreenOn();
        ScreenOff OFF = new ScreenOff();
        BatteryInfo BATTERY_INFO = new BatteryInfo();
    }

    /**
     * Switch to Active State and send broadcast when the screen is on.
     */
    private static class ScreenOn implements ReceiveState
    {

        /**
         * 
         * Switch the meter to active state and send broadcast.
         *
         * @param intent The received intent.
         *          Range: Valid object 
         *          Unit: Intent 
         *          Scaling: 1
         */
        @Override
        public void doAction(final Intent intent)
        {
            Debug.printI(TAG, "[ScreenOn] doAction");

            // switch to active state
            pmSM.sendMessage(PowerManagerStateMachine.CMD_ACT_STATE);
            Debug.printI(TAG, "[enter] ActiveState start");

            // Broadcast the screen is on
            final Intent sendIntent = new Intent();
            sendIntent.setAction(SCREEN_ON);
            CustPowerManagerReceiver.mContext.sendBroadcast(sendIntent);
            Debug.printI(TAG, " Intent = " + sendIntent);
        }
    }

    /**
     * Switch to Standby State and send broadcast when the screen is off.
     */
    private static class ScreenOff implements ReceiveState
    {

        /**
         * Switch the meter to standby state and send broadcast.
         * 
         * @param intent The received intent.
         *          Range: Valid object 
         *          Unit: Intent 
         *          Scaling: 1
         */
        @Override
        public void doAction(final Intent intent)
        {
            Debug.printI(TAG, "[ScreenOff] doAction");
            // Broadcast the screen is off
            final Intent sendIntent = new Intent();
            sendIntent.setAction(SCREEN_OFF);
            CustPowerManagerReceiver.mContext.sendBroadcast(sendIntent);
            Debug.printI(TAG, "Intent = " + sendIntent);

            // switch to standby state
            pmSM.sendMessage(PowerManagerStateMachine.CMD_STANDBY_STATE);
            Debug.printI(TAG, "[enter] StandbyState start");
        }
    }

    /**
     * Provides extra battery information when received intent.
     * 
     */
    private static class BatteryInfo implements ReceiveState
    {

        /**
         * True when LED is closed and False when LED is on.
         */
        private static boolean mCloseLed = false;

        /**
         * Define Json object
         */
        private JSONObject mJson = new JSONObject();

        /**
         * 
         * Get the battery information and save it.
         *
         * @param intent The received intent.
         *          Range: Valid object 
         *          Unit: Intent 
         *          Scaling: 1
         */
        @Override
        public void doAction(final Intent intent)
        {
            Debug.printI(TAG, "[BatteryInfo] doAction");

            // Check input parameter
            CommonUtils.objectCheck(intent);

            final int health = intent.getIntExtra(BatteryManager.EXTRA_HEALTH,
                    0);
            final int level = intent.getIntExtra(BatteryManager.EXTRA_LEVEL, 0);
            final int plugged = intent.getIntExtra(
                    BatteryManager.EXTRA_PLUGGED, 0);
            final boolean present = intent.getExtras().getBoolean(
                    BatteryManager.EXTRA_PRESENT);
            final int scale = intent.getIntExtra(BatteryManager.EXTRA_SCALE, 0);
            final int status = intent.getIntExtra(BatteryManager.EXTRA_STATUS,
                    0);

            // Turn off the LED when the battery is fully charged.
            if (status == BatteryManager.BATTERY_STATUS_FULL)
            {
                final LEDManager mLED = (LEDManager) ICustomizedHWManager
                        .getSystemService(ICustomizedHWManager.LED_SERVICE);

                closeLed(mLED);
            }

            final int temperature = intent.getIntExtra(
                    BatteryManager.EXTRA_TEMPERATURE, 0);
            final int chargePlug = intent.getIntExtra(
                    BatteryManager.EXTRA_PLUGGED, -1);
            try
            {
                mJson.put(BatteryManager.EXTRA_HEALTH, health);
                mJson.put(BatteryManager.EXTRA_LEVEL, level);
                mJson.put(BatteryManager.EXTRA_PLUGGED, plugged);
                mJson.put(BatteryManager.EXTRA_PRESENT, present);
                mJson.put(BatteryManager.EXTRA_SCALE, scale);
                mJson.put(BatteryManager.EXTRA_STATUS, status);
                mJson.put(BatteryManager.EXTRA_TEMPERATURE, temperature);
                mJson.put(BatteryManager.EXTRA_PLUGGED, chargePlug);
                mJson.toString();
                final SafetyString batteryInfo = new SafetyString();
                batteryInfo.set(mJson.toString(),
                        CRCTool.generateCRC16(mJson.toString().getBytes()));

                NugenSettingModel
                        .setString(
                                mContext,
                                NugenFrameworkConstants.PowerManagerConstants.BATTERY_INFO,
                                batteryInfo);
            }
            catch (JSONException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // empty for static code analysis
            }
        }

        /**
         * 
         * Close LED
         *
         * @param ledManager Interface of LED
         *          Range: Valid object of LEDManager
         *          Unit: LEDManager
         *          Scaling: 1
         *            
         * return void [out] None
         */
        private void closeLed(final LEDManager ledManager)
        {
            CommonUtils.objectCheck(ledManager);

            if (mCloseLed == false)
            {
                if (ledManager != null)
                {
                    try
                    {
                        ledManager.closeLED(LEDTYPE.INFORMATION);
                    }
                    catch (OperationFailException e)
                    {
                        // call EMWR
                        e.printStackTrace();
                    }
                    finally
                    {
                        // empty for static code analysis
                    }

                    mCloseLed = true;
                }
                else
                {
                    Debug.printI(TAG, "mLED is null");
                }
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
