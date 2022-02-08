/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: PowerManagerStateMachine
 * Brief: This class implement four states for monitoring the power. 
 *
 * Create Date: 06/09/2015
 * $Revision: 23976 $
 * $Author: StanleyWu $
 * $Id: PowerManagerStateMachine.java 23976 2015-11-12 08:58:13Z StanleyWu $
 */

package com.accu_chek.solo_m.rcapp.application.power;

import android.content.Context;
import android.os.BatteryManager;
import android.os.CountDownTimer;
import android.os.Message;
import android.os.RemoteException;
import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.request.TimeStampRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.UIStatusUpdateRequest;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.timemanagement.ITimeManagementService;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.State;
import com.accu_chek.solo_m.rcapp.application.util.StateMachine;

import java.util.Calendar;
import org.json.JSONException;
import org.json.JSONObject;

public final class PowerManagerStateMachine extends StateMachine
{

    /**
     * Define the State base as 0
     */
    public static final int BASE = 0;

    /**
     * Define the integer 1 for Active State
     */
    public static final int CMD_ACT_STATE = BASE + 1;

    /**
     * Define the integer 2 for Safe State
     */
    public static final int CMD_SAFE_STATE = CMD_ACT_STATE + 1;

    /**
     * Define the integer 3 for Standby State
     */
    public static final int CMD_STANDBY_STATE = CMD_SAFE_STATE + 1;

    /**
     * Define the integer 4 for Lower Back Light State
     */
    public static final int CMD_LOWBACKLIGHT_STATE = CMD_STANDBY_STATE + 1;

    /**
     * Define Max minutes
     */
    private static final int MAX_MINUTES = 60;

    /**
     * Define half hour
     */
    private static final int HALF_HOUR = 30;

    /**
     * Define millisecond
     */
    private static final int MILLISECOND = 1000;

    private static final String TAG = "PowerManagerStateMachine";

    /**
     * Define the power manager state string
     */
    private static final String PWER_MANAGER_STATE = "power manager state";

    /**
     * Define default year
     */
    private static final int YEAR_DEFAULT = 1970;

    /**
     * Define for integer 10
     */
    private static final int INT_TEN = 10;

    /**
     * Define for safety boolean true
     */
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();

    /**
     * Define for safety boolean false
     */
    private static final byte SAFETY_FALSE = SafetyBoolean.FALSE.getByte();

    /**
     * Define the context object
     */
    private static Context mContext = null;

    /**
     * Check whether it is default state or not
     */
    private boolean mIsDefaultState = true;
    
    /**
     * Check whether it is in active state or not
     */
    private boolean mIsFirstActiveState = true;
    
    private boolean mIsActiveState = false;

    /**
     * Define for current day
     */
    private int mDay = 1;

    /**
     * Define for current hour
     */
    private int mHour = 0;

    /**
     * Define for current minute
     */
    private int mMin = 0;

    /**
     * Define for current month
     */
    private int mMonth = 1;

    /**
     * Define for current second
     */
    private int mSec = 0;

    /**
     * Define for current year
     */
    private int mYear = YEAR_DEFAULT;
    
    /**
     * State instance
     */
    private DefaultState mDefaultState = new DefaultState();

    /**
     * Define for Active State
     */
    private ActiveState mActiveState = new ActiveState();

    /**
     * Define for Safe State
     */
    private SafeState mSafeState = new SafeState();

    /**
     * Define for Standby State
     */
    private StandbyState mStandbyState = new StandbyState();

    /**
     * Define for Lower Back Light State
     */
    private LowBacklightState mLowerBackLightState = new LowBacklightState();

    /**
     * Define a global object of CountDownTimer
     */
    private CountDownTimer mTimer = null;

    /**
     * Add states to PowerManagerStateMachine and set the initial state.
     * 
     * @param name Name of the state machine.
     *          Range: valid name
     *          Unit: String
     *          Scaling: 1
     * 
     */
    private PowerManagerStateMachine(final String name)
    {
        super(name);
        Debug.printI(TAG,
                "[PowerManagerStateMachine] start add state in state machine");
        addState(mDefaultState);
        addState(mActiveState, mDefaultState);
        addState(mSafeState, mDefaultState);
        addState(mStandbyState, mDefaultState);
        addState(mLowerBackLightState, mDefaultState);

        // Set the initial state
        Debug.printI(TAG, "[PowerManagerStateMachine] Set the initial state");
        setInitialState(mActiveState);

    }

    /**
     * 
     * Check the current battery temperature whether it is in the operating
     * range(0 to 45 degree) or not.
     *
     * @param batteryTemp current battery temperature
     *          Range: 0.0 to 45.0
     *          Unit: float
     *          Scaling: 1
     * 
     * return void [out] None
     */
    private void checkTemperature(final float batteryTemp)
    {
        CommonUtils.objectCheck(batteryTemp);

        // Get the battery allowable range from Config Matrix
        final SafetyString sKey1 = new SafetyString(
                ConfigParameter.BATTERY_RECHARGE_TEMPERATURE_MIN,
                CRCTool.generateCRC16(ConfigParameter.BATTERY_RECHARGE_TEMPERATURE_MIN
                        .getBytes()));
        final SafetyNumber<Integer> minTemp = ReadConfig
                .getIntegerDataByKey(sKey1);
        CommonUtils.objectCheck(minTemp);
        final int minRechargeTemp = minTemp.get();

        final SafetyString sKey2 = new SafetyString(
                ConfigParameter.BATTERY_RECHARGE_TEMPERATURE_MAX,
                CRCTool.generateCRC16(ConfigParameter.BATTERY_RECHARGE_TEMPERATURE_MAX
                        .getBytes()));
        final SafetyNumber<Integer> maxTemp = ReadConfig
                .getIntegerDataByKey(sKey2);
        CommonUtils.objectCheck(maxTemp);
        final int maxRechargeTemp = maxTemp.get();

        final boolean isUnderZero = minRechargeTemp > batteryTemp;
        final boolean isAboveFortyFive = maxRechargeTemp < batteryTemp;

        if (isUnderZero || isAboveFortyFive)
        {
            // Display M84_M_RC_TEMP_WARNING then switch to standby state
            final NotifyMessage msg = new NotifyMessage(EMWRList.EMW48401);
            NotifyProxy.showEMWR(mContext, msg);

        }
    }

    /**
     * 
     * Check the current battery level to identify whether it is in Low Battery
     * or
     * Empty Battery.
     *
     * @param level Current battery level
     *            Range: 0 to 100
     *            Unit: int
     *            Scaling: 1
     * 
     * return void [out] None
     */
    private void checkBatteryLevel(final int level)
    {
        CommonUtils.objectCheck(level);

        // Get battery threshold from Config Matrix
        final SafetyString sKey1 = new SafetyString(
                ConfigParameter.DEAD_BATTERY_THRESHOLD,
                CRCTool.generateCRC16(ConfigParameter.DEAD_BATTERY_THRESHOLD
                        .getBytes()));
        final SafetyNumber<Integer> safetyDeadValue = ReadConfig
                .getIntegerDataByKey(sKey1);
        CommonUtils.objectCheck(safetyDeadValue);
        final int deadValue = safetyDeadValue.get();

        final SafetyString sKey2 = new SafetyString(
                ConfigParameter.EMPTY_BATTERY_THRESHOLD,
                CRCTool.generateCRC16(ConfigParameter.EMPTY_BATTERY_THRESHOLD
                        .getBytes()));
        final SafetyNumber<Integer> safetyEmptyValue = ReadConfig
                .getIntegerDataByKey(sKey2);
        CommonUtils.objectCheck(safetyEmptyValue);
        final int emptyValue = safetyEmptyValue.get();

        final SafetyString sKey3 = new SafetyString(
                ConfigParameter.LOW_BATTERY_THRESHOLD,
                CRCTool.generateCRC16(ConfigParameter.LOW_BATTERY_THRESHOLD
                        .getBytes()));
        final SafetyNumber<Integer> safetyLowValue = ReadConfig
                .getIntegerDataByKey(sKey3);
        CommonUtils.objectCheck(safetyLowValue);
        final int lowValue = safetyLowValue.get();

        // Display W50_W_RC_BATTERY_LOW
        if ((lowValue > level) && (emptyValue < level))
        {
            final NotifyMessage msg = new NotifyMessage(EMWRList.EMW48402);
            NotifyProxy.showEMWR(mContext, msg);
        }

        // Display M59_M_RC_BATTERY_EMPTY
        if ((emptyValue > level) && (deadValue < level))
        {
            // Send command to Comms to deactivate all RF communication
            BLEController.getInstance(mContext).setFlightMode(
                    SafetyBoolean.TRUE, new ResponseCallback()
                    {
                        public void onRequestCompleted(
                                final SafetyBoolean result)
                        {
                            if (SAFETY_TRUE == result.getByte())
                            {
                                Debug.printD(TAG,
                                        " FlightMode Command Success ");
                            }
                            else
                            {
                                Debug.printD(TAG,
                                        " FlightMode Command Fail");
                            }
                        }
                    });

            final NotifyMessage msg = new NotifyMessage(EMWRList.EMW48403);
            NotifyProxy.showEMWR(mContext, msg);

        }
    }

    /**
     * 
     * Increase Standby to Activity cycle by 1
     *
     * @param context Context object
     *            Range: Valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * return void [out] None
     */
    private void increaseStandbyToActivityCycles(final Context context)
    {
        CommonUtils.objectCheck(context);

        final SafetyNumber<Integer> safeActivityCycle = NugenGeneralModel
                .getInt(context,
                        NugenFrameworkConstants.PowerManagerConstants.STANDBY_TO_ACTIVE_CYCLE);
        if (null == safeActivityCycle)
        {
            final SafetyNumber<Integer> safeActivityCycleSet = new SafetyNumber<Integer>();
            safeActivityCycleSet.set(1, -1);
            NugenGeneralModel
                    .setInt(context,
                            NugenFrameworkConstants.PowerManagerConstants.STANDBY_TO_ACTIVE_CYCLE,
                            safeActivityCycleSet);
        }
        else
        {
            int activityCycle = (Integer) safeActivityCycle.get();
            activityCycle++;
            final SafetyNumber<Integer> safePowerCycleSet = new SafetyNumber<Integer>();
            safePowerCycleSet.set(activityCycle, -activityCycle);
            NugenGeneralModel
                    .setInt(context,
                            NugenFrameworkConstants.PowerManagerConstants.STANDBY_TO_ACTIVE_CYCLE,
                            safeActivityCycle);
        }
    }

    /**
     * 
     * Send command to Comms when the user has been continue active for 30 mins
     *
     * return void [out] None
     */
    private void autoOffCounter()
    {

        mTimer = new SyncCommsTimer(HALF_HOUR * MAX_MINUTES * MILLISECOND,
                MILLISECOND);
        mTimer.start();

    }

    /**
     * 
     * Send command to Comms when meter state changed.
     *
     * @param data UI State of meter
     *            Range: Valid integer defined by Comms module
     *            Unit: int
     *            Scaling: 1
     * 
     * return void [out] None
     */
    private void sendCommandToComms(final int data)
    {
        final SafetyNumber<Integer> state = new SafetyNumber<Integer>(data,
                -data);
        
        BLERequestParameter parameter = new BLERequestParameter();
        UIStatusUpdateRequest request = (UIStatusUpdateRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_UI_STATE_UPDATE);
        request.setState(state);
        parameter.setRequest(request); 
        
        BLEController.getInstance(mContext).updateUIState(state,
                new ResponseCallback()
                {
                    public void onRequestCompleted(final SafetyBoolean result)
                    {

                        if (SAFETY_TRUE == result.getByte())
                        {
                            Debug.printD(TAG, " Command Success ");
                        }
                        else if (SAFETY_FALSE == result.getByte())
                        {
                            Debug.printD(TAG, " Command Fail");
                        }
                    }
                });
    }

    /**
     * 
     * Verify time when the meter state changes to ACTIVE mode.
     *
     * @param context Current context object.
     *            Range: Valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * return void [out] None
     */
    private void syncTime(final Context context)
    {

        // Read Post result
        final SafetyBoolean isPostResultOK = NugenSettingModel
                .getSafetyBoolean(context,
                        NugenFrameworkConstants.KEY_POST_RESULT_STATE,
                        SafetyBoolean.FALSE);

        if (SafetyBoolean.TRUE == isPostResultOK)
        {
            final ITimeManagementService tmSvr = CustJavaFrameworkManager
                    .getTimeManagementService(mContext);
            try
            {
                tmSvr.syncBGMTime();
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
        }
        else
        {
            // Do nothing
        }
    }

    /**
     * 
     * To initial PowerManagerStateMachine
     *
     * @param context Context object
     *              Range: Valid object
     *              Unit: Context
     *              Scaling: 1
     * 
     * return PowerManagerStateMachine [out] None
     */
    public static PowerManagerStateMachine makeState(final Context context)
    {
        Debug.printI(TAG, "[makeState] enter");
        mContext = context;
        final PowerManagerStateMachine pms = new PowerManagerStateMachine(
                PWER_MANAGER_STATE);
        pms.start();
        Debug.printI(TAG, "[makeState] exit");
        return pms;
    }

    /**
     * Send command to Comms when the user has been continue active for 30 mins.
     */
    private final class SyncCommsTimer extends CountDownTimer
    {

        /**
         * Constructor of CountDownTimer.
         * 
         * @param millisInFuture The number of millis in the future from the call 
         * to start() until the countdown is done and onFinish() is called.
         *              Range: -2^63 to (2^63)-1 
         *              Unit: long 
         *              Scaling: 1
         *              
         * @param countDownInterval The interval along the way to receive
         *            onTick(long) callbacks.
         *              Range: -2^63 to (2^63)-1 
         *              Unit: long 
         *              Scaling: 1
         */
        private SyncCommsTimer(
                final long millisInFuture, final long countDownInterval)
        {
            super(millisInFuture, countDownInterval);
        }

        /**
         * 
         * Callback fired when the time is up. Then send command to Comms.
         *
         */
        public void onFinish()
        {
            doAutoOffCounter();
            
        }

        /**
         * 
         * Callback fired on regular interval.
         *
         * @param millisUntilFinished The amount of time until finished.
         *              Range: -2^63 to (2^63)-1 
         *              Unit: long 
         *              Scaling: 1
         */
        @Override
        public void onTick(final long millisUntilFinished)
        {


        }

    }
    
    /**
     * 
     * Send current time to Comms.
     *
     * @return void [out] None
     */
    private void doAutoOffCounter()
    {

        final TimeStampRequest request = (TimeStampRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_SET_TIME_STAMP);

        final ITimeManagementService bpTimeManager = CustJavaFrameworkManager.getTimeManagementService(mContext);
        
        final Calendar cal = Calendar.getInstance();
        
        // Get forcedOffsetMicroPumpTimeBase from Config Matrix
        final SafetyString sKey1 = new SafetyString(
                ConfigParameter.KEY_FORCED_OFFSET_MICRO_PUMP_TIME_BASE,
                CRCTool.generateCRC16(ConfigParameter.KEY_FORCED_OFFSET_MICRO_PUMP_TIME_BASE
                        .getBytes()));
        final SafetyNumber<Integer> safetyOffsetValue = ReadConfig
                .getIntegerDataByKey(sKey1);
        CommonUtils.objectCheck(safetyOffsetValue);
        final int forcedOffsetMicroPumpTimeBase = safetyOffsetValue.get();
        
        try
        {
            cal.setTimeInMillis(bpTimeManager.getRCTime() - forcedOffsetMicroPumpTimeBase*MILLISECOND);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }

        mYear = cal.get(Calendar.YEAR);
        mMonth = cal.get(Calendar.MONTH) + 1;
        mDay = cal.get(Calendar.DAY_OF_MONTH);
        mHour = cal.get(Calendar.HOUR_OF_DAY);
        mMin = cal.get(Calendar.MINUTE);
        mSec = cal.get(Calendar.SECOND);

        request.setYear(new SafetyNumber<Integer>(mYear, -mYear));
        request.setMonth(new SafetyNumber<Integer>(mMonth, -mMonth));
        request.setDay(new SafetyNumber<Integer>(mDay, -mDay));
        request.setHours(new SafetyNumber<Integer>(mHour, -mHour));
        request.setMinutes(new SafetyNumber<Integer>(mMin, -mMin));
        request.setSeconds(new SafetyNumber<Integer>(mSec, -mSec));

        final BLERequestParameter parameter = new BLERequestParameter();
        parameter.setRequest(request);

        BLEController.getInstance(mContext).sendRequest(parameter,
                new ResponseCallback()
                {
                    public void onRequestCompleted(
                            final SafetyBoolean result)
                    {
                        if (SAFETY_TRUE == result.getByte())
                        {
                            Debug.printD(TAG,
                                    " AutoOffCounter Command Success ");
                        }
                        else
                        {
                            Debug.printD(TAG,
                                    " AutoOffCounter Command Fail");
                        }
                    }
                });

        // Continue call autoOffCounter() if still in active state.
        if ((SAFETY_TRUE == BLEController.getInstance(mContext).isBonded()
                .getByte())
                && (SAFETY_TRUE == BLEController.getInstance(mContext)
                        .isConnected().getByte()))
        {           
            if (true == mIsActiveState)
            {
                autoOffCounter();
            }          
        }
    }

    /**
     * Default state
     */
    private class DefaultState extends State
    {

        /**
         * 
         * Enter the default state.
         *
         */
        @Override
        public void enter()
        {
            Debug.printI(TAG, "[enter] DefaultState start");
            super.enter();
        }

        /**
         * 
         * Exit the default state.
         *
         */
        @Override
        public void exit()
        {
            Debug.printI(TAG, "[exit] DefaultState start");
            super.exit();

        }

        /**
         * Enqueue a message to this state machine.
         *
         * @param msg Message to pass.
         *          Range: True or False
         *          Unit: boolean
         *          Scaling: 1
         * 
         * return Return True if processing has completed and False if the message wasn't processed.
         */
        @Override
        public boolean processMessage(final Message msg)
        {
            Debug.printI(TAG, "[processMessage] DefaultState start");
            boolean ret = HANDLED;
            Debug.printI(TAG, "======>" + getName() + msg.toString());
            switch (msg.what)
            {
            case CMD_ACT_STATE :
                transitionTo(mActiveState);
                break;
            case CMD_SAFE_STATE :
                transitionTo(mSafeState);
                break;
            case CMD_STANDBY_STATE :
                transitionTo(mStandbyState);
                break;
            case CMD_LOWBACKLIGHT_STATE :
                transitionTo(mLowerBackLightState);
                break;
            default :
                ret = NOT_HANDLED;
                break;
            }

            return ret;
        }

    }

    /**
     * Active State
     */
    private class ActiveState extends State
    {
        /**
         * When enter the Activity State, PowerManagerStateMachine will do the
         * following things.
         * 1. Increase Standby to Activity cycle by 1.
         * 2. Verify meter date and time.
         * 3. Read battery info.
         * 4. Send command to Comms.
         * 5. Verify meter date/time
         */
        @Override
        public void enter()
        {
            Debug.printI(TAG, "[enter] ActiveState start");
            super.enter();
            
            // Set mIsActiveState to true
            mIsActiveState = true;
            
            // To increase Standby to Activity cycle by 1
            increaseStandbyToActivityCycles(mContext);

            // Verify the meter date and time. Skip when the meter is in first active state
            syncTime(mContext);

            // Read battery info
            final SafetyString safeBatteryInfo = NugenSettingModel.getString(
                    mContext,
                    NugenFrameworkConstants.PowerManagerConstants.BATTERY_INFO);

            if (null != safeBatteryInfo)
            {
                final String batteryInfo = safeBatteryInfo.getString();

                // Check battery temperature and remaining battery capacity
                try
                {
                    final JSONObject json = new JSONObject(batteryInfo);

                    json.get(BatteryManager.EXTRA_TEMPERATURE);
                    final float batteryTemp = ((float) (Integer) json
                            .get(BatteryManager.EXTRA_TEMPERATURE)) / INT_TEN;
                    Debug.printI(TAG, "battery temp: " + batteryTemp);

                    checkTemperature(batteryTemp);

                    final int batteryLevel = (Integer) json
                            .get(BatteryManager.EXTRA_LEVEL);
                    Debug.printI(
                            TAG,
                            "battery level: "
                                    + json.get(BatteryManager.EXTRA_LEVEL));
                    checkBatteryLevel(batteryLevel);

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

            // Send command to Comms
            if (mIsDefaultState == true)
            {
                mIsDefaultState = false;
            }
            else
            {
                sendCommandToComms(CommsConstant.UiState.ACTIVE);

            }

            // Execute auto off counter when meter is bonded with pump
            if ((SAFETY_TRUE == BLEController.getInstance(mContext).isBonded()
                    .getByte())
                    && (SAFETY_TRUE == BLEController.getInstance(mContext)
                            .isConnected().getByte()))
            {
                if (false == mIsFirstActiveState)
                {
                    Debug.printD(TAG, "Start auto off counter");
                    // Call autoOffCounter
                    autoOffCounter();
                }               
            }

        }

        /**
         * Exit the Active state.
         * 
         *
         */
        @Override
        public void exit()
        {
            Debug.printI(TAG, "[exit] ActiveState exit");
            // Set mIsFirstActiveState to false
            mIsFirstActiveState = false;
            
            // Set mIsActiveState to false
            mIsActiveState = false;
            
            super.exit();
        }

        /**
         * Enqueue a message to this state machine.
         *
         * @param msg Message to pass.
         *          Range: True or False
         *          Unit: boolean
         *          Scaling: 1
         * 
         * return Return TRUE if processing has completed and FALSE if the message wasn't processed.
         */
        @Override
        public boolean processMessage(final Message msg)
        {
            Debug.printI(TAG, "[processMessage] ActiveState start");
            return NOT_HANDLED;
        }

    }

    /**
     * Safe State
     */
    private class SafeState extends State
    {
        /**
         * Enter the Safe state.
         * 
         *
         */
        @Override
        public void enter()
        {
            Debug.printI(TAG, "[enter] SafeState start");
            super.enter();
        }

        /**
         * Exit the Safe state.
         * 
         *
         */
        @Override
        public void exit()
        {
            Debug.printI(TAG, "[exit] SafeState start");
            super.exit();
        }

        /**
         * Enqueue a message to this state machine.
         *
         * @param msg Message to pass.
         *          Range: True or False
         *          Unit: boolean
         *          Scaling: 1
         * 
         * return Return TRUE if processing has completed and FALSE if the message wasn't processed.
         */
        @Override
        public boolean processMessage(final Message msg)
        {
            Debug.printI(TAG, "[processMessage] SafeState start");
            return NOT_HANDLED;
        }

    }

    /**
     * Standby State
     */
    private class StandbyState extends State
    {
        
        private final ICustPowerManager mPms = CustJavaFrameworkManager
                .getCustPowerManagerService(mContext);
        
        /**
         * When enter the Standby state, the following things will be executed.
         * 1. Send command to Comms when enter Standby State.
         * 2. Cancel CountDownTimer.
         */
        @Override
        public void enter()
        {
            Debug.printI(TAG, "[enter] StandbyState start");
            super.enter();
           
            try
            {
                // Get wake lock
                mPms.newWakeLock();
                
                // Execute AutoOffCounter
                doAutoOffCounter();
                
                // Send command to Comms
                sendCommandToComms(CommsConstant.UiState.STANDBY);
            }
            catch (RemoteException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
                    
                    
        }

        /**
         * Exit the Standby state.
         * 
         *
         */
        @Override
        public void exit()
        {
            Debug.printI(TAG, "[exit] StandbyState start");
            
            
            try
            {
                // Release wake lock
                mPms.release();
            }
            catch (RemoteException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            
            super.exit();
        }

        /**
         * Enqueue a message to this state machine.
         *
         * @param msg Message to pass.
         *          Range: True or False
         *          Unit: boolean
         *          Scaling: 1
         * 
         * return Return TRUE if processing has completed and FALSE if the message wasn't processed.
         */
        @Override
        public boolean processMessage(final Message msg)
        {
            Debug.printI(TAG, "[processMessage] StandbyState start");
            return NOT_HANDLED;
        }

    }

    /**
     * Lower Back Light State
     */
    private class LowBacklightState extends State
    {

        /**
         * Enter the Lower back light state.
         * 
         *
         */
        @Override
        public void enter()
        {
            Debug.printI(TAG, "[enter] LowBacklightState start");
            super.enter();
        }

        /**
         * Exit the Lower back light state.
         * 
         *
         */
        @Override
        public void exit()
        {
            Debug.printI(TAG, "[exit] LowBacklightState start");
            super.exit();
        }

        /**
         * Enqueue a message to this state machine.
         *
         * @param msg Message to pass.
         *          Range: True or False
         *          Unit: boolean
         *          Scaling: 1
         * 
         * return Return TRUE if processing has completed and FALSE if the message wasn't processed.
         */
        @Override
        public boolean processMessage(final Message msg)
        {
            Debug.printI(TAG, "[processMessage] LowBacklightState start");
            return NOT_HANDLED;
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
