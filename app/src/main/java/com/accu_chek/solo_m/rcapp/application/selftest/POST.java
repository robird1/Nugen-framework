/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: POST
 * Brief: Implements all power on self-test items.
 *
 * Create Date: 07/02/2015
 * $Revision: 23744 $
 * $Author: VictorChen $
 * $Id: POST.java 23744 2015-11-10 08:16:10Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.selftest;

import android.content.Context;

import android.os.BatteryManager;
import android.os.ConditionVariable;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GetErrorLog;

import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;

import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrix;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;

import com.accu_chek.solo_m.rcapp.application.timemanagement.ITimeManagementService;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import org.json.JSONException;
import org.json.JSONObject;

public class POST extends Selftest
{

    // Define the insulin button is at released state
    public static final SafetyBoolean KEY_RELEASE = SafetyBoolean.FALSE;

    // Define the insulin button is pressed
    public static final SafetyBoolean KEY_PRESSED = SafetyBoolean.TRUE;

    // Define debug tag
    private static final String TAG = "POST";

    // The byte value of SafetyBoolean TRUE
    private static byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();

    // The byte value of SafetyBoolean FALSE
    private static byte mByteSafetyFALSE = SafetyBoolean.FALSE.getByte();

    // Define the singleton instance
    private static POST mInstance = null;

    // Record post result of BGM component.
    private static SafetyBoolean mIsBGM_POST_Result_OK = SafetyBoolean.FALSE;

    // Record the post result of communication processor is okay.
    private static SafetyBoolean mIsCommsInfo_Result_OK = SafetyBoolean.FALSE;

    // Point to context activity in android.
    private Context mContext = null;

    // Record the insulin button key state
    private SafetyBoolean mInsulinKeyState = KEY_RELEASE;

    // For use as a synchronous signal
    private final ConditionVariable mSyncCommsReady = new ConditionVariable();

    // For use as a synchronous signal
    private final ConditionVariable mSyncBGMReady = new ConditionVariable();

    // Record time-out is occurred in BGM component during post.
    private SafetyBoolean mIsBGMTimeoutOccurred = SafetyBoolean.FALSE;

    /**
     * constructor
     * 
     * @param context[in] : context of application or activity in android. Range
     *            :valid context object Unit: context
     *            Scaling: 1
     * 
     * @return None
     */
    public POST(final Context context)
    {
        super(context);

        initial(context);

    }

    /**
     * Initialize the POST that needs the objects
     * 
     * @param context
     *            [in] : the context of activity in android. Range :valid
     *            context object Unit: context
     *            Scaling: 1
     * 
     * @see mContext [out]
     *      mInsulinKeyState [out]
     * 
     * @return None
     */
    private void initial(final Context context)
    {
        Debug.printI(TAG, " initial POST constructor : " + context);
        mContext = context.getApplicationContext();

        // Initialize the insult button state
        mInsulinKeyState = KEY_RELEASE;
        super.enableLogToFile(SafetyBoolean.TRUE);

        // Add the self-test items of POST
        hook();
    }

    /**
     * Get the instance object of the POST
     *
     * @param context[in] : the context of activity in android.
     *            Range :valid context object
     *            Unit: context
     *            Scaling: 1
     * 
     * @see mInstance [out]
     * 
     * @return POST [out] the instance object.
     *         Range :valid POST object
     *         Unit:POST
     *         Scaling: 1
     * 
     */
    public static synchronized POST getInstance(final Context context)
    {
        if (null == mInstance)
        {
            mInstance = new POST(context);
            Debug.printI(TAG, "mInstance = " + mInstance);
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }

    /**
     * Check whether the self-test result of communication processor is success.
     * 
     * @param commsInfoResult[in] : Set the result of Post from communication
     *            processor.
     *            Range:-2^31 ~2^31-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @see mSyncCommsReady[in]
     *      mIsCommsInfo_Result_OK [out]
     * 
     * @return None
     */
    public final void setCommsPostResult(final int commsPostResult)
    {
        byte commsPostResultByte = (byte) commsPostResult;
        
        Debug.printI(TAG, "[sync. mSyncCommsReady] <<<< unLock notify["
                + commsPostResultByte + "]");

        if (commsPostResultByte == mByteSafetyTRUE)
        {
            // Return the result of the POST is okay from the Comms. Proc.
            mIsCommsInfo_Result_OK = SafetyBoolean.TRUE;
        }
        else
        {
            // Return the result of the POST is failed from the Comms. Proc.
            mIsCommsInfo_Result_OK = SafetyBoolean.FALSE;
        }

        // unlock synchronized signal
        mSyncCommsReady.open();
    }

    /**
     * Add all registered self-test items by interface ISelfTestListener in POST
     * sequence
     * 
     * @return None
     * 
     */
    protected void hook()
    {
        // The production is one of post items.
        final SelftestProduction production = new SelftestProduction();

        // The configuration is one of post items.
        final SelftestConfiguration configuration = new SelftestConfiguration();

        // The measure Engine of BG is one of post items.
        final SelftestMeasureEngine measureEngine = new SelftestMeasureEngine();

        // The verified time of BG is one of post items.
        final SelftestVerifyRTC verifyRTC = new SelftestVerifyRTC();

        // The power supply test is one of post items.
        final SelftestPowerSupplyTest powerSupply = new SelftestPowerSupplyTest();

        // The insulin button test is one of post items.
        final SelftestInsulinButton checkInsulinBtn = new SelftestInsulinButton();

        // The post of communication processor test is one of post items.
        final SelftestCheckCommsPOST checkCommsPOST = new SelftestCheckCommsPOST();

        // The verified hardware version is one of post items.
        final SelftestHardwareVersion hwVersion = new SelftestHardwareVersion();

        super.addListener(production);
        super.addListener(configuration);
        super.addListener(measureEngine);
        super.addListener(verifyRTC);
        super.addListener(powerSupply);
        super.addListener(checkInsulinBtn);
        super.addListener(checkCommsPOST);
        super.addListener(hwVersion);
    }

    // One of power on self-test items - production test
    protected class SelftestProduction implements ISelftestListener
    {

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check non-volatile memory test.
         * During production line it shall set a proper value to the storage.
         * and check this string gets from the property service.
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range :valid SafetyBoolean object
         *         Unit:SafetyBoolean
         *         Scaling: 1
         */
        @Override
        public SafetyBoolean doAction()
        {
            final String CHECKNUMBER = "55AA";

            final String PERSIST_SYS_PRODUCTION = "persist.sys.productionDone";

            boolean isProductionDone = false;

            // If the isResultOK is true, the test result is success.
            SafetyBoolean isResultOK = SafetyBoolean.TRUE;

            String property = null;

            try
            {
                property = CustJavaFrameworkManager.getRCSystemPropertyService(
                        null).getProperty(PERSIST_SYS_PRODUCTION, "");

                // Check the string whether is equal to "55AA"
                isProductionDone = property.equals(CHECKNUMBER);
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Apply to the coding standard
            }

            // temperate use for development.
            isProductionDone = true;

            if (true == isProductionDone)
            {
                isResultOK = SafetyBoolean.TRUE;
            }
            else
            {
                isResultOK = SafetyBoolean.FALSE;
            }

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41002, null);

            Debug.printI(TAG, "[isResultOK] checkProductData ?" + isResultOK);
            return isResultOK;
        }

    }

    // One of power on self-test items - configuration
    protected class SelftestConfiguration implements ISelftestListener
    {

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check configuration data.
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range: Valid SafetyBoolean object
         *         Unit: SafetyBoolean
         *         Scaling: 1
         * 
         */
        @Override
        public SafetyBoolean doAction()
        {
            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            final MeterParameterMatrix mp = MeterParameterMatrix
                    .getMeterParameterMatrixInstance();

            // Validate object
            CommonUtils.objectCheck(mp);

            // To do test configure data which load parameters and
            // verify CRC of configure data from power on android system.
            // Get initialization result
            isResultOK = mp.getInitializationResult();

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41003, null);

            Debug.printI(TAG, "[isResultOK] checkConfigData ?: " + isResultOK);

            return isResultOK;
        }

    }

    // One of power on self-test items - check measure engine result
    protected class SelftestMeasureEngine implements ISelftestListener
    {
        // set a count down timer for checking time out is occurred.
        private TimeoutTimer mBGMTimeOutTimer = null;

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check the post of measurement engine.
         * 
         * @see mSyncBGMReady[in]
         *      mContext[in]
         *      mBGMTimeOutTimer[out]
         *      mIsBGMTimeoutOccurred[out]
         *      mIsBGM_POST_Result_OK[out]
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range: valid SafetyBoolean object
         *         Unit: SafetyBoolean
         *         Scaling: 1
         * 
         */
        @Override
        public SafetyBoolean doAction()
        {
            // long CheckMesurementTimeout = TIMEOUT_MSEC;
            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            byte isBGMTimeoutOccurredByte = 0;

            mBGMTimeOutTimer = new TimeoutTimer(mContext)
            {
                /**
                 * During dispatching the command time-out occurred.
                 * 
                 * @see mSyncBGMReady[in]
                 *      mIsBGMTimeoutOccurred[out]
                 * 
                 * @return None
                 */
                @Override
                public void onFinish()
                {
                    mIsBGMTimeoutOccurred = SafetyBoolean.TRUE;
                    mSyncBGMReady.open();
                    Debug.printI(TAG,
                            "[sync. mSyncBGMReady]Timeout is occurred ");
                }

                /**
                 * Callback fired on regular interval.
                 * 
                 * @see mContext[in]
                 *      mByteSafetyTRUE[in]
                 *      mBGMTimeOutTimer[in]
                 *      mSyncBGMReady[in]
                 *      mIsBGMTimeoutOccurred[out]
                 * 
                 * @return None
                 */
                @Override
                public void onTick()
                {
                    byte isSettingResultByte = (byte) 0X00;

                    // Read the BGM's post result
                    final SafetyBoolean isSettingResultOK = NugenGeneralModel
                            .getSafetyBoolean(
                                    mContext,
                                    NugenFrameworkConstants.BGMConstants.KEY_BG_POST,
                                    SafetyBoolean.FALSE);

                    Debug.printI(TAG, "[sync. mSyncBGMReady]key_BG_POST "
                            + isSettingResultOK);

                    isSettingResultByte = isSettingResultOK.getByte();

                    if (mByteSafetyTRUE == isSettingResultByte)
                    {
                        // release time-out object
                        mBGMTimeOutTimer.cancel();

                        mIsBGMTimeoutOccurred = SafetyBoolean.FALSE;
                        mSyncBGMReady.open();
                    }
                }
            };

            // start time-out timer
            mBGMTimeOutTimer.start();

            Debug.printI(TAG, "[sync. mSyncBGMReady]>>>>>> Lock wait ");
            // Lock and wait synchronized signal
            mSyncBGMReady.block();

            Debug.printI(TAG, "[sync. mSyncBGMReady] Lock release ");

            // check whether the time out is occurred
            isBGMTimeoutOccurredByte = mIsBGMTimeoutOccurred.getByte();

            if (mByteSafetyTRUE == isBGMTimeoutOccurredByte)
            {
                mIsBGM_POST_Result_OK = SafetyBoolean.FALSE;
            }
            else
            {
                mIsBGM_POST_Result_OK = SafetyBoolean.TRUE;
            }

            isResultOK = mIsBGM_POST_Result_OK;

            Debug.printI(TAG, "[isResultOK] CheckMeasureEngine ?: "
                    + isResultOK);

            mSyncBGMReady.close();
            mBGMTimeOutTimer.release();
            mBGMTimeOutTimer = null;

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41004, null);
            return isResultOK;
        }

    }

    // One of power on self-test items - verify data and time
    protected class SelftestVerifyRTC implements ISelftestListener
    {

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check the RTC for errors.
         * 
         * @see mContext[in]
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range :valid SafetyBoolean object
         *         Unit: SafetyBoolean
         *         Scaling: 1
         */
        @Override
        public SafetyBoolean doAction()
        {
            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            boolean result = false;

            ITimeManagementService mTMControl = null;

            // Validate object
            CommonUtils.objectCheck(mContext);

            mTMControl = CustJavaFrameworkManager
                    .getTimeManagementService(mContext);

            try
            {
                // Validate object
                if (null != mTMControl)
                {
                    result = mTMControl.verifyBGMTime();

                    Debug.printI(TAG, "[isResultOK] verifyTime -- result : "
                            + result);

                    if (true == result)
                    {
                        isResultOK = SafetyBoolean.TRUE;
                    }
                    else
                    {
                        isResultOK = SafetyBoolean.FALSE;
                    }
                }
                else
                {
                    isResultOK = SafetyBoolean.FALSE;
                }

            }
            catch (RemoteException e)
            {
                isResultOK = SafetyBoolean.FALSE;
            }
            finally
            {
                // Apply to the coding standard
            }

            Debug.printI(TAG, "[isResultOK] checkVerifyRTC ? " + isResultOK);

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41005, null);

            return isResultOK;
        }

    }

    // One of power on self-test items - verify charge and battery status
    protected class SelftestPowerSupplyTest implements ISelftestListener
    {

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check power supply test.
         * 
         * @see mContext[in]
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range :valid SafetyBoolean object
         *         Unit: SafetyBoolean
         *         Scaling: 1
         */
        @Override
        public SafetyBoolean doAction()
        {
            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            String batteryInfo = null;

            int batteryLevel = 0;

            final SafetyString safeBatteryInfo = NugenSettingModel.getString(
                    mContext,
                    NugenFrameworkConstants.PowerManagerConstants.BATTERY_INFO);

            // Validate object
            CommonUtils.objectCheck(safeBatteryInfo);

            batteryInfo = safeBatteryInfo.getString();

            JSONObject json = null;

            try
            {
                json = new JSONObject(batteryInfo);
                batteryLevel = (Integer) json.get(BatteryManager.EXTRA_LEVEL);
                isResultOK = checkBatteryLevel(batteryLevel);
            }
            catch (JSONException e)
            {
                isResultOK = SafetyBoolean.FALSE;
            }
            finally
            {
                // Apply to the coding standard
            }

            Debug.printI(TAG, "[isResultOK] check PowerSupply ?: " + isResultOK);

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41006, null);

            isResultOK = SafetyBoolean.TRUE;

            return isResultOK;
        }

        /**
         * Check the current battery level to identify if it is in Low Battery
         * or Empty Battery.
         * 
         * @param level[in] Current battery level
         *            Range: -2^31 - 2^31-1
         *            Unit: int
         *            Scaling: 1
         * 
         * @return SafetyBoolean [out] return the result that check whether the
         *         battery
         *         level is in the range.
         *         Range: Valid SafetyBoolean object
         *         Unit:SafetyBoolean
         *         Scaling: 1
         */
        private SafetyBoolean checkBatteryLevel(final int level)
        {
            int constBATTERY_DEAD = 0;

            int constBATTERY_EMPTY = 0;

            SafetyBoolean isResultOK = SafetyBoolean.TRUE;

            // Get battery threshold from Config Matrix
            final SafetyString sKey1 = new SafetyString(
                    ConfigParameter.DEAD_BATTERY_THRESHOLD,
                    CRCTool.generateCRC16(ConfigParameter.DEAD_BATTERY_THRESHOLD
                            .getBytes()));

            SafetyNumber<Integer> deadValue = null;

            final SafetyString sKey2 = new SafetyString(
                    ConfigParameter.EMPTY_BATTERY_THRESHOLD,
                    CRCTool.generateCRC16(ConfigParameter.EMPTY_BATTERY_THRESHOLD
                            .getBytes()));

            SafetyNumber<Integer> emptyValue = null;

            deadValue = ReadConfig.getIntegerDataByKey(sKey1);
            emptyValue = ReadConfig.getIntegerDataByKey(sKey2);

            if ((null != deadValue) && (null != emptyValue))
            {
                constBATTERY_DEAD = deadValue.get();
                constBATTERY_EMPTY = emptyValue.get();

                // check battery range
                if ((constBATTERY_EMPTY > level) && (constBATTERY_DEAD < level))
                {
                    isResultOK = SafetyBoolean.FALSE;
                }
                else
                {
                    isResultOK = SafetyBoolean.TRUE;
                }
            }
            else
            {
                // Apply to the coding standard
            }

            return isResultOK;
        }

    }

    // One of power on self-test items - check hardware insulin button
    protected class SelftestInsulinButton implements ISelftestListener
    {

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check the insulin button is not pressed.
         * 
         * @see mInsulinKeyState[in]
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range: Valid SafetyBoolean object
         *         Unit: SafetyBoolean
         *         Scaling: 1
         */
        @Override
        public SafetyBoolean doAction()
        {
            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            // SelftestNative.getInstance();
            // SelftestNative.peekInsulinKeyState();

            if (KEY_PRESSED == mInsulinKeyState)
            {
                isResultOK = SafetyBoolean.FALSE;
            }
            else
            {
                isResultOK = SafetyBoolean.TRUE;
            }

            Debug.printI(TAG, "[isResultOK] checkInsulinButtonPressed ?? "
                    + isResultOK);

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41007, null);

            return isResultOK;
        }
    }

    // One of power on self-test items - check Comms'POST result
    protected class SelftestCheckCommsPOST implements ISelftestListener
    {

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check the post ready of communication
         * processor.
         * 
         * @see mContext[in]
         *      mSyncCommsReady[in]
         *      mIsCommsInfo_Result_OK[out]
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range :valid SafetyBoolean object
         *         Unit:SafetyBoolean
         *         Scaling: 1
         */
        @Override
        public SafetyBoolean doAction()
        {
            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            byte isCommsInfoByte = (byte) 0X00;

            String errorlog = null;

            SafetyString safetyErrorlog = null;

            // Define Count Down Timer local variable
            TimeoutTimer mCommsTimeOutTimer = null;

            byte isTimeoutOccurred = mByteSafetyFALSE;

            mCommsTimeOutTimer = new TimeoutTimer(mContext)
            {
                @Override
                public void onFinish()
                {
                    mSyncCommsReady.open();
                }
            };

            // start time-out timer
            mCommsTimeOutTimer.start();

            Debug.printI(TAG, "[sync. mSyncCommsReady]>>>>>> Lock wait ");

            // Lock and wait synchronized signal
            mSyncCommsReady.block();

            Debug.printI(TAG, "[sync. mSyncCommsReady] Lock release ");

            // check whether the time out is occurred
            isTimeoutOccurred = mCommsTimeOutTimer.getResult().getByte();
            isCommsInfoByte = mIsCommsInfo_Result_OK.getByte();

            if (isTimeoutOccurred == mByteSafetyTRUE)
            {
                mIsCommsInfo_Result_OK = SafetyBoolean.FALSE;
            }
            else
            {
                if (isCommsInfoByte == mByteSafetyFALSE)
                {
                    errorlog = getCommsErrorLog();

                    Debug.printI(TAG, "[errorCommslog] : " + errorlog);

                    safetyErrorlog = new SafetyString(errorlog,
                            CRCTool.generateCRC16(errorlog.getBytes()));
                }
                else
                {
                    mIsCommsInfo_Result_OK = SafetyBoolean.TRUE;
                }
            }

            isResultOK = mIsCommsInfo_Result_OK;

            Debug.printI(TAG, "[isResultOK] checkCommsPOST ? " + isResultOK);

            // release time-out object
            mCommsTimeOutTimer.cancel();
            mCommsTimeOutTimer.release();
            mCommsTimeOutTimer = null;

            // close synchronized signal
            mSyncCommsReady.close();

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41008, safetyErrorlog);

            return isResultOK;
        }

        /**
         * Get the error log comes from communication processor.
         * 
         * @see mContext[in]
         * 
         * @return String [out] return error log string
         *         processor
         *         Range :valid String object
         *         String
         *         Scaling: 1
         */
        private String getCommsErrorLog()
        {
            String errCommsLog = "";

            // For use as a synchronous signal
            final ConditionVariable syncCommsErrLog = new ConditionVariable();

            // Send command to Comms to get error log
            final BLEController instance = BLEController.getInstance();

            // Validate object
            if (instance != null)
            {
                instance.getErrorLog(new ResponseCallback()
                {
                    public void onRequestCompleted(SafetyBoolean result)
                    {
                        // unlock synchronized signal
                        syncCommsErrLog.open();
                    }
                });

                // Use asynchronous way needs to wait callback function finish.
                syncCommsErrLog.block();

                // get error log string
                errCommsLog = NugenGeneralModel.getString(mContext,
                        GetErrorLog.COMM_ERROR_LOG).getString();

            }
            else
            {
                // Apply to the coding standard
            }

            // close signal
            syncCommsErrLog.close();

            return errCommsLog;
        }

    }

    // One of power on self-test items - check hardware version result
    protected class SelftestHardwareVersion implements ISelftestListener
    {

        /**
         * Implement the action of the assigned self-test item here.
         * This action shall pass check the hardware version test.
         * 
         * @see mContext[in]
         * 
         * @return SafetyBoolean [out] the result of the assigned self-test
         *         item
         *         Range :valid SafetyBoolean object
         *         Unit:SafetyBoolean
         *         Scaling: 1
         */
        @Override
        public SafetyBoolean doAction()
        {
            // Define debug tag
            final String HARDWARE_FORMAL_VERSION = "ES2.3";

            String extraString = "";

            SafetyString safetyString = null;

            SafetyBoolean isResultOK = SafetyBoolean.FALSE;

            String hwVersion = null;
            //FIXME
            // Get HW version number
//            final SafetyString safetyHWVersion = NugenProductionModel
//                    .getString(
//                            mContext,
//                            NugenFrameworkConstants.ProductionConstants.KEY_HARDWAREVERSION);
//
//            // Validate object
//            CommonUtils.objectCheck(safetyHWVersion);
//
//            hwVersion = safetyHWVersion.getString();
//
//            // Is hardware version equal to null?
//            if (hwVersion != null)
//            {
//                // compare hardware version
//                if (hwVersion.equals(HARDWARE_FORMAL_VERSION))
//                {
//                    isResultOK = SafetyBoolean.TRUE;
//                }
//                else
//                {
//                    isResultOK = SafetyBoolean.FALSE;
//                    extraString = " Now setup in meter use hardware version is "
//                            + hwVersion;
//                }
//            }
//            else
//            {
//                isResultOK = SafetyBoolean.FALSE;
//            }
            isResultOK = SafetyBoolean.TRUE;
            Debug.printI(TAG, "[isResultOK] checkhwVersion ? " + isResultOK);

            safetyString = new SafetyString(extraString,
                    CRCTool.generateCRC16(extraString.getBytes()));

            // Set the EMWR-ID is currently in use
            logEmwrID(EMWRList.EMW41009, safetyString);

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
 */// (R23542 2015-11-06 05:49:52 JamesLee)
// ----------------------------------------------------------------------------
// Refine for production parameter.
