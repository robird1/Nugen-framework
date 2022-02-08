/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: ${package_name}.${type_name}
 * Brief:
 * 
 * Create Date: ${date}
 * $$Revision: 24063 $$
 * $$Author: TerryHsieh $$
 * $$Id: TimeManagementService.java 24063 2015-11-13 05:30:12Z TerryHsieh $$
 */
package com.accu_chek.solo_m.rcapp.application.timemanagement;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.ConditionVariable;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmUtils;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBgmConstant;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrix;
import com.accu_chek.solo_m.rcapp.application.timemanagement.IDateTimeSettingCallback;
import com.accu_chek.solo_m.rcapp.application.timemanagement.ITimeManagementService;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBGMControl;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IMeInformationListener;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEResponseReceiver;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.CommandCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.BlankMessageResponse;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRButtonCallback;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.DataTypeMismatchException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.TimeSegmentTable;

import android.util.Log;

/**
 * <h1>java cust framework service name.</h1> Description class imfornation.
 * 
 * @author Name
 * @version 1.0
 * @since YYYY-MM-DD
 */
@SuppressLint("DefaultLocale")
public class TimeManagementService extends ITimeManagementService.Stub
{

    // Tag for debugging
    private static final String TAG = "[TM]TimeManagementService";

    // Flag for debugging
    private static final boolean isDEBUG = true;

    // key post-fix for accessing 2 channels data in non-volatile memory
    private final static String KEY2_POSTFIX = "_2";

    // Key to access meter time in non-volatile memory
    private static final String GLOBAL_METER_TIME = "global_meter_time";

    // Time translated factors
    private static final int MINUTES_IN_AN_HOUR = 60;
    private static final int SECONDS_IN_A_MINUTE = 60;
    private static final int MILLISEC_IN_A_SECOND = 1000;

    // Database op code
    private static final int INSERT_DATA = 22;
    private static final int QUERY_ALL_DATA = 23;
    private static final int QUERY_TIMEPERIOD_DATA = 24;
    private static final int QUERY_ID_DATA = 25;
    private static final int UPDATE_DATA = 26;
    private static final int DELETE_DATA = 27;

    // Activity context
    private Context mContext = null;

    // Current time segment id
    private SafetyChannel<Integer> mCurrentRecordId = null;

    // TimeManagementModel object
    private TimeManagementModel mTM_Model = null;

    // Configuration matrix object
    private MeterParameterMatrix mMPM = null;

    // DateTimeSetting UI activity callback
    private IDateTimeSettingCallback mDateTimeSettingCallback = null;

    // Bgm Date in format string
    private String mBgmDate = null; // yymmdd

    // Bgm time in format string
    private String mBgmTime = null; // hhmmss

    // Time in SafetyChannel<Long>
    private SafetyChannel<Long> mSCTime = null;

    // Bgm control object
    private IBGMControl mBgmCtrl = null;

    // For use as a synchronous signal
    private ConditionVariable mSyncBgmReadReady = null;

    // An executor to create a thread to read bgm time
    private ExecutorService mBgmReadExecutor = null;

    // An executor to create a thread to write bgm time
    private ExecutorService mBgmWriteExecutor = null;

    // An executor to create a thread to set pump time
    private ExecutorService mSetPumpTimeExecutor = null;

    // An timeout scheduler for bgm read thread
    private ScheduledExecutorService mBgmReadScheduler = null;

    // An timeout scheduler for bgm write thread
    private ScheduledExecutorService mBgmWriteScheduler = null;

    // An ConditionVariable to block set pump thread for waiting response
    private ConditionVariable mSetPumpTimeResponseReady = null;

    // An ConditionVariable to block set pump thread for waiting confirm
    private ConditionVariable mSetPumpTimeConfirmReady = null;

    // An timeout scheduler for set pump time response
    private ScheduledExecutorService mSetPumpTimeResponseScheduler = null;

    // An timeout scheduler for set pump time confirm
    private ScheduledExecutorService mSetPumpTimeConfirmScheduler = null;

    // A retry counter of set pump time response
    private int mSetPumpTimeResponseRetryCount = 0;

    // A retry counter of set pump time confirm
    private int mSetPumpTimeConfirmRetryCount = 0;

    // A flag to check set bgm time status
    private SafetyBoolean mSetBGMTime = SafetyBoolean.FALSE;

    // A flag to check set bgm date status
    private SafetyBoolean mSetBGMDate = SafetyBoolean.FALSE;

    // New a parameter for set pump time
    private BLERequestParameter mSetPumpTimeParameter = new BLERequestParameter();

    // Callback object for asynchronous comm command
    private SetPumpTimeCallBack mSetPumpTimeCallback = new SetPumpTimeCallBack();

    /**
     * A Runnable process for setting pump date/time
     */
    Runnable mSetPumpTimeRunnable = new Runnable()
    {
        /**
         * Override function run of Runnable
         * 
         * @param void [in]
         * 
         * @return void [out]
         */
        public void run()
        {
            BLEController controller = BLEController.getInstance();
            mSetPumpTimeConfirmRetryCount = 2;
            do
            {
                mSetPumpTimeResponseRetryCount = 2;
                do
                {
                    mSetPumpTimeResponseReady = new ConditionVariable();
                    // Timeout scheduler
                    mSetPumpTimeResponseScheduler = Executors.newScheduledThreadPool(1);
                    mSetPumpTimeResponseScheduler.schedule(new Runnable()
                    {
                        /**
                         * Override function run of Runnable
                         * 
                         * @param void [in]
                         * 
                         * @return void [out]
                         */
                        public void run()
                        {
                            //Decrease set pump time response retry count
                            mSetPumpTimeResponseRetryCount--;
                            mSetPumpTimeResponseReady.open();
                        }
                    }, 10, TimeUnit.SECONDS);
                    // Send command to Comms to set pump time
                    controller.sendSoloMCP(mSetPumpTimeParameter,
                            mSetPumpTimeCallback);
                    mSetPumpTimeResponseReady.block();
                    mSetPumpTimeResponseScheduler.shutdown();
                } while (mSetPumpTimeResponseRetryCount != 0);
                
                mSetPumpTimeConfirmReady = new ConditionVariable();
                // Timeout scheduler
                mSetPumpTimeConfirmScheduler = Executors.newScheduledThreadPool(1);
                mSetPumpTimeConfirmScheduler.schedule(new Runnable()
                {
                    /**
                     * Override function run of Runnable
                     * 
                     * @param void [in]
                     * 
                     * @return void [out]
                     */
                    public void run()
                    {
                        //Decrease set pump time confirm retry count
                        mSetPumpTimeConfirmRetryCount--;
                        mSetPumpTimeConfirmReady.open();
                    }
                }, 10, TimeUnit.SECONDS);
                mSetPumpTimeConfirmReady.block();
                mSetPumpTimeConfirmScheduler.shutdown();
            } while (mSetPumpTimeConfirmRetryCount != 0);
            mSetPumpTimeExecutor.shutdown();
        }
    };

    /**
     * A Runnable process for read bgm date/time
     */
    Runnable mBgmReadRunnable = new Runnable()
    {
        /**
         * Override function run of Runnable
         * 
         * @param void [in]
         * 
         * @return void [out]
         */
        @Override
        public void run()
        {
            SafetyBoolean sbBgmReady = null;
            mBgmTime = null;
            mBgmDate = null;
            mSCTime = null;
            sbBgmReady = NugenGeneralModel.getSafetyBoolean(
                    BgmUtils.getContext(),
                    NugenFrameworkConstants.BGMConstants.KEY_BG_POST);
            
            // Check BGM module ready status
            if (sbBgmReady.equals(SafetyBoolean.TRUE))
            {
                // The bgmCtrl null check has been done outside
                try
                {
                    // Call get BGM datetime
                    mBgmCtrl.getMeInformation(IBgmConstant.GETBGMDATETIME,
                            new IMeInformationListener.Stub()
                            {

                                /**
                                 * Override onCodeKeyInformation function of
                                 * IMeInformationListener
                                 * 
                                 * @param SafetyByteArray [in] number
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @param SafetyByteArray [in] date
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @param SafetyByteArray [in] status
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @param SafetyByteArray [in] field
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onCodeKeyInformation(
                                        SafetyByteArray number,
                                        SafetyByteArray date,
                                        SafetyByteArray status,
                                        SafetyByteArray field)
                                        throws RemoteException
                                {
                                    /**
                                     * This method is forced to override the functionality of an existing
                                     * method of super class and no need to do action in this method.
                                     */
                                }

                                /**
                                 * Override onStripCounter function of
                                 * IMeInformationListener
                                 * 
                                 * @param SafetyByteArray [in] counter
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onStripCounter(
                                        SafetyByteArray counter)
                                        throws RemoteException
                                {
                                    /**
                                     * This method is forced to override the functionality of an existing
                                     * method of super class and no need to do action in this method.
                                     */
                                }

                                /**
                                 * Override onSuccess function of
                                 * IMeInformationListener
                                 * 
                                 * @param SafetyString [in] key of the success
                                 *            command
                                 *            Range: valid SafetyString object
                                 *            Unit: SafetyString
                                 *            Scaling: 1
                                 * 
                                 *            SafetyString [in] string parameter
                                 *            Range: valid SafetyString object
                                 *            Unit: SafetyString
                                 *            Scaling: 1
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onSuccess(SafetyString key,
                                        SafetyString s) throws RemoteException
                                {
                                    if (s != null)
                                    {
                                        if (key.getString().equals(
                                                "ReadTimeCommand"))
                                        {
                                            // Format is HHmmss
                                            mBgmTime = s.getString();
                                        }
                                        else if (key.getString().equals(
                                                "ReadDateCommand"))
                                        {
                                            // Format is yyMMdd
                                            mBgmDate = s.getString();
                                        }
                                        // If both BGM time and BGM date
                                        // are read back
                                        if ((mBgmTime != null)
                                                && (mBgmDate != null))
                                        {
                                            String formatStr = null;
                                            SimpleDateFormat sdf = new SimpleDateFormat(
                                                    "yyMMdd HHmmss", Locale.US);
                                            Date date = null;
                                            long ltime = 0;
                                            Number ch1Val = 0;
                                            Number ch2Val = 0;
                                            formatStr = mBgmDate + " "
                                                    + mBgmTime;
                                            try
                                            {
                                                Calendar cal = Calendar
                                                        .getInstance();
                                                // Set the 2 digit year base is
                                                // 2000 year
                                                cal.clear();
                                                cal.set(Calendar.YEAR, 2000);
                                                sdf.set2DigitYearStart(cal
                                                        .getTime());
                                                date = sdf.parse(formatStr);
                                            }
                                            catch (ParseException e)
                                            {
                                                e.printStackTrace();
                                            }
                                            finally
                                            {
                                                // Apply to the coding
                                                // standard.
                                            }
                                            // Check whether date is null or not
                                            CommonUtils.objectCheck(date);
                                            //MeterTime
                                            ltime = date.getTime();
                                            ch1Val = CommonUtils
                                                    .encodeCH1Value(ltime);
                                            ch2Val = CommonUtils
                                                    .encodeCH2Value(ltime);
                                            mSCTime = new SafetyChannel<Long>(
                                                    ch1Val.longValue(), ch2Val
                                                            .longValue());
                                            // Open the mSyncBgmReadReady.block
                                            mSyncBgmReadReady.open();
                                        }
                                    }
                                }

                                /**
                                 * Override onError function of
                                 * IMeInformationListener
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onError() throws RemoteException
                                {
                                    mSCTime = null;
                                    // Open the mSyncBgmReadReady.block
                                    mSyncBgmReadReady.open();
                                }

                            });
                }
                catch (RemoteException e1)
                {

                    e1.printStackTrace();
                }
                finally
                {
                    // Apply to the coding standard.
                }
            }
            else
            {
                //Bgm is not ready
                mSCTime = null;
                // Open the mSyncBgmReadReady.block
                mSyncBgmReadReady.open();
            }
        } // End of run()
    };

    /**
     * An inner class for implementing a response callback of set pump time
     * 
     */
    final class SetPumpTimeCallBack implements ResponseCallback
    {
        /**
         * Override function onRequestCompleted of ResponseCallback
         * 
         * @param SafetyBoolean [in] result
         *            Range: valid SafetyBoolean object
         *            Unit: SafetyBoolean
         *            Scaling: 1
         * 
         * @return void [out]
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isCallBack = result.equals(SafetyBoolean.TRUE);
            if (isCallBack)
            {
                // Command response sucess
                mSetPumpTimeResponseRetryCount = 0;
            }
            else
            {
                //Command response Fail!
                mSetPumpTimeResponseRetryCount--;
            }
        }
    }

    /**
     * A broadcast receiver inner class for receiving specific messages which
     * include BLE response messages and internal one shot alarm messages.
     * 
     */
    public class TimeManagementReceiverInService extends BroadcastReceiver
    {
        /**
         * Override function onReceive of BroadcastReceiver
         * 
         * 
         * @param Context [in] activity context
         *            Range: valid Context object
         *            Unit: Context
         *            Scaling: 1
         * @param Intent [in] intent
         *            Range: valid Intent object
         *            Unit: Intent
         *            Scaling: 1
         */
        @Override
        public void onReceive(Context context, Intent intent)
        {
            String action = null;
            Bundle bBundle = null;
            String msgStr = null;

            CommonUtils.objectCheck(intent);
            CommonUtils.objectCheck(context);

            action = intent.getAction();

            if (BlankMessageResponse.class.getName().equalsIgnoreCase(action))
            {
                ResponsePack pack = intent
                        .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
                BlankMessageResponse response = (BlankMessageResponse) pack
                        .getResponse();
                int command = response.getCommand().get();
                if (CommandCode.BT_TIME_SYNC_IND == command)
                {
                    BLEController.getInstance(context).syncMicroPumpTime(null);
                }
                else
                {
                    // Apply to the coding standard.
                }
            }
            else if (ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND
                    .equalsIgnoreCase(action))
            {
                // OpCode
                int opCode = 0;
                int requestOpCode = 0;
                int responseCode = 0;
                SafetyByteArray sbData = null;
                byte[] bData = null;
                ByteBuffer bbData = null;

                // Get Response Pack
                ResponsePack pack = intent
                        .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
                // Get Response
                AttributeChangeNotification attResponse = (AttributeChangeNotification) pack
                        .getResponse();

                // attResponse.getResult().get();
                // Get Indication Data
                sbData = attResponse.getData();
                bData = sbData.getByteArray();
                bbData = ByteBuffer.wrap(bData);

                bbData.rewind();
                bbData.order(ByteOrder.LITTLE_ENDIAN);
                // Get OpCode
                opCode = bbData.getShort() & 0xffff;
                // Get Request Op Code
                requestOpCode = bbData.getShort() & 0xffff;
                // Get Response Code Value
                responseCode = bbData.getShort() & 0xff;

                // The date/time change shell be confirmed by micro pump
                if ((ControlPointConstant.OpCode.SOLOM_CP_RESP == opCode)
                        && (ControlPointConstant.OpCode.SOLOM_SET_DATE_AND_TIME == requestOpCode))
                {
                    if (ControlPointConstant.ResponseCode.SUCCESS == responseCode)
                    {
                        // Set pump date/time confirm
                        mSetPumpTimeConfirmRetryCount = 0;
                    }
                    else
                    {
                        mSetPumpTimeConfirmRetryCount--;
                        // Apply to the coding standard
                    }
                }
                else
                {
                    // Apply to the coding standard.
                }
            }
            else
            {
                bBundle = intent.getExtras();
                // Check whether bBundle is null or not
                CommonUtils.objectCheck(bBundle);
                msgStr = (String) bBundle.get("msg");
                if (msgStr.equals("sync_time"))
                {
                    try
                    {
                        syncBGMTime();
                    }
                    catch (RemoteException e)
                    {
                        e.printStackTrace();
                    }
                    finally
                    {
                        // Apply to the coding standard
                    }
                }
                else if (msgStr.equals("backup_time"))
                {
                    backupRCTime();
                }
                else if (msgStr.equals("TM DATE CHANGED"))
                {
                    backupRCTime();
                    try
                    {
                        setRCTime(System.currentTimeMillis());
                    }
                    catch (RemoteException e)
                    {

                        e.printStackTrace();
                    }
                    finally
                    {
                        // Apply to the coding standard
                    }
                }
                else if (msgStr.equals("TM TIME CHANGED"))
                {
                    try
                    {
                        setRCTime(System.currentTimeMillis());
                    }
                    catch (RemoteException e)
                    {

                        e.printStackTrace();
                    }
                    finally
                    {
                        // Apply to the coding standard
                    }
                }
                else if (msgStr.equals("TM TIMEZONE CHANGED"))
                {
                    try
                    {
                        setRCTime(System.currentTimeMillis());
                    }
                    catch (RemoteException e)
                    {

                        e.printStackTrace();
                    }
                    finally
                    {
                        // Apply to the coding standard
                    }
                }
            }
        }
    }

    /**
     * Constructor of TimeManagementService
     * 
     * @param Context [in] context
     *            Activity context
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling 1
     * 
     * @return void [out]
     */
    public TimeManagementService(Context context)
    {
        long ltimestamp = System.currentTimeMillis();
        long ch1Val = CommonUtils.encodeCH1Value(ltimestamp);
        long ch2Val = CommonUtils.encodeCH2Value(ltimestamp);
        SafetyChannel<Long> currentTime = new SafetyChannel<Long>(ch1Val, ch2Val);
        
        mContext = context;
        mMPM = MeterParameterMatrix.getMeterParameterMatrixInstance();
        //Create the first TimeSegment and return the record id
        mTM_Model = new TimeManagementModel(mContext);
        mCurrentRecordId = createTimeSegment(currentTime);
    }

    /**
     * A function for some time sensitive activities to register time sync
     * listener to TimeManagement.
     * 
     * @param TimeSyncListener [in] listener
     *            TimeSyncListener to listen time sync event for running
     *            pre-process and post-process of time sync.
     *            Range: valid TimeSyncListener object
     *            Unit: TimeSyncListener
     *            Scale: 1
     * @return SafetyBoolean [out] Delete pre line return if exist. Parameter
     *         Description
     * @throws DataIntegrityException
     */
    public SafetyBoolean registerTimeSyncListener(TimeSyncListener listener)
    {
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(listener);
        return mTM_Model.addTimeSyncListener(listener);
    }

    /**
     * A function for some time sensitive activities to unregister time sync
     * listener from TimeManagement.
     * 
     * @param TimeSyncListener [in] listener
     *            TimeSyncListener to listen time sync event for running
     *            pre-process and post-process of time sync.
     *            Range: valid TimeSyncListener object
     *            Unit: TimeSyncListener
     *            Scale: 1
     * @return SafetyBoolean [out] result of unregisterTimeSyncListener
     *         Range: valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     * @throws DataIntegrityException
     */
    public SafetyBoolean unregisterTimeSyncListener(TimeSyncListener listener)
    {
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(listener);
        return mTM_Model.removeTimeSyncListener(listener);
    }

    /**
     * A function to set meter time.
     * 
     * @param SafetyChannel<Long> [in] meterTime
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * @return void [out]
     * @throws DataIntegrityException
     */
    public void setMeterTime(SafetyChannel<Long> timestamp)
    {
        SafetyBoolean isTimeChangeable = null;

        // Check whether input parameter is null or not
        CommonUtils.objectCheck(timestamp);
        isTimeChangeable = isRCTimeChangeable(timestamp, SafetyBoolean.FALSE);
        if (isTimeChangeable.equals(SafetyBoolean.TRUE))
        {
            mTM_Model.triggerBeforeTimeSyncListener();
            run_am_setTime(timestamp);
            mTM_Model.triggerAfterTimeSyncListener();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * A function to get meter time.
     * 
     * @param void [in]
     * @return SafetyChannel<Long> [out] Timestamp of meter time
     *         Range: valid SafetyChannel<Long> object
     *         Unit: SafetyChannel<Long>
     *         Scale: 1
     */
    public SafetyChannel<Long> getMeterTime()
    {
        // Calendar calendar = Calendar.getInstance();
        // long ltime = calendar.getTimeInMillis();
        long ltime = System.currentTimeMillis();
        Number ch1Val = CommonUtils.encodeCH1Value(ltime);
        Number ch2Val = CommonUtils.encodeCH2Value(ltime);
        SafetyChannel<Long> scTime = new SafetyChannel<Long>(
                ch1Val.longValue(), ch2Val.longValue());
        try
        {
            syncBGMTime();
        }
        catch (RemoteException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
        return scTime;
    }

    /**
     * A function to reset BGM time to be the same as meter(system) time.
     * 
     * @param
     * 
     * @return void [out]
     */
    public void resetBGMTime() throws RemoteException, DataIntegrityException
    {
        long ch1Val = 0;
        long ch2Val = 0;
        long ltime = System.currentTimeMillis();
        SafetyChannel<Long> scTime = null;
        SafetyBoolean isTimeChangeable = null;

        ch1Val = CommonUtils.encodeCH1Value(ltime);
        ch2Val = CommonUtils.encodeCH2Value(ltime);
        scTime = new SafetyChannel<Long>(ch1Val, ch2Val);

        isTimeChangeable = isRCTimeChangeable(scTime, SafetyBoolean.FALSE);

        if (isTimeChangeable.equals(SafetyBoolean.TRUE))
        {
            IBGMControl bgmCtrl = run_CustJavaFrameworkManager_getBGMControlService(mContext);
            // Check whether bgmCtrl is null or not
            CommonUtils.objectCheck(bgmCtrl);
            run_bgmCtrl_writeTimeStampToBgm(bgmCtrl);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * A function to get BGM time.
     * 
     * @param void [in]
     * 
     * @return SafetyChannel<Long> [out] Timestamp of BGM time
     *         Range: valid SafetyChannel<Long> object
     *         Unit: SafetyChannel<Long>
     *         Scale: 1
     * @throws DataIntegrityException, RemoteException
     */
    public SafetyChannel<Long> getBGMTime() throws RemoteException,
            DataIntegrityException
    {
        SafetyChannel<Long> scBGMTime = null;
        IBGMControl bgmCtrl = run_CustJavaFrameworkManager_getBGMControlService(mContext);

        // Check whether bgmCtrl is null or not
        CommonUtils.objectCheck(bgmCtrl);
        // Transfer bgm date/time to safety long
        scBGMTime = run_bgmCtrl_readBGMTimeStamp(bgmCtrl);
        if (null == scBGMTime)
        {
            // BGM is not ready
        }
        else
        {
            // Apply to the coding standard
        }
        return scBGMTime;
    }

    /**
     * A function to create a TimeSegment and store it to database.
     * 
     * @param SafetyChannel<Long> [in] startTime
     *            Start time of TimeSegment
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * 
     * @return SafetyChannel<Integer> [out] A new create TimeSegment id
     *         Range: valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scale: 1
     */
    @SuppressWarnings("unchecked")
    public SafetyChannel<Integer> createTimeSegment(
            SafetyChannel<Long> startTime)
    {
        long ltime = 0;
        SafetyChannel<Long> endTime = null;
        SafetyChannel<Integer> scSegmentID = null;
        SafetyChannel<Integer> scRecordID = null;
        DataBaseTimeSegment_Model db_insert = null;
        long ch1Val = 0;
        long ch2Val = 0;

        // Check whether input parameter is null or not
        CommonUtils.objectCheck(startTime);
        scSegmentID = mTM_Model.newTimeSegmentID();
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scSegmentID);
        db_insert = new DataBaseTimeSegment_Model();
        ltime = 0;
        ch1Val = CommonUtils.encodeCH1Value(ltime);
        ch2Val = CommonUtils.encodeCH2Value(ltime);
        endTime = new SafetyChannel<Long>(ch1Val, ch2Val);
        // Terry:Error
        scRecordID = (SafetyChannel<Integer>)run_DataBaseOp(db_insert, INSERT_DATA, scSegmentID, startTime, endTime);
        return scRecordID;
    }

    /**
     * A function to update a specific TimeSegment content in database.
     * 
     * @param SafetyChannel<Integer> [in] scRecordID
     *            The id of TimeSegment record which will be updated.
     * 
     *            Range: valid SafetyChannel<Integer> object
     *            Unit: SafetyChannel<Integer>
     *            Scale: 1
     * 
     * @param SafetyChannel<Long> [in] startTime
     *            Start time of TimeSegment
     * 
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * 
     * @param SafetyChannel<Long> [in] endTime
     *            End time of TimeSegment
     * 
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * 
     * @return void [out]
     */
    public void updateTimeSegment(SafetyChannel<Integer> scRecordID,
            SafetyChannel<Long> startTime, SafetyChannel<Long> endTime)
    {
        DataBaseTimeSegment_Model db_update = new DataBaseTimeSegment_Model();

        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scRecordID);
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(startTime);
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(endTime);
        run_DataBaseOp(db_update, UPDATE_DATA, scRecordID, startTime, endTime);
    }

    /**
     * A function to Save current timesegment and create a new timesegment
     * 
     * @param void [in]
     * 
     * @return void [out]
     */
    public void changeTimeSegment()
    {
        long ch1Val = 0;
        long ch2Val = 0;

        long ltimestamp = System.currentTimeMillis();
        SafetyChannel<Long> currentTime = null;

        ch1Val = CommonUtils.encodeCH1Value(ltimestamp);
        ch2Val = CommonUtils.encodeCH2Value(ltimestamp);
        currentTime = new SafetyChannel<Long>(ch1Val, ch2Val);

        if (null != mCurrentRecordId)
        {
            SafetyChannel<Long> startTime = null;
            TimeSegmentTable ts_data = getTimeSegmentByRecID(mCurrentRecordId);
            startTime = ts_data.getStartTime();
            if(null != startTime)
            {
                // Update start-time and end-time of last timesegment
                updateTimeSegment(mCurrentRecordId, startTime, currentTime);
            }
            else
            {
                // Apply to the coding standard
            }
        }
        else
        {
            // Apply to the coding standard
        }
        mCurrentRecordId = createTimeSegment(currentTime);
    }

    /**
     * A function to delete a specific TimeSegment in database.
     * 
     * @param SafetyChannel<Integer> [in] scRecordID
     *            The id of TimeSegment record which will be deleted.
     * 
     *            Range: valid SafetyChannel<Integer> object
     *            Unit: SafetyChannel<Integer>
     *            Scale: 1
     * 
     * @return void [out]
     */
    public void deleteTimeSegment(SafetyChannel<Integer> scRecordID)
    {
        DataBaseTimeSegment_Model db_delete = null;

        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scRecordID);
        db_delete = new DataBaseTimeSegment_Model();
        run_DataBaseOp(db_delete, DELETE_DATA, scRecordID, null, null);
    }

    /**
     * A function to get current TimeSegment ID.
     * 
     * @param void [in]
     * 
     * @return SafetyChannel<Integer> [out] Current TimeSegment id
     *         Range: valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scale: 1
     */
    public SafetyChannel<Integer> getCurrentTimeSegmentID()
    {
        SafetyChannel<Integer> scID = mTM_Model.restoreTimeSegmentID();
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scID);
        return scID;
    }

    /**
     * A function to get TimeSegment from database by specific ID
     * 
     * @param SafetyChannel<Integer> [in] timeSegmentID
     * 
     *            Range: valid SafetyChannel<Integer> object
     *            Unit: SafetyChannel<Integer>
     *            Scale: 1
     * 
     * @return ArrayList<TimeSegmentTable> [out] TimeSegmentTable object
     *         arraylist of the input id
     *         Range: valid ArrayList<TimeSegmentTable> object
     *         Unit: ArrayList<TimeSegmentTable>
     *         Scale: 1
     */
    @SuppressWarnings("unchecked")
    public TimeSegmentTable getTimeSegmentByRecID(
            SafetyChannel<Integer> scID)
    {
        TimeSegmentTable ts_data = null;
        DataBaseTimeSegment_Model db_query = null;
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scID);
        db_query = new DataBaseTimeSegment_Model();
        ts_data = (TimeSegmentTable) run_DataBaseOp(db_query,
                QUERY_ID_DATA, scID, null, null);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(ts_data);
        return ts_data;
    }

    /**
     * A function to get TimeSegments in a period of time
     * 
     * @param SafetyChannel<Long> [in] startTime
     *            Start time of searching TimeSegment
     * 
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * 
     *            SafetyChannel<Long> [in] endTime
     *            End time of searching TimeSegment
     * 
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * 
     * @return ArrayList<TimeSegmentTable> [out] TimeSegment object list which
     *         collects the TimeSegments in time slot between the input start
     *         time and end time
     *         Range: valid ArrayList<TimeSegmentTable> object
     *         Unit: ArrayList<TimeSegmentTable>
     *         Scale: 1
     */
    @SuppressWarnings("unchecked")
    public ArrayList<TimeSegmentTable> getTimeSegmentsByTimePeriod(
            SafetyChannel<Long> startTime, SafetyChannel<Long> endTime)
    {
        ArrayList<TimeSegmentTable> ts_array = null;
        DataBaseTimeSegment_Model db_query = null;

        // Check whether input parameter is null or not
        CommonUtils.objectCheck(startTime);
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(endTime);
        db_query = new DataBaseTimeSegment_Model();
        ts_array = (ArrayList<TimeSegmentTable>) run_DataBaseOp(db_query,
                QUERY_TIMEPERIOD_DATA, null, startTime, endTime);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(ts_array);
        return ts_array;
    }

    /**
     * A function to set meter time to non volatile memory.
     * 
     * @param SafetyChannel<Long> [in] meterTime
     * 
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * 
     * @return void [out]
     * @throws DataIntegrityException
     */
    public void setMeterTimeToNVM(SafetyChannel<Long> scMeterTime)
    {
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scMeterTime);
        run_NugenGeneralModel_setLong(mContext, GLOBAL_METER_TIME, scMeterTime);
    }

    /**
     * A function to get meter time from non volatile memory.
     * 
     * @param void [in]
     * 
     * @return SafetyChannel<Long> [out] Meter time
     *         Range: valid SafetyChannel<Long> object
     *         Unit: SafetyChannel<Long>
     *         Scale: 1
     */
    public SafetyChannel<Long> getMeterTimeFromNVM()
    {
        SafetyChannel<Long> scMeterTime = null;

        scMeterTime = run_NugenGeneralModel_getLong(mContext, GLOBAL_METER_TIME);
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scMeterTime);
        return scMeterTime;
    }

    /**
     * A function to return the permission condition of changing RC time
     * 
     * @param SafetyChannel<Long> [in]scTime
     * 
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scale: 1
     * 
     * @return SafetyBoolean [out] Time change enable flag
     */
    @SuppressWarnings("deprecation")
    @SuppressLint("SimpleDateFormat")
    public SafetyBoolean isRCTimeChangeable(SafetyChannel<Long> scTime, SafetyBoolean isBLEChecked)
    {
        long lTime = 0;
        SafetyString ssString = null;
        SafetyString ssMinDate = null;
        SafetyString ssMaxDate = null;
        String sMinDate = null;
        String sMaxDate = null;
        long lMinDate = 0xffffffff;
        long lMaxDate = -0xffffffff;
        long lManufactDate = -0xffffffff;
        Date date = null;
        SimpleDateFormat sdf1 = new SimpleDateFormat("dd.MM.yyyy");
        SimpleDateFormat sdf2 = new SimpleDateFormat("dd-MM-yyyy");
        SafetyBoolean isTimeChangeable = SafetyBoolean.TRUE;
        BLEController controller = null;

        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scTime);
        try
        {
            String sManufactDate = null;
            SafetyString ssManufactDate = null;
            // The meter shall support dates between and including the years
            // 2013 and 2031
            ssString = new SafetyString(ConfigParameter.KEY_MIN_DATE,
                    CRCTool.generateCRC16(ConfigParameter.KEY_MIN_DATE
                            .getBytes()));
            ssMinDate = mMPM.getParameterString(ssString);
            // Check whether input parameter is null or not
            CommonUtils.objectCheck(ssMinDate);

            sMinDate = ssString.getString();

            try
            {
                //Parse min date
                date = sdf1.parse(sMinDate);
                lMinDate = date.getTime();
            }
            catch (ParseException e)
            {

                e.printStackTrace();
            }
            finally
            {
                // Apply to the coding standard
            }

            ssString = new SafetyString(ConfigParameter.KEY_MAX_DATE,
                    CRCTool.generateCRC16(ConfigParameter.KEY_MAX_DATE
                            .getBytes()));
            ssMaxDate = mMPM.getParameterString(ssString);
            // Check whether input parameter is null or not
            CommonUtils.objectCheck(ssMaxDate);

            sMaxDate = ssString.getString();

            try
            {
                //Parse max date
                date = sdf1.parse(sMaxDate);
                lMaxDate = date.getTime();
            }
            catch (ParseException e)
            {

                e.printStackTrace();
            }
            finally
            {
                // Apply to the coding standard
            }

            lTime = CommonUtils.getOriginValue(scTime.getValueCH1(),
                    scTime.getValueCH2());

            if ((lTime < lMinDate) && (lTime > lMaxDate))
            {
                NotifyMessage errMsg = new NotifyMessage(EMWRList.EMW48003);
                NotifyProxy.showEMWR(mContext, errMsg);
                isTimeChangeable = SafetyBoolean.FALSE;
            }
//            else if(Bolus process merge is active)
//            {
//                notify the user of the prohibition of changing date and time
//            }
            else if(isBLEChecked.equals(SafetyBoolean.TRUE))
            {
                controller = BLEController.getInstance();
                if (controller != null)
                {
                    // The meter is bonded to a micro pump
                    // There is a connection to the micro pump
                    if (controller.isBonded().equals(
                            SafetyBoolean.TRUE)
                            && controller.isConnected().equals(
                                    SafetyBoolean.TRUE))
                    {
                        //BLE is bounded and connected
                        isTimeChangeable = SafetyBoolean.TRUE;
                    }
                    else
                    {
                        //BLE is not bounded or not connected
                        isTimeChangeable = SafetyBoolean.FALSE;
                    }
                }
                else
                {
                    //BLE controller is not available
                    // Apply to the coding standard
                }
            }
            else
            {
                //Do not check BLE
                isTimeChangeable = SafetyBoolean.TRUE;
            }
            // Get manufacturing date
            ssString = new SafetyString(ProductionConstants.KEY_DATE_DEFAULT,
                    CRCTool.generateCRC16(ProductionConstants.KEY_DATE_DEFAULT
                            .getBytes()));
            ssManufactDate = NugenProductionModel.getString(ssString);

            if(isTimeChangeable.equals(SafetyBoolean.TRUE))
            {
                if (ssManufactDate != null)
                {
                    sManufactDate = ssManufactDate.getString();
                    try
                    {
                        date = sdf2.parse(sManufactDate);
                        lManufactDate = date.getTime();
                        // Date earlier than or equal to the manufacturing
                        // date is
                        // unacceptable.
//The code was marked temporary, due to some device manufacturing date is wrong                      
//                        if (lTime <= lManufactDate)
//                        {
//                            NotifyMessage errMsg = new NotifyMessage(
//                                    EMWRList.EMW48004);
//                            NotifyProxy.showEMWR(mContext, errMsg);
//                            isTimeChangeable = SafetyBoolean.FALSE;
//                        }
//                        else
                        {
                            //Time is changeable
                            isTimeChangeable = SafetyBoolean.TRUE;
                        }
                    }
                    catch (ParseException e)
                    {
    
                        e.printStackTrace();
                    }
                    finally
                    {
                        // Apply to the coding standard
                    }
                }
                else
                {
                    //Manufacturing date is not acceable, ignore it!
                    isTimeChangeable = SafetyBoolean.TRUE;
                }
            }
            else
            {
                // Apply to the coding standard
            }
        }
        catch (DataIntegrityException e)
        {

            e.printStackTrace();
        }
        catch (NoSuchElementException e)
        {

            e.printStackTrace();
        }
        catch (DataTypeMismatchException e)
        {

            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }

        return isTimeChangeable;
    }

    /**
     * The following functions are export functions of the time management
     * service, the functions are declared in aidl file.
     */

    /**
     * A function to backup meter's time to share preference
     * 
     * @param void [in]
     * 
     * @return void [out]
     */
    @Override
    public void backupRCTime()
    {
        Calendar cal = null;
        Intent intent = null;
        PendingIntent pi = null;
        SafetyString ssString = null;
        SafetyNumber<Integer> snTime = null;
        long ltime = System.currentTimeMillis();
        Number ch1Val = CommonUtils.encodeCH1Value(ltime);
        Number ch2Val = CommonUtils.encodeCH2Value(ltime);
        SafetyChannel<Long> scTime = new SafetyChannel<Long>(
                ch1Val.longValue(), ch2Val.longValue());
        // backup meter time
        setMeterTimeToNVM(scTime);

        // SafetyString of backup time interval
        ssString = new SafetyString(
                ConfigParameter.KEY_DATE_TIME_BACKUP_PERIOD,
                CRCTool.generateCRC16(ConfigParameter.KEY_DATE_TIME_BACKUP_PERIOD
                        .getBytes()));
        try
        {
            snTime = mMPM.getParameterInteger(ssString);
            ltime = snTime.get();

            // Get current time
            cal = Calendar.getInstance();
            // Setting time sync execution interval
            cal.add(Calendar.HOUR, (int) ltime);
            ltime = cal.getTimeInMillis();

            intent = new Intent(mContext, TimeManagementReceiverInService.class);
            intent.putExtra("msg", "backup_time");
            pi = PendingIntent.getBroadcast(mContext, 1, intent,
                    PendingIntent.FLAG_ONE_SHOT);

            // Convert long to SafetyChannel<Long>
            ch1Val = CommonUtils.encodeCH1Value(ltime);
            ch2Val = CommonUtils.encodeCH2Value(ltime);
            scTime = new SafetyChannel<Long>(ch1Val.longValue(),
                    ch2Val.longValue());

            run_am_setAlarm(scTime, pi);
        }
        catch (DataIntegrityException e)
        {

            e.printStackTrace();
        }
        catch (NoSuchElementException e)
        {

            e.printStackTrace();
        }
        catch (DataTypeMismatchException e)
        {

            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * A function to restore meter's time from share preference
     * 
     * @param void [in]
     * 
     * @return void [out]
     */
    @Override
    public void restoreRCTime()
    {
        SafetyChannel<Long> scTime = null;
        SafetyBoolean isTimeChangeable = null;
        scTime = getMeterTimeFromNVM();
        // Check whether input parameter is null or not
        CommonUtils.objectCheck(scTime);

        isTimeChangeable = isRCTimeChangeable(scTime, SafetyBoolean.FALSE);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(isTimeChangeable);

        if (isTimeChangeable.equals(SafetyBoolean.TRUE))
        {
            try
            {
                Number ch1Val = scTime.getValueCH1();
                Number ch2Val = scTime.getValueCH2();
                // Get original value of SafetyChannel
                long ltime = CommonUtils.getOriginValue(ch1Val.longValue(),
                        ch2Val.longValue());

                setRCTime(ltime);
            }
            catch (DataIntegrityException e)
            {
                e.printStackTrace();
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Apply to the coding standard.
            }
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * A function to set RC time which including set meter time; set pump time
     * and set BGM time.
     * 
     * @param long [in] timestamp
     *        Timestamp for setting RC time
     *        Range: -2^63 to (2^63)-1
     *        Unit: long
     *        Scale: 1
     * 
     * @return void [out]
     * @throws RemoteException, DataIntegrityException
     */
    @Override
    public void setRCTime(long timestamp) throws RemoteException,
            DataIntegrityException
    {
        long ch1Val = CommonUtils.encodeCH1Value(timestamp);
        long ch2Val = CommonUtils.encodeCH2Value(timestamp);
        SafetyChannel<Long> scTimestamp = new SafetyChannel<Long>(ch1Val,
                ch2Val);
        printI("Enter setRCTime");
        changeTimeSegment(); // Save current timesegment and create a new
                             // timesegment before time setting
        printI("setMeterTime");
        setMeterTime(scTimestamp);
        try
        {
            SafetyNumber<Integer> snTimeForcedOffset = null;
            long lTimeForceOffset = 0;
            SafetyString ssString = new SafetyString(
                    ConfigParameter.KEY_FORCED_OFFSET_MICRO_PUMP_TIME_BASE,
                    CRCTool.generateCRC16(ConfigParameter.KEY_FORCED_OFFSET_MICRO_PUMP_TIME_BASE
                            .getBytes())); // SafetyString of time before meter
            try
            {
                snTimeForcedOffset = mMPM.getParameterInteger(ssString);
            }
            catch (NoSuchElementException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (DataTypeMismatchException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            // Check whether input parameter is null or not
            CommonUtils.objectCheck(snTimeForcedOffset);
            // change it to milliseconds, the orginal unit is second
            lTimeForceOffset = snTimeForcedOffset.get() * MILLISEC_IN_A_SECOND;
            // Adjust pump time to be earlier than system time (refer to CM P-094.1)
            timestamp = timestamp - lTimeForceOffset; 
            ch1Val = CommonUtils.encodeCH1Value(timestamp);
            ch2Val = CommonUtils.encodeCH2Value(timestamp);
            scTimestamp = new SafetyChannel<Long>(ch1Val,
                    ch2Val);
            printI("setPumpTime");
            setPumpTime(scTimestamp);
            printI("resetBGMTime");
            resetBGMTime();
        }
        catch (DataIntegrityException e)
        {
            e.printStackTrace();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard.
        }
        printI("Exit setRCTime");
    }

    /**
     * A function to get RC time which including sync RC and bgm time.
     * 
     * @param None
     * 
     * @return long [out] RC's timestamp
     *         Range: -2^63 to (2^63)-1
     *         Unit: long
     *         Scale: 1
     */
    @Override
    public long getRCTime()
    {
        SafetyChannel<Long> scTime = getMeterTime();
        Number ch1Val = scTime.getValueCH1();
        Number ch2Val = scTime.getValueCH2();
        // Get original value of SafetyChannel
        long ltime = CommonUtils.getOriginValue(ch1Val.longValue(),
                ch2Val.longValue());
        return ltime;
    }

    /**
     * A function to sync meter time and BGM time
     * 
     * @param void [in]
     * 
     * @return void [out]
     * @throws RemoteException
     */
    @Override
    public void syncBGMTime() throws RemoteException
    {
        Intent intent = null;
        @SuppressWarnings("unused")
        PendingIntent pi = null;
        SafetyChannel<Long> scBGMTime = null;
        SafetyChannel<Long> scTime = null;
        SafetyNumber<Integer> snTime = null;
        long lMeterTime = 0;
        long lBGMTime = 0;
        long lTimeDiff = 0;
        long ltime = 0;
        Number ch1Val = null;
        Number ch2Val = null;
        SafetyString ssString = null;
        Calendar cal = null;

        try
        {
            // ssString = SafetyString of time behind meter
            ssString = new SafetyString(
                    ConfigParameter.KEY_SYNC_INFO_TIME_DIFFERENCE_RC_VS_MP,
                    CRCTool.generateCRC16(ConfigParameter.KEY_SYNC_INFO_TIME_DIFFERENCE_RC_VS_MP
                            .getBytes()));
            // Get the time difference criterion of BGM
            snTime = mMPM.getParameterInteger(ssString);
            //Get current BGM time
            scBGMTime = getBGMTime();
            if (null != scBGMTime)
            {
                ltime = snTime.get() * SECONDS_IN_A_MINUTE * MILLISEC_IN_A_SECOND;
                lMeterTime = System.currentTimeMillis();
                // Get original value of SafetyChannel
                lBGMTime = CommonUtils.getOriginValue(scBGMTime.getValueCH1(),
                        scBGMTime.getValueCH2());
                lTimeDiff = Math.abs(lMeterTime - lBGMTime);
                if (lTimeDiff <= ltime)
                {
                    resetBGMTime(); // reset BGM time to be as same as meter's
                                    // time
                }
                else
                {
                    // Issue EMWR error
                    NotifyMessage errMsg = new NotifyMessage(EMWRList.EMW48001);
                    NotifyProxy.showEMWR(mContext, errMsg);
                    errMsg.setCenterButtonClickListener(new EMWRButtonCallback()
                    {
                        /**
                         * Override function onClick of EMWRButtonCallback
                         * 
                         * @param void [in]
                         * 
                         * @return void [out]
                         * @throws RemoteException
                         */
                        @Override
                        public void onClick()
                        {
                            Log.e("Center Button Click",
                                    "Click center button!!!");
                            if (null != mDateTimeSettingCallback)
                            {
                                try
                                {
                                    long time = System.currentTimeMillis();
                                    time = mDateTimeSettingCallback
                                            .dateTimeSettingCallback(time);
                                    setRCTime(time);
                                }
                                catch (RemoteException e)
                                {

                                    e.printStackTrace();
                                }
                                finally
                                {
                                    // Apply to the coding standard
                                }
                            }
                            else
                            {
                                // Apply to the coding standard
                            }
                        }
                    });
                }

                // Get the BGM time sync interval (hours)
                ssString = new SafetyString(
                        ConfigParameter.KEY_METER_INTERNAL_TIME_SYNC_INTERVAL,
                        CRCTool.generateCRC16(ConfigParameter.KEY_METER_INTERNAL_TIME_SYNC_INTERVAL
                                .getBytes()));
                snTime = mMPM.getParameterInteger(ssString);
                ltime = snTime.get() * MINUTES_IN_AN_HOUR; // Convert hours to
                                                           // minutes

                // Calculate alarm time
                cal = Calendar.getInstance();
                cal.add(Calendar.MINUTE, (int) ltime);
                ltime = cal.getTimeInMillis();

                // Setting one shot pending intent
                intent = new Intent(mContext,
                        TimeManagementReceiverInService.class);
                intent.putExtra("msg", "sync_time");
                pi = PendingIntent.getBroadcast(mContext, 1, intent,
                        PendingIntent.FLAG_ONE_SHOT);
                // Convert long to SafetyChannel<Long>
                ch1Val = CommonUtils.encodeCH1Value(ltime);
                ch2Val = CommonUtils.encodeCH2Value(ltime);
                scTime = new SafetyChannel<Long>(ch1Val.longValue(),
                        ch2Val.longValue());
                // Setting one shot alarm
                run_am_setAlarm(scTime, pi);
            }
            else
            {
                // BGM is not ready
            }
        }
        catch (RemoteException e)
        {

            e.printStackTrace();
        }
        catch (DataIntegrityException e)
        {

            e.printStackTrace();
        }
        catch (NoSuchElementException e)
        {

            e.printStackTrace();
        }
        catch (DataTypeMismatchException e)
        {

            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * A function to set pump time
     * 
     * @param timestamp[in] pump time stamp
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scaling: 1
     * 
     * @return void [out]
     */
    public void setPumpTime(SafetyChannel<Long> timestamp)
    {
        // Get original value of SafetyChannel
        Number ch1Val = timestamp.getValueCH1();
        Number ch2Val = timestamp.getValueCH2();
        long ltime = CommonUtils.getOriginValue(ch1Val.longValue(),
                ch2Val.longValue());
        // A parameters container
        SafetyByteArray data = null;
        Calendar calendar = null;
        ByteBuffer byteBuffer = ByteBuffer.allocate(4);
        byte[] bytes = new byte[4];
        // A command byte array for set pump time
        byte[] sendcommand = {
                (ControlPointConstant.OpCode.SOLOM_SET_DATE_AND_TIME & 0xFF),
                ((ControlPointConstant.OpCode.SOLOM_SET_DATE_AND_TIME & 0xFF00) >> 8),
                0, 0, 0, 0, 0, 0, 0, 0, 0 };
        SafetyBoolean isTimeChangeable = isRCTimeChangeable(timestamp, SafetyBoolean.TRUE); 
        if (isTimeChangeable.equals(SafetyBoolean.TRUE))
        {
            // Enable the parameter object
            mSetPumpTimeParameter.setIsEnable(SafetyBoolean.TRUE);
            // Set the calendar time to meter time
            calendar = Calendar.getInstance();
            calendar.setTimeInMillis(ltime);
            // Set the ByteBuffer order to little endian
            byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
            // Clear the ByteBuffer before put integer in it
            byteBuffer.clear();
            // Get year in integer and convert it to 2-byte data
            bytes = byteBuffer.putInt(calendar.get(Calendar.YEAR)).array();
            sendcommand[2] = bytes[0];
            sendcommand[3] = bytes[1];

            // Get month in integer and convert it to 1-byte data
            byteBuffer.clear();
            bytes = byteBuffer.putInt(calendar.get(Calendar.MONTH) + 1).array();
            sendcommand[4] = bytes[0];

            // Get day in integer and convert it to 1-byte data
            byteBuffer.clear();
            bytes = byteBuffer.putInt(calendar.get(Calendar.DAY_OF_MONTH))
                    .array();
            sendcommand[5] = bytes[0];

            // Get hour in integer and convert it to 1-byte data
            byteBuffer.clear();
            bytes = byteBuffer.putInt(calendar.get(Calendar.HOUR)).array();
            sendcommand[6] = bytes[0];

            // Get minute in integer and convert it to 1-byte data
            byteBuffer.clear();
            bytes = byteBuffer.putInt(calendar.get(Calendar.MINUTE)).array();
            sendcommand[7] = bytes[0];

            // Get second in integer and convert it to 1-byte data
            byteBuffer.clear();
            bytes = byteBuffer.putInt(calendar.get(Calendar.SECOND)).array();
            sendcommand[8] = bytes[0];

            // Get timezone in integer and convert it to 1-byte data
            sendcommand[9] = (byte)0x80;    //default timezone index
            // Get daylight saving in integer and convert it to 1-byte data
            sendcommand[10] = (byte)0xff;   //default dst index
            // Translate sendcommand byte array to SafetyByteArray object
            data = new SafetyByteArray(sendcommand,
                    CRCTool.generateCRC16(sendcommand));
            // Set the SafetyByteArray object into parameter object
            mSetPumpTimeParameter.setData(data);
            // controller.setTimeStamp(parameter, callback);
            mSetPumpTimeExecutor = Executors.newFixedThreadPool(1);
            mSetPumpTimeExecutor.execute(mSetPumpTimeRunnable);
        }
        else
        {
            //Cannot setPumpTime
            // Apply to the coding standard
        }
    }

    /**
     * A function to verify meter time and pump time
     * 
     * @param byte[] [in] pumptime: input pump time
     *        Range: valid byte[]
     *        Unit: byte[]
     *        Scaling: 1
     * 
     * @return byte[] [out] adjusted pump time
     *         Range: valid byte[]
     *         Unit: byte[]
     *         Scaling: 1
     */
    @SuppressLint({ "SimpleDateFormat", "DefaultLocale" })
    @Override
    public byte[] verifyPumpTime(byte[] pumpdata)
    {
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(
                "MM-dd-yyyy HH:mm:ss");
        String datetime = null;
        int year = 0;
        int month = 0;
        int day = 0;
        int hour = 0;
        int minute = 0;
        int second = 0;
        int tzIndex = 0;
        int dstIndex = 0;
        long lPumpTime = 0;
        long lSystemTime = System.currentTimeMillis();
        SafetyNumber<Integer> snTimeSyncInfoDiff = null;
        long lTimeSyncInfoDiff = 0;
        SafetyNumber<Integer> snTimeForcedOffset = null;
        long lTimeForceOffset = 0;
        SafetyNumber<Integer> snTimeDiff = null;
        long lTimeDiff = 0;
        SafetyNumber<Integer> snTimeTolerance = null;
        long lTimeTolerance = 0;
        Date date = null;
        Calendar calendar = null;
        SafetyString ssString = null;
        Calendar cal = Calendar.getInstance();
        byte[] bytes = new byte[4];
        ByteBuffer byteBuffer = ByteBuffer.allocate(4);
        byte[] retdata = new byte[9];

        // Set the 2 digit year base is 2000 year
        cal.clear();
        cal.set(Calendar.YEAR, 2000);
        simpleDateFormat.set2DigitYearStart(cal.getTime());
        // Convert byte(s) to int
        // Excahnge pump data byte order, because ByteBuffer default order is
        // BIG ENDIAN

        // Convert 2-byte data to year in integer
        bytes[0] = pumpdata[0]; // Exchange byte 0 and byte 1, because
                                // ByteBuffer
                                // default order is big endian
        bytes[1] = pumpdata[1];
        bytes[2] = 0;
        bytes[3] = 0;
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        year = byteBuffer.getInt();

        // Convert 1-byte data to month in integer
        bytes[0] = pumpdata[2];
        bytes[1] = 0;
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        month = byteBuffer.getInt();
        if (0 == month)
        {
            month = 1;
        }

        // Convert 1-byte data to day in integer
        bytes[0] = pumpdata[3];
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        day = byteBuffer.getInt();

        // Convert 1-byte data to hour in integer
        bytes[0] = pumpdata[4];
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        hour = byteBuffer.getInt();

        // Convert 1-byte data to minute in integer
        bytes[0] = pumpdata[5];
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        minute = byteBuffer.getInt();

        // Convert 1-byte data to second in integer
        bytes[0] = pumpdata[6];
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        second = byteBuffer.getInt();

        // Get timezone index
        bytes[0] = pumpdata[7];
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        tzIndex = byteBuffer.getInt();

        // Get daylight saving time index
        bytes[0] = pumpdata[8];
        byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        dstIndex = byteBuffer.getInt();

        // Convert ints to datetime format string
        datetime = String.format("%2d-%2d-%4d %2d:%2d:%2d", month, day, year,
                hour, minute, second);
        // Convert format string to DATE object
        try
        {
            date = simpleDateFormat.parse(datetime);
        }
        catch (ParseException e1)
        {

            e1.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
        // Convert DATE object to long
        lPumpTime = date.getTime();

        // Get the time difference criteria
        try
        {
            ssString = new SafetyString(
                    ConfigParameter.KEY_SYNC_INFO_TIME_DIFFERENCE_RC_VS_MP,
                    CRCTool.generateCRC16(ConfigParameter.KEY_SYNC_INFO_TIME_DIFFERENCE_RC_VS_MP
                            .getBytes())); // SafetyString of time before meter
            snTimeSyncInfoDiff = mMPM.getParameterInteger(ssString);
            // Check whether input parameter is null or not
            CommonUtils.objectCheck(snTimeSyncInfoDiff);

            ssString = new SafetyString(
                    ConfigParameter.KEY_FORCED_OFFSET_MICRO_PUMP_TIME_BASE,
                    CRCTool.generateCRC16(ConfigParameter.KEY_FORCED_OFFSET_MICRO_PUMP_TIME_BASE
                            .getBytes())); // SafetyString of time before meter
            snTimeForcedOffset = mMPM.getParameterInteger(ssString);
            // Check whether input parameter is null or not
            CommonUtils.objectCheck(snTimeForcedOffset);

            ssString = new SafetyString(
                    ConfigParameter.KEY_METER_PUMP_TIME_DIFFERENCE,
                    CRCTool.generateCRC16(ConfigParameter.KEY_METER_PUMP_TIME_DIFFERENCE
                            .getBytes())); // SafetyString of time before meter
            snTimeDiff = mMPM.getParameterInteger(ssString);
            // Check whether input parameter is null or not
            CommonUtils.objectCheck(snTimeDiff);

            ssString = new SafetyString(
                    ConfigParameter.KEY_METER_PUMP_TIME_DIFFERENCE_TOLERANCE,
                    CRCTool.generateCRC16(ConfigParameter.KEY_METER_PUMP_TIME_DIFFERENCE_TOLERANCE
                            .getBytes())); // SafetyString of time before meter
            snTimeTolerance = mMPM.getParameterInteger(ssString);
            // Check whether input parameter is null or not
            CommonUtils.objectCheck(snTimeTolerance);
        }
        catch (DataIntegrityException e)
        {

            e.printStackTrace();
        }
        catch (NoSuchElementException e)
        {

            e.printStackTrace();
        }
        catch (DataTypeMismatchException e)
        {

            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
        // change it to milliseconds, the orginal unit is minute
        lTimeSyncInfoDiff = snTimeSyncInfoDiff.get() * SECONDS_IN_A_MINUTE
                * MILLISEC_IN_A_SECOND;
        // change it to milliseconds, the orginal unit is second
        lTimeForceOffset = snTimeForcedOffset.get() * MILLISEC_IN_A_SECOND;
        // change it to milliseconds, the orginal unit is second
        lTimeDiff = snTimeDiff.get() * MILLISEC_IN_A_SECOND;
        // The original unit is second, so check pump time, is it out of
        // criteria? (refer to CM P-094.2)
        lTimeTolerance = snTimeTolerance.get() * MILLISEC_IN_A_SECOND;
        if ((lPumpTime > lSystemTime + lTimeSyncInfoDiff)
                || (lPumpTime < lSystemTime - lTimeSyncInfoDiff))
        {
// Mark temporary.....            
//            NotifyMessage errMsg = new NotifyMessage(EMWRList.EMW48002);
//            NotifyProxy.showEMWR(mContext, errMsg);
        }
        else
        {
            // Apply to the coding standard
        }
        // Check pump time, is it out of criteria? (refer to CM P-066.3,
        // P-066.4)
        if ((lPumpTime > lSystemTime - lTimeDiff + lTimeTolerance)
                || (lPumpTime < lSystemTime - lTimeDiff - lTimeTolerance))

        {
            // Adjust pump time to be earlier than system time (refer to CM P-094.1)
            lPumpTime = lSystemTime - lTimeForceOffset;
        }
        else
        {
            // Apply to the coding standard
        }
        // Convert new pump time to byte-data
        calendar = Calendar.getInstance();
        calendar.setTimeInMillis(lPumpTime);
        // Convert year to 2-byte data
        byteBuffer.clear();
        bytes = byteBuffer.putInt(calendar.get(Calendar.YEAR)).array();
        retdata[0] = bytes[0];
        retdata[1] = bytes[1];

        // Convert month to 1-byte data
        byteBuffer.clear();
        bytes = byteBuffer.putInt(calendar.get(Calendar.MONTH) + 1).array();
        retdata[2] = bytes[0];

        // Convert day to 1-byte data
        byteBuffer.clear();
        bytes = byteBuffer.putInt(calendar.get(Calendar.DAY_OF_MONTH)).array();
        retdata[3] = bytes[0];

        // Convert hour to 1-byte data
        byteBuffer.clear();
        bytes = byteBuffer.putInt(calendar.get(Calendar.HOUR)).array();
        retdata[4] = bytes[0];

        // Convert minute to 1-byte data
        byteBuffer.clear();
        bytes = byteBuffer.putInt(calendar.get(Calendar.MINUTE)).array();
        retdata[5] = bytes[0];

        // Convert second to 1-byte data
        byteBuffer.clear();
        bytes = byteBuffer.putInt(calendar.get(Calendar.SECOND)).array();
        retdata[6] = bytes[0];

        // Convert timezone to 1-byte data
        retdata[7] = (byte) tzIndex;

        // Convert daylight saving to 1-byte data
        retdata[8] = (byte) dstIndex;

        return retdata;
    }

    /**
     * A function to verify the difference between meter time and BGM time, this
     * function is customized for POST to check BGM time status at power up
     * stage.
     * 
     * @param
     * 
     * @return boolean [out] true: verify time ok, false: verify time fail
     *         Range: true, false
     *         Unit: boolean
     *         Scaling: 1
     */
    @Override
    public boolean verifyBGMTime()
    {
        SafetyChannel<Long> scBGMTime = null;
        SafetyNumber<Integer> snTime = null;
        long lMeterTime = 0;
        long lBGMTime = 0;
        long lTimeDiff = 0;
        long ltime = 0;
        SafetyString ssString = null;
        boolean bRet = false;

        try
        {
            // Get the SafetyString of sync info time difference between RC and
            // MP
            ssString = new SafetyString(
                    ConfigParameter.KEY_SYNC_INFO_TIME_DIFFERENCE_RC_VS_MP,
                    CRCTool.generateCRC16(ConfigParameter.KEY_SYNC_INFO_TIME_DIFFERENCE_RC_VS_MP
                            .getBytes()));
            snTime = mMPM.getParameterInteger(ssString);
            // Get BGM time
            scBGMTime = getBGMTime();
            if (null != scBGMTime)
            {
                // The original unit is minute
                ltime = snTime.get() * SECONDS_IN_A_MINUTE * MILLISEC_IN_A_SECOND; 
                // Get meter time
                lMeterTime = System.currentTimeMillis();
                // Get original value of SafetyChannel
                lBGMTime = CommonUtils.getOriginValue(scBGMTime.getValueCH1(),
                        scBGMTime.getValueCH2());
                lTimeDiff = Math.abs(lMeterTime - lBGMTime);
                if (lTimeDiff <= ltime)
                {
                    bRet = true;
                }
                else
                {
                    bRet = false;
                }
            }
            else
            {
                // BGM is not ready
                bRet = false;
            }
        }
        catch (RemoteException e)
        {

            e.printStackTrace();
        }
        catch (DataIntegrityException e)
        {

            e.printStackTrace();
        }
        catch (NoSuchElementException e)
        {

            e.printStackTrace();
        }
        catch (DataTypeMismatchException e)
        {

            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
        return bRet;
    }

    /**
     * A function to register datetime setting callback function
     * 
     * @param IDateTimeSettingCallback [in] cb
     *            Range: valid IDateTimeSettingCallback
     *            Unit: IDateTimeSettingCallback
     *            Scaling: 1
     * 
     * @return void [out]
     */
    public void registerTimeSettingCallback(IDateTimeSettingCallback cb)
    {
        // Save callback interface
        mDateTimeSettingCallback = cb;
    }

    /**
     * A function to unregister datetime setting callback function
     * 
     * @param IDateTimeSettingCallback [in] cb
     *            Range: valid IDateTimeSettingCallback
     *            Unit: IDateTimeSettingCallback
     *            Scaling: 1
     * 
     * @return void [out]
     */
    public void unregisterTimeSettingCallback(IDateTimeSettingCallback cb)
    {
        // Reset callback interface to null
        if(mDateTimeSettingCallback == cb)
        {
            mDateTimeSettingCallback = null;
        }
    }

    // VVVVVVVVVVVVVVVVVVVV Design for test VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
    /**
     * calling this function to get BGM
     * control service object.
     * 
     * @param Context [in] context
     *            Activity context
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling 1
     * 
     * @return IBGMControl [out] return IBGMControl service object
     */
    protected IBGMControl run_CustJavaFrameworkManager_getBGMControlService(
            Context context)
    {
        // The context null check has been done outside
        return CustJavaFrameworkManager.getBGMControlService(context);
    }

    /**
     * calling this function to read BGM
     * timestamp
     * 
     * @param IBGMControl [in] bgmCtrl
     *            Range: valid IBGMControl object
     *            Unit: IBGMControl
     *            Scaling 1
     * 
     * @return SafetyChannel<Long> [out] safety timestamp value,
     *         = null means BGM is not ready
     *         Range: valid SafetyChannel<Long> object
     *         Unit: SafetyChannel<Long>
     *         Scaling: 1
     */
    @SuppressLint("SimpleDateFormat")
    protected SafetyChannel<Long> run_bgmCtrl_readBGMTimeStamp(
            IBGMControl bgmCtrl) throws RemoteException
    {
        mBgmCtrl = bgmCtrl;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(mBgmCtrl);

        // Schedule a 3 seconds timeout timer for mSyncBgmReadReady
        mBgmReadScheduler = Executors.newScheduledThreadPool(1);
        mBgmReadScheduler.schedule(new Runnable()
        {
            public void run()
            {
                //Bgm read timeout
                mSCTime = null;
                mSyncBgmReadReady.open(); // Open the block of ConditionVariable
            }
        }, 3, TimeUnit.SECONDS);

        mSyncBgmReadReady = new ConditionVariable();
        mBgmReadExecutor = Executors.newFixedThreadPool(1);
        
        mBgmReadExecutor.execute(mBgmReadRunnable);

        mSyncBgmReadReady.block(); // Block and wait for open
        mSyncBgmReadReady.close(); // Close the ContionVariable
        mBgmReadScheduler.shutdown(); // Shutdown timeout scheduler
        mBgmReadExecutor.shutdown(); // Shutdown runnable executor

        return mSCTime;
    }

    /**
     * calling this function to write BGM
     * timestamp
     * 
     * @param IBGMControl [in] bgmCtrl
     *            Range: valid IBGMControl object
     *            Unit: IBGMControl
     *            Scaling 1
     * 
     * @return void [out]
     */
    protected void run_bgmCtrl_writeTimeStampToBgm(IBGMControl bgmCtrl)
            throws RemoteException
    {
        mSetBGMTime = SafetyBoolean.FALSE;
        mSetBGMDate = SafetyBoolean.FALSE;
        mBgmCtrl = bgmCtrl;
        
        mBgmWriteExecutor = Executors.newFixedThreadPool(1);
        mBgmWriteScheduler = Executors.newScheduledThreadPool(1);
        
        mBgmWriteExecutor.execute(new Runnable()
        {
            public void run()
            {
                // The bgmCtrl null check has been done outside
                try
                {
                    // Schedule a 3 seconds timeout timer for mBgmWriteExecutor
                    mBgmWriteScheduler.schedule(new Runnable()
                    {
                        public void run()
                        {
                            //Bgm write timeout
                            mBgmWriteExecutor.shutdown();
                            mBgmWriteScheduler.shutdown(); // Shutdown timeout
                                                           // scheduler
                        }
                    }, 3, TimeUnit.SECONDS);
                    // Call set BGM datetime
                    mBgmCtrl.getMeInformation(IBgmConstant.SETBGMDATETIME,
                            new IMeInformationListener.Stub()
                            {

                                /**
                                 * Override onCodeKeyInformation function of
                                 * IMeInformationListener
                                 * 
                                 * @param SafetyByteArray [in] number
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @param SafetyByteArray [in] date
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @param SafetyByteArray [in] status
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @param SafetyByteArray [in] field
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onCodeKeyInformation(
                                        SafetyByteArray number,
                                        SafetyByteArray date,
                                        SafetyByteArray status,
                                        SafetyByteArray field)
                                        throws RemoteException
                                {
                                    // Apply to the coding standard

                                }

                                /**
                                 * Override onStripCounter function of
                                 * IMeInformationListener
                                 * 
                                 * @param SafetyByteArray [in] counter
                                 *            Range: valid SafetyByteArray
                                 *            object
                                 *            Unit: SafetyByteArray
                                 *            Scaling: 1
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onStripCounter(
                                        SafetyByteArray counter)
                                        throws RemoteException
                                {
                                    // Apply to the coding standard

                                }

                                /**
                                 * Override onSuccess function of
                                 * IMeInformationListener
                                 * 
                                 * @param SafetyString [in] key of the success
                                 *            command
                                 *            Range: valid SafetyString object
                                 *            Unit: SafetyString
                                 *            Scaling: 1
                                 * 
                                 *            SafetyString [in] string parameter
                                 *            Range: valid SafetyString object
                                 *            Unit: SafetyString
                                 *            Scaling: 1
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onSuccess(SafetyString key,
                                        SafetyString s) throws RemoteException
                                {
                                    if (key.getString()
                                            .equals("SetTimeCommand"))
                                    {
                                        // Set BGM time has done
                                        mSetBGMTime = SafetyBoolean.TRUE;
                                    }
                                    else if (key.getString().equals(
                                            "SetDateCommand"))
                                    {
                                        // Set BGM date has done
                                        mSetBGMDate = SafetyBoolean.TRUE;
                                    }
                                    if (mSetBGMTime.equals(SafetyBoolean.TRUE)
                                            && mSetBGMDate
                                                    .equals(SafetyBoolean.TRUE))
                                    {
                                        // Shutdown runnable executor
                                        mBgmWriteExecutor.shutdown();
                                        // Shutdown timeout scheduler
                                        mBgmWriteScheduler.shutdown();
                                    }
                                    else
                                    {
                                        // Apply to the coding standard
                                    }
                                }

                                /**
                                 * Override onError function of
                                 * IMeInformationListener
                                 * 
                                 * @throws RemoteException
                                 */
                                @Override
                                public void onError() throws RemoteException
                                {
                                    // Shutdown runnable executor
                                    mBgmWriteExecutor.shutdown();
                                    // Shutdown timeout scheduler
                                    mBgmWriteScheduler.shutdown();
                                }

                            });
                }
                catch (RemoteException e1)
                {

                    e1.printStackTrace();
                }
                finally
                {
                    // Apply to the coding standard
                }
            }
        });
    }

    /**
     * calling this function execute
     * database operation
     * 
     * @param DataBaseTimeSegment_Model [in] db
     *            Range: valid DataBaseTimeSegment_Model object
     *            Unit: DataBaseTimeSegment_Model
     *            Scaling 1
     * 
     * @param int [in] op
     * 
     * @param SafetyChannel<Integer> [in] recid
     *            Range: valid SafetyChannel<Integer> object
     *            Unit: SafetyChannel<Integer>
     *            Scaling 1
     * 
     * @param SafetyChannel<Long> [in] startTime
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scaling 1
     * 
     * @param SafetyChannel<Long> [in] endTime
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scaling 1
     * 
     * @return Object [out] Only for return query object
     */
    protected Object run_DataBaseOp(DataBaseTimeSegment_Model db, int op,
            SafetyChannel<Integer> recid, SafetyChannel<Long> startTime,
            SafetyChannel<Long> endTime)
    {
        Object ret = null;
        // db was new outside, it doesn't need to do null check, id, startTime
        // and endTime have been done null check outside.

        switch (op)
        {
        case INSERT_DATA :
            // Insert TimeSegment to database
            ret = (Object)db.insertData(mContext, recid, startTime, endTime);
            
            break;
        case QUERY_ID_DATA :
            // Query TimeSegment from database by id
            ret = (Object) db.queryDataByRecID(mContext, recid);
            break;
        case QUERY_TIMEPERIOD_DATA :
            // Query TimeSegments from database by time period
            ret = (Object) db.queryDataByTimePeriod(mContext, startTime,
                    endTime);
            break;
        case UPDATE_DATA :
            // Update TimeSegment to database
            db.updateData(mContext, recid, startTime, endTime);
            break;
        case DELETE_DATA :
            // Delete TimeSegment in database
            db.deleteData(mContext, recid);
            break;
        default :
            break;
        }
        return ret;
    }

    /**
     * calling this function to set a long
     * value to share preference with a key
     * 
     * @param Context [in] context
     *            Activity context
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling 1
     * 
     * @param String [in] key
     *            Share preference key
     *            Range: valid String object
     *            Unit: String
     *            Scaling 1
     * 
     * @param SafetyChannel<Long> [in] scVal
     *            Safety long value
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scaling 1
     * 
     * @return void [out]
     */
    protected void run_NugenGeneralModel_setLong(Context context, String key,
            SafetyChannel<Long> scVal)
    {
        Number ch1Val = 0;
        Number ch2Val = 0;
        SafetyNumber<Long> snVal = null;

        // context, key and scVal null check have been done outside
        // Translate SafetyChannel<Long> to SafetyNumber<Long>
        ch1Val = scVal.getValueCH1();
        ch2Val = scVal.getValueCH2();
        snVal = new SafetyNumber<Long>(ch1Val.longValue(), -ch1Val.longValue());
        NugenGeneralModel.setLong(context, key, snVal);
        snVal = new SafetyNumber<Long>(ch2Val.longValue(), -ch2Val.longValue());
        NugenGeneralModel.setLong(context, key + KEY2_POSTFIX, snVal);
    }

    /**
     * calling this function to get a safety long value from share preference
     * with a key
     * 
     * @param Context [in] context
     *            Activity context
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling 1
     * 
     * @param String [in] key
     *            Share preference key
     *            Range: valid String object
     *            Unit: String
     *            Scaling 1
     * 
     * @return SafetyChannel<Long> [out] return safety long
     *         Range: valid SafetyChannel<Long> object
     *         Unit: SafetyChannel<Long>
     *         Scaling: 1
     */
    protected SafetyChannel<Long> run_NugenGeneralModel_getLong(
            Context context, String key)
    {
        Number ch1Val = 0;
        Number ch2Val = 0;
        SafetyNumber<Long> snVal = null;
        SafetyChannel<Long> scVal = null;
        // context, key null check have been done outside
        snVal = NugenGeneralModel.getLong(context, key);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(snVal);
        // Translate SafetyNumber<Long> to SafetyChannel<Long>
        ch1Val = CommonUtils.encodeCH1Value(snVal.get());
        ch2Val = CommonUtils.encodeCH2Value(snVal.get());
        scVal = new SafetyChannel<Long>(ch1Val.longValue(), ch2Val.longValue());
        return scVal;
    }

    /**
     * calling this function to call AlarmManager.setTime function
     * for changing system time.
     * 
     * @param SafetyChannel<Long> [in] timestamp
     *            Safety timestamp value
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scaling 1
     * 
     * @return void [out]
     */
    protected void run_am_setTime(SafetyChannel<Long> timestamp)
    {
        AlarmManager am = null;
        Number ch1Val = 0;
        Number ch2Val = 0;
        long ltime = 0;
        Date date = null;
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

        // timestamp null check has been done outside
        am = (AlarmManager) mContext.getSystemService(Context.ALARM_SERVICE);
        ch1Val = timestamp.getValueCH1();
        ch2Val = timestamp.getValueCH2();
        // Get original value of SafetyChannel
        ltime = CommonUtils.getOriginValue(ch1Val.longValue(),
                ch2Val.longValue());
        printI("run_am_setTime");
        date = new Date(ltime);
        printI(sdf.format(date));
        // Set system time with AlarmManager
        am.setTime(ltime);
    }

    /**
     * calling this function to call AlarmManager.set function for setting
     * an interval time of alarm.
     * 
     * @param SafetyChannel<Long> [in] timestamp
     *            Safety timestamp value
     *            Range: valid SafetyChannel<Long> object
     *            Unit: SafetyChannel<Long>
     *            Scaling 1
     * 
     * @return void [out]
     */
    protected void run_am_setAlarm(SafetyChannel<Long> timestamp,
            PendingIntent pi)
    {
        AlarmManager am = null;
        Number ch1Val = 0;
        Number ch2Val = 0;
        long ltime = 0;

        // timestamp null check has been done outside
        am = (AlarmManager) mContext.getSystemService(Context.ALARM_SERVICE);
        ch1Val = timestamp.getValueCH1();
        ch2Val = timestamp.getValueCH2();
        // Get original value of SafetyChannel
        ltime = CommonUtils.getOriginValue(ch1Val.longValue(),
                ch2Val.longValue());
        am.set(AlarmManager.RTC_WAKEUP, ltime, pi);
    }

    // ^^^^^^^^^^^^^^^^^^^^ Design for test ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    /**
     * A function design for debug, print out message to Logcat or log file
     * 
     * @param str [in] message string
     *            Range: valid String object
     *            Unit: String
     *            Scaling: 1
     * 
     * @return void [out]
     */
    private static void printI(String str)
    {
        if (isDEBUG)
        {
            // Debug.printI(TAG, str);
            Log.i(TAG, str);
        }
        else
        {
            // Apply to the coding standard
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
