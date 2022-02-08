/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ${package_name}.${type_name}
 * Brief:
 * 1.Parse received challenge data response from Comms's
 * 2.Compare with payload's table which build in the total eight
 * array data sets to get next index. As if can't find in the table,
 * feedback to SFM's modules.
 * 3.Response the next index data sets which from respone's table define in .
 * 4.if ACK's result from received commos's is fail . Feedback to SFM's module
 * 5.Provide one API for SFM to set the status to put in
 * first column of response data.
 *
 * Create Date: ${date}
 * $$Revision: 24583 $$
 * $$Author: JamesLee $$
 * $$Id: Challenge.java 24583 2015-11-23 03:31:06Z JamesLee $$
 */
package com.accu_chek.solo_m.rcapp.application.challenge;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.os.RemoteException;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.IUICommandDispatcher;
import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.ble.RequestPack;
import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.request.BlankMessageRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.WatchDogChallengeConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.response.RunTimeTestResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.WatchDogChallengeCfmResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.WatchDogChallengeIndication;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.sfm.SafetyFlowMonitoring;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 *
 */
public class Challenge
{
    /**
     * Defines debug tag
     */
    private String TAG = "Challenge";

    /**
     * Provided POST or Run-Time to test CPU interface and
     * finish challenge cycle status.
     */
    public interface IChallenge
    {

        /**
         * Callback to self-test component to do ALU test.
         * 
         * @param add
         *            [in] : ALU parameter.
         *            Range : [0-255]
         *            Unit:SafetyChannel<Integer>
         *            Scaling:1
         * 
         * @param sub
         *            [in] : ALU parameter.
         *            Range : [0-255]
         *            Unit:SafetyChannel<Integer>
         *            Scaling:1
         * 
         * @param mul
         *            [in] : ALU parameter.
         *            Range : [0-255]
         *            Unit:SafetyChannel<Integer>
         *            Scaling:1
         * 
         * @return SafetyChannel<Integer> [out] Return calculated result of ALU
         *         Range: [0-255]
         *         Unit: SafetyChannel<Integer>
         *         Scaling:1
         */
        SafetyChannel<Integer> doALUTest(SafetyChannel<Integer> add,
                SafetyChannel<Integer> sub, SafetyChannel<Integer> mul);

        /**
         * set challenge current state
         * 
         * @param state
         *            [in] : object of State
         *            Range : valid State object.
         *            Unit: State
         *            Scaling:1
         * 
         */

        void stateChange(State state);

    }

    /**
     * Record UI challenge state
     */
    public enum State
    {
        /**
         * challenge state is idle
         */
        STATE_IDLE(HammingDistance.SAFETY_NUMBER_UINT8_01),
        /**
         * start INDICATION
         */
        STATE_INDICATION(HammingDistance.SAFETY_NUMBER_UINT8_02),
        /**
         * end Challenge cycle
         */
        STATE_END(HammingDistance.SAFETY_NUMBER_UINT8_03),
        /**
         * initial UI-Challenge Ready
         */
        STATE_INITIAL_READY(HammingDistance.SAFETY_NUMBER_UINT8_04);

        /**
         * save challenge state
         */
        private int mState;

        private State(int value)
        {
            mState = value;
        }
    }

    /**
     * Define Command type of request
     */
    public enum RequestType
    {
        /**
         * challenge state is idle
         */
        TYPE_BLANK_MSG(HammingDistance.SAFETY_NUMBER_UINT8_01),
        /**
         * start INDICATION
         */
        TYPE_CONFIRMATION(HammingDistance.SAFETY_NUMBER_UINT8_02),
        /**
         * do not thing for junit test
         */
        TYPE_DO_NOT_THING(HammingDistance.SAFETY_NUMBER_UINT8_03);

        private int mType;

        private RequestType(int value)
        {
            mType = value;
        }
    }

    /**
     * Defines challenge message table.
     */
    private static final byte[][] mChallengeTable = {

            /* Set 0 Challenge */
            { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a,
                    0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00, 0x01, 0x02, 0x03, 0x04,
                    0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
                    0x0f, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
                    0x09, 0x0a, 0x0b, 0x0c, 0x09, 0x08, 0x07 },

            /* Set 1 Challenge */
            { 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a,
                    0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x10, 0x11, 0x12, 0x13, 0x14,
                    0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e,
                    0x1f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
                    0x19, 0x1a, 0x1b, 0x1c, 0x08, 0x07, 0x06 },

            /* Set 2 Challenge */
            { 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a,
                    0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x20, 0x21, 0x22, 0x23, 0x24,
                    0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e,
                    0x2f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
                    0x29, 0x2a, 0x2b, 0x2c, 0x07, 0x06, 0x05 },

            /* Set 3 Challenge */
            { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a,
                    0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x30, 0x31, 0x32, 0x33, 0x34,
                    0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e,
                    0x3f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
                    0x39, 0x3a, 0x3b, 0x3c, 0x06, 0x05, 0x04 },

            /* Set 4 Challenge */
            { 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a,
                    0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x40, 0x41, 0x42, 0x43, 0x44,
                    0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e,
                    0x4f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
                    0x49, 0x4a, 0x4b, 0x4c, 0x05, 0x04, 0x03 },

            /* Set 5 Challenge */
            { 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
                    0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x50, 0x51, 0x52, 0x53, 0x54,
                    0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e,
                    0x5f, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
                    0x59, 0x5a, 0x5b, 0x5c, 0x04, 0x03, 0x02 },

            /* Set 6 Challenge */
            { 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a,
                    0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x60, 0x61, 0x62, 0x63, 0x64,
                    0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e,
                    0x6f, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
                    0x69, 0x6a, 0x6b, 0x6c, 0x03, 0x02, 0x01 },

            /* Set 7 Challenge */
            { 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a,
                    0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x70, 0x71, 0x72, 0x73, 0x74,
                    0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e,
                    0x7f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
                    0x79, 0x7a, 0x7b, 0x7c, 0x02, 0x01, 0x09 }

    };

    /**
     * Define response message table. In first column put SFM's status and
     * calculate the result of ALU test. Put the result in tail column.
     */
    private static final byte[][] mResponseTable = {

    /* Set 0 Response */
    { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x1b },

    /* Set 1 Response */
    { 0x00, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x77 },

    /* Set 2 Response */
    { 0x00, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x5a },

    /* Set 3 Response */
    { 0x00, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x41 },

    /* Set 4 Response */
    { 0x00, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x2c },

    /* Set 5 Response */
    { 0x00, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x1b },

    /* Set 6 Response */
    { 0x00, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x0e },

    /* Set 7 Response */
    { 0x00, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x05 }

    };

    // /**
    // * if true, allow Comms. Proc. to do run time test next.
    // * if false, Comms Proc. is not ready.
    // */
    // private static boolean isCommsInfo_Result_OK = true;

    /**
     * Define each row size of challenge message table.
     */
    private static final int CHALLENGE_TABLE_ARRAY_ROW_SIZE = 8;

    /**
     * Define each row size of response message table.
     */
    private static final int RESPONSE_TABLE_ARRAY_ROW_SIZE = 8;

    /**
     * If no response or no ACK is sent from Comms. ,define the setting of time
     * out time.
     */
    private static final long CHALLENGE_TIME_OUT_SEC = 10000;
    /**
     * Define the interval time for check during count down
     */
    private static final long CHALLENGE_TIME_OUT_INTERVERAL_SEC = 1000;

    /**
     * Define byte array data max length in challenge tableMax
     */
    private static final int CHALLENGE_DATA_LENGTH = 48;

    /**
     * Handler message for watch dog challenge indication command response
     */
    private static final int CHALLENGEMSG_INDICATION = 0;

    /**
     * Handler message for watch dog challenge request command response
     */
    private static final int CHALLENGEMSG_REQUEST = 1;

    /**
     * Handler message for watch-dog challenge confirmed command response
     */
    private static final int CHALLENGEMSG_CONFIRM = 2;

    /**
     * Define whether the wathch dog's status is ok in Comms
     */
    private static final int COMMS_WDG_STATE_OK = 5;

    /**
     * Define whether the wathch dog's is failed in Comms
     */
    private static final int COMMS_WDG_STATE_FAIL = 6;
    /**
     * Define whether confirmation result is ok and received from Comms
     */
    private static final int CONFIRM_RESULT_OK = 0;

    /**
     * Define whether confirmation result received from Comms. is failed.
     */
    private static final int CONFIRM_RESULT_FAIL = 1;

    /**
     * Point to context of application or activity in android.
     */
    private Context mContext = null;

    /**
     * Define the setting to Selftest test listener
     */
    private IChallenge mSelftestListen = null;
    /**
     * Define Count Down Timer local variable
     */
    private static CountDownTimer mCheckTimeOutTimer = null;

    /**
     * Define point to challenge message table's current index.
     */
    private static int mCurrentChallengeIndex = 0;

    /**
     * Define point to response message table's current index.
     */
    private static int mCurrentResponseIndex = 0;

    /**
     * Define an data array put in current response data buffer
     */
    private static byte[] mResponseData = new byte[RESPONSE_TABLE_ARRAY_ROW_SIZE];

    /**
     * Define the variable needed in ALU test parameter
     */
    private int mALUTest_Add = 0;

    /**
     * Define the variable needed in ALU test parameter
     */
    private int mALUTest_Sub = 0;

    /**
     * Define the variable needed in ALU test parameter
     */
    private int mALUTest_Mul = 0;

    /**
     * Define the variable in ALU test calculated result
     */
    private int mALUCalcuatedResult = 0;

    /**
     * Point to BT Control interface
     */
    private IUICommandDispatcher mUICommandDispatcher = null;

    /**
     * Define the result for notifying SFM¡¦s component when challenge-response
     * cycle is finished.Equal to SafetyBoolean.FALSE means there is no error
     * during challenge cycle.
     */
    private SafetyBoolean mIsChallengeError = SafetyBoolean.FALSE;

    /**
     * For Blank Message command requests
     */
    private BlankMessageRequest mBlankmsgRequest = null;

    /**
     * For WatchDog Challenge Confirmation command requests
     */
    private WatchDogChallengeConfirmation mConfirmRequest = null;

    /**
     * Pack request info for parcelable container
     */
    private RequestPack mRequestPack = null;

    /**
     * The handler which needed to use with self-looper
     */
    private LooperThread mChallengeLooperThread = null;

    /**
     * Handler for challenge to send message.
     */
    private WatchDogHandler hWatchDogChallenge = null;

    /**
     * BroadcastReceiver for challenge to receive messages from COMMS
     */
    private BroadcastReceiver mChallengeBroadcast = null;

    /**
     * Request interface object for getting messages request
     */
    private IRequest mIRequest = null;

    /**
     * constructor
     * 
     * @param context
     *            [in] : context of application or activity in android. Range :
     *            context is equal to valid context object Unit: context
     *            Scaling: 1
     * 
     * @see mContext [in]
     */
    public Challenge(Context context)
    {
        mContext = context;
       
    }

    /**
     * Initialize UI-Challenge that needs the objects
     * 
     * @see mContext [in]
     *      mUICommandDispatcher [in]
     *      mChallengeBroadcast [in]
     * 
     */
    public void initial()
    {

        // check input parameter context if is equal to null
        CommonUtils.objectCheck(mContext);

        mChallengeBroadcast = new ChallengeBroadcast();

        // Create self-looper thread to handler message
        ExecutorService service = Executors.newFixedThreadPool(1);
        mChallengeLooperThread = new LooperThread();
        service.execute(mChallengeLooperThread);

        // dynamic register receiver filter
        IntentFilter intentFilter = new IntentFilter();
        intentFilter
                .addAction(ResponseAction.CommandResponse.WATCHDOG_CHAL_IND);
        intentFilter
                .addAction(ResponseAction.CommandResponse.WATCHDOG_CHAL_REQ);
        intentFilter
                .addAction(ResponseAction.CommandResponse.WATCHDOG_CHAL_CFM);
        mContext.registerReceiver(mChallengeBroadcast, intentFilter);

        mUICommandDispatcher = CustJavaFrameworkManager.getUICommandDispatcher(mContext);

    }

    /**
     * There is not message queues in normal thread.
     * Need to create a Looper-loops thread to generate message queues for
     * handler messages.
     */
    public class LooperThread implements Runnable
    {

        public void run()
        {

            Looper.prepare();

            hWatchDogChallenge = new WatchDogHandler();

            // initial instance for timer out count
            mCheckTimeOutTimer = new CheckTimeOutTimer(CHALLENGE_TIME_OUT_SEC,
                    CHALLENGE_TIME_OUT_INTERVERAL_SEC);

            Debug.printI(TAG, " Init Looper Thread  Finish "
                    + hWatchDogChallenge);
            
            mSelftestListen.stateChange(State.STATE_INITIAL_READY);
            
            Looper.loop();

        }
    }

    /**
     * Set a time out timer to check if the main processor does not receive an
     * answer from Comms Proc. within a specified time
     */
    public class CheckTimeOutTimer extends CountDownTimer
    {
        /**
         * CountDownTimer class constructor
         */
        public CheckTimeOutTimer(long millisInFuture, long countDownInterval)
        {
            super(millisInFuture, countDownInterval);
        }

        /**
         * During dispatching the command time-out occurred
         *
         */
        @Override
        public void onFinish()
        {
            mIsChallengeError = SafetyBoolean.TRUE;

            // Inform the result of challenge-response scheme to Safety Flow
            // Monitor
            SafetyFlowMonitoring.getInstance().reportErrorState(mIsChallengeError);
        }

        @Override
        public void onTick(long millisUntilFinished)
        {
            // Apply to the coding standard
        }

    }

    /**
     * Use wrap command pack to submit data to Comms. Proc.
     * 
     * @param type
     *            [in] : object of RequestType
     *            Range : valid RequestType object.
     *            Unit: RequestType
     *            Scaling:1
     * @param commandId
     *            [in] : Comms command id
     *            Range : [0-255]
     *            Unit: Integer
     *            Scaling:1
     * 
     * @see mConfirmRequest [out]
     *      mBlankmsgRequest [out]
     *      mUICommandDispatcher[in]
     *      mResponseData[in]
     * 
     * 
     * @return SafetyBoolean [out] Return if the process has error occurred
     *         SafetyBoolean.TRUE -- error occurred SafetyBoolean.FALSE -- no
     *         error occurred
     * 
     *         Range: valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         RemoteException [out] :Return failed. If submitting request pack
     *         to
     *         Comms failed to notify SFM component
     * 
     */
    protected SafetyBoolean wrapCommandPack(RequestType type, int commandId)
    {

        final int TRIM_INDEX_1 = 1;
        final int TRIM_INDEX_8 = 8;
        ArrayList<byte[]> packData = new ArrayList<byte[]>();

        RequestPack requestPack = new RequestPack();
        SafetyByteArray trimData = new SafetyByteArray();
        SafetyNumber<Byte> flags = new SafetyNumber<Byte>();

        mConfirmRequest = null;
        mBlankmsgRequest = null;

        byte bSFMSatus;
        byte[] trimrespdata;
        SafetyBoolean isErrorResult = SafetyBoolean.FALSE;

        CommonUtils.objectCheck(type, commandId);

        Debug.printI(TAG, " call wrapCommandPack ");
        if (type == RequestType.TYPE_BLANK_MSG)
        {
            Debug.printI(TAG, " exec RequestType.TYPE_BLANK_MSG ");

            mBlankmsgRequest = (BlankMessageRequest) getMessageRequest(commandId);

            // Fix Klocwork null point issue
            if (mBlankmsgRequest != null)
            {
                Debug.printI(TAG, "mRequestPack.setRequest" + requestPack);

                requestPack.setRequest(mBlankmsgRequest);

                requestPack.writeToByteArrayList(packData, 0);

            }
            else
            {
                isErrorResult = SafetyBoolean.TRUE;
                // Fix Klocwork null point issue
                CommonUtils.objectCheck(mBlankmsgRequest);
            }

        }
        else if (type == RequestType.TYPE_CONFIRMATION)
        {

            mConfirmRequest = (WatchDogChallengeConfirmation) getMessageRequest(commandId);

            // Fix Klocwork null point issue
            if (mConfirmRequest != null)
            {

                bSFMSatus = mResponseData[0];

                // trim response data into two segments.
                trimrespdata = Arrays.copyOfRange(mResponseData, TRIM_INDEX_1,
                        TRIM_INDEX_8);

                trimData.set(trimrespdata, CRCTool.generateCRC16(trimrespdata));

                mConfirmRequest.setResponseData(trimData);

                flags.set(bSFMSatus, (byte) -bSFMSatus);
                mConfirmRequest.setUI_SFM_Status(flags);
                requestPack.setRequest(mConfirmRequest);

                requestPack.writeToByteArrayList(packData, 0);

            }
            else
            {
                isErrorResult = SafetyBoolean.TRUE;
                // Fix Klocwork null point issue
                CommonUtils.objectCheck(mConfirmRequest);
            }

        }
        else
        {
            isErrorResult = SafetyBoolean.TRUE;
        }

        if (isErrorResult.equals(SafetyBoolean.FALSE))
        {

            byte[] byteArr = ByteConverter.buildByteArray(packData);

            SafetyByteArray safetyBA = new SafetyByteArray(byteArr,
                    CRCTool.generateCRC16(byteArr));

            // Start check time out timer
            mCheckTimeOutTimer.start();

            try
            {
                Debug.printI(TAG, " call btControl.submitRequest -XXXXX");
                mUICommandDispatcher.submitRequest( safetyBA );
            }
            catch (RemoteException e)
            {
                isErrorResult = SafetyBoolean.TRUE;
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
        return isErrorResult;

    }

    // /**
    // * During POST of main processor need to submit get Comms Info request
    // first
    // * to get POST result of Comms Proc.
    // *
    // * @return SafetyBoolean [out] Return if the process has error occurred
    // * SafetyBoolean.TRUE -- error occurred SafetyBoolean.FALSE -- no
    // * error occurred
    // *
    // * Range: valid SafetyBoolean object
    // * Unit:
    // * SafetyBoolean
    // * Scaling: 1
    // *
    // */
    // public SafetyBoolean submitGetCommsInfo()
    // {
    // SafetyBoolean isErrorResult = SafetyBoolean.FALSE;
    //
    // isErrorResult = wrapCommandPack(RequestType.TYPE_BLANK_MSG,
    // CommsConstant.CommandCode.COMM_GET_INFO);
    //
    // return isErrorResult;
    // }

    /**
     * 
     * Start sending a request for "challenge" to Comms to enter
     * challenge-response
     * procedures.
     * 
     * @see isCommsInfo_Result_OK [in]
     * 
     * @return SafetyBoolean [out] Return if the process has error occurred
     *         SafetyBoolean.TRUE -- error occurred
     *         SafetyBoolean.FALSE -- no error occurred
     * 
     *         Range: valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     * 
     */
    public SafetyBoolean runRequestForChallenge()
    {
        SafetyBoolean isErrorResult = SafetyBoolean.FALSE;

        // if (isCommsInfo_Result_OK == false)
        // {
        // Debug.printI( TAG, "-------------isCommsInfo False");
        // isErrorResult = SafetyBoolean.TRUE;
        //
        // }
        // else
        // {
        //
        // isErrorResult = wrapCommandPack(RequestType.TYPE_BLANK_MSG,
        // CommsConstant.CommandCode.WATCHDOG_CHAL_REQ);
        //
        // }

        isErrorResult = wrapCommandPack(RequestType.TYPE_BLANK_MSG,
                CommsConstant.CommandCode.WATCHDOG_CHAL_REQ);
        // WATCHDOG_CHAL_REQ
        return isErrorResult;
    }

    /**
     * Set the listen point to IChallenge interface
     * 
     * @param iSelftest
     *            [in]: object of interface IChallenge
     *            Range :point to valid IChallenge object.
     *            Unit: IChallenge
     *            Scaling:1
     * 
     * @return SafetyBoolean [out] Return if the process has error occurred
     *         SafetyBoolean.TRUE -- error occurred
     *         SafetyBoolean.FALSE -- no error occurred
     * 
     *         Range: valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     * 
     * */
    public SafetyBoolean setChallengeListen(IChallenge iSelftest)
    {

        SafetyBoolean isErrorResult = SafetyBoolean.FALSE;

        if (iSelftest != null)
        {
            mSelftestListen = iSelftest;
            isErrorResult = SafetyBoolean.FALSE;
        }
        else
        {
            isErrorResult = SafetyBoolean.TRUE;
        }

        return isErrorResult;

    }

    /**
     * Every challenge's cycle needs to feedback the response data including
     * SFM's status of main processor in the response data to Comms The purpose
     * of this function is to send confirmation package of response to Comms.
     * 
     * 
     * @return SafetyBoolean [out] Return if the process has error occurred
     *         SafetyBoolean.TRUE -- error occurred
     *         SafetyBoolean.FALSE -- no error occurred
     * 
     *         Range: valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     * 
     */
    protected SafetyBoolean confirmResponsedResultToComms()
    {

        SafetyBoolean isErrorResult = SafetyBoolean.FALSE;

        isErrorResult = wrapCommandPack(RequestType.TYPE_CONFIRMATION,
                CommsConstant.CommandCode.WATCHDOG_CHAL_CFM);

        return isErrorResult;

    }

    /**
     * To get challenge data from Comms processor first. Then search the input
     * challenge data from Comms in Challenge table to find out the
     * index of mChallengeTable If don't find out any index in mChallengeTable
     * return CHALLENGE_TABLE_ARRAY_MAX_SIZE. if index is equal to
     * CHALLENGE_TABLE_ARRAY_MAX_SIZE means it is over the limit of table size.
     * 
     * @param a_InputData[in]
     *            : the challenge's data get from Comms.
     *            Range : The length is a constant 48 bytes
     *            Unit:byte
     *            Scaling:1
     * 
     * @see mChallengeTable [in]
     * 
     * 
     * @return Integer [out] point to index of mChallengeTable
     *         if index is CHALLENGE_TABLE_ARRAY_ROW_SIZE ,means it did not find
     *         the index.
     *         Range: 0-CHALLENGE_TABLE_ARRAY_ROW_SIZE[8]
     *         Unit: Integer
     *         Scaling: 1
     *
     */
    protected int getChallengeIndex(byte[] a_InputData)
    {
        int Index = CHALLENGE_TABLE_ARRAY_ROW_SIZE;

        Debug.printI(TAG, " a_InputData: " + a_InputData);

        if (a_InputData == null)
        {
            Index = CHALLENGE_TABLE_ARRAY_ROW_SIZE;
        }
        // It's wrong length of byte arry.
        else if (a_InputData.length != CHALLENGE_DATA_LENGTH)
        {
            Index = CHALLENGE_TABLE_ARRAY_ROW_SIZE;
        }
        else
        {

            Index = CHALLENGE_TABLE_ARRAY_ROW_SIZE;
            // To look for index in mChallengeTable.
            for (Index = 0; Index < CHALLENGE_TABLE_ARRAY_ROW_SIZE; Index++)
            {
                if (Arrays.equals(mChallengeTable[Index], a_InputData))
                {

                    break;
                }
            }
        }
        Debug.printI(TAG, " get result index: " + Index);
        return Index;
    }

    /**
     * Handle receive challenge and send response to Comms or another procedures
     * when error occurred.
     * Note:
     * The Handler is a component of android.
     * Need to override handleMessage(Message msg)
     * 
     */
    public class WatchDogHandler extends Handler
    {
        final int INDEX_0 = 0;
        final int SHIFT_8_BIT = 8;
        final int LOWBYTE = 0x000F;
        final int ADD_INDEX = 3;
        final int SUB_INDEX = 2;
        final int MUL_INDEX = 1;

        int WDG_StateCheck = 0;
        int ChallengeConfirmResult = 0;

        private SafetyByteArray getChallengeData = new SafetyByteArray();
        private ResponsePack pack = null;
        private WatchDogChallengeCfmResponse confirmresponse = null;
        private WatchDogChallengeIndication indication = null;
        private int confirmdata = 0;

        // private SafetyBoolean isErrorResult = SafetyBoolean.FALSE;

        /**
         * Receive messages to handle the indication and confirm command
         * from Comms.Proc.
         * 
         * @param msg
         *            [in] : object of Message
         *            Range : valid Message object.
         *            Unit: Message
         *            Scaling:1
         */
        @Override
        public void handleMessage(Message msg)
        {
            SafetyChannel<Integer> safetyAddValue = null;
            SafetyChannel<Integer> safetySubValue = null;
            SafetyChannel<Integer> safetyMulValue = null;
            SafetyChannel<Integer> safetyCalcResult = null;
            Number ch1CalcResult = 0;
            Number ch2CalcResult = 0;
            SafetyBoolean sfmStatus;

            Debug.printI(TAG, " ------- msg -------- " + msg.what);

            super.handleMessage(msg);

            switch (msg.what)
            {
            case CHALLENGEMSG_INDICATION :

                Debug.printI(TAG, " ResponseAction.CommandResponse : "
                        + ResponseAction.CommandResponse.WATCHDOG_CHAL_IND);

                pack = (ResponsePack) msg.obj;
                indication = (WatchDogChallengeIndication) pack.getResponse();

                getChallengeData = indication.getChallengeData();
                mSelftestListen.stateChange(State.STATE_INDICATION);

                Debug.printI(TAG,
                        " getChallengeData: " + getChallengeData.getByteArray());

                // Parse received challenge data response from Comms's
                mCurrentChallengeIndex = getChallengeIndex(getChallengeData
                        .getByteArray());

                if (mCurrentChallengeIndex >= CHALLENGE_TABLE_ARRAY_ROW_SIZE)
                {
                    mIsChallengeError = SafetyBoolean.TRUE;

                    // Inform the result of challenge-response scheme to Safety
                    // Flow Monitor
                    SafetyFlowMonitoring.getInstance().reportErrorState(
                            mIsChallengeError);
                    break;
                }
                else
                {
                    // Apply to the coding standard
                }

                Debug.printI(TAG, " mCurrentChallengeIndex : "
                        + mCurrentChallengeIndex);

                mALUTest_Add = mChallengeTable[mCurrentChallengeIndex][CHALLENGE_DATA_LENGTH
                        - ADD_INDEX];
                mALUTest_Sub = mChallengeTable[mCurrentChallengeIndex][CHALLENGE_DATA_LENGTH
                        - SUB_INDEX];
                mALUTest_Mul = mChallengeTable[mCurrentChallengeIndex][CHALLENGE_DATA_LENGTH
                        - MUL_INDEX];

                // transform input parameter into safetychannel format
                safetyAddValue = CommonUtils.getSafetyChannel(mALUTest_Add);
                safetySubValue = CommonUtils.getSafetyChannel(mALUTest_Sub);
                safetyMulValue = CommonUtils.getSafetyChannel(mALUTest_Mul);

                safetyCalcResult = mSelftestListen.doALUTest(safetyAddValue,
                        safetySubValue, safetyMulValue);

                Log.i("Challenge",
                        "Challenge - exit mCPUTestListen.doALUTest  ");

                mALUCalcuatedResult = CommonUtils.getOriginValue(
                        (Integer) safetyCalcResult.getValueCH1(),
                        (Integer) safetyCalcResult.getValueCH2());

                ch1CalcResult = safetyCalcResult.getValueCH1();
                ch2CalcResult = safetyCalcResult.getValueCH2();
                mALUCalcuatedResult = CommonUtils.getOriginValue(
                        ch1CalcResult.intValue(), ch2CalcResult.intValue());

                // Get next index
                mCurrentResponseIndex = mCurrentChallengeIndex + 1;

                if (mCurrentResponseIndex == CHALLENGE_TABLE_ARRAY_ROW_SIZE)
                {
                    mCurrentResponseIndex = 0;
                }
                else
                {
                    // Apply to the coding standard
                }

                Debug.printI(TAG, ">>>> mCurrentResponseIndex : "
                        + mCurrentResponseIndex);

                mResponseData = mResponseTable[mCurrentResponseIndex];

                // put calcuated result into index 7 of mResponseData
                // put SFM status into index 0 of mResponseData
                mResponseData[RESPONSE_TABLE_ARRAY_ROW_SIZE - 1] = (byte) mALUCalcuatedResult;
                mResponseData[INDEX_0] = getSFMStatus();

                for (int i = 0; i < mResponseData.length; i++)
                {
                    Debug.printI(TAG, "<<<<< mResponseData[" + i + "]:"
                            + mResponseData[i]);
                }

                // Confirm next index data from responded table to communication
                // processor.
                mIsChallengeError = confirmResponsedResultToComms();

                // Inform the result of challenge-response scheme to Safety Flow
                // Monitor
                SafetyFlowMonitoring.getInstance().reportErrorState(
                        mIsChallengeError);
                break;

            // case CHALLENGEMSG_REQUEST :
            //
            // pack = (ResponsePack) msg.obj;
            //
            // // Response to UI'response for challenge
            // challengeresponse = (WatchDogChallengeResponse) pack
            // .getResponse();
            // responsebytes = challengeresponse.getResponseByte();
            //
            // Debug.printI( TAG, " EXTRA_RESPONSEPACK bytes : "
            // + responsebytes);
            //
            // break;

            case CHALLENGEMSG_CONFIRM :

                pack = (ResponsePack) msg.obj;

                confirmresponse = (WatchDogChallengeCfmResponse) pack
                        .getResponse();
                confirmdata = confirmresponse.getResponseByte();
                Debug.printI(TAG, " original - confirmdata : " + confirmdata);

                WDG_StateCheck = (confirmdata >> SHIFT_8_BIT);
                ChallengeConfirmResult = (confirmdata & LOWBYTE);

                Debug.printI(TAG, " WDG_StateCheck : " + WDG_StateCheck);
                Debug.printI(TAG, " ChallengeConfirmResult :  "
                        + ChallengeConfirmResult);

                // It will send the result [0 , WDG_e_StateCheckOK] (means OK)
                // or [1, WDG_e_StateCheckFail]
                // (means response error) 2bytes in command payload for you.
                if ((ChallengeConfirmResult == CONFIRM_RESULT_FAIL))
                {
                    mIsChallengeError = SafetyBoolean.TRUE;

                }
                else if ((WDG_StateCheck == COMMS_WDG_STATE_FAIL))
                {
                    mIsChallengeError = SafetyBoolean.TRUE;
                }
                else
                {
                    mIsChallengeError = SafetyBoolean.FALSE;
                }

                // Inform the result of challenge-response scheme to Safety Flow
                // Monitor
                SafetyFlowMonitoring.getInstance().reportErrorState(
                        mIsChallengeError);
                mSelftestListen.stateChange(State.STATE_END);
                break;

            default :
                mIsChallengeError = SafetyBoolean.FALSE;
                break;

            }
        }

    };

    /**
     * To get SFM result with response to Comms Processor.
     * 
     * @return byte [out] get SFM error status
     *         Range: 0,0x0F
     *         Unit: byte
     *         Scaling: 1
     *
     */
    private byte getSFMStatus()
    {
        final byte STATUS_FAIL = 0x0F;
        final byte STATUS_OK = 0;
        byte status = 0;
        SafetyBoolean sfmStatus = SafetyBoolean.FALSE;

        SafetyFlowMonitoring sfmInstance = null;

        sfmInstance = SafetyFlowMonitoring.getInstance();

        sfmStatus = sfmInstance.getStatus();

        if (sfmStatus.equals(SafetyBoolean.TRUE))
        {
            status = STATUS_FAIL;
        }
        else
        {
            status = STATUS_OK;
        }

        return status;
    }

    /**
     * handle receive intent of android
     * Note:
     * The BroadcastReceiver is a component of android.
     * Need to override onReceive(Context context,Intent intent)
     */
    public class ChallengeBroadcast extends BroadcastReceiver
    {
        private ResponsePack pack = null;
        private Message msg = null;
        private RunTimeTestResponse respones = null;
        private int result = 0;

        @Override
        public void onReceive(Context context, Intent intent)
        {
            String action = null;
            Debug.printI(TAG, " Action[ " + intent.getAction() + "]");
            CommonUtils.objectCheck(context, intent);

            action = intent.getAction();

            if (action == null)
            {
                CommonUtils.objectCheck(action);
            }
            // else if (action
            // .equals(ResponseAction.CommandResponse.COMM_GET_INFO))
            // {
            // // this action is worked during POST
            //
            // pack = intent
            // .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
            // CommsInfoResponse respones = (CommsInfoResponse) pack
            // .getResponse();
            //
            // // to retrieve info
            // result = respones.getResult();
            // if (result == CommsConstant.Result.RESULT_OK)
            // {
            // Debug.printI( TAG,
            // "  CommsConstant.Result.RESULT_OK == true");
            // isCommsInfo_Result_OK = true;
            // // for test
            // Debug.printI( TAG,
            // "send boardcast  BOARDCAST_STRING_COMMS -2");
            // Intent intent1 = new Intent();
            // String BOARDCAST_STRING_COMMS = "COMMS_READY";
            // intent1.setAction(BOARDCAST_STRING_COMMS);
            //
            // mContext.sendBroadcast(intent1);
            // }
            // else
            // {
            // isCommsInfo_Result_OK = false;
            // }
            //
            // }
            else if (action
                    .equals(ResponseAction.CommandResponse.WATCHDOG_CHAL_REQ))
            {

                pack = intent
                        .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
                respones = (RunTimeTestResponse) pack.getResponse();

                result = respones.getResult().get();

                if (CommsConstant.Result.RESULT_OK == result)
                {
                    Debug.printI(TAG, "  wait for challenge from Comms ");
                    mIsChallengeError = SafetyBoolean.FALSE;
                }
                else
                {
                    // Apply to the coding standard
                    Debug.printI(TAG, "   SAFETY_RUNTIME_TEST RESULT_FAIL ");
                    mIsChallengeError = SafetyBoolean.TRUE;
                    // inform SFM component error occurred

                }

                // to retrieve info

            }
            else if (action
                    .equals(ResponseAction.CommandResponse.WATCHDOG_CHAL_IND))
            {
                Debug.printI(TAG, " onReceive Intent : WATCHDOG_CHAL_IND ");

                // cancel check time out timer
                mCheckTimeOutTimer.cancel();

                pack = intent
                        .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
                msg = hWatchDogChallenge.obtainMessage(CHALLENGEMSG_INDICATION,
                        pack);

                hWatchDogChallenge.sendMessage(msg);

            }
            // else if (action
            // .equals(ResponseAction.CommandResponse.WATCHDOG_CHAL_REQ))
            // {
            // Debug.printI( TAG, " onReceive Intent : WATCHDOG_CHAL_REQ ");
            //
            // // Response to UI'response for challenge
            // pack = intent
            // .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
            // msg = hWatchDogChallenge.obtainMessage(CHALLENGEMSG_REQUEST,
            // pack);
            // hWatchDogChallenge.sendMessage(msg);
            //
            // }
            else if (action
                    .equals(ResponseAction.CommandResponse.WATCHDOG_CHAL_CFM))
            {
                Debug.printI(TAG, " onReceive Intent : WATCHDOG_CHAL_CFM ");

                // cancel check time out timer
                mCheckTimeOutTimer.cancel();

                pack = intent
                        .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
                msg = hWatchDogChallenge.obtainMessage(CHALLENGEMSG_CONFIRM,
                        pack);
                hWatchDogChallenge.sendMessage(msg);

            }
            else
            {
                // Apply to the coding standard
            }

        }
    };

    /**
     * Get message request interface for JUnit test
     * 
     * @param commandId
     *            [in]: define command id
     *            Range : [0-2^31-1]
     *            Unit: Integer
     *            Scaling:1
     * 
     * @return IRequest [out] Return object of IRequest
     * 
     *         Range: valid IRequest object.
     *         Unit: IRequest
     *         Scaling: 1
     * 
     * */
    protected IRequest getMessageRequest(int commandId)
    {
        mIRequest = null;

        mIRequest = RequestPayloadFactory.getRequestPayload(commandId);

        return mIRequest;
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
