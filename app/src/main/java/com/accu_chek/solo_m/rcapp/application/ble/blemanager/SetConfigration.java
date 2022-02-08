/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetConfigration
 * Brief: This class handles SetConfigration request and its response.
 *
 * Create Date: 2015/8/12
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: SetConfigration.java 25071 2015-11-30 03:09:48Z IvanHuang $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.request.ConfigurationDataRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.ConfigurationDataResponse;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * This class handles the SetConfiguration request and its response.
 *
 */
public class SetConfigration implements IBLERequest
{
    private static final String TAG = "SetConfigration";
    
    /**
     * The instance of SetConfiguration class
     */
    private static volatile SetConfigration mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    
    /**
     * This method gets the one and only instance of the class SetConfigration.
     * 
     * 
     * @return Scan : the one and only instance of the class SetConfigration
     *         Range: A valid object of SetConfigration
     *         Unit: SetConfigration
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetConfigration
     */
    public static SetConfigration getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetConfigration(context);
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }
    
    
    /**
     * The class constructor
     * 
     * @param context: an application context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mContext            
     *            
     */
    protected SetConfigration(Context context)
    {
        mContext = context;
    }
    
    /**
     * This method is called after receiving the ConfigurationDataResponse broadcast.
     * It gets the cause of the response to check if the SetConfigration is successful or not.
     * It returns the result of response via callback function.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: a valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mCallback  
     * @see mContext           
     *            
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
        Debug.printD(TAG, "[SetConfigration]: Response enter ");
        
        ResponsePack pack = intent
                .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);

        ConfigurationDataResponse response = (ConfigurationDataResponse) pack
                .getResponse();

        int result = response.getResult().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        if (CommsConstant.Result.RESULT_OK == result)
        {
            Debug.printI(TAG, "Set config OK!");
            isResult = SafetyBoolean.TRUE;
        }
        else
        {
            Debug.printI(TAG, "Set config error!");
        }
        returnResult(isResult);
    }
        

    /**
     * This method shall check if the callback exists or not. 
     * If yes, it returns the response result via callback
     * 
     * @param isResult [in] the result of response of current request  
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1
     *                      
     * @return void [out] 
     * 
     * @see mCallback  
     */
    protected void returnResult(SafetyBoolean isResult)
    {
        if( null != mCallback )
        {
            mCallback.onRequestCompleted(isResult);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    
    /**
     * This method handles the SetConfigration request. 
     * 
     * @param parameter [in] BLERequestParameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] ResponseCallback.     
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1   
     *                      
     * @return void [out] 
     * 
     * @see mCallback 
     * @see mContext
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        /**
         * Set communication subsystem's configuration.
         */
        Debug.printD(TAG, "[SetConfigration]: Request enter ");
        
        ConfigurationDataRequest request = (ConfigurationDataRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_CONFIG_DATA);

        mCallback = callback;
        
        if (null != request)
        {
            request.setConnIntervalMin(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_CONN_INTERVAL_MIN,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_CONN_INTERVAL_MIN.getBytes()))));
            
            request.setConnIntervalMax(new SafetyNumber<Integer>(50, -50));
//            request.setConnIntervalMax(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_CONN_INTERVAL_MAX,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_CONN_INTERVAL_MAX.getBytes()))));
            
            request.setConnLatency(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_CONN_LATENCY,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_CONN_LATENCY.getBytes()))));
            
            request.setConnLatencyIntensive(new SafetyNumber<Integer>(10, -10));
//            request.setConnLatencyIntensive(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_CONN_LATENCY_INTENSIVE,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_CONN_LATENCY_INTENSIVE.getBytes()))));
            
            request.setSuperVisionTimeout(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_SUPERVISION_TIMEOUT,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_SUPERVISION_TIMEOUT.getBytes()))));

            request.setActiveScanMode(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_SCAN_TYPE,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_SCAN_TYPE.getBytes()))));

            request.setActiveFilterPolicyOne(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_INIT_FILTER_POLICY_MODE1,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_INIT_FILTER_POLICY_MODE1.getBytes()))));

            request.setActiveFilterPolicyTwo(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_INIT_FILTER_POLICY_MODE2,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_INIT_FILTER_POLICY_MODE2.getBytes()))));

            request.setActiveFilterPolicyThree(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_INIT_FILTER_POLICY_MODE3,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_INIT_FILTER_POLICY_MODE3.getBytes()))));
            
            request.setActiveScanWindow(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_INIT_WINDOWS,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_INIT_WINDOWS.getBytes()))));

            request.setActiveScanInterval(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_INIT_INTERVAL,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_INIT_INTERVAL.getBytes()))));
            
            request.setActiveScanStateDuration(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_INIT_STATE_MODE3,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_INIT_STATE_MODE3.getBytes()))));

            request.setActiveScanPauseDuration(ReadConfig.getIntegerDataByKey(new SafetyString(ConfigParameter.KEY_BLE_INIT_PAUSE_MODE3,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_INIT_PAUSE_MODE3.getBytes()))));

            request.setStandbyTimeout(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_STANDBY_TIMEOUT,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_STANDBY_TIMEOUT.getBytes()))));

            request.setProcedureTimeout(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_PROCEDURE_TIMEOUT,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_PROCEDURE_TIMEOUT.getBytes()))));
            
            request.setProcedureTimeoutKES(new SafetyNumber<Integer>(10000, -10000));
//            request.setProcedureTimeoutKES(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_PROCEDURE_TIMEOUT_KES,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_PROCEDURE_TIMEOUT_KES.getBytes()))));   
            
            request.setFastReconnDuration(new SafetyNumber<Integer>(300, -300));
//            request.setFastReconnDuration(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_FAST_RECONNECTION_DURATION,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_FAST_RECONNECTION_DURATION.getBytes()))));
            
            request.setProcedureTimeoutHistory(new SafetyNumber<Integer>(30000, -30000));
//            request.setProcedureTimeoutHistory(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_PROCEDURE_TIMEOUT_HISTORY,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_PROCEDURE_TIMEOUT_HISTORY.getBytes()))));
            
            request.setE2ERetries(new SafetyNumber<Integer>(5, -5));
//            request.setE2ERetries(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_E2E_RETRY,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_E2E_RETRY.getBytes()))));
            
            request.setProcedureRetries(new SafetyNumber<Integer>(2, -2));
//            request.setProcedureRetries(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_PROCEDURE_RETRY,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_PROCEDURE_RETRY.getBytes()))));
           
            request.setReadHistoryMaxCount(new SafetyNumber<Integer>(25, -25));
//            request.setReadHistoryMaxCount(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_READ_HISTORY_MAX_COUNT,
//                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_READ_HISTORY_MAX_COUNT.getBytes()))));
            
//            request.setChallengeResponseTime(new SafetyNumber<Integer>(1000, -1000));
            request.setChallengeResponseTime(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_CHALLENGE_RESPONSE_TIME,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_CHALLENGE_RESPONSE_TIME.getBytes()))));
            
//            request.setChallengeRepeatTime(new SafetyNumber<Integer>(3600, -3600));
            request.setChallengeRepeatTime(ReadConfig.getIntegerDataByKey(new SafetyString( ConfigParameter.KEY_BLE_CHALLENGE_REPEAT_TIME,
                    CRCTool.generateCRC16(ConfigParameter.KEY_BLE_CHALLENGE_REPEAT_TIME.getBytes()))));
            
            request.setPumpRecordNumberMin(new SafetyNumber<Integer>(0x00020000, -0x00020000));//spec min 0x00020000 
            request.setPumpRecordNumberMax(new SafetyNumber<Integer>(0x0003FFFF,-0x0003FFFF));//spec max 0x0003FFFF
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
            .registerReceiver(ResponseAction.CommandResponse.COMM_CONFIG_DATA,this);
            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            returnResult(SafetyBoolean.FALSE);
        }
        
    }

}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
