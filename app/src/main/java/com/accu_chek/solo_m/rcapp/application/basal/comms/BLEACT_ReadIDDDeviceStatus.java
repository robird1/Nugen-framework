/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BLEACT_ReadIDDDeviceStatus
 * Brief: BLE Activity: Read IDD device status
 *
 * Create Date: 11/18/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ReadIDDDeviceStatus;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeReadResponse;
import com.accu_chek.solo_m.rcapp.application.ble.state.AbstractBLEStateHandler;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLECallbackContext;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEWaitingState;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEACT_ReadIDDDeviceStatus extends AbstractBLEStateHandler
{
    private static final String TAG = "TestServiceApp";    
    
    // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();
    
    private static final byte SAFETY_FALSE = SafetyBoolean.FALSE.getByte();
    
    // Command State Constants
    private static final int CMD_NORMAL = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0125;
    
    private static final int CMD_RESET_RETRY = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0126;
    
    // Read IDD Device State Length
    private static final int READ_IDD_DEVICE_STATE_LEN = 8;    
    
    // Response of ReadIDDDeviceStatus
    private ResponseReadIDDDeviceState mResponseReadIDDDeviceState = 
                                            new ResponseReadIDDDeviceState();    
        
    // Response of ResetConnection
    private ResponseResetConnection mResponseResetConnection = 
                                           new ResponseResetConnection();    
    
    // BLECallbackContext of ACT
    private BLECallbackContext mActCbContext = null;
    
    // Limit of BLE E2E Retry Times 
    private int mE2ERetryLimit = 5;
    
    // Limit of BLE Procedure Retry Times 
    private int mProcedureRetryLimit = 2;    
    
    // E2E Retry Times
    private int mE2ERetryCount = 0;
    
    // Procedure Retry Times
    private int mProcedureRetryCount = 0;
    
    // Command
    private int mBLECommand = 0;
    
    // Context of Activity
    private Context mActContext = null;
    
    // ResponseCallback from Calling Function
    private ResponseCallbackWithData mCallback = null; 
    
    // Command State
    private int mCmdState = CMD_NORMAL;    
    
    /**
     * Class constructor
     * 
     * @param callbackContext [in] BLECallbackContext
     * 
     *          Callback context of this activity
     *          
     *          Range: Valid BLECallbackContext
     *          Unit: BLECallbackContext
     *          Scaling: 1
     *          
     * @param callback [in] ResponseCallback
     * 
     *          Callback for this activity
     *          
     *          Range: Valid ResponseCallback
     *          Unit: ResponseCallback
     *          Scaling: 1         
     * 
     */
    public BLEACT_ReadIDDDeviceStatus(BLECallbackContext callbackContext,
                               ResponseCallbackWithData callback)
    {
        super(callbackContext);

        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
        
        // Assign mACTCbContext 
        mActCbContext = callbackContext;
        
        // Assign mACT71RhContext
        mActContext = mActCbContext.getBLERequestHandler().getContext();
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        // Start GetActiveBRD
        callReadIDDDeviceState();       
    }
    
    /**
     * Action when timeout happens
     * 
     * @param request [in] IBLERequest
     * 
     *          Command which timeout happens
     *          
     *          Range: Valid IBLERequest
     *          Unit: IBLERequest
     *          Scaling: 1
     *          
     * @param parameter [in] BLERequestParameter
     * 
     *          Parameter of command which timeout happens
     *          
     *          Range: Valid BLERequestParameter
     *          Unit: BLERequestParameter
     *          Scaling: 1
     * 
     * @return None
     * 
     */    
    @Override
    public void onRequestTimeout(IBLERequest request,
            BLERequestParameter parameter)
    {
        mProcedureRetryCount++;
        
        if (mProcedureRetryCount <= mProcedureRetryLimit)
        {
            callResetConnection();
        }
        else
        {
            callEmwr(EMWRList.EMW45949);
            
            setupReturnResult(SafetyBoolean.FALSE, null);
        }        
    }
    
    @Override
    public void onConnectionStateChanged(int state)
    {

        if (CommsConstant.BtState.CONNECTED == state)
        {
            if (mCmdState == CMD_RESET_RETRY)
            {
               Debug.printD(TAG, "[Reconnected] retry CMD:" + Integer.toString(mBLECommand, 16));
               
               callReadIDDDeviceState();
            }
            else
            {
               // Apply to the coding standard
            }
        }
    	else if ( CommsConstant.BtState.DISCONNECTED == state )
        {
            if (mCmdState != CMD_RESET_RETRY)
            {
                Debug.printD(TAG, "[CommErr] Not Connected");
                
                setupReturnResult(SafetyBoolean.FALSE ,null);
            }
            else
            {
                // Apply to the coding standard
            }
        }
    	else if ( CommsConstant.BtState.CONNECTIONLOST == state )
        {
            Debug.printD(TAG, "Connection Lost");
            
            setupReturnResult(SafetyBoolean.FALSE, null);
        }
        else
        {
            // Apply to the coding standard
        }
        
    }
    /**
     * load limit of E2E retry times from Config Matrix
     * 
     * @return None
     * 
     */    
    protected void loadE2ERetryLimit()
    {
        // Key String
        SafetyString ssCmKey = null;
                
        // Get BRP max number from Config Matrix
        ssCmKey = new SafetyString(ConfigParameter.KEY_BLE_E2E_RETRY,
                CRCTool.generateCRC16(ConfigParameter.KEY_BLE_E2E_RETRY.getBytes()));
        
        mE2ERetryLimit = ReadConfig.getIntegerDataByKey(ssCmKey).get();
    } 
    
    /**
     * load limit of procedure retry times from Config Matrix
     * 
     * @return None
     * 
     */    
    protected void loadProcedureRetryLimit()
    {
        // Key String
        SafetyString ssCmKey = null;
                
        // Get BRP max number from Config Matrix
        ssCmKey = new SafetyString(ConfigParameter.KEY_BLE_PROCEDURE_RETRY,
                CRCTool.generateCRC16(ConfigParameter.KEY_BLE_PROCEDURE_RETRY.getBytes()));
        
        mProcedureRetryLimit = ReadConfig.getIntegerDataByKey(ssCmKey).get();
    }
    
    /**
     * Handle operation: read IDD device status
     * 
     * @param pack [in] ResponsePack
     * 
     *          Response data from MP response
     * 
     *          Range: Valid ResponsePack object
     *          Unit: ResponsePack
     *          Scaling: 1
     *          
     * @return None
     */    
    protected void handleReadIDDDeviceStatus(ResponsePack pack)
    {    
        // Response
        AttributeReadResponse attResponse = 
                                    (AttributeReadResponse) pack.getResponse();
        
        // Data byte array
        byte[] bData = attResponse.getData().getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // Therapy Control State
        byte bTCState = 0;
        
        // Operational State
        byte bOpState = 0;
        
        // Reservoir Remaining Amount
        int iRRA = 0;
        
        // Flags
        byte bFlag = 0;        
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        
        // Get therapy control state
        bTCState = bbData.get();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus TCState: " + Integer.toString(bTCState, 16));
        
        // Get operational state
        bOpState = bbData.get();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus OpState: " + Integer.toString(bOpState, 16));
        
        // Get Reservoir Remaining Amount
        iRRA = bbData.getShort();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus RRA: " + Integer.toString(iRRA, 16));
        
        // Get Flags
        bFlag = bbData.get();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus Flag: " + Integer.toString(bFlag, 16));        
                
        // Check data length
        if (bData.length != READ_IDD_DEVICE_STATE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleReadIDDDeviceStatus NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE, null);          
        }
        else
        {
            // Return pack to calling activity
            setupReturnResult(SafetyBoolean.TRUE, pack);
        }
    }
    
    /**
     * Handle E2E Error
     * 
     * @param e2e_result [in] int
     * 
     *          Data from MP response
     * 
     *          Range: Valid ResponsePack object
     *          Unit: ResponsePack
     *          Scaling: 1
     *          
     * @return None
     * 
     */    
    protected void handleE2EError(int e2e_result)
    {        
        switch(e2e_result)
        {
        case E2E_Result.CRC_ERROR:
            /* falls through */
        case E2E_Result.UNKNOWN:
            mE2ERetryCount++;
            
            if (mE2ERetryCount <= mE2ERetryLimit)
            {
                callReadIDDDeviceState();
            }
            else
            {
                callEmwr(EMWRList.EMW45947);
                
                setupReturnResult(SafetyBoolean.FALSE, null);
            }
            
            break;
            
        case E2E_Result.COUNTER_ERROR:
            mE2ERetryCount++;
            
            if (mE2ERetryCount <= mE2ERetryLimit)
            {
                callResetConnection();
            }
            else
            {
                callEmwr(EMWRList.EMW45947);
                
                setupReturnResult(SafetyBoolean.FALSE, null);
            }            
            
            break;
            
        default:
            // Apply to the coding standard
            break;
        }
    }  
    
    
    
    /**
     * Call command: read IDD device status
     *          
     * @return None
     * 
     */    
    protected void callReadIDDDeviceState()
    {
        Debug.printD(TAG, "[Call CMD] Read IDD Device Status ");
        
        // Run Command
        mActCbContext.getBLERequestHandler()
                       .addRequest(
                               ReadIDDDeviceStatus.getInstance(mActContext), 
                               null, mResponseReadIDDDeviceState);
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mActCbContext.getBLERequestHandler().startHandleRequest();
    }
    
    /**
     * Call command: Reset connection
     *          
     * @return None
     * 
     */    
    protected void callResetConnection()
    {
        Debug.printD(TAG, "[Call CMD] Reset Connection");
        
        
        // Add Request: Reset Connection
        mActCbContext.getBLERequestHandler().addRequest(
                            ResetConnection.getInstance(mActContext), 
                            null, mResponseResetConnection);
        
        mCmdState = CMD_RESET_RETRY;
      
        // Start Request
        mActCbContext.getBLERequestHandler().startHandleRequest();
    }    
    
    /**
     * Setup command set returned result
     * 
     * @param sbResult [in] SafetyBoolean
     * 
     *          Returned result of this command set
     *          
     *          Range: Valid SafetyBoolean
     *          Unit: SafetyBoolean
     *          Scaling: 1
     *          
     * @return None
     * 
     */    
    protected void setupReturnResult(SafetyBoolean sbResult, ResponsePack pack)
    {
        mCallback.onRequestCompleted(sbResult, pack);
        
        // Set wait state to leave this activity 
        mCallbackContext.setCurrentState(
                            new BLEWaitingState(mCallbackContext));
    }    
    
    /**
     * Call EMWR for error handling
     * 
     * @param ErrorCode [in] EMWRList
     * 
     *          Internal Error Code Enum Value of Basal Delivery 
     * 
     *          Range: Valid EMWRList
     *          Unit: EMWRList
     *          Scaling: 1
     * 
     * @return None
     */    
    protected void callEmwr(EMWRList ErrorCode)
    {    
        NotifyMessage msg = new NotifyMessage(ErrorCode);
     
        NotifyProxy.showEMWR(msg);
    }    
    
    /**
     * Response call back for Read IDD Device Status
     * 
     */    
    private class ResponseReadIDDDeviceState implements ResponseCallbackWithData
    {                
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         *        
         * @param pack [in] ResponsePack
         * 
         *        Data of indication
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
            // Result byte value
            byte isSuccess = result.getByte();
            
            // AttributeChangeNotification Indication
            AttributeReadResponse attResponse = null;            
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE, null);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                attResponse = (AttributeReadResponse) pack.getResponse();
                
                handleE2EError(attResponse.getResult().get());
            }
            else
            {
                handleReadIDDDeviceStatus(pack);
            }
        }
        
        /**
         * Action when command response is returned.
         * No use in this response
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            // No functionality
        }
    }
    

    /**
     * Response call back for ResetConnection
     * 
     */    
    private class ResponseResetConnection implements ResponseCallback
    {
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */
        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            byte isSuccess = result.getByte();
            
            if (isSuccess == SAFETY_TRUE)
            {
                // Apply to the coding standard
                Debug.printD(TAG, "[Reset Connection Send OK ] "); 
            }
            else
            {
                callEmwr(EMWRList.EMW45950);
                Debug.printD(TAG, " [Reset Connection NGGGGGGGGGGGGGG ] ");
                
                setupReturnResult(SafetyBoolean.FALSE, null);
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
// [Update] Add readIDDDeviceStatus and GetActveBRD for UISD spec change
