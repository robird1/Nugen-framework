/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BLEACT_GetActiveBRD
 * Brief: 
 *
 * Create Date: 2015�~11��18��
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
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
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

public class BLESEQ75 extends AbstractBLEStateHandler
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
    
    // Get Active BRD Response Basic Length
    private static final int GET_ACTIVE_BRD_RESPONSE_BASIC_LEN = 9;
    
    // Get Active BRD Response with TBR Length
    private static final int GET_ACTIVE_BRD_RESPONSE_TBR_LEN = 16;
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF;
    
    // Bit Operation
    private static final byte BIT0_SET = (byte)0x01;
    
    // Response of ReadIDDDeviceStatus
    private ResponseGetActiveBRD mResponseGetActiveBRD = 
                                            new ResponseGetActiveBRD();    
        
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
    public BLESEQ75(BLECallbackContext callbackContext,
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
        callGetActiveBRD();       
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

    /**
     * Action when connection status changes
     * 
     * @param isConnected [in] SafetyBoolean
     * 
     *          Connection status
     *          
     *          SafetyBoolean.TRUE -- Connected
     *          SafetyBoolean.FALSE -- Disconnected
     *          
     *          Range: Valid SafetyBoolean
     *          Unit: SafetyBoolean
     *          Scaling: 1
     *          
     * @return None
     * 
     */    
    
    
    @Override
    public void onConnectionStateChanged(int state)
    {

        if (CommsConstant.BtState.CONNECTED == state)
        {
            if (mCmdState == CMD_RESET_RETRY)
            {
               Debug.printD(TAG, "[Reconnected] retry CMD:" + Integer.toString(mBLECommand, 16));
               
               callGetActiveBRD();
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
                
                setupReturnResult(SafetyBoolean.FALSE, null);
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
     * Handle operation: get Active BRD
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
    protected void handleGetActiveBRD(ResponsePack pack)
    {    
        // Response
        AttributeChangeNotification attResponse = 
                                (AttributeChangeNotification)pack.getResponse();
        
        // Data byte array
        byte[] bData = attResponse.getData().getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // Flags
        Byte bFlag = 0;
        
        // TBR Set Flag
        Byte bTbrSetFlag = 0;
        
        // Package Length
        int iPkgLen = 0;
                
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleGetActiveBRD OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get Flag
        bFlag = bbData.get();
        
        Debug.printD(TAG, " handleGetActiveBRD Flag: " + bFlag);
        
        // Get TBR Set Flag
        bTbrSetFlag = (byte) (bFlag & BIT0_SET);
        
        Debug.printD(TAG, " handleGetActiveBRD TbrSet: " + bTbrSetFlag);
        
        if (bTbrSetFlag == BIT0_SET)
        {
            iPkgLen = GET_ACTIVE_BRD_RESPONSE_TBR_LEN;
        }
        else
        {
            iPkgLen = GET_ACTIVE_BRD_RESPONSE_BASIC_LEN;
        }
                
        // Check data length
        if (bData.length != iPkgLen)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleGetActiveBRD NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE, null);          
        }
        else
        {
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
                callGetActiveBRD();
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
     * Call command: call active basal rate delivery
     *          
     * @return None
     * 
     */    
    protected void callGetActiveBRD()
    {
        Debug.printD(TAG, "[Call CMD] Get Active BRD ");
        
        // Add Request: BLE_SEQ94 get basal rate template status
        mActCbContext.getBLERequestHandler().addRequest(
                            GetActiveBRD.getInstance(mActContext), 
                            null, mResponseGetActiveBRD);
        
        mBLECommand = ControlPointConstant.OpCode.IDSREAD_ACTIVE_BASAL;
      
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
     * Response call back for GetActiveBRD
     * 
     */    
    private class ResponseGetActiveBRD implements ResponseCallbackWithData
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
                handleGetActiveBRD(pack);
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
