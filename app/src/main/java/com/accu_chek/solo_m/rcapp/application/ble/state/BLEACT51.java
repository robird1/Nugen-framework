/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEACT51
 * Brief: BLEACT51
 *
 * Create Date: 2015/7/21
 * $Revision: 21192 $
 * $Author: KiddYeh $
 * $Id: BLEACT51.java 21192 2015-10-08 02:59:05Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GeneralConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ReadIDDDeviceStatus;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID.UUID16;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeReadResponse;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEACT51 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEACT51";

    // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();
    
    private static final byte SAFETY_FALSE = SafetyBoolean.FALSE.getByte();
    
    // Command State Constants
    private static final int CMD_NORMAL = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0125;
    
    private static final int CMD_RESET_RETRY = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0126;
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF; 
    
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
    private ResponseCallback mCallback = null;
      
    // Command State
    private int mCmdState = CMD_NORMAL;
    
    // Response of ResetConnection
    private ResponseResetConnection mResponseResetConnection =  new ResponseResetConnection(); 
    
    // Response of ReadIDDDeviceStatus
    private ResponseReadIDDDeviceStatus mResponseReadIDDDeviceStatus = new ResponseReadIDDDeviceStatus();
               
    /**
     *  The response callback function of the latest request
     */
    private ResponseCallback mACT51Done = new ACT51Done();
    
    /**
     * This method adds the BLEACT51 sequence requests in request list of the 
     * request handler and start to execute the request.
     * 
     * @param callbackContext: Callback context of this activity
     *            Range: a valid object of BLECallbackContext
     *            Unit: BLECallbackContext
     *            Scaling: 1
     * @param callback: Callback for this activity
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1           
     * 
     * @return void [out]
     * 
     * @see mCallback
     * @see mCallbackContext
     */
    public BLEACT51(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        super(callbackContext);
        
        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
        
        // Assign mACT51RhContext
        mActContext = mCallbackContext.getBLERequestHandler().getContext();
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        // Start ReadIDDDeviceStatus
        callReadIDDDeviceStatus();
        
    }

    /**
     * Call command: call active Read IDD Device status
     *          
     * @return None
     * 
     */    
    protected void callReadIDDDeviceStatus()
    {
        Debug.printD(TAG, "[Call CMD] Read IDD Device Status ");
        
        // Add Request: BLE_SEQ94 get basal rate template status
        mCallbackContext.getBLERequestHandler().addRequest(
                ReadIDDDeviceStatus.getInstance(mActContext), 
                            null, mResponseReadIDDDeviceStatus);
        
        mBLECommand = UUID16.IDD_DEVICE_STATUS;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mCallbackContext.getBLERequestHandler().startHandleRequest();
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
        Debug.printD(TAG, "onRequestTimeout");
        Debug.printD(TAG, " Time out Request: " + request.getClass().getSimpleName());
        mProcedureRetryCount++;

        if (mProcedureRetryCount <= mProcedureRetryLimit)
        {
            callResetConnection();
        }
        else
        {
            callEmwr(EMWRList.EMW45949);
            
            setupReturnResult(SafetyBoolean.FALSE);
        }        
    }

    /**
     * Retry comms command
     *          
     * @return None
     * 
     */    
    
    protected void retryCommand()
    {
        switch(mBLECommand)
        {
        case UUID16.IDD_DEVICE_STATUS:
            
            Debug.printD(TAG, "[Retry CMD] Read IDD Device Status E2E: " + mE2ERetryCount
                              + "    TO: " + mProcedureRetryCount);
            
            callReadIDDDeviceStatus();
            
            break;                           
        default:
            
            // Apply to the coding standard
            
            break;
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
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
                // Apply to the coding standard
            }
        }
    	else if ( CommsConstant.BtState.CONNECTIONLOST == state )
        {
            Debug.printD(TAG, "Connection Lost");
            setupReturnResult(SafetyBoolean.FALSE);
        }
        else
        {
            // Apply to the coding standard
        }
        
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
        mCallbackContext.getBLERequestHandler().addRequest(
                            ResetConnection.getInstance(mActContext), 
                            null, mResponseResetConnection);
        
        mCmdState = CMD_RESET_RETRY;
      
        // Start Request
        mCallbackContext.getBLERequestHandler().startHandleRequest();
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
    protected void setupReturnResult(SafetyBoolean sbResult)
    {
        if (null != mCallback)
        {
            mCallback.onRequestCompleted(sbResult);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Set wait state to leave this activity 
        mCallbackContext.setCurrentState(
                            new BLEWaitingState(mCallbackContext));
    }
    
    /**
     * Response call back for ReadIDDDeviceStatus
     * 
     */    
    private class ResponseReadIDDDeviceStatus implements ResponseCallbackWithData
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
            Debug.printD(TAG,"ResponseReadIDDDeviceStatus enter");
            // Result byte value
            byte isSuccess = result.getByte();
                     
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                handleE2EError(pack);
            }
            else
            {

                AttributeReadResponse response= (AttributeReadResponse) pack.getResponse();
            
                handleReadIDDDeviceStatus(response.getData());
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
     * Handle operation: Read IDDDevice Status
     * 
     * @param sbData [in] SafetyByteArray
     * 
     *          Data from MP response
     * 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Scaling: 1
     *          
     * @return None
     */
    
    protected void handleReadIDDDeviceStatus(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        Debug.printD(TAG, "bData");
        Debug.dumpPayload(TAG, bData);
        Debug.printD(TAG, "++++++++++++++++++++");
            
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleGetActiveBRD OpCode: " + Integer.toString(iOpCode, 16));
        
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        callDeviceStatusConfirm();
                
    } 
    
    /**
     * Call command: call confirm IDD Device status
     *          
     * @return None
     * 
     */    
    protected void callDeviceStatusConfirm()
    {
        Debug.printD(TAG, "[Call CMD] Device Status Confirm ");

        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setCommandCode(CommsConstant.CommandCode.BT_SYNC_DEVICE_STATUS_CFM);
        mCallbackContext.getBLERequestHandler().addRequest(
                GeneralConfirmation.getInstance(mActContext), parameter, mACT51Done);
        // start to execute  
        mCallbackContext.getBLERequestHandler().startHandleRequest();
     
    }
    
    
    /**
     * Handle E2E Error
     * 
     * @param pack [in] ResponsePack
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
    protected void handleE2EError(ResponsePack pack)
    {
        Debug.printD(TAG, "[handleE2EError enter ] "); 
        // AttributeChangeNotification Indication
        AttributeReadResponse attResponse= (AttributeReadResponse) pack
                .getResponse();
        
        // E2E Result
        int e2e_result = attResponse.getResult().get();
        
        switch(e2e_result)
        {
        case E2E_Result.CRC_ERROR:
            Debug.printD(TAG, "[handleE2EError ==> E2E_Result.CRC_ERROR ] ");
        case E2E_Result.UNKNOWN:
            Debug.printD(TAG, "[handleE2EError ==> E2E_Result.UNKNOWN ] ");
            mE2ERetryCount++;
            
            if (mE2ERetryCount <= mE2ERetryLimit)
            {
                
                retryCommand();
            }
            else
            {
                callEmwr(EMWRList.EMW45947);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            
            break;
            
        case E2E_Result.COUNTER_ERROR:
            Debug.printD(TAG, "[handleE2EError ==> E2E_Result.COUNTER_ERROR ] ");
            mE2ERetryCount++;
            
            if (mE2ERetryCount <= mE2ERetryLimit)
            {
                callResetConnection();
            }
            else
            {
                callEmwr(EMWRList.EMW45947);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }            
            
            break;
            
        default:
            
            // Apply to the coding standard
            
            break;
        }
    }
     
    private class ACT51Done implements ResponseCallback
    {
        /**
         * This method is called after the response is received.
         * If the result is failed, it disconnects BD. 
         * 
         * 
         * @param result: the result of request
         *            Range: A valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return void [out]            
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isResultOK = 
                    (SafetyBoolean.TRUE.getByte() == result.getByte());
            
            if ( isResultOK )
            {
                Debug.printD(TAG, "[ACT51 done ] "); 
            }
            else
            {
                Debug.printD(TAG, "[ACT51 failed ] ");  
                
            }
            setupReturnResult(result);
        }
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
        Debug.printD(TAG, "[call EMWR] ");
        NotifyMessage msg = new NotifyMessage(ErrorCode);
     
        NotifyProxy.showEMWR(msg);
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
            mCallbackContext.getBLERequestHandler().clearRequest();
            if (isSuccess == SAFETY_TRUE)
            {
                // Apply to the coding standard
                Debug.printD(TAG, "[Reset Connection Send OK ] "); 
                Debug.printD(TAG, "[Reset Retry] retry CMD:" + Integer.toString(mBLECommand, 16));
                retryCommand();
            }
            else
            {
                callEmwr(EMWRList.EMW45950);
                Debug.printD(TAG, " [Reset Connection NGGGGGGGGGGGGGG ] ");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
        }
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */