/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEACT52
 * Brief: BLEACT52
 *
 * Create Date: 2015/7/21
 * $Revision: 21192 $
 * $Author: KiddYeh $
 * $Id: BLEACT52.java 21192 2015-10-08 02:59:05Z KiddYeh $
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
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ReadAnnunciationStatus;
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

public class BLEACT52 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEACT52";

 // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();
    
    private static final byte SAFETY_FALSE = SafetyBoolean.FALSE.getByte();
    
    // Command State Constants
    private static final int CMD_NORMAL = HammingDistance.SAFETY_NUMBER_VALUE_0125;
    
    private static final int CMD_RESET_RETRY = HammingDistance.SAFETY_NUMBER_VALUE_0126;
    
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
    private ResponseReadAnnunciationStatus mResponseReadAnnunciationStatus = new ResponseReadAnnunciationStatus();
         
    /**
     *  The response callback function of the latest request
     */
    private ResponseCallback mACT52Done = new ACT52Done();
    
    
    /**
     * This method adds the BLEACT52 sequence requests in request list of the 
     * request handler and start to execute the request.
     * 
     * @param callbackContext: the ble state handler
     *            Range: a valid object of BLECallbackContext
     *            Unit: BLECallbackContext
     *            Scaling: 1
     * @param callback: the listener of the bond state
     *            Range: a valid object of IOnBondStateChangedListener
     *            Unit: IOnBondStateChangedListener
     *            Scaling: 1           
     * 
     * @return void [out]
     * 
     * @see mCallback
     * @see mCallbackContext
     */
    public BLEACT52(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        super(callbackContext);
        
        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
        
        // Assign mACT52RhContext
        mActContext = mCallbackContext.getBLERequestHandler().getContext();
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        // Start ReadAnnunciationStatus
        callReadAnnunciationStatus();
        
    }
    
    /**
     * Call command: call active Read Annunciation status
     *          
     * @return None
     * 
     */    
    protected void callReadAnnunciationStatus()
    {
        Debug.printD(TAG, "[Call CMD] Read IDD Device Status ");
        
        // Add Request: BLE_SEQ94 get basal rate template status
        mCallbackContext.getBLERequestHandler().addRequest(
                ReadAnnunciationStatus.getInstance(mActContext), 
                            null, mResponseReadAnnunciationStatus);
        
        mBLECommand = UUID16.IDD_ANNUNCIATION_STATUS;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }
    
    /**
     * This method is called when Request-Timeout. It calls reset-connection
     * or reports to EMWR and returns result via callback  
     *
     * @param request [in] the request     
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1  
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1 
     *                      
     * @return void [out]
     * @see mProcedureRetryCount 
     * @see mProcedureRetryLimit
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
        case UUID16.IDD_ANNUNCIATION_STATUS:
            
            Debug.printD(TAG, "[Retry CMD] Read Annunciation Status E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callReadAnnunciationStatus();
            
            break;                           
        default:
            // Apply to the coding standard
            break;
        }
    } 
    
    /**
     * This method is called after the ConnectStateChanged notification is received. 
     * If it is connected with retry process, it calls retry function.
     * If it is disconnected without retry process, it returns failed via callback function. 
     *
     * @param isConnected [in] the BLE-Device is disconnected or not.      
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1  
     *            
     * @return void [out]
     * @see mCallback
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
        mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
    }
    
    
    /**
     * Response call back for ReadIDDDeviceStatus
     * 
     */    
    private class ResponseReadAnnunciationStatus implements ResponseCallbackWithData
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
            
                handleReadAnnunciationStatus(response.getData());
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
    
    protected void handleReadAnnunciationStatus(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        Debug.printD(TAG, "bData");
        Debug.dumpPayload(TAG, bData);
        Debug.printD(TAG, "++++++++++++++++++++");
                
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        callSoloMEMWRConfirm();
                
    }
    

    

    
    /**
     * Call command: call confirm Sync SoloM EMWR
     *          
     * @return None
     * 
     */    
    protected void callSoloMEMWRConfirm()
    {
        Debug.printD(TAG, "[Call CMD] Device Status Confirm ");

        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setCommandCode(CommsConstant.CommandCode.BT_SOLOM_EMWR_CFM);
        mCallbackContext.getBLERequestHandler().addRequest(
                GeneralConfirmation.getInstance(mActContext), parameter, mACT52Done);
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
        // AttributeReadResponse
        AttributeReadResponse attResponse= (AttributeReadResponse) pack
                .getResponse();
        
        // E2E Result
        int e2e_result = attResponse.getResult().get();

        switch(e2e_result)
        {
        case E2E_Result.CRC_ERROR:
        case E2E_Result.UNKNOWN:
            
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
     
    private class ACT52Done implements ResponseCallback
    {
        /**
         * This method is called after the response is received.
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
                Debug.printD(TAG, "[ACT52 Request done ] "); 
            }
            else
            {
                Debug.printD(TAG, "[ACT52 Request failed ] ");  
                
            }
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
            
            if (isSuccess == SAFETY_TRUE)
            {
                // Apply to the coding standard
                Debug.printD(TAG, "[Reset Connection Send OK ] "); 
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
