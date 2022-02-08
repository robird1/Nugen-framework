/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEACT56
 * Brief: BLEACT56
 *
 * Create Date: 2015/7/21
 * $Revision: 21192 $
 * $Author: KiddYeh $
 * $Id: BLEACT56.java 21192 2015-10-08 02:59:05Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GeneralConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GetActiveBolusDelivery;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GetActiveBolusIDs;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeReadResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.BolusDelivery;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEACT56 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEACT56";

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
    
    // Number of Active Boluses
    private int mNumberOfActBoluses = 0;
    
    
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
    private ResponseGetActiveBolusIDs mResponseGetActiveBolusIDs = new ResponseGetActiveBolusIDs();
        
    // Response of ReadIDDDeviceStatus
    private ResponseGetActiveBolus mResponseGetActiveBolus = new ResponseGetActiveBolus();
    
    /**
     * The Active Bolus IDs list
     */
    private static List<Short> mActiveBolusIDsList = new LinkedList<Short>();
    
    /**
     *  The response callback function of the latest request
     */
    private ResponseCallback mACT56Done = new ACT56Done();
    
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
    public BLEACT56(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        super(callbackContext);
        
        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
        
        // Assign mACT56RhContext
        mActContext = mCallbackContext.getBLERequestHandler().getContext();
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        // Start ReadIDDDeviceStatus
        callGetActiveBolusIDs();
        
    }

    /**
     * Call command: call active Read IDD Device status
     *          
     * @return None
     * 
     */    
    protected void callGetActiveBolusIDs()
    {
        Debug.printD(TAG, "[Call CMD] Get Active Bolus IDs ");
        
        // Add Request: BLE_SEQ76 get basal rate template status
        mCallbackContext.getBLERequestHandler().addRequest(
                GetActiveBolusIDs.getInstance(mActContext), 
                            null, mResponseGetActiveBolusIDs);
        
        mBLECommand = ControlPointConstant.OpCode.IDSREAD_ACTIVE_BOLUS_ID;
        
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
        case ControlPointConstant.OpCode.IDSREAD_ACTIVE_BOLUS_ID:
            
            Debug.printD(TAG, "[Retry CMD] Get Active Bolus IDs E2E: " + mE2ERetryCount
                              + "    TO: " + mProcedureRetryCount);
            
            callGetActiveBolusIDs();
            
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
    private class ResponseGetActiveBolusIDs implements ResponseCallbackWithData
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
            Debug.printD(TAG,"ResponseGetActiveBolusIDs enter");
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

                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                handleGetActiveBolusIDs(response.getData());
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
     * Response call back for ReadIDDDeviceStatus
     * 
     */    
    private class ResponseGetActiveBolus implements ResponseCallbackWithData
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
            Debug.printD(TAG,"ResponseGetActiveBolus enter");
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

                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                handleGetActiveBolus(response.getData());
            }
        }

		@Override
		public void onRequestCompleted(SafetyBoolean result) 
		{
			// TODO Auto-generated method stub
			
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
    
    protected void handleGetActiveBolus(SafetyByteArray sbData)
    {    
    	final int LENGTH_OF_OPCODE = 2;
    	final int LENGTH_OF_CRC_AND_COUNTER = 3;
        // Data byte array
        byte[] bData = sbData.getByteArray();
        byte[] dataArray = Arrays.copyOfRange(bData, LENGTH_OF_OPCODE, bData.length - LENGTH_OF_CRC_AND_COUNTER);
        
        BolusDelivery.parseBolusDelivery(mActContext, new SafetyByteArray(dataArray,
        		CRCTool.generateCRC16(dataArray)));
        
        mActiveBolusIDsList.remove(0);        
        
        
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        if ( !mActiveBolusIDsList.isEmpty())
        {
        	callGetActiveBolusDelivery();	
        }
        else
        {
        	callSyncBolusConfirm();
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
    
    protected void handleGetActiveBolusIDs(SafetyByteArray sbData)
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
        
        mActiveBolusIDsList.clear(); 
        for(int i=0; i<mNumberOfActBoluses; i++)
        { 
            mActiveBolusIDsList.add((short) (bbData.getShort() & TWO_BYTES_OPERAND));
        }
        
        
        Debug.printD(TAG, " handleGetActiveBolusIDs OpCode: " + Integer.toString(iOpCode, 16));
        
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        if ( !mActiveBolusIDsList.isEmpty())
        {
        	callGetActiveBolusDelivery();	
        }
        else
        {
        	callSyncBolusConfirm();
        }
        
                
    } 
    
    /**
     * Call command: call Sync Bolus Confirm
     *          
     * @return None
     * 
     */    
    protected void callSyncBolusConfirm()
    {
        Debug.printD(TAG, "[Call CMD] Sync Bolus Confirm ");

        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setCommandCode(CommsConstant.CommandCode.BT_ACTIVE_BOLUS_CFM);
        mCallbackContext.getBLERequestHandler().addRequest(
                GeneralConfirmation.getInstance(mActContext), parameter, mACT56Done);
        // start to execute  
        mCallbackContext.getBLERequestHandler().startHandleRequest();
     
    }
    
    /**
     * Call command: call Get Active Bolus Delivery - Programmed
     *          
     * @return None
     * 
     */    
    protected void callGetActBolusDeliveryProgrammed()
    {
        Debug.printD(TAG, "[Call CMD] Get Active Bolus Delivery - Programmed ");

        BLERequestParameter parameter = new BLERequestParameter();
        
        parameter.setCommandCode(CommsConstant.CommandCode.BT_ACTIVE_BOLUS_CFM);
        mCallbackContext.getBLERequestHandler().addRequest(
                GeneralConfirmation.getInstance(mActContext), parameter, mACT56Done);
        // start to execute  
        mCallbackContext.getBLERequestHandler().startHandleRequest();
     
    }
    
    /**
     * Call command: call confirm IDD Device status
     *          
     * @return None
     * 
     */    
    protected void callGetActiveBolusDelivery()
    {
        Debug.printD(TAG, "[Call CMD] Get Active Bolus Delivery ");        
        final byte DELIVERED = 0x3C;
        
        byte[] bolusId = new byte[]{0, 0};
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        BLERequestParameter parameter = new BLERequestParameter();
        
        Short id = mActiveBolusIDsList.get(0);
    	
    	bolusId = BLEController.parseInt16(id);
    	
    	buffer.append(bolusId, 0, bolusId.length);
        buffer.append(DELIVERED);
                       
        parameter.setData(new SafetyByteArray(buffer.toByteArray(), 
        		CRCTool.generateCRC16(buffer.toByteArray())));
        
        mCallbackContext.getBLERequestHandler().addRequest(
        		GetActiveBolusDelivery.getInstance(mActContext), parameter, mACT56Done);
        
        mBLECommand = ControlPointConstant.OpCode.IDSREAD_ACTIVE_BOLUS;
        
        mCmdState = CMD_NORMAL;
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
     
    private class ACT56Done implements ResponseCallback
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
                Debug.printD(TAG, "[ACT56 done ] "); 
            }
            else
            {
                Debug.printD(TAG, "[ACT56 failed ] ");  
                
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