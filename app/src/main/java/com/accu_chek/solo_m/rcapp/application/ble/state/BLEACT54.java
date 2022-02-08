/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEACT54
 * Brief: BLEACT54
 * 
 * Create Date: 2015/10/05
 * $Revision: 20559 $
 * $Author: DWYang $
 * $Id: BLEACT10.java 20559 2015-10-01 14:09:49Z DWYang $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GeneralConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GetDateTime;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetDateTime;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.OpCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.ResponseCode;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEACT54 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEACT54";
    
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
    
    // TBR Data Array
    private BLERequestParameter mParameter = new BLERequestParameter();
    
    // ResponseCallback from Calling Function
    private ResponseCallback mCallback = null;
    
    
    // Command State
    private int mCmdState = CMD_NORMAL;
    
    /**
     *  The response callback function of GetDateTime request
     */
    private ResponseCallbackWithData mResponseGetDateTime = new ResponseGetDateTime();
    
    /**
     *  The response callback function of SetDateTime request
     */
    private ResponseCallbackWithData mResponseSetDateTime = new ResponseSetDateTime();
    
    /**
     *  The Confirmation response callback function of request
     */
    private ResponseCallback mConfirmationDone = new ConfirmationDone();
    
 
    
    /**
     * This method adds the BLEACT54 sequence requests in request list of the 
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
    public BLEACT54(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        super(callbackContext);
        Debug.printD(TAG, "[ACT54] "); 
        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
        
        // Assign mACTCbContext 
        mActCbContext = callbackContext;
        
        // Assign mACT54RhContext
        mActContext = mActCbContext.getBLERequestHandler().getContext();
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        // Start GetDateTime
        callGetDateTime();

    }
      
    /**
     * This method is called after Request-Timeout. 
     * Action when timeout happens
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
            
            setupReturnResult(SafetyBoolean.FALSE);
        }        
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
        // AttributeChangeNotification Indication
        AttributeChangeNotification 
                attResponse = (AttributeChangeNotification) pack.getResponse();
        
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
     * Retry comms command
     *          
     * @return None
     * 
     */    
    
    protected void retryCommand()
    {
        switch(mBLECommand)
        {
        case ControlPointConstant.OpCode.SOLOM_GET_DATE_AND_TIME:
            
            Debug.printD(TAG, "[Retry CMD] Get Date Time E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callGetDateTime();
            
            break; 
            
        case ControlPointConstant.OpCode.SOLOM_SET_DATE_AND_TIME:
            
            Debug.printD(TAG, "[Retry CMD] Set Date Time E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callSetDateTime();
            
            break;

   
        default:
            
            // Apply to the coding standard
            
            break;
        }
    }    
    
    /**
     * Call command: call Date Time
     *          
     * @return None
     * 
     */    
    
    protected void callGetDateTime()
    {
        Debug.printD(TAG, "[Call CMD] Get Date Time ");
        
        // BLE_SEQ79: get date and time
        // add Get Date and time request in list
        mActCbContext.getBLERequestHandler().addRequest(
                GetDateTime.getInstance(mActContext), null, mResponseGetDateTime);
        
        mBLECommand = ControlPointConstant.OpCode.SOLOM_GET_DATE_AND_TIME;
        
        mCmdState = CMD_NORMAL;

        // Start Request
        mActCbContext.getBLERequestHandler().startHandleRequest();
    }
    
    
    /**
     * Call command: call Date Time
     *          
     * @return None
     * 
     */    
    
    protected void callSetDateTime()
    {
        Debug.printD(TAG, "[Call CMD] Set Date Time ");
        
        // add Set Date and time request in list
        mActCbContext.getBLERequestHandler().addRequest(
                SetDateTime.getInstance(mActContext), mParameter, mResponseSetDateTime);
        
        mBLECommand = ControlPointConstant.OpCode.SOLOM_SET_DATE_AND_TIME;
        
        mCmdState = CMD_NORMAL;
      
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
    protected void setupReturnResult(SafetyBoolean sbResult)
    {
        if (null != mCallback)
        {     
            mCallback.onRequestCompleted(sbResult);
        }
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
    
    private class ResponseGetDateTime implements ResponseCallbackWithData
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
                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                handleGetDateTime(response.getData());
            }
        }
        
        
        /**
         * This method is called after the response is received.
         * No use in this response
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
           // No functionality
        }
    }

    
    
    /**
     * Handle operation: get Date Time
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
    
    protected void handleGetDateTime(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
           
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleGetDateTime OpCode: " + Integer.toString(iOpCode, 16));
        
        byte[] dateTimeOfMP = new byte[9];
        bbData.get(dateTimeOfMP, 0, 9);
        
        Debug.printD(TAG, "dateTimeOfMP length = " + dateTimeOfMP.length);
        Debug.dumpPayload(TAG, dateTimeOfMP);
        
        byte[] dateTimeOfRC =new byte[9];
        try
        {
            dateTimeOfRC = CustJavaFrameworkManager.getTimeManagementService(mActContext).verifyPumpTime(dateTimeOfMP);
            
            Debug.printD(TAG, "dateTimeOfRC length = " + dateTimeOfRC.length);
            Debug.dumpPayload(TAG, dateTimeOfRC);
            
            if (!Arrays.equals(dateTimeOfMP, dateTimeOfRC))
            {
                ByteArrayBuffer newBuffer = new ByteArrayBuffer(0);
              
                newBuffer.append(OpCode.SOLOM_SET_DATE_AND_TIME);
                newBuffer.append(OpCode.SOLOM_SET_DATE_AND_TIME >> 8);
                newBuffer.append(dateTimeOfRC, 0, dateTimeOfRC.length);
              
                mParameter.setData(new SafetyByteArray(newBuffer.toByteArray(), CRCTool.generateCRC16(newBuffer.toByteArray())));
              
                Debug.printD(TAG, "dateTime length = " + newBuffer.length());
                Debug.dumpPayload(TAG, newBuffer.toByteArray());
              
                mE2ERetryCount = 0;
                mProcedureRetryCount = 0;
                
                callSetDateTime();
              
            }
            else
            {
                Debug.printD(TAG, "[GetDateTime Request ===> MP & RC Time matche!!! ] "); 
                mE2ERetryCount = 0;
                mProcedureRetryCount = 0;
                callDateTimeConfirm();
            }
        }
        catch (RemoteException e)
        {
            
            e.printStackTrace();
        }
        finally
        {
            
        }
        
        
    }
    
    /**
     * Call command: call confirm IDD Device status
     *          
     * @return None
     * 
     */    
    protected void callDateTimeConfirm()
    {
        Debug.printD(TAG, "[Call CMD] Date Time Confirm ");

        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setCommandCode(CommsConstant.CommandCode.BT_TIME_SYNC_CFM);
        mCallbackContext.getBLERequestHandler().addRequest(
                GeneralConfirmation.getInstance(mActContext), parameter, mConfirmationDone);
        // start to execute  
        mCallbackContext.getBLERequestHandler().startHandleRequest();
     
    }
    
    
    /**
     * Handle operation: set Date Time
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
    
    protected void handleSetDateTime(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
           
        // Opcode
        int iOpCode = 0;
        int iRequestOpCode =0; 
        int iResponseValue =0;
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        iRequestOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        iResponseValue = bbData.get();
        
        Debug.printD(TAG, " iOpCode = " + iOpCode);
        Debug.printD(TAG, " iRequestOpCode = " + iRequestOpCode);
        Debug.printD(TAG, " iResponseValue = " + iResponseValue);
        
        Debug.printD(TAG, " OpCode.SOLOM_CP_RESP = " + OpCode.SOLOM_CP_RESP);
        Debug.printD(TAG, " OpCode.SOLOM_SET_DATE_AND_TIME = " + OpCode.SOLOM_SET_DATE_AND_TIME);
        Debug.printD(TAG, " ResponseCode.SUCCESS = " + ResponseCode.SUCCESS);
        
        boolean isOpCodeOK = OpCode.SOLOM_CP_RESP == iOpCode;
        boolean isRequestOpCodeOK = OpCode.SOLOM_SET_DATE_AND_TIME == iRequestOpCode;
        boolean isResponseSuccess = ResponseCode.SUCCESS == iResponseValue;
      
        if ( isOpCodeOK && isRequestOpCodeOK && isResponseSuccess)
        {
            Debug.printD(TAG, "[SetDateTime Request Done ] "); 
            callDateTimeConfirm();
        }
        else
        {
            Debug.printD(TAG, "[SetDateTime Request failed ] ");
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleSetDateTime NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);
        }
           
    }
    private class ResponseSetDateTime implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

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
                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                handleSetDateTime(response.getData());
            }

        }

        
    }
    
    private class ConfirmationDone implements ResponseCallback
    {
        /**
         * This method checks the response result of the request. 
         * If the result is failed, then it disconnects BD.
         * 
         * 
         * @param result: the result of request.
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return void [out] 
         *             
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isResultFailed = 
                    (SafetyBoolean.FALSE.getByte() == result.getByte());
            
            if ( isResultFailed )
            {
                Debug.printD(TAG, "[Confirmation Request failed ] "); 
            }
            else
            {
                Debug.printD(TAG, "[Confirmation Request done ] ");  
            }
            setupReturnResult(result);
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
/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */