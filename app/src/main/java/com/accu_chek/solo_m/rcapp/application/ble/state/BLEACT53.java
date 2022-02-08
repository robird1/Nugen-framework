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

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GeneralConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetConfigBlock;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetDateTime;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetTherapyState;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.OpCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.ResponseCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.TherapyControlState;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEACT53 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEACT53";
    
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
    private ResponseResetConnection mResponseResetConnection = new ResponseResetConnection(); 
    
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
    
 // Command State
    private int mCmdState = CMD_NORMAL;
    
    /**
     * The current state callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * The BLEACT71 callback
     */
    private ResponseCallback mACT71Done = new ACT71Done();
    
    /**
     *  The response callback function of ConfigBlockOne request
     */
    private ResponseCallbackWithData mResponseSetConfigBlockOne = new ResponseSetConfigBlockOne();
    
    /**
     *  The response callback function of ConfigBlockTwo request
     */
    private ResponseCallbackWithData mResponseSetConfigBlockTwo = new ResponseSetConfigBlockTwo();
         
    /**
     *  The response callback function of SetTherapyState request
     */
    private ResponseCallbackWithData mTherapyStateDone = new TherapyStateDone();
    
    /**
     *  The Confirmation response callback function of request
     */
    private ResponseCallback mConfirmationDone = new ConfirmationDone();
    
    /**
     *  The response callback function of SetDateTime request
     */
    private ResponseCallbackWithData mResponseSetDateTime = new ResponseSetDateTime();
    
    /**
     * This method adds the BLEACT53 sequence requests in request list of the 
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
    public BLEACT53(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        
        super(callbackContext);
        Debug.printD(TAG, "[ACT53] "); 
        
        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
           
        // Assign mACT54RhContext
        mActContext = mCallbackContext.getBLERequestHandler().getContext();
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
               
        callSetTherapyState();
    }

    private void callSetTherapyState()
    {
        // BLE_SEQ86 set therapy control state to stop
        BLERequestParameter paramTherapyState = new BLERequestParameter();
        
        byte[] data = {(byte) OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE, (OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE >> 8),TherapyControlState.STOP};
        
        paramTherapyState.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
        
        mCallbackContext.getBLERequestHandler().addRequest(
                SetTherapyState.getInstance(mActContext), paramTherapyState, mTherapyStateDone);
        
        mBLECommand = ControlPointConstant.OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mCallbackContext.getBLERequestHandler().startHandleRequest();
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
        
        case ControlPointConstant.OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE:
        	Debug.printD(TAG, "[Retry CMD] IDSCMD_SET_THERAPY_CONTROL_STATE E2E:" + mE2ERetryCount
                    + "TO:" + mProcedureRetryCount);
        	callSetTherapyState();
        	break; 
        	
        case ControlPointConstant.OpCode.SOLOM_SET_CONFIG_BLOCK1:
            
            Debug.printD(TAG, "[Retry CMD] Set Config block 1 E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callSetConfigBlockOne();
            
            break; 
            
        case ControlPointConstant.OpCode.SOLOM_SET_CONFIG_BLOCK2:
            
            Debug.printD(TAG, "[Retry CMD] Set Config block 2 E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callSetConfigBlockTwo();
            
            break;

   
        default:
            
            // Apply to the coding standard
            
            break;
        }
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
    	Debug.printD(TAG, "[handleE2EError] "); 
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
        mCallbackContext.getBLERequestHandler().addRequest(
                            ResetConnection.getInstance(mActContext), 
                            null, mResponseResetConnection);
        
        mCmdState = CMD_RESET_RETRY;
      
        // Start Request
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }
    
    /**
     * Call command: call SetConfigBlockOne
     *          
     * @return None
     * 
     */    
    
    protected void callSetConfigBlockOne()
    {
        Debug.printD(TAG, "[Call CMD] callSetConfigBlockOne ");
        
        byte[] configDataBlock1 = {0x74,0x21,0x33,0x0C,0x00,0x14,0x00,0x28,0x05,0x0C,0x00,0x33,0x33,0x02,0x00,0x0f};
        BLERequestParameter paramConfigBlockOne = new BLERequestParameter();
        paramConfigBlockOne.setData(new SafetyByteArray(configDataBlock1,CRCTool.generateCRC16(configDataBlock1)));
        mCallbackContext.getBLERequestHandler().addRequest(
              SetConfigBlock.getInstance(mActContext), paramConfigBlockOne, mResponseSetConfigBlockOne);
        
        // add callSetConfigBlockOne request in list

        mBLECommand = ControlPointConstant.OpCode.SOLOM_SET_CONFIG_BLOCK1;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }
    
    /**
     * Call command: call SetConfigBlockTwo
     *          
     * @return None
     * 
     */    
    
    protected void callSetConfigBlockTwo()
    {
        Debug.printD(TAG, "[Call CMD] Set Date Time ");
        
        byte[] configDataBlock2= {0x7B,0x21,0x14,0x00,0x0f,(byte) 0xC4,0x09,(byte) 0xC4,0x09,0x33};
        BLERequestParameter paramConfigBlock2 = new BLERequestParameter();      
        paramConfigBlock2.setData(new SafetyByteArray(configDataBlock2, CRCTool.generateCRC16(configDataBlock2)));
        mCallbackContext.getBLERequestHandler().addRequest(
                SetConfigBlock.getInstance(mActContext), paramConfigBlock2, mResponseSetConfigBlockTwo);
        
        mBLECommand = ControlPointConstant.OpCode.SOLOM_SET_CONFIG_BLOCK2;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }
    
    
    /**
     * Handle operation: Set Config Block One
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
    
    protected void handleSetTherapyState(SafetyByteArray sbData)
    {    
    	Debug.printD(TAG, " handleSetTherapyState " );
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
        
        Debug.printD(TAG, " OpCode = " + OpCode.IDSCMD_RESP);
        Debug.printD(TAG, " OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE = " + OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE);
        Debug.printD(TAG, " ResponseCode.SUCCESS = " + ResponseCode.SUCCESS);
        
        boolean isOpCodeOK = OpCode.IDSCMD_RESP == iOpCode;
        boolean isRequestOpCodeOK = OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE == iRequestOpCode;
        boolean isResponseSuccess = ResponseCode.SUCCESS == iResponseValue;
      
        if ( isOpCodeOK && isRequestOpCodeOK && isResponseSuccess)
        {
            Debug.printD(TAG, "[SetTherapyState Request Done ] "); 
            mE2ERetryCount = 0;
            mProcedureRetryCount = 0;
            callSetDateTime();
        }
        else
        {
            Debug.printD(TAG, "[SetTherapyState Request failed ] ");
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleSetTherapyState NG ");
            
            setupReturnResult(SafetyBoolean.FALSE);
        }
           
    }
    
    /**
     * Handle operation: Set Config Block One
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
    
    protected void handleSetConfigBlockOne(SafetyByteArray sbData)
    {    
    	Debug.printD(TAG, " handleSetConfigBlockOne " );
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
        Debug.printD(TAG, " OpCode.SOLOM_SET_CONFIG_BLOCK1= " + OpCode.SOLOM_SET_CONFIG_BLOCK1);
        Debug.printD(TAG, " ResponseCode.SUCCESS = " + ResponseCode.SUCCESS);
        
        boolean isOpCodeOK = OpCode.SOLOM_CP_RESP == iOpCode;
        boolean isRequestOpCodeOK = OpCode.SOLOM_SET_CONFIG_BLOCK1 == iRequestOpCode;
        boolean isResponseSuccess = ResponseCode.SUCCESS == iResponseValue;
      
        if ( isOpCodeOK && isRequestOpCodeOK && isResponseSuccess)
        {
            Debug.printD(TAG, "[SetConfigBlockOne Request Done ] "); 
            mE2ERetryCount = 0;
            mProcedureRetryCount = 0;
            callSetConfigBlockTwo();
        }
        else
        {
            Debug.printD(TAG, "[SetConfigBlockOne Request failed ] ");
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleSetConfigBlockOne NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);
        }
           
    }
    
    /**
     * Handle operation: Set Config Block Two
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
    
    protected void handleSetConfigBlockTwo(SafetyByteArray sbData)
    {   
    	Debug.printD(TAG, " handleSetConfigBlockTwo " );
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
        Debug.printD(TAG, " OpCode.SOLOM_SET_CONFIG_BLOCK2= " + OpCode.SOLOM_SET_CONFIG_BLOCK2);
        Debug.printD(TAG, " ResponseCode.SUCCESS = " + ResponseCode.SUCCESS);
        
        boolean isOpCodeOK = OpCode.SOLOM_CP_RESP == iOpCode;
        boolean isRequestOpCodeOK = OpCode.SOLOM_SET_CONFIG_BLOCK2 == iRequestOpCode;
        boolean isResponseSuccess = ResponseCode.SUCCESS == iResponseValue;
      
        if ( isOpCodeOK && isRequestOpCodeOK && isResponseSuccess)
        {
            Debug.printD(TAG, "[SetConfigBlock2 Request Done ] "); 
            mE2ERetryCount = 0;
            mProcedureRetryCount = 0;
            Debug.printD(TAG, "[Call BLE ACT71 ] ");
            BLEController.getInstance(mActContext).BLEACT71(mACT71Done);
        }
        else
        {
            Debug.printD(TAG, "[SetConfigBlock2 Request failed ] ");
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleSetConfigBlock2 NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);
        }
           
    }
    
    /**
     * Call command: call confirm IDD Device status
     *          
     * @return None
     * 
     */    
    protected void callSyncConfigConfirm()
    {
        Debug.printD(TAG, "[Call CMD] Date Time Confirm ");

        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setCommandCode(CommsConstant.CommandCode.BT_SOLOM_CONFIG_CFM);
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
    	Debug.printD(TAG, " handleSetDateTime " );
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
            mE2ERetryCount = 0;
            mProcedureRetryCount = 0;
            callSetConfigBlockOne();
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
    
    private class ACT71Done implements ResponseCallback
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
            boolean isResultFailed = (SafetyBoolean.FALSE.getByte() == result.getByte());
            
            if ( isResultFailed )
            {
                Debug.printD(TAG, "[ACT 71 Two Request failed ] "); 
            }
            else
            {
                Debug.printD(TAG, "[ACT 71 Request Done ] ");  
                 
                callSyncConfigConfirm();
                
//                BLERequestParameter confirmSoloMConfigParameter = new BLERequestParameter();
//                confirmSoloMConfigParameter.setCommandCode(CommsConstant.CommandCode.BT_SOLOM_CONFIG_CFM);
//                mCallbackContext.getBLERequestHandler().addRequest(
//                        GeneralConfirmation.getInstance(mActContext), confirmSoloMConfigParameter, mStateDone);
                // start to execute  
                mCallbackContext.getBLERequestHandler().startHandleRequest();
            }
        }
    }
       
    private class TherapyStateDone implements ResponseCallbackWithData
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
            
                handleSetTherapyState(response.getData());
            }

        }  
    }
    
    private void callSetDateTime()
    {
        // BLE_SEQ87 set date and time
        byte[] dateTimeOfRC = {0,0,0,0,0,0,0,0,0};
        try
        {
            BLERequestParameter paramDateTime = new BLERequestParameter();
            dateTimeOfRC =  CustJavaFrameworkManager.getTimeManagementService(mCallbackContext.getBLERequestHandler().getContext()).verifyPumpTime(dateTimeOfRC);
            ByteArrayBuffer newBuffer = new ByteArrayBuffer(0);
            
            newBuffer.append(OpCode.SOLOM_SET_DATE_AND_TIME);
            newBuffer.append(OpCode.SOLOM_SET_DATE_AND_TIME >> 8);
            newBuffer.append(dateTimeOfRC, 0, dateTimeOfRC.length);
            Debug.printD(TAG, "dateTime length = " + newBuffer.length());
            Debug.dumpPayload(TAG, newBuffer.toByteArray());
            paramDateTime.setData(new SafetyByteArray(newBuffer.toByteArray(), CRCTool.generateCRC16(newBuffer.toByteArray())));
            mCallbackContext.getBLERequestHandler().addRequest(
                    SetDateTime.getInstance(mActContext), paramDateTime, mResponseSetDateTime);
        }
        catch (RemoteException e)
        {
            
            e.printStackTrace();
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
    
    
    private class ResponseSetConfigBlockOne implements ResponseCallbackWithData
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
            
                handleSetConfigBlockOne(response.getData());
            }

        }

        
    }
    
    private class ResponseSetConfigBlockTwo implements ResponseCallbackWithData
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
            
                handleSetConfigBlockTwo(response.getData());
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
        	boolean isResult = 
                    (SafetyBoolean.TRUE.getByte() == result.getByte());
            
            
            if ( isResult )
            {
            	Debug.printD(TAG, "[ACT 53 GeneralConfirmation Request Done ] ");
            }
            else
            {
            	Debug.printD(TAG, "[ACT 53 GeneralConfirmation Request failed  ] ");  
            }
            setupReturnResult(result);
        }
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