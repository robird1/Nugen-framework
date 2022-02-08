/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ccom.accu_chek.solo_m.rcapp.application.ble.blemanager.SetConfigBlock
 * Brief: This class handles SetConfigBlock request and its response.
 *
 * Create Date: 2015/10/13
 * $Revision: 21344 $
 * $Author: KiddYeh $
 * $Id: SetConfigBlock.java 21344 2015-10-12 12:12:13Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeWriteResponse;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SetConfigBlock implements IBLERequest
{
    private static final String TAG = "SetConfigBlock";
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF;
    
    /**
     * The instance of SetConfigBlock class
     */
    private static volatile SetConfigBlock mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    

    /**
     *  The AttrNotification response
     */
    private AttrNotification mAttrNotification = new AttrNotification();
    
    
    /**
     * This method gets the one and only instance of the class SetConfigBlock.
     * 
     * 
     * @return mInstance : the one and only instance of the class SetConfigBlock
     *         Range: A valid object of SetConfigBlock
     *         Unit: SetConfigBlock
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetConfigBlock
     */
    public static SetConfigBlock getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetConfigBlock(context);
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
    protected SetConfigBlock(Context context)
    {
        mContext = context;
    }

    
    /**
     * This method shall get the cause of response to check if the SetConfigBlock is successful or not.
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
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {

        Debug.printD(TAG, "[SetConfigBlock]: Write Response enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
  
        
        AttributeWriteResponse response = 
                (AttributeWriteResponse) pack.getResponse();
        
        
        
        int cause = response.getCause().get();
        
        Debug.printD(TAG, "cause =" + cause);
        Debug.printD(TAG, "subcause =" + response.getSubCause().get());
        Debug.printD(TAG, "command =" + response.getCommand().get());
        
        if ( 0 == cause )
        {  
  
        }
        else
        {
            BLEResponseReceiver.getInstance(context).unregisterReceiver();
            setupReturnResult(SafetyBoolean.FALSE,null);
        }
           

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
    protected void setupReturnResult(SafetyBoolean isResult, ResponsePack pack)
    {
        // Unregister Receiver
        BLEResponseReceiver.getInstance(mContext).unregisterReceiver();
        
        if(null != mCallback)
        {
            ((ResponseCallbackWithData) mCallback).onRequestCompleted(isResult, pack);
        }
        else
        {
            
        }
    }
    
    /**
     * This method handles the SetConfigBlock request. 
     * 
     * @param parameter [in] the request parameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the callback function     
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
        Debug.printD(TAG, "[SetConfigBlock]: Request enter ");
        
        mCallback = callback;
        
        byte[] data = parameter.getData().getByteArray();
        
        AttributeWriteTypeRequest request = (AttributeWriteTypeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);

        if ( null != request )
        {
            Debug.printD(TAG, "[SetConfigBlock]: start ");
            
            SafetyByteArray bdAddress = new SafetyByteArray( BLEController.getBDAddress(mContext),
                  CRCTool.generateCRC16( BLEController.getBDAddress(mContext)));
            
            request.setRemoteBDAddress(bdAddress);

            request.setWriteType(new SafetyNumber<Integer>(
                    BlueConstant.WriteType.REQUEST,
                    -BlueConstant.WriteType.REQUEST));

            request.setAttributeHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.BLANK_HANDLE,
                    -BlueConstant.HANDLE.BLANK_HANDLE));

            request.setUUID16(new SafetyNumber<Integer>(
                    UUID.UUID16.SOLOM_CP, -UUID.UUID16.SOLOM_CP));

            request.setAttributeLength(new SafetyNumber<Integer>(data.length,
                    -data.length));

            request.setWriteOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));

            request.setData(new SafetyByteArray(data, CRCTool
                    .generateCRC16(data)));
            
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_WRITE, this);
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND ,mAttrNotification);
            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            setupReturnResult(SafetyBoolean.FALSE,null);
        }
        
    }
    
    
    class AttrNotification implements ResponseProcess
    {
        
        /**
         * This method gets the E2E-Result of the indication 
         * to check if the SetDateTimeCP Opcode is successful or not.
         * It returns the result of response and response pack via callback function.
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
         */
        @Override
        public void doProcess(Context context, Intent intent)
        {
            Debug.printD(TAG, " [SetConfigBlock]: AttrNotification ");
            
            // OpCode
            int iOpCode = 0; 
            int iCommandCode = 0;
          
            // Command Result
            SafetyBoolean isResult = SafetyBoolean.FALSE;
            
            // Response Pack
            ResponsePack attPack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
            
            // Response
            AttributeChangeNotification response = (AttributeChangeNotification) attPack.getResponse();
            
            // Indication Data
            SafetyByteArray sbData = response.getData();
            byte[] bData = sbData.getByteArray();
            ByteBuffer bbData = ByteBuffer.wrap(bData);
            
            // Get OpCode
            bbData.rewind();
            bbData.order(ByteOrder.LITTLE_ENDIAN);
            iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;

            Debug.printD(TAG, " Opcode: " + iOpCode);
            
            // Operation corresponded with OpCode
            switch(iOpCode)
            {
  
            case ControlPointConstant.OpCode.SOLOM_CP_RESP:
                
                // Get Command Code
                iCommandCode = bbData.getShort() & TWO_BYTES_OPERAND;
                
                boolean isCommandOK = (iCommandCode == ControlPointConstant.OpCode.SOLOM_SET_CONFIG_BLOCK1)
                        || (iCommandCode == ControlPointConstant.OpCode.SOLOM_SET_CONFIG_BLOCK2);
                if (isCommandOK)
                {
                    Debug.printD(TAG, " [SetDateTime]: done");
                    isResult = SafetyBoolean.TRUE;
                    setupReturnResult(isResult, attPack);
                }
                
                break;
                
            default:
                
                // Apply to the coding standard
                
                break;
            }

//            ResponsePack pack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
//            
//            AttributeChangeNotification attResponse = (AttributeChangeNotification) pack.getResponse();
//            
//            SafetyBoolean isResult = SafetyBoolean.FALSE;
//            
//            BLEResponseReceiver.getInstance(context).unregisterReceiver();
//            
//            Debug.printD(TAG, "data");
//            Debug.dumpPayload(TAG, attResponse.getData().getByteArray());
//            Debug.printD(TAG, "result" + attResponse.getResult().get());
//            
//            if (E2E_Result.RESULT_OK == attResponse.getResult().get())
//            {
//                ControlPointResponsePack cpPack = BLEController.getInstance(context).parseToControlPoint(pack);
//                ControlPointCodeResponse cpresponse = (ControlPointCodeResponse) cpPack.getControlPointResponse(); 
//                
//                boolean isOpCodeOK = (OpCode.SOLOM_CP_RESP == cpresponse.getOpCode());
//                
//                boolean isRequestOpCodeOK = (OpCode.SOLOM_SET_CONFIG_BLOCK1== cpresponse.getRequestOpCode() 
//                        || OpCode.SOLOM_SET_CONFIG_BLOCK2== cpresponse.getRequestOpCode());
//                
//                boolean isResponseOK = (ResponseCode.SUCCESS == cpresponse.getResponseCode());
//                
//                if (  isOpCodeOK && isRequestOpCodeOK && isResponseOK)
//                {
//                    isResult = SafetyBoolean.TRUE;
//                    Debug.printD(TAG, " [SetConfigBlock]: AttrNotification E2E_Result.RESULT_OK && Response OK");
//                }
//                else
//                {
//                    Debug.printD(TAG, " [SetConfigBlock]: AttrNotification E2E_Result.RESULT_OK but Response NNNNNOK");
//                }
//            }
//            else
//            {
//                Debug.printD(TAG, " [SetConfigBlock]: AttrNotification E2E_Result.RESULT_NNNNNNNNNNNNNNNNNNNNNOK");
//            }
//            
//            returnResult(isResult);
            
        }
        
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
