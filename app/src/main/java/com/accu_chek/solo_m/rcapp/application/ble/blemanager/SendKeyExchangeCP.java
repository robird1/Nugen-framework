/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.blemanager.SendKeyExchangeCP
 * Brief: This class handles the SendKeyExchangeCP process and its responses sequence.
 *
 * Create Date: 2015/7/22
 * $Revision: 23935 $
 * $Author: KiddYeh $
 * $Id: SendKeyExchangeCP.java 23935 2015-11-12 06:17:00Z KiddYeh $
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
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SendKeyExchangeCP implements IBLERequest
{
    private static final String TAG = "SendKeyExchangeCP";
    
    
    /**
     *  The instance of SendKeyExchangeCP class
     */
    private static volatile SendKeyExchangeCP mInstance = null;
    
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
     * Get the one and only instance of the class SendKeyExchangeCP.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1 
     * 
     * @return mInstance : the one and only instance of the class SendKeyExchangeCP
     *         Range: A valid object of SendKeyExchangeCP
     *         Unit: SendKeyExchangeCP
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class ConfirmOOB
     */
    public static SendKeyExchangeCP getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SendKeyExchangeCP(context);
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
     */
    private SendKeyExchangeCP(Context context)
    {
        mContext = context;
    }
    
    
    /**
     * This method is called after receiving the broadcast of 
     * Return the result of response via callback function.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: A valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {     
      
        Debug.printD(TAG, "[SendKeyExchangeCP]: WriteResponse");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        AttributeWriteResponse response = (AttributeWriteResponse) pack.getResponse();

        int cause = response.getCause().get();
        
        if(0 != cause)
        {   
            setupReturnResult(SafetyBoolean.FALSE, null);
            Debug.printD(TAG, "[SendKeyExchangeCP]: WriteResponse ==> failed ");
        }
        else
        {
            Debug.printD(TAG, "[SendKeyExchangeCP]: WriteResponse ==> OK ");
            // Apply to the coding standard    
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
    private void setupReturnResult(SafetyBoolean isResult, ResponsePack pack)
    {
//        // Unregister Receiver
//        BLEResponseReceiver.getInstance(mContext).unregisterReceiver();
        
        if(null != mCallback)
        {
        	Debug.printD(TAG, "[SendKeyExchangeCP]: callback enter ");
            ((ResponseCallbackWithData) mCallback).onRequestCompleted(isResult, pack);
        }
        else
        {
            
        }
    }
    
    /**
     * This method handles the SendKeyExchangeCP request. 
     * 
     * @param parameter [in] the request parameter
     *            Range: A valid object of BLERequestParameter or null object
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response callback       
     *            Range: A valid object of ResponseCallback or null object
     *            Unit: ResponseCallback
     *            Scaling: 1   
     *                      
     * @return void [out] 
     * 
     * @see mContext
     * @see mCallback
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[SendKeyExchangeCP]: Request enter ");
        
        AttributeWriteTypeRequest request = (AttributeWriteTypeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);

        
        byte[] data = parameter.getData().getByteArray();
        
        mCallback = callback;
        
        if ( null != request )
        {
            request.setRemoteBDAddress(GlobalTools.MPR.getMpAddress());

            request.setWriteType(new SafetyNumber<Integer>(
                    BlueConstant.WriteType.REQUEST,
                    -BlueConstant.WriteType.REQUEST));

            request.setAttributeHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.BLANK_HANDLE,
                    -BlueConstant.HANDLE.BLANK_HANDLE));

            request.setUUID16(new SafetyNumber<Integer>(
                    UUID.UUID16.KEY_EXCHANGE_CP, -UUID.UUID16.KEY_EXCHANGE_CP));

            request.setAttributeLength(new SafetyNumber<Integer>(data.length,
                    -data.length));

            request.setWriteOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));

            request.setData(new SafetyByteArray(data, CRCTool
                    .generateCRC16(data)));

            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_WRITE,this);
              
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND ,mAttrNotification);
            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            mCallback.onRequestCompleted(SafetyBoolean.FALSE);
        }
        
    }
    
    class AttrNotification implements ResponseProcess
    {
        /**
         * This method gets the E2E-Result of the indication 
         * to check if the GetDateTimeCP Opcode is successful or not.
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
            Debug.printD(TAG, "[SendKeyExchangeCP]: Response enter ==> AttrNotification");
            
            // OpCode
            int iOpCode = -1; 
            
            // Command Result
            SafetyBoolean isResult = SafetyBoolean.FALSE;
            
            // Response Pack
            ResponsePack attPack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
            
            // Response
            AttributeChangeNotification response = (AttributeChangeNotification) attPack.getResponse();
            
            //UUID 
            boolean isUUIDOK = UUID.UUID16.KEY_EXCHANGE_CP == (response.getAttribHandle().get() & 0xFFFF);
            
			if (isUUIDOK)
            {
				Debug.printD(TAG, " UUID.UUID16.KEY_EXCHANGE_CP ");
				// Indication Data
	            SafetyByteArray sbData = response.getData();
	            byte[] bData = sbData.getByteArray();
	            ByteBuffer bbData = ByteBuffer.wrap(bData);
	            
	            // Get OpCode
	            bbData.rewind();
	            bbData.order(ByteOrder.LITTLE_ENDIAN);
	            iOpCode = bbData.get();

	            Debug.printD(TAG, " Opcode: " + iOpCode);
	            
	            // Operation corresponded with OpCode
	            switch(iOpCode)
	            {
	            case ControlPointConstant.KESOpCode.VERSION_REQUEST_RESPONSE: 
	            case ControlPointConstant.KESOpCode.M1_DATA:	
	            case ControlPointConstant.KESOpCode.M3_DATA:	
	            case ControlPointConstant.KESOpCode.M5_DATA:	
	            	Debug.printD(TAG, "[SendKeyExchangeCP]: Response enter ==> OCode = " + iOpCode); 
	            	isResult = SafetyBoolean.TRUE;
	                setupReturnResult(isResult, attPack);
	                break;
	              
	            default:
	                
	                // Apply to the coding standard
	                
	                break;
	            }
            }
        }
        
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */