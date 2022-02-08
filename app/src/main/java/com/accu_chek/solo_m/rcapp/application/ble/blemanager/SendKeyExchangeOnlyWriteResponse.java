/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.blemanager.SendKeyExchangeOnlyWriteResponse
 * Brief: This class handles the SendKeyExchangeOnlyWriteResponse process and its responses sequence.
 *
 * Create Date: 2015/7/22
 * $Revision: 23935 $
 * $Author: KiddYeh $
 * $Id: SendKeyExchangeOnlyWriteResponse.java 23935 2015-11-12 06:17:00Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeWriteResponse;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SendKeyExchangeOnlyWriteResponse implements IBLERequest
{
    private static final String TAG = "SendKeyExchangeOnlyWriteResponse";
    
    
    /**
     *  The instance of SendKeyExchangeOnlyWriteResponse class
     */
    private static volatile SendKeyExchangeOnlyWriteResponse mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    
    /**
     * Get the one and only instance of the class SendKeyExchangeOnlyWriteResponse.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1 
     * 
     * @return mInstance : the one and only instance of the class SendKeyExchangeOnlyWriteResponse
     *         Range: A valid object of SendKeyExchangeOnlyWriteResponse
     *         Unit: SendKeyExchangeOnlyWriteResponse
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SendKeyExchangeOnlyWriteResponse
     */
    public static SendKeyExchangeOnlyWriteResponse getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SendKeyExchangeOnlyWriteResponse(context);
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
    private SendKeyExchangeOnlyWriteResponse(Context context)
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
      
        Debug.printD(TAG, "[SendKeyExchangeOnlyWriteResponse]: WriteResponse");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        AttributeWriteResponse response = (AttributeWriteResponse) pack.getResponse();

        int cause = response.getCause().get();
        
        if(0 != cause)
        {   
            setupReturnResult(SafetyBoolean.FALSE, null);
            Debug.printD(TAG, "[SendKeyExchangeOnlyWriteResponse]: WriteResponse ==> failed ");
        }
        else
        {
        	setupReturnResult(SafetyBoolean.TRUE, null);
            Debug.printD(TAG, "[SendKeyExchangeOnlyWriteResponse]: WriteResponse ==> OK ");
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
            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            mCallback.onRequestCompleted(SafetyBoolean.FALSE);
        }
        
    }
    


}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
