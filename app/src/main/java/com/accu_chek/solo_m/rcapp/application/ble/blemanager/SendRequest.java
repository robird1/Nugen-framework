/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.blemanager.SendRequest
 * Brief: This class handles BLE request and its response.
 *
 * Create Date: 2015/10/27
 * $Revision: 22446 $
 * $Author: KiddYeh $
 * $Id: ConfirmWatchDogChallenge.java 22446 2015-10-23 09:03:35Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommandDataSentIndication;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SendRequest implements IBLERequest
{
    private static final String TAG = "SendRequest";
    
    /**
     * The instance of SendRequest class
     */
    private static volatile SendRequest mInstance = null;
    
    /**
     * The application class
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    
    /**
     * Get the one and only instance of the class SendRequest.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SendRequest
     *         Range: A valid object of SendRequest
     *         Unit: SendRequest
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SendRequest
     */
    public static SendRequest getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SendRequest(context);
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
    protected SendRequest(Context context)
    {
        mContext = context;
    }
    
    
    /**
     * This method gets the cause of response to check if the SendRequest is successful or not. 
     * It returns the result of response via callback function.
     * This method is called after receiving the certain broadcast.
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
     *            
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
        Debug.printD(TAG,"[SendRequest]: Command Data Sent Indication ");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CommandDataSentIndication response = 
                (CommandDataSentIndication) pack.getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int result = response.getResult().get();
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        if (CommsConstant.Result.RESULT_OK == result) 
        {
            isResult = SafetyBoolean.TRUE;
        }
        else
        {
            // Apply to the coding standard
        }
        
        returnResult(isResult);
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
    protected void returnResult(SafetyBoolean isResult)
    {
        if( null != mCallback )
        {
            mCallback.onRequestCompleted(isResult);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * This method handles the request. 
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the callback of response    
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
        Debug.printD(TAG,"[SendRequest] enter");      
        
        mCallback = callback;
        
        IRequest request = parameter.getRequest();
       
        if (null != request )
        {     
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.COMM_DATA_SENT, this);
            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            returnResult(SafetyBoolean.FALSE);
        } 
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */