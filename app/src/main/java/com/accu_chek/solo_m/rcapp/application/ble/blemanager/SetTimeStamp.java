/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetTimeStamp
 * Brief: This class handles SetTimeStamp request.
 *
 * Create Date: 2015/8/10
 * $Revision: 20558 $
 * $Author: KiddYeh $
 * $Id: SetSystemSync.java 20558 2015-10-01 14:02:41Z KiddYeh $
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


public class SetTimeStamp implements IBLERequest
{
    private static final String TAG = "SetTimeStamp";
    
    /**
     * The instance of SetTimeStamp class
     */
    private static volatile SetTimeStamp mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * This method gets the one and only instance of the class SetTimeStamp.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SetTimeStamp
     *         Range: A valid object of SetTimeStamp
     *         Unit: SetTimeStamp
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetTimeStamp
     */
    public static SetTimeStamp getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetTimeStamp(context);
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
    protected SetTimeStamp(Context context)
    {
        mContext = context;
    }

    /**
     * This method gets the result of response to check if the SetTimeStamp is successful or not.
     * Return the result of response via callback function.
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

        Debug.printD(TAG, "[SetTimeStamp]: Response enter ");
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CommandDataSentIndication response = (CommandDataSentIndication) pack.getResponse();

        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int result = response.getResult().get();

        if ((CommsConstant.Result.RESULT_OK == result))
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
     * This method handles the SetSystemSync request. 
     * 
     * @param parameter [in] BLERequestParameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] ResponseCallback.     
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
        Debug.printD(TAG, "[SetTimeStamp]: Request enter ");

        mCallback = callback;
        
        IRequest request = parameter.getRequest();
        
        if ( null != request )
        {            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                    .registerReceiver(ResponseAction.CommandResponse.BT_SET_TIME_STAMP ,this);
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