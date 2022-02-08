/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection
 * Brief: his class handles ResetConnection request and its response.
 *
 * Create Date: 2015/10/27
 * $Revision: 22446 $
 * $Author: KiddYeh $
 * $Id: ResetConnection.java 22446 2015-10-23 09:03:35Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.request.BlankMessageRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.ResetConnectionRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.ErrorLogResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.ResetConnectionResponse;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class ResetConnection implements IBLERequest
{
    private static final String TAG = "ResetConnection";
    
    public static final String COMM_ERROR_LOG = "ResetConnection";
    
    /**
     * The instance of ResetConnection class 
     */
    private static volatile ResetConnection mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * This method shall get the one and only instance of the class ResetConnection.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1  
     * 
     * @return mInstance : the one and only instance of the class GetErrorLog
     *         Range: A valid object of GetErrorLog
     *         Unit: GetErrorLog
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class ResetConnection
     */
    public static ResetConnection getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new ResetConnection(context);
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
    protected ResetConnection(Context context)
    {
        mContext = context;
    }

    
    /**
     * This method gets the cause of response to check if the GetErrorLog request is successful or not.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: a valid object of intent
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
        Debug.printD(TAG, "[ResetConnection]: Response enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        ResetConnectionResponse response = (ResetConnectionResponse) pack.getResponse();

        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        
        int result = response.getResult();
        
        Debug.printD(TAG, "[result]:" + result);
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
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
     * This method sets the parameter of the ResetConnection request and sends 
     * it out. 
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response callback     
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1    
     *                     
     * @return void [out] 
     * 
     * @see mCallback 
     * @see mContext
     * 
     */

    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[ResetConnection]: Request enter ");
        
        BlankMessageRequest request = (BlankMessageRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_RESET_CONNECTION);

        mCallback = callback;
        
        if ( null != request )
        {
         
            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                    .registerReceiver(ResponseAction.CommandResponse.BT_RESET_CONNECTION,
                            this);
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