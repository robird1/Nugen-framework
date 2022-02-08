/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.GeneralConfirmation
 * Brief: This class handles GeneralConfirmation request.
 *
 * Create Date: 2015/8/10
 * $Revision: 20558 $
 * $Author: KiddYeh $
 * $Id: SetSystemSync.java 20558 2015-10-01 14:02:41Z KiddYeh $
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
import com.accu_chek.solo_m.rcapp.application.ble.response.CommandDataSentIndication;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;


public class GeneralConfirmation implements IBLERequest
{
    private static final String TAG = "GeneralConfirmation";
    
    /**
     * The instance of GeneralConfirmation class
     */
    private static volatile GeneralConfirmation mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * This method shall get the one and only instance of the class GeneralConfirmation.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class GeneralConfirmation
     *         Range: A valid object of ConfirmTimeSync
     *         Unit: ConfirmTimeSync
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetTimeStamp
     */
    public static GeneralConfirmation getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new GeneralConfirmation(context);
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
    private GeneralConfirmation(Context context)
    {
        mContext = context;
    }

    /**
     * This method shall get the cause of response to check if the GeneralConfirmation is successful or not.
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
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
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
    private void returnResult(SafetyBoolean isResult)
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
     * This method handles the GeneralConfirmation request. 
     * 
     * @param parameter [in] the request parameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response Callback.     
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
        Debug.printD(TAG, "[GeneralConfirmation]: Request enter " + parameter.getCommandCode() );

        mCallback = callback;
        
        int commandCode = parameter.getCommandCode();
        
        BlankMessageRequest request = (BlankMessageRequest) RequestPayloadFactory
                .getRequestPayload(commandCode);
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        if ( null != request )
        {            
            BLEController.sendRequestToComms(mContext, request);
            isResult = SafetyBoolean.TRUE;        
        }
        else
        {
            // Apply to the coding standard
        }
        returnResult(isResult);
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */