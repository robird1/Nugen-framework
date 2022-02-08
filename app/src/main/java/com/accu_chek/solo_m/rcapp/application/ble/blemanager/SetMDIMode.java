/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetMDIMode
 * Brief: This class handles SetMDIMode request and its response.
 *
 * Create Date: 2015/7/30
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: SetMDIMode.java 25071 2015-11-30 03:09:48Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.CommandCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.request.BGModeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CauseOnlyResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles SetMDIMode request and its response.
*/
public class SetMDIMode implements IBLERequest
{
    private static final String TAG = "SetMDIMode";
    
    /**
     * The instance of SetMDIMode class
     */
    private static volatile SetMDIMode mInstance = null;
    
    /**
     * The application context 
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * This method gets the one and only instance of the class SetMDIMode.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SetMDIMode
     *         Range: A valid object of SetMDIMode
     *         Unit: SetMDIMode
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetMDIMode
     */
    public static SetMDIMode getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetMDIMode(context);
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
    protected SetMDIMode(Context context)
    {
        mContext = context.getApplicationContext();
    }
    
    /**
     * This method is called after receiving the BGModeResponse broadcast.
     * It gets the cause of the response to check the SetMDIMode is success or not.
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
     *            
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {  
        Debug.printD(TAG,"[SetMDIMode]: Response enter ");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CauseOnlyResponse response = (CauseOnlyResponse) pack.getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int cause = response.getCause().get();
        int command = response.getCommand().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        if ((0 == cause) && (CommandCode.COMM_BG_MODE == command) )
        {
            Debug.printD(TAG,"[SetMDIMode]: TRUE ");
            isResult = SafetyBoolean.TRUE;
            
        }
        else
        {
            Debug.printD(TAG,"[SetMDIMode]: FALSE ");
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
     * This method handles the SetMDIMode request. 
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the callback function of response  
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
        Debug.printD(TAG,"[setMDIMode]: Request enter " );
        mCallback = callback;

        BGModeRequest request = (BGModeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_BG_MODE);
        
        if (null != request)
        {
            request.setMode(parameter.getIsEnable());
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.COMM_BG_MODE, this);
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
// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
