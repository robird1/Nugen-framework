/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.PrePowerOffComms
 * Brief: This class handles PrePowerOffComms request and its response. 
 *
 * Create Date: 2015/7/27
 * $Revision: 25216 $
 * $Author: IvanHuang $
 * $Id: PrePowerOffComms.java 25216 2015-12-01 06:36:25Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.BlankMessageRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CauseOnlyResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.CommsJNI;

/**
* This class handles PrePowerOffComms request and its response.
*/
public class PrePowerOffComms implements IBLERequest
{
    private static final String TAG = "PrePowerOffComms";
    
    /**
     * The instance of PrePowerOffComms class
     */
    private static volatile PrePowerOffComms mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * 
     * 
     * Get the one and only instance of the class PrePowerOffComms.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class PrePowerOffComms
     *         Range: A valid object of PrePowerOffComms
     *         Unit: PrePowerOffComms
     *         Scaling: 1 
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class PrePowerOffComms
     */
    public static PrePowerOffComms getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new PrePowerOffComms(context);
        }
        
        return mInstance;
    }
    
    
    /**
     * The class constructor
     * 
     * @param context: 
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return void [out] 
     * 
     * @see mContext               
     */
    protected PrePowerOffComms(Context context)
    {
        mContext = context;
    }
      
    
    /**
     * This method is called after receiving the certain broadcast.
     * It gets the cause of response to check PrePowerOffComms is success or not.
     * If the callback exists, it return the result of response via callback function.
     * If the cause is zero, it turns off the power of Communication processor. 
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
        Debug.printD(TAG, "[PrePowerOffComms]: Response enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CauseOnlyResponse response = (CauseOnlyResponse) pack.getResponse();
        
        int cause = response.getCause().get();
        int command = response.getCommand().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        if (0 == cause && (CommandCode.COMM_PRE_POWER_OFF == command) )
        {
                 
            CommsJNI.powerOffComms();
            
            isResult = SafetyBoolean.TRUE;
        }
        else
        {
            isResult = SafetyBoolean.FALSE;
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
     * This method handles the prePowerOff request. 
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

        BlankMessageRequest request = (BlankMessageRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_PRE_POWER_OFF);
        
        mCallback = callback;
        
        if ( null != request )
        {
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.COMM_PRE_POWER_OFF, this);
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
