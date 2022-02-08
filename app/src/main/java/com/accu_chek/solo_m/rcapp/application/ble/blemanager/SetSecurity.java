/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetSecurity
 * Brief: This class shall handle the set Security and its responses sequence.
 *
 * Create Date: 2015/7/23
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: SetSecurity.java 25071 2015-11-30 03:09:48Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.CommandCode;
import com.accu_chek.solo_m.rcapp.application.ble.request.SecurityRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.OOBRequestIndication;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;


/**
 * This class shall handle the set Security and its responses sequence.
 */
public class SetSecurity implements IBLERequest
{
    private final static String TAG = "SetSecurity";
    
    /**
     * The instance of SetSecurity class
     */
    private static volatile SetSecurity mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * This method gets the one and only instance of the class SetSecurity.
     *
     * * @param context: The Context in which the receiver is running.
     *            Range: A valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SetSecurity
     *         Range: A valid object of SetSecurity
     *         Unit: SetSecurity
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetSecurity
     */
    public static SetSecurity getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetSecurity(context);
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
     * @param context: the application context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return void [out] 
     *   
     * @see mContext            
     */
    protected SetSecurity(Context context)
    {
        mContext = context;
    }
    
     
    /**
     * This method gets the CommandCode of response to check if the setting security is successful or not.
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
   
        Debug.printD(TAG,"[SetSecurity]: Response enter ");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
        
        OOBRequestIndication response = 
                (OOBRequestIndication) pack.getResponse(); 

        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int command =  response.getCommand().get();

        SafetyBoolean isResult = SafetyBoolean.FALSE;
       
        if (CommandCode.BT_OOB_IND == command)
        {
            isResult = SafetyBoolean.TRUE;

            Debug.printD(TAG,"[SetSecurity]: OK ");
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
     * This method handles the Security request.
     * 
     * @param parameter: the parameter of request
     *            Range:a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1        
     * @param callback [in] the callback function of response.     
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
        Debug.printD(TAG,"[SetSecurity]: Request enter ");
        SecurityRequest request = (SecurityRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_SECURITY);
        
        mCallback = callback;
        
        if (null != request)
        {
            request.setRemoteBD(GlobalTools.MPR.getMpAddress());
            
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.BT_OOB_IND, 
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
// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
