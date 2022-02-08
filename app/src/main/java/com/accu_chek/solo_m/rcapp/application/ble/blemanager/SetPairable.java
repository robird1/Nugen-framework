/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetPairable
 * Brief: This class shall handle the Pair-able setting and its responses sequence.
 *
 * Create Date: 2015/7/23
 * $Revision: 25022 $
 * $Author: IvanHuang $
 * $Id: SetPairable.java 25022 2015-11-27 08:48:03Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.PairableRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CauseOnlyResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class shall handle the Pair-able setting and its responses sequence.
*/
public class SetPairable implements IBLERequest
{
    private static final String TAG = "SetPairable";
    
    /**
     * The instance of SetPairable class
     */
    private static volatile SetPairable mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */ 
    private ResponseCallback mCallback = null;
    
    /**
     * This method gets the one and only instance of the class SetPairable.
     *
     * @param context: The Context 
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class
     *         SetPairable
     *         Range: A valid object of SetPairable
     *         Unit: SetPairable
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetPairable
     */
    public static SetPairable getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetPairable(context);
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
    protected SetPairable(Context context)
    {
        mContext = context;
    }
    
    /**
     * This method gets the cause of the response to check if the pair setting is successful or not.
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
        
        Debug.printD(TAG, "[SetPairable]: Response enter ");
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CauseOnlyResponse response = (CauseOnlyResponse) pack.getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int cause = response.getCause().get();

        int command = response.getCommand().get();
        
        if ( (0 == cause) && (CommandCode.BT_PAIRABLE == command) )
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
     * This method handles the Pair-able request.
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
        Debug.printD(TAG, "[SetPairable]: Request enter ");
        
        mCallback = callback;

        PairableRequest request = (PairableRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_PAIRABLE);
        
        if ( null != request )
        {
            request.setMode(SafetyBoolean.TRUE);
            
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.BT_PAIRABLE, this);
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
