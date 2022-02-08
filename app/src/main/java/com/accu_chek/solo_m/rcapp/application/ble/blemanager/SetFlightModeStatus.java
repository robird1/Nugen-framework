/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetFlightModeStatus
 * Brief: This class handles SetFlightModeStatus request and its response.
 *
 * Create Date: 2015/7/30
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: SetFlightModeStatus.java 25071 2015-11-30 03:09:48Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.Result;
import com.accu_chek.solo_m.rcapp.application.ble.request.FlightModeStatusRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommandDataSentIndication;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles SetFlightModeStatus request and its response.
*/
public class SetFlightModeStatus implements IBLERequest
{
    private static final String TAG = "SetFlightModeStatus";
    
    /**
     * The instance of SetFlightModeStatus class
     */
    private static volatile SetFlightModeStatus mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * This method gets the one and only instance of the class SetFlightModeStatus.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SetFlightModeStatus
     *         Range: a valid object of SetFlightModeStatus
     *         Unit: SetFlightModeStatus
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetFlightModeStatus
     */
    public static SetFlightModeStatus getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetFlightModeStatus(context);
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
    protected SetFlightModeStatus(Context context)
    {
        mContext = context.getApplicationContext();
    }
    
    
    /**
     * This method gets the result of the response to check if the SetFlightModeStatus is successful or not.
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
        Debug.printD(TAG, "[SetFlightModeStatus]: Response enter ");
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CommandDataSentIndication response = (CommandDataSentIndication) pack.getResponse();
        
        int result =  response.getResult().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        
        if (Result.RESULT_OK == result)  
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
     * This method handles the SetFlightModeStatus request. 
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: SafetyByteArray
     *            Scaling: 1
     * @param callback [in] the callback function of response     
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1   
     *                      
     * @return void [out] 
     * 
     * @see mContext  
     *
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[SetFlightModeStatus]: Request enter ");
        
        FlightModeStatusRequest request = (FlightModeStatusRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_FLIGHT_MODE_STATUS);
        
        mCallback = callback;
        
        if (null != request)
        {
            request.setMode(parameter.getIsEnable());
            
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.COMM_FLIGHT_MODE_STATUS, this);
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
