/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.UIStatusUpdate
 * Brief: This class handles UpdateUIState request and its response.
 *
 * Create Date: 2015/8/12
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: UpdateUIState.java 25071 2015-11-30 03:09:48Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.UIStatusUpdateRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommandDataSentIndication;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class UpdateUIState implements IBLERequest
{
    private static final String TAG = "UpdateUIState";
    
    /**
     * The instance of UpdateUIState class
     */
    private static volatile UpdateUIState mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * Get the one and only instance of the class UpdateUIState.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1 
     * 
     * @return mInstance : the one and only instance of the class UpdateUIState
     *         Range: A valid object of UpdateUIState
     *         Unit: UpdateUIState
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class UpdateUIState
     */
    public static UpdateUIState getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new UpdateUIState(context);
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
     */
    protected UpdateUIState(Context context)
    {
        mContext = context;
    }

    /**
     * This method gets the result of response to check if the UpdateUIState is successful or not.
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

        Debug.printD(TAG, "[UpdateUIState]: Response enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        CommandDataSentIndication response = (CommandDataSentIndication) pack.getResponse();

        int result = response.getResult().get();

        BLEResponseReceiver.getInstance(context).unregisterReceiver();

        if (CommsConstant.Result.RESULT_OK == result)
        {
            Debug.printD(TAG, "[UpdateUIState]: done ");
            isResult = SafetyBoolean.TRUE;
        }  
        else
        {
            Debug.printD(TAG, "[UpdateUIState]: failed ");
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
     * This method handles the UpdateUIState request. 
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
        Debug.printD(TAG, "[UIStatusUpdateRequest]: Request enter ");
        mCallback = callback;

        UIStatusUpdateRequest request = (UIStatusUpdateRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_UI_STATE_UPDATE);

        SafetyNumber<Integer> state = new SafetyNumber<Integer>(parameter.getParameter(), -parameter.getParameter());
        
        if ( null != request )
        {
            request.setState(state);

            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                    .registerReceiver(ResponseAction.CommandResponse.COMM_DATA_SENT,
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
