/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetSystemSync
 * Brief: This class handles SetSystemSync request and its response.
 *
 * Create Date: 2015/8/10
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: SetSystemSync.java 25071 2015-11-30 03:09:48Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.BlankMessageRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommandDataSentIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.SystemSyncResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles SetSystemSync request and its response.
*/
public class SetSystemSync implements IBLERequest
{
    private static final String TAG = "SetSystemSync";
    
    /**
     * The instance of SetSystemSync class
     */
    private static volatile SetSystemSync mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * This method gets the one and only instance of the class SetSystemSync.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SetSystemSync
     *         Range: A valid object of SetSystemSync
     *         Unit: SetSystemSync
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetSystemSync
     */
    public static SetSystemSync getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetSystemSync(context);
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
    protected SetSystemSync(Context context)
    {
        mContext = context;
    }

    /**
     * This method gets the result of response to check if SetSystemSync is sent out or not.
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

        Debug.printD(TAG, "[SetSystemSync]: Response enter ");
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CommandDataSentIndication response = (CommandDataSentIndication) pack.getResponse();
  
//        SystemSyncResponse response = (SystemSyncResponse) pack.getResponse();

        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int result = response.getResult().get();
        int command = response.getCommand().get();

//        if ((CommsConstant.Result.RESULT_OK == result) 
//                && (CommandCode.BT_SYSTEM_SYNC == command))
        
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
        
        Debug.printD(TAG, "[setSystemSync]: Request enter ");
        
        BlankMessageRequest request = (BlankMessageRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_SYSTEM_SYNC);

        mCallback = callback;
        

        
        if ( null != request )
        {

//            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
//                    .registerReceiver(ResponseAction.CommandResponse.BT_SYSTEM_SYNC,this);
            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                       .registerReceiver(ResponseAction.CommandResponse.COMM_DATA_SENT,this);
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
