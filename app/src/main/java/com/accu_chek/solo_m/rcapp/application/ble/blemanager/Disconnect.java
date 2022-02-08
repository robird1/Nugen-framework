/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.Diconnect
 * Brief: This class handles Disconnect request and its response.
 *
 * Create Date: 2015/8/19
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: Disconnect.java 25071 2015-11-30 03:09:48Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.DisconnectRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.RemoteBdCauseResponse;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles the setting of Disconnect request and its response.
*/
public class Disconnect implements IBLERequest
{
    private static final String TAG = "Diconnect";
    
    /**
     * The instance of Disconnect class
     */
    private static volatile Disconnect mInstance = null;
    
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
     * This method shall get the one and only instance of the class Disconnect.
     * 
     * @param context: an application context
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class Disconnect
     *         Range: A valid object of Disconnect
     *         Unit: Disconnect
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class Disconnect
     */
    public static Disconnect getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new Disconnect(context);
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
    protected Disconnect(Context context)
    {
        mContext = context;
    }

    
    /**
     * 
     * This method checks the cause of response to check disconnect is successful or
     * not. It returns the result of response via callback function.
     * 
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
        Debug.printD(TAG, "[Diconnect]: Response ");

        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        RemoteBdCauseResponse response = (RemoteBdCauseResponse) pack
                .getResponse();
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        int cause = response.getCause().get();
        
        if (0 == cause)
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
     * 
     * 
     * This method sets the parameter of the request and sends it to UICommandDispatcher.
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter or null object
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback or null object
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
        Debug.printD(TAG, "[DISCONNECTED] request");
        
        SafetyByteArray address = GlobalTools.MPR.getMpAddress();
        
        DisconnectRequest request = (DisconnectRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_DISCONNECT);
        
        mCallback = callback;
        
        if ( null != request )
        {
            request.setRemoteBD(address);
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
            .registerReceiver(ResponseAction.CommandResponse.BT_DISCONNECT,
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
