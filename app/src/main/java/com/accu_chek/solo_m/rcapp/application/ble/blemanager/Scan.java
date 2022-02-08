/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.Scan
 * Brief: This class handles Scan request and its response.
 *
 * Create Date: 2015/7/21
 * $Revision: 24741 $
 * $Author: IvanHuang $
 * $Id: Scan.java 24741 2015-11-25 03:05:47Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.ScanRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CauseOnlyResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles Scan request and its response.
*/
public class Scan implements IBLERequest
{
    private static final String TAG = "Scan";
    
    /**
     * The instance of Scan class
     */
    private static volatile Scan mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    
    /**
     * This method shall get the one and only instance of the Scan class.
     * 
     * @param context: an application context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return Scan : the one and only instance of the Scan class
     *         Range: A valid object of Scan
     *         Unit: Scan
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class Scan
     */
    public static Scan getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new Scan(context);
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
    protected Scan(Context context)
    {
        mContext = context;
    }

    
    /**
     * This method gets the cause of response to check if the Scan is successful or not.
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

        Debug.printD(TAG, "[Scan]: Response enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CauseOnlyResponse response = (CauseOnlyResponse) pack.getResponse();

        int cause = response.getCause().get();
        int command = response.getCommand().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;

        BLEResponseReceiver.getInstance(context).unregisterReceiver();

        if ((0 == cause) && (CommandCode.BT_SCAN == command))
        {
            isResult = SafetyBoolean.TRUE;
            BLEResponseReceiver.getInstance(context.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_SCAN_INFO,
                    ScanInfo.getInstance());
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
     * This method sets the parameter of the Scan request and sends it to 
     * UICommandDispatcher. 
     * 
     * @param parameter [in] the request parameter 
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response callback function     
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
        Debug.printD(TAG, "[Scan]: Request enter ");
        
        ScanRequest request = (ScanRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_SCAN);

        mCallback = callback;
        
        if ( null != request )
        {
            request.setMode(parameter.getIsEnable());

            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                    .registerReceiver(ResponseAction.CommandResponse.BT_SCAN,
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
