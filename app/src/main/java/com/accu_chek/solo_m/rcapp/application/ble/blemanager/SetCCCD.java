/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetCCCD
 * Brief: This class handles SetCCCD request and its response.
 *
 * Create Date: 2015/7/22
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: SetCCCD.java 25071 2015-11-30 03:09:48Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.CCCDRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CCCDResponse;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles SetCCCD request and its response.
*/
public class SetCCCD implements IBLERequest
{
    private static final String TAG = "SetCCCD";
    
    /**
     * The instance of SetCCCD class
     */
    private static volatile SetCCCD mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    
    /**
     * This method gets the one and only instance of the class SetCCCD.
     *
     * @param context: The Context 
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1 
     * 
     * @return mInstance : the one and only instance of the class SetCCCD
     *         Range: A valid object of SetCCCD
     *         Unit: SetCCCD
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetCCCD
     */
    public static SetCCCD getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetCCCD(context);
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
    protected SetCCCD(Context context)
    {
        mContext = context;
    }
    
    
    /**
     * This method is called after receiving the CCCDResponse broadcast.
     * It gets the cause of the response to check if the SetCCCD is successful or not.
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
        Debug.printD(TAG, "[SetCCCD]: Response enter ");

        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CCCDResponse response = (CCCDResponse) pack.getResponse();

        int command = response.getCommand().get();
        int cause = response.getCause().get();
        int subCause = response.getSubCause().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        Debug.printD(TAG, "----------------------------------");
        Debug.printD(TAG, "setCCCD Command = " + command);
        Debug.printD(TAG, "setCCCD Type = " + cause);
        Debug.printD(TAG, "setCCCD SubCause = " + subCause);
        Debug.printD(TAG, "----------------------------------");

        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
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
     * This method handles the SetCCCD request. 
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
        Debug.printD(TAG,"[SetCCCD]: Request enter ");
        
        mCallback = callback;
        
        SafetyByteArray sfBDAddress = GlobalTools.MPR.getMpAddress();
        
        CCCDRequest request = (CCCDRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_CCCD_CONFIG);

        if ( null != request )
        {
            request.setRemoteBDAddress(sfBDAddress);

            request.setUUID(new SafetyNumber<Integer>(parameter.getUUID(),
                        -parameter.getUUID()));
                
            request.setParameter(new SafetyNumber<Integer>(parameter.getParameter(),
                    -parameter.getParameter()));
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_CCCD_CONFIG, this);
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
