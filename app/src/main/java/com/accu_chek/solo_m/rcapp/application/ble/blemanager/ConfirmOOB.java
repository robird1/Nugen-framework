/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.ConfirmOOB
 * Brief: This class handles ConfirmOOB request and its response.
 *
 * Create Date: 2015/7/23
 * $Revision: 25022 $
 * $Author: IvanHuang $
 * $Id: ConfirmOOB.java 25022 2015-11-27 08:48:03Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.OOBConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.response.RemoteBdCauseResponse;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles ConfirmOOB request and its response.
*/
public class ConfirmOOB implements IBLERequest
{
    private static final String TAG = "ConfirmOOB";
    
    /**
     *  The instance of ConfirmOOB class
     */
    private static volatile ConfirmOOB mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * Get the one and only instance of the class ConfirmOOB.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1 
     * 
     * @return mInstance : the one and only instance of the class ConfirmOOB
     *         Range: A valid object of ConfirmOOB
     *         Unit: ConfirmOOB
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class ConfirmOOB
     */
    public static ConfirmOOB getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new ConfirmOOB(context);
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
     * @param context: 
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mContext            
     */
    protected ConfirmOOB(Context context)
    {
        mContext = context;
    }
    
    
    /**
     * This method is called after receiving the broadcast of CauseOnlyresponse.
     * Return the result of response via callback function.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: A valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {     
        Debug.printD(TAG, "[ConfirmOOB]: Response enter "); 
        byte[] remoteBD = null;
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        RemoteBdCauseResponse response = 
                (RemoteBdCauseResponse) pack.getResponse(); 
        
        int cause = response.getCause().get();
        int command =  response.getCommand().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;

        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        remoteBD = response.getRemoteBD().getByteArray();
        
        BLEController.storeBDAddress(context, remoteBD);
        
        if ((0 == cause) && (CommandCode.BT_SECURITY == command))
        {
            isResult = SafetyBoolean.TRUE;

            Debug.printD(TAG,"[ConfirmOOB]: Bonding successful ");
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
     * This method handles the ConfirmOOB request. 
     * 
     * @param parameter [in] the request parameter
     *            Range: A valid object of BLERequestParameter or null object
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response callback       
     *            Range: A valid object of ResponseCallback or null object
     *            Unit: ResponseCallback
     *            Scaling: 1   
     *                      
     * @return void [out] 
     * 
     * @see mContext
     * @see mCallback
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[ConfirmOOB]: Request enter ");
        byte[] pinTemp = null;
        
        SafetyByteArray safetypin = null;
        OOBConfirmation request = (OOBConfirmation) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_OOB_CFM);

        mCallback = callback;

        if ( null != request )
        {
            request.setRemoteBD(GlobalTools.MPR.getMpAddress());
            pinTemp = GlobalTools.MPR.getKey();
            safetypin = new SafetyByteArray(pinTemp,CRCTool.generateCRC16(pinTemp));
            request.setCbin(safetypin);
            // register OOB-confirmation response
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_OOB_CFM, this);
            
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
