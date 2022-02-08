/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.blemanager.RemoveBonding
 * Brief: This class handles RemoveBonding request and its response.
 *
 * Create Date: 2015/7/21
 * $Revision: 25242 $
 * $Author: KiddYeh $
 * $Id: RemoveBonding.java 25242 2015-12-01 08:40:05Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.request.RemoveBondingRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.RemoteBdTypeCauseResponse;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles RemoveBonding request and its response.
*/
public class RemoveBonding implements IBLERequest
{
    private static final String TAG = "RemoveBonding";
    
    /**
     * The instance of RemoveBonding class
     */
    private static volatile RemoveBonding mInstance = null;
    
    /**
     * The application class
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    
    /**
     * Get the one and only instance of the class RemoveBonding.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class RemoveBonding
     *         Range: A valid object of RemoveBonding
     *         Unit: RemoveBonding
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class RemoveBonding
     */
    public static RemoveBonding getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new RemoveBonding(context);
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
    protected RemoveBonding(Context context)
    {
        mContext = context;
    }
    
    
    /**
     * This method gets the cause of response to check if the RemoveBonding is successful or not. 
     * It returns the result of response via callback function.
     * This method is called after receiving the certain broadcast.
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
        Debug.printD(TAG,"[RemoveBonding]: InitResponse ");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        RemoteBdTypeCauseResponse response = 
                (RemoteBdTypeCauseResponse) pack.getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int cause = response.getCause().get();
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        if ( 0 == cause )
        {
            isResult = SafetyBoolean.TRUE;
            NugenGeneralModel.delete(context, BLEController.BONDED_ADDRESS);
            GlobalTools.getInstance().setBLEBondState(false);
            Debug.printD(TAG, "isBooded result = "
                    + BLEController.getInstance(context).isBonded());
        }
        else
        {
            Debug.printD(TAG,"[RemoveBonding]: failed ");
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
     * This method handles the RemoveBonding request. 
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the callback of response    
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
        Debug.printD(TAG,"[removeBonding] enter"); 
        Debug.printD(TAG, "isBooded result = "
                + BLEController.getInstance(mContext).isBonded());
        Debug.printD(TAG, "isConnecteded result = "
                        + BLEController.getInstance(mContext).isConnected());
        mCallback = callback;
        
        GlobalTools.MPR.setPinCode(null);
        SafetyNumber<Integer> req_BDType = null;

        byte[] btAddress = BLEController.getBDAddress(mContext);
              
        RemoveBondingRequest request = (RemoveBondingRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_BT_RMV_BOND);
        
        if (null != request )
        {
            if (btAddress.length > 0)
            {
            	Debug.printD(TAG, "[removeBonding] request"); 
                req_BDType = new SafetyNumber<Integer>(
                        BlueConstant.RemoteBDType.BLE_PUBLIC,
                        -BlueConstant.RemoteBDType.BLE_PUBLIC);
                request.setRemoteBD(new SafetyByteArray(btAddress, CRCTool
                        .generateCRC16(btAddress)));
                request.setRemoteBdType(req_BDType);
                BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                        ResponseAction.CommandResponse.COMM_BT_RMV_BOND, this);
                BLEController.sendRequestToComms(mContext, request);
            }
            else
            {
               Debug.printD(TAG, "empty whitelist"); 
               returnResult(SafetyBoolean.TRUE);
            }         
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
