/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.GetCommsInfo
 * Brief: This class handles the GetCommsInfo request and its responses.
 *
 * Create Date: 2015/7/22
 * $Revision: 23543 $
 * $Author: JamesLee $
 * $Id: GetCommsInfo.java 23543 2015-11-06 09:51:56Z JamesLee $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.request.BlankMessageRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommsInfoResponse;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.selftest.IPOSTManager;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * This class handles GetCommsInfo request and its response.
 *
 */
public class GetCommsInfo implements IBLERequest
{
    private static final String TAG = "GetCommsInfo";
    
    /**
     * The instance of GetCommsInfo class
     */
    private static volatile GetCommsInfo mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    
    /**
     * Get the one and only instance of the class GetCommsInfo.
     * 
     * 
     * @return GetCommsInfo : the one and only instance of the class GetCommsInfo
     *         Range: A valid object of GetCommsInfo
     *         Unit: GetCommsInfo
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class Scan
     */
    public static GetCommsInfo getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new GetCommsInfo(context);
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
    private GetCommsInfo(Context context)
    {
        mContext = context;
    }

    
    /**
     * 
     * This method checks the result of the response of GetCommsInfo.cIf the 
     * callback exists, it returns the result of response via callback function.
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

        Debug.printD(TAG, "[GetCommsInfo]: Response enter ");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CommsInfoResponse response = (CommsInfoResponse) pack.getResponse();
        
        // save ble version
        String bleVersion = response.getVersion().substring(0, 5);

        NugenGeneralModel.setString(
                context,
                NugenFrameworkConstants.BTLEConstants.KEY_BLEVERSION,
                new SafetyString(bleVersion, CRCTool
                        .generateCRC16(bleVersion.getBytes())));
        
        boolean isSendCommsInfoDone = (CommsConstant.Result.RESULT_OK == response
                .getResult().get());
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        IPOSTManager postControl = CustJavaFrameworkManager.getPOSTService(context);
        
        Debug.printI(TAG, "[response.getPostResult().get().intValue()]  "
                + response.getPostResult().get().intValue());
        try
        {
            postControl.setCommsReadyState(response.getPostResult().get().intValue());
        }
        catch (RemoteException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        finally
        {
          // Apply to the coding standard
        }
        
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();

        if (isSendCommsInfoDone)
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
    private void returnResult(SafetyBoolean isResult)
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
     * This method sets parameter of the GetCommsInfo request and submits it to 
     * communication processor. 
     * 
     * @param parameter [in] the request parameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response callback     
     *            Range: a valid object of ResponseCallback
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
        Debug.printD(TAG, "[GetCommsInfo]: Request enter ");
        
        BlankMessageRequest request = (BlankMessageRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_GET_INFO);
        
        mCallback = callback;
        
        if (null != request)
        {
            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                    .registerReceiver(ResponseAction.CommandResponse.COMM_GET_INFO,
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
