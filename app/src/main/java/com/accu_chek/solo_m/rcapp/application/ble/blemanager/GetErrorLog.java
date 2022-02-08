/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.GetErrorLog
 * Brief: his class handles GetErrorLog request and its response.
 *
 * Create Date: 2015/8/11
 * $Revision: 25217 $
 * $Author: IvanHuang $
 * $Id: GetErrorLog.java 25217 2015-12-01 06:36:45Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.request.BlankMessageRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.ErrorLogResponse;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles GetErrorLog request and its response.
* The error log is of the communication processor.  
*/
public class GetErrorLog implements IBLERequest
{
    private static final String TAG = "GetErrorLog";
    
    public static final String COMM_ERROR_LOG = "comm_error_log";
    
    /**
     * The instance of GetErrorlog class 
     */
    private static volatile GetErrorLog mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * This method shall get the one and only instance of the class GetErrorLog.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1  
     * 
     * @return mInstance : the one and only instance of the class GetErrorLog
     *         Range: A valid object of GetErrorLog
     *         Unit: GetErrorLog
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class GetErrorLog
     */
    public static GetErrorLog getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new GetErrorLog(context);
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
    protected GetErrorLog(Context context)
    {
        mContext = context;
    }

    
    /**
     * This method gets the cause of response to check if the GetErrorLog request is successful or not.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: a valid object of intent
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
        String errorlog = null;
        
        Debug.printD(TAG, "[GetErrorLog]: Response enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        ErrorLogResponse response = (ErrorLogResponse) pack.getResponse();
       
        int mCount = response.getCount().get();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        Debug.printD(TAG,"Count = " + mCount);
        
        Debug.dumpPayload(TAG, response.getMessage());
        
        errorlog = new String(response.getMessage());
        
        NugenGeneralModel.setString(context, GetErrorLog.COMM_ERROR_LOG, 
                new SafetyString(errorlog,CRCTool.generateCRC16(errorlog.getBytes())));

        returnResult(SafetyBoolean.TRUE);
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
     * This method sets the parameter of the GetCommsErrorLog request and sends 
     * it out. 
     * 
     * @param parameter [in] the parameter of request
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
     * @see mCallback 
     * @see mContext
     * 
     */

    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[GetErrorLog]: Request enter ");
        
        BlankMessageRequest request = (BlankMessageRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_GET_ERROR_LOG);

        mCallback = callback;
        
        if ( null != request )
        {

            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                    .registerReceiver(ResponseAction.CommandResponse.COMM_GET_ERROR_LOG,
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
