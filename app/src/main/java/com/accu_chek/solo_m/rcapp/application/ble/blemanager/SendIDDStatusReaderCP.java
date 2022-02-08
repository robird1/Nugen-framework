/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SendIDDStatusReaderCP
 * Brief: This class handles SendIDDStatusReaderCP request and its response.
 *
 * Create Date: 2015/10/21
 * $Revision: 20558 $
 * $Author: KiddYeh $
 * $Id: SendSoloMControlPoint.java 20558 2015-10-01 14:02:41Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeWriteResponse;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SendIDDStatusReaderCP implements IBLERequest
{
    private static final String TAG = "SendIDDStatusReaderCP";
    
    /**
     * The instance of SendIDDStatusReaderCP class
     */
    private static volatile SendIDDStatusReaderCP mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
 

    
    /**
     * This method gets the one and only instance of the class SendIDDCommandCP.
     * 
     * 
     * @return SendIDDCommandCP : the one and only instance of the class SendIDDCommandCP
     *         Range: A valid object of SendIDDCommandCP
     *         Unit: SendIDDCommandCP
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SendIDDCommandCP
     */
    public static SendIDDStatusReaderCP getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SendIDDStatusReaderCP(context);
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
    protected SendIDDStatusReaderCP(Context context)
    {
        mContext = context;
    }

    
    /**
     * This method gets the cause of response to check if SendIDDStatusReaderCP is successful or not.
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

        Debug.printD(TAG, "[SendIDDStatusReaderCP]: AttributeWriteResponse enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
        
        AttributeWriteResponse response = (AttributeWriteResponse) pack.getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        int cause = response.getCause().get();
        
        Debug.printD(TAG, "cause =" + cause);
        Debug.printD(TAG, "subcause =" + response.getSubCause().get());
        Debug.printD(TAG, "command =" + response.getCommand().get());
        
        if ( 0 == cause )
        {  
            isResult = SafetyBoolean.TRUE;
        }
        else
        {
            // Apply to the coding standard
        }
//        SystemClock.sleep(2000L);   
        returnResult(isResult, pack);
     
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
    protected void returnResult(SafetyBoolean isResult, ResponsePack pack)
    {
        if(mCallback instanceof ResponseCallbackWithData)
        {
            ((ResponseCallbackWithData) mCallback).onRequestCompleted(isResult, pack);
        }
        else if (mCallback instanceof ResponseCallback)
        {
            mCallback.onRequestCompleted(isResult);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    
    /**
     * This method handles the SendIDDStatusReaderCP request. 
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response callback.     
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
        Debug.printD(TAG, "[SendIDDStatusReaderCP]: Request enter ");
        
        mCallback = callback;
        
        byte[] data = parameter.getData().getByteArray();
        
        AttributeWriteTypeRequest request = (AttributeWriteTypeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);

        if ( null != request )
        {
            Debug.printD(TAG, "[SendIDDStatusReaderCP]: start ");
            
            SafetyByteArray bdAddress = new SafetyByteArray( BLEController.getBDAddress(mContext),
                  CRCTool.generateCRC16( BLEController.getBDAddress(mContext)));
            
            request.setRemoteBDAddress(bdAddress);

            request.setWriteType(new SafetyNumber<Integer>(
                    BlueConstant.WriteType.REQUEST,
                    -BlueConstant.WriteType.REQUEST));

            request.setAttributeHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.BLANK_HANDLE,
                    -BlueConstant.HANDLE.BLANK_HANDLE));

            request.setUUID16(new SafetyNumber<Integer>(
                    UUID.UUID16.IDD_STATUS_READER_CP, -UUID.UUID16.IDD_STATUS_READER_CP));

            request.setAttributeLength(new SafetyNumber<Integer>(data.length,
                    -data.length));

            request.setWriteOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));

            request.setData(new SafetyByteArray(data, CRCTool
                    .generateCRC16(data)));
            
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_WRITE, this);
                       
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            returnResult(SafetyBoolean.FALSE, null);
        }
        
    }
    


}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
