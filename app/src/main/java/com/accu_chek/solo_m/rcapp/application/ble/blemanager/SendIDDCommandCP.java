/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SendIDDCommandCP
 * Brief: This class handles SendIDDCommandCP request and its response.
 *
 * Create Date: 2015/9/21
 * $Revision: 21344 $
 * $Author: KiddYeh $
 * $Id: Scan.java 21344 2015-10-12 12:12:13Z KiddYeh $
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
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeWriteResponse;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;


public class SendIDDCommandCP implements IBLERequest
{
    private static final String TAG = "SendIDDCommandCP";
    
    /**
     * The instance of Scan class
     */
    private static volatile SendIDDCommandCP mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    private int mOpCode = 0;

    
    /**
     * this method gets the one and only instance of the class SendIDDCommandCP.
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
    public static SendIDDCommandCP getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SendIDDCommandCP(context);
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
     *            
     */
    protected SendIDDCommandCP(Context context)
    {
        mContext = context;
    }

    
    /**
     * This method gets the cause of  response to check if the  SendIDDCommandCP is successful or not.
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

        Debug.printD(TAG, "[SendIDDCommandCP]: Response enter ");
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
     * This method handles the SendIDDCommandCP request. 
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
        Debug.printD(TAG, "[SendIDDCommandCP]: Request enter ");
        
        mCallback = callback;
        
        byte[] data = parameter.getData().getByteArray();
        
        AttributeWriteTypeRequest request = (AttributeWriteTypeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);

        if ( null != request )
        {
            Debug.printD(TAG, "[SendIDDCommandCP]: start ");
            
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
                    UUID.UUID16.IDD_COMMAND_CP, -UUID.UUID16.IDD_COMMAND_CP));

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
            returnResult(SafetyBoolean.FALSE);
        }
        
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
// [BT] modified SetConfigration & added header/footer.
