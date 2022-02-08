/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.GetDateTime
 * Brief: This class handles SyncDateTime process.
 *
 * Create Date: 2015/7/21
 * $Revision: 21344 $
 * $Author: KiddYeh $
 * $Id: GetDateTime.java 21344 2015-10-12 12:12:13Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

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
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.OpCode;
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

public class GetDateTime implements IBLERequest
{
    private static final String TAG = "GetDateTime";
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF;
    
    /**
     * The instance of GetDateTime class
     */
    private static volatile GetDateTime mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     *  The AttrNotification response
     */
    private AttrNotification mAttrNotification = new AttrNotification();
    
    
    
    /**
     * This method gets the one and only instance of the class GetDateTime.
     * 
     * @param context: an application context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return Scan : the one and only instance of the class GetDateTime
     *         Range: A valid object of GetDateTime
     *         Unit: GetDateTime
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class GetDateTime
     */
    public static GetDateTime getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new GetDateTime(context);
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
    private GetDateTime(Context context)
    {
        mContext = context;
    }

    
    /**
     * This method gets the cause of response to check if the GetDateTime is successful or not.
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
        Debug.printD(TAG, "[GetDateTime]: Response enter ==> WriteResponse");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        AttributeWriteResponse response = (AttributeWriteResponse) pack.getResponse();
        
        int cause = response.getCause().get();
        
        if(0 != cause)
        {   
            setupReturnResult(SafetyBoolean.FALSE, null);
            Debug.printD(TAG, "[GetDateTime]: WriteResponse ==> failed ");
        }
        else
        {
            Debug.printD(TAG, "[GetDateTime]: WriteResponse ==> OK ");
            // Apply to the coding standard    
        }
        
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
    private void setupReturnResult(SafetyBoolean isResult, ResponsePack pack)
    {
        // Unregister Receiver
        BLEResponseReceiver.getInstance(mContext).unregisterReceiver();
        
        if(null != mCallback)
        {
            ((ResponseCallbackWithData) mCallback).onRequestCompleted(isResult, pack);
        }
        else
        {
            
        }
    }

    
    /**
     * 
     * This method sets the parameter of SyncTimeCP request and sends it out. 
     * 
     * @param data[] [in] Command relevant data (OP code,Date&Time data)
     *            Range: a valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     *            
     * @return void [out]
     * 
     * @see mContext
     * @see mCallback 
     */
    protected void requestSyncTimeCP(byte[] data)
    {   
        
        
        AttributeWriteTypeRequest request = (AttributeWriteTypeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);

        if ( null != request )
        {       
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
                    UUID.UUID16.SOLOM_CP, -UUID.UUID16.SOLOM_CP));

            request.setAttributeLength(new SafetyNumber<Integer>(data.length,
                    -data.length));

            request.setWriteOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));

            request.setData(new SafetyByteArray(data, CRCTool
                    .generateCRC16(data)));

            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            setupReturnResult(SafetyBoolean.FALSE, null);
        }
          
    }
    
    
    /**
     * This method sets the parameter of the GetDateTime request and sends it to 
     * UICommandDispatcher. 
     * 
     * @param parameter [in] the request parameter 
     *            Range: a valid object of BLERequestParameter or null object
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
        Debug.printD(TAG, "[GetDateTime]: Request enter ");
        
        mCallback = callback;
        
        byte[] data = {(byte) OpCode.SOLOM_GET_DATE_AND_TIME,(OpCode.SOLOM_GET_DATE_AND_TIME >> 8)};
              
        BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                ResponseAction.CommandResponse.BT_ATTR_WRITE,this);
          
        BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND ,mAttrNotification);

        requestSyncTimeCP(data);
             
    }
    
     
    
    class AttrNotification implements ResponseProcess
    {
        /**
         * This method gets the E2E-Result of the indication 
         * to check if the GetDateTimeCP Opcode is successful or not.
         * It returns the result of response and response pack via callback function.
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
            Debug.printD(TAG, "[GetDateTime]: Response enter ==> AttrNotification");
            
            // OpCode
            int iOpCode = 0; 
            int iCommandCode = 0;
            
            // E2E Result
            int iE2EResult = 0;
            
            // Command Result
            SafetyBoolean isResult = SafetyBoolean.FALSE;
            
            // Response Pack
            ResponsePack attPack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
            
            // Response
            AttributeChangeNotification response = (AttributeChangeNotification) attPack.getResponse();
            
            // Indication Data
            SafetyByteArray sbData = response.getData();
            byte[] bData = sbData.getByteArray();
            ByteBuffer bbData = ByteBuffer.wrap(bData);
            
            // Get OpCode
            bbData.rewind();
            bbData.order(ByteOrder.LITTLE_ENDIAN);
            iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;

           // Get E2E Result
            iE2EResult = response.getResult().get();
            
            Debug.printD(TAG, " Opcode: " + iOpCode);
            
            // Operation corresponded with OpCode
            switch(iOpCode)
            {
            case ControlPointConstant.OpCode.SOLOM_GET_DATE_AND_TIME_RESP:

                if (iE2EResult == E2E_Result.RESULT_OK)
                {
                    isResult = SafetyBoolean.TRUE;
                    Debug.printD(TAG, " [GetDateTime]: E2E_Result.RESULT_OK");
                }
                else
                {
                    isResult = SafetyBoolean.FALSE;
                    Debug.printD(TAG, " [GetDateTime]: E2E_Result.RESULT_NNNNNNNNNNNNNNNNNNNNNOK");
                }
                setupReturnResult(isResult, attPack);
                break;
                
            case ControlPointConstant.OpCode.SOLOM_CP_RESP:
                
                // Get Command Code
                iCommandCode = bbData.getShort() & TWO_BYTES_OPERAND;
                
                if (iCommandCode == 
                        ControlPointConstant.OpCode.SOLOM_GET_DATE_AND_TIME)
                {
                    Debug.printD(TAG, " [GetDateTime]: STD CMD Error");
                    isResult = SafetyBoolean.FALSE;
                    attPack = null;
                    setupReturnResult(isResult, attPack);
                }
                
                break;
                
            default:
                
                // Apply to the coding standard
                
                break;
            }
            
            
        }
        
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
