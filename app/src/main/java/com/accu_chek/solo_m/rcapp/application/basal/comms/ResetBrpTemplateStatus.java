/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ResetBrpTemplateStatus
 * Brief: BLE Command: reset BRP template status
 *
 * Create Date: 11/04/2015
 * $Revision: 24156 $
 * $Author: JacksonHuang $
 * $Id: ResetBrpTemplateStatus.java 24156 2015-11-16 05:50:26Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms;

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
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEResponseReceiver;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeWriteResponse;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class ResetBrpTemplateStatus implements IBLERequest
{
    private static final String TAG = "TestServiceApp";
    
    // BIT 0
    private static final byte BIT0_SET = (byte)0x01;    
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF;    
    
    // BRP Template Code
    private static final byte BRP_TEMPLATE_CODE = 
                    (byte)ControlPointConstant.TemplateType.BASAL_RATE_PROFILE;
    
    // Instance
    private static volatile ResetBrpTemplateStatus mInstance = null;
    
    // Context
    private Context mContext = null;
    
    // Response Callback
    private ResponseCallback mCallback = null;
    
    // Indication Response
    private ResetBrpTemplateStatusResponse mResetBrpTemplateStatusResponse = 
                                            new ResetBrpTemplateStatusResponse();    
    
    /**
     * Class Constructor
     * 
     * @param context [in] Context
     * 
     *          Interface to global information about an application environment
     * 
     *          Range: Valid Context
     *          Unit: Context
     *          Scaling: 1
     *              
     */
    
    private ResetBrpTemplateStatus(Context context)
    {
        mContext = context;       
    }    
    
    /**
     * Return the one and only instance of the class GetBasalTemplate
     * 
     * @param context [in] Context
     * 
     *          Interface to global information about an application environment
     * 
     *          Range: Valid Context
     *          Unit: Context
     *          Scaling: 1
     * 
     * @return GetBasalTemplate [out]
     * 
     *         Instance of GetBasalTemplate
     * 
     *         Range: Valid GetBasalTemplate
     *         Unit: GetBasalTemplate
     *         Scaling: 1
     * 
     */
    public static ResetBrpTemplateStatus getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new ResetBrpTemplateStatus(context);
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }    

    /**
     * Action when write response received  
     *
     * @param context [in] Context
     * 
     *          Interface to global information about an application environment
     * 
     *          Range: Valid Context
     *          Unit: Context
     *          Scaling: 1
     * 
     * @param intent [in] Intent
     * 
     *          Data from BLE response
     * 
     *          Range: Valid Intent
     *          Unit: Intent
     *          Scaling: 1
     * 
     * @return None
     * 
     */

    @Override
    public void doProcess(Context context, Intent intent)
    {
        // Response Data
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        // Response from BLE
        AttributeWriteResponse response = 
                (AttributeWriteResponse) pack.getResponse();
        
        // Result of command sending
        int cause = response.getCause().get();
        
        if(0 != cause)
        {   
            setupReturnResult(SafetyBoolean.FALSE, null);
        }
        else
        {
            // Apply to the coding standard    
        }
    }
    
    /**
     * Setup returned callback result
     * 
     * @param isResult [in] SafetyBoolean
     * 
     *          Result of response of current request 
     *           
     *          Range: Valid SafetyBoolean
     *          Unit: SafetyBoolean
     *          Scaling: 1
     *          
     * @param pack [in] ResponsePack
     * 
     *          Data of response of current request 
     *           
     *          Range: Valid ResponsePack
     *          Unit: ResponsePack
     *          Scaling: 1
     *                      
     * @return None 
     * 
     */
    
    private void setupReturnResult(SafetyBoolean isResult, ResponsePack pack)
    {
        // Unregister Receiver
        BLEResponseReceiver.getInstance(mContext).unregisterReceiver();
        
        if(mCallback instanceof ResponseCallbackWithData)
        {
            ((ResponseCallbackWithData) mCallback)
                                        .onRequestCompleted(isResult, pack);
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
     * Action of command
     *
     * @param parameter [in] BLERequestParameter
     * 
     *          Data sent to MP
     * 
     *          Range: Valid BLERequestParameter
     *          Unit: BLERequestParameter
     *          Scaling: 1
     * 
     * 
     * @param callback [in] ResponseCallback
     * 
     *          Callback for receiving response from BLE
     *          
     *          Range: Valid ResponseCallback
     *          Unit: ResponseCallback
     *          Scaling: 1
     *          
     */

    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        // Data to BLE 
        byte[] data = { (byte)0x1D, (byte)0x12, 
                        BRP_TEMPLATE_CODE, BIT0_SET, 
                        (byte)0x00 };
        SafetyByteArray bleData = new SafetyByteArray(data,
                CRCTool.generateCRC16(data));
        
        
        mCallback = callback;
        
        BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                           .registerReceiver(ResponseAction
                                             .CommandResponse
                                             .BT_ATTR_NOTIF_IND, 
                                             mResetBrpTemplateStatusResponse);
        
        BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                           .registerReceiver(ResponseAction
                                             .CommandResponse
                                             .BT_ATTR_WRITE, 
                                             this);
        
        requestCmd(bleData);
    }
    
    /**
     * Send Command
     *
     * @param data [in] SafetyByteArray
     * 
     *          Data sent to MP
     * 
     *          Range: Valid SafetyByteArray
     *          Unit: SafetyByteArray
     *          Scaling: 1
     * 
     * @return None
     *          
     */    
    
    protected void requestCmd(SafetyByteArray data)
    {   
        // Get request
        AttributeWriteTypeRequest request = 
                (AttributeWriteTypeRequest) RequestPayloadFactory
                    .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);
        
        // Data Length
        int iLen = data.getByteArray().length;

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
                    UUID.UUID16.IDD_COMMAND_CP, -UUID.UUID16.IDD_COMMAND_CP));

            request.setAttributeLength(new SafetyNumber<Integer>(iLen,
                    -iLen));

            request.setWriteOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));

            request.setData(data);

            
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_WRITE, this);
            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            setupReturnResult(SafetyBoolean.FALSE, null);
        }
          
    }
    

    
    /**
     * Call EMWR for error handling
     * 
     * @param ErrorCode [in] EMWRList
     * 
     *          Internal Error Code Enum Value of Basal Delivery 
     * 
     *          Range: Valid EMWRList
     *          Unit: EMWRList
     *          Scaling: 1
     * 
     * @return None
     */
    
    protected void callEmwr(EMWRList ErrorCode)
    {    
        NotifyMessage msg = new NotifyMessage(ErrorCode);
     
        NotifyProxy.showEMWR(msg);
    }
    
    
    /**
     * Indication of ResetBrpTemplateStatus
     * 
     */  
    
    class ResetBrpTemplateStatusResponse implements ResponseProcess
    {
        
        /**
         * Process of received indication 
         * 
         * @param context [in] Context 
         * 
         *          The Context in which the receiver is running.
         *          
         *          Range: Valid Context
         *          Unit: Context
         *          Scaling: 1
         *          
         * @param intent [in] Intent 
         * 
         *          The Intent being received.
         *          
         *          Range: Valid Intent
         *          Unit: Intent 
         *          Scaling: 1
         * 
         * @return None
         *  
         */
        
        @Override
        public void doProcess(Context context, Intent intent)
        {
            Debug.printD(TAG, "Response enter ==> Reset BRP template");
            
            // OpCode
            int iOpCode = 0; 
            int iCommandCode = 0;
            
            // E2E Result
            int iE2EResult = 0;
            
            // Command Result
            SafetyBoolean isResult = SafetyBoolean.FALSE;
            
            // Response Pack
            ResponsePack attPack = intent
                    .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
            
            // Response
            AttributeChangeNotification response = 
                 (AttributeChangeNotification)attPack.getResponse();
            
            // Indication Data
            SafetyByteArray sbData = response.getData();
            byte[] bData = sbData.getByteArray();
            ByteBuffer bbData = ByteBuffer.wrap(bData);
            
            // Get OpCode
            bbData.rewind();
            bbData.order(ByteOrder.LITTLE_ENDIAN);
            iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
            
            Debug.printD(TAG, " Opcode: " + iOpCode);

            // Operation corresponded with OpCode
            switch(iOpCode)
            {
            case ControlPointConstant.OpCode.IDSCMD_RESET_TMPL_STATUS_RESP:
                
                // Get E2E Result
                iE2EResult = response.getResult().get();
                
                if (iE2EResult == E2E_Result.RESULT_OK)
                {
                    isResult = SafetyBoolean.TRUE;
                    Debug.printD(TAG, " [ResetBrpTemplateStatus]: E2E_Result.RESULT_OK");
                }
                else
                {
                    isResult = SafetyBoolean.FALSE;
                    Debug.printD(TAG, " [ResetBrpTemplateStatus]: E2E_Result.RESULT_NNNNNNNNNNNNNNNNNNNNNOK");
                }
                
                setupReturnResult(isResult, attPack);
                
                break;
                
            case ControlPointConstant.OpCode.IDSCMD_RESP:
                
                // Get Command Code
                iCommandCode = bbData.getShort() & TWO_BYTES_OPERAND;
                
                if (iCommandCode == 
                        ControlPointConstant.OpCode.IDSCMD_RESET_TMPL_STATUS)
                {
                    Debug.printD(TAG, " [ResetBrpTemplateStatus]: STD CMD Error");
                    setupReturnResult(SafetyBoolean.FALSE, null);
                }
                
                break;
                
            default:
                
                // Apply to the coding standard
                
                break;                
            }
        }
    }
}


/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// [New Feature] Basal Delivery comms functions
// [Update] Modify comments
// [Update] Add WaitDelivery for long time delivery time
