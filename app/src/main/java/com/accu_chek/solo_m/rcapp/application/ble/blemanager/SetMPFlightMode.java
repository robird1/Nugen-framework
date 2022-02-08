/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetMPFlightMode
 * Brief: This class handles SetMPFlightMode request and its response.
 *
 * Create Date: 2015/8/7
 * $Revision: 25192 $
 * $Author: KiddYeh $
 * $Id: SetMPFlightMode.java 25192 2015-12-01 02:34:08Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.OpCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.ResponseCode;
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

/**
* This class handles SetMPFlightMode request and its response.
*/
public class SetMPFlightMode implements IBLERequest
{
    private static final String TAG = "SetMPFlightMode";
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF;
    /**
     * The instance of SetMPFlightMode class
     */
    private static volatile SetMPFlightMode mInstance = null;
   
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
     * Get the one and only instance of the class SetMPFlightMode.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SetMPFlightMode
     *         Range: A valid object of SetMPFlightMode
     *         Unit: SetMPFlightMode
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetMPFlightMode
     */
    public static SetMPFlightMode getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetMPFlightMode(context);
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
     * @see mContext           
     */
    protected SetMPFlightMode(Context context)
    {
        mContext = context;
    }

    /**
     * This method shall the get cause of response to check if 
     * the SetMPFlightMode is successful or not.
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

        Debug.printD(TAG, "[SetMPFlightMode]: Response enter ");

        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
  
        
        AttributeWriteResponse response = 
                (AttributeWriteResponse) pack.getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int cause = response.getCause().get();
        
        Debug.printD(TAG, "cause =" + cause);
        Debug.printD(TAG, "subcause =" + response.getSubCause().get());
        Debug.printD(TAG, "command =" + response.getCommand().get());
        
        if ( 0 == cause )
        {  
        	Debug.printD(TAG, "[SetMPFlightMode]: WriteResponse ==> OK ");   
        }
        else
        {
        	
        	Debug.printD(TAG, "[SetMPFlightMode]: WriteResponse ==> failed ");
            BLEResponseReceiver.getInstance(context).unregisterReceiver();
            setupReturnResult(SafetyBoolean.FALSE, null);

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
    protected void setupReturnResult(SafetyBoolean isResult, ResponsePack pack)
    {
        // Unregister Receiver
        BLEResponseReceiver.getInstance(mContext).unregisterReceiver();
        
        if (null != mCallback)
        {
             mCallback.onRequestCompleted(isResult);
        }
        else
        {
            
        }
    }
    
    
    /**
     * This method handles the SetMPFlightMode request. 
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
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[setMPFlightMode] : request enter " );
        
        AttributeWriteTypeRequest request = (AttributeWriteTypeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);

        byte[] data = parameter.getData().getByteArray();
        byte[] btAddress = BLEController.getBDAddress(mContext);
        
        mCallback = callback;
        
        if ( null != request )
        {

            request.setRemoteBDAddress(new SafetyByteArray(btAddress, CRCTool.generateCRC16(btAddress)));

            // COMMAND
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

            request.setData(new SafetyByteArray(data, CRCTool.generateCRC16(data)));

            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_WRITE, this);
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND ,mAttrNotification);
                       
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
        	setupReturnResult(SafetyBoolean.FALSE, null);
        }
        
    }
    
    class AttrNotification implements ResponseProcess
    {
        
        /**
         * This method gets the E2E-Result and ControlPointCodeResponse of the indication 
         * to check if the StartStopPriming Opcode is successful or not.It returns the 
         * result of response via callback function.
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
            Debug.printD(TAG, " [setMPFlightMode]: AttrNotification ");
            
           // OpCode
            int iOpCode = 0; 
            int iCommandCode = 0;
            int iResponseValue = 0;
            
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
            
            if (iE2EResult == E2E_Result.RESULT_OK)
            {
             // Operation corresponded with OpCode
                switch(iOpCode)
                {      
                case ControlPointConstant.OpCode.IDSCMD_RESP:
                    // Get Command Code
                    iCommandCode = bbData.getShort() & TWO_BYTES_OPERAND;
                    iResponseValue = bbData.get();
                    
                    boolean isOpCodeOK = (iCommandCode == OpCode.IDSCMD_SET_FLIGHT);
                    boolean isResponseSuccess =  (ResponseCode.SUCCESS == iResponseValue);
                    
                    if (isOpCodeOK && isResponseSuccess)
                    {
                        Debug.printD(TAG, " [setMPFlightMode]: E2E_Result.RESULT_OK & Success");
                        isResult = SafetyBoolean.TRUE;
                        
                    }
                    else
                    {
                        Debug.printD(TAG, " [setMPFlightMode]: E2E_Result.RESULT_OK but failed");
                        isResult = SafetyBoolean.FALSE;
                        attPack = null;
                        
                    }
                    setupReturnResult(isResult, attPack);
                    break;
                default:  
                    // Apply to the coding standard
                    break;
                }
            }
            else
            {
                Debug.printD(TAG, " [SetTherapyState]: E2E_Result.RESULT_failed");
                isResult = SafetyBoolean.FALSE;
                attPack = null; 
                setupReturnResult(isResult, attPack);
            }

            
        }    
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R16250 2015-09-03 04:05:07 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
