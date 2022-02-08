/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.ReadIDDFeature
 * Brief: This class handles ReadIDDFeature request and its response.
 *
 * Create Date: 2015/7/21
 * $Revision: 25071 $
 * $Author: IvanHuang $
 * $Id: ReadIDDFeature.java 25071 2015-11-30 03:09:48Z IvanHuang $
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
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID.UUID16;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeReadRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeReadResponse;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles ReadIDDFeature request and its response.
*/
public class ReadIDDFeature implements IBLERequest
{
    private static final String TAG = "ReadIDDFeature";
    
    /**
     * The instance of ReadIDDFeature class
     */
    private static volatile ReadIDDFeature mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * 
     * 
     * Get the one and only instance of the class ReadIDDFeature.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class ReadIDDFeature
     *         Range: A valid object of ReadIDDFeature
     *         Unit: ReadIDDFeature
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class ReadIDDFeature
     */
    public static ReadIDDFeature getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new ReadIDDFeature(context);
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
    protected ReadIDDFeature(Context context)
    {
        mContext = context;
    }

    
    /**
     * 
     * This method is called after receiving the certain broadcast.
     * It get cause response to check ReadIDDFeature success or not. 
     * Return the result of response via callback function.
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
        Debug.printD(TAG, "[doProcess]: ReadIDDFeature ");
        
        ResponsePack pack = intent
                .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
        
        AttributeReadResponse response= (AttributeReadResponse) pack
                .getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        int result = response.getResult().get();
        
        if ( CommsConstant.Result.RESULT_OK == result )
        {
            isResult = SafetyBoolean.TRUE;
        }
        else
        {
            // Apply to the coding standard
        }
        Debug.printD(TAG, "++++++++++++++++++++");
        Debug.printD(TAG, "message");
        Debug.dumpPayload(TAG, response.getMessage());
        Debug.printD(TAG, "Command =" + response.getCommand().get());
        Debug.printD(TAG, "RemoteBD =" );
        Debug.dumpPayload(TAG, response.getRemoteBD().getByteArray());
        Debug.printD(TAG, "Result =" + result);
        Debug.printD(TAG, "ReadType =" + response.getReadType().get());
        Debug.printD(TAG, "Cause =" + response.getCause().get());
        Debug.printD(TAG, "Subcause =" + response.getSubcause().get());
        Debug.printD(TAG, "ReadOffset =" + response.getReadOffset().get());
        Debug.printD(TAG, "TotalLength =" + response.getTotalLength().get());
        Debug.printD(TAG, "AttributeLength =" + response.getAttributeLength().get());
        Debug.printD(TAG, "NumberOfHandle =" + response.getNumberOfHandle().get());
        Debug.printD(TAG, "Gap =" + response.getGap().get());

        Debug.printD(TAG, "Data");
        Debug.dumpPayload(TAG, response.getData().getByteArray());
        Debug.printD(TAG, "++++++++++++++++++++");
        

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
     *
     * 
     * This method handles the IDDFeature request. It sets the parameter of request 
     * and sends it out. 
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
    public void request(BLERequestParameter parameter,
            ResponseCallback callback)
    {
        Debug.printD(TAG, "[ReadIDDFeature]: Request enter ");
        
        AttributeReadRequest request = (AttributeReadRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_READ);

        mCallback = callback;
        
        if ( null != request )
        {
            request.setRemoteBD(GlobalTools.MPR.getMpAddress());
            
            request.setReadType(new SafetyNumber<Integer>(
                    BlueConstant.ReadType.BASIC, 
                    -BlueConstant.ReadType.BASIC));
            
            request.setStartHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MIN_HANDLE, 
                    -BlueConstant.HANDLE.MIN_HANDLE));
            
            request.setEndHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MAX_HANDLE, 
                    -BlueConstant.HANDLE.MAX_HANDLE));
             
            request.setUUID16(new SafetyNumber<Integer>(UUID16.IDD_FEATURES,-UUID16.IDD_FEATURES));
            
            request.setUUID128(new SafetyByteArray(UUID.UUID128.BLANK, CRCTool
                    .generateCRC16(UUID.UUID128.BLANK)));
            
            request.setReadOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));
            
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.BT_ATTR_READ, this);
            
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
// (R15818 2015-08-31 04:11:59 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
