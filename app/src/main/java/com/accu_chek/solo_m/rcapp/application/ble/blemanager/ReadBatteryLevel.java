/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.ReadBatteryLevel
 * Brief: This class handles the ReadBatteryLevel process and its responses sequence.
 *
 * Create Date: 2015/7/22
 * $Revision: 22446 $
 * $Author: KiddYeh $
 * $Id: ReadBatteryLevel.java 22446 2015-10-23 09:03:35Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;
import android.util.Log;

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
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.PowerManagerConstants;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * This class handles ReadBatteryLevel request and its response.
 *
 */
public class ReadBatteryLevel implements IBLERequest
{
    private static final String TAG = "ReadBatteryLevel";
    
    /**
     * The instance of ReadBatteryLevel class
     */
    private static volatile ReadBatteryLevel mInstance = null;
    
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
     * Get the one and only instance of the class ReadBatteryLevel.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class ReadBatteryLevel
     *         Range: A valid object of ReadBatteryLevel
     *         Unit: ReadBatteryLevel
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class ReadBatteryLevel
     */
    public static ReadBatteryLevel getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new ReadBatteryLevel(context);
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
     * @param context:  an application context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mContext             
     *            
     */
    private ReadBatteryLevel(Context context)
    {
        mContext = context;
    }

    
    /**
     * 
     * This method shall the get cause of response to check if the ReadBatteryLevel successfull or not. 
     * If the callback function exists, it returns the result of response via callback function .
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
        Debug.printD(TAG, "[doProcess]: ReadBatteryLevel ");
        
        ResponsePack pack = intent
                .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
        
        AttributeReadResponse response= (AttributeReadResponse) pack
                .getResponse();
        
        byte[] data = response.getData().getByteArray();
        
        int result = response.getResult().get();
        
        SafetyBoolean isResult = SafetyBoolean.FALSE;
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        if (CommsConstant.Result.RESULT_OK == result)
        {
            
            SafetyNumber<Integer> batteryLevel = new SafetyNumber<Integer>((int) (data[0]),-(data[0]));

            NugenGeneralModel.setInt(mContext, PowerManagerConstants.PUMP_BATTRY_STATUS, batteryLevel);
            
//          SafetyNumber<Integer> safePumpBatteryStatus = NugenGeneralModel.getInt(mContext, PowerManagerConstants.PUMP_BATTRY_STATUS);
//          int pumpBatteryStatus = safePumpBatteryStatus.get();
//          Debug.printD(TAG, " pump battery status: " + pumpBatteryStatus);
            
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
     * This method handles the ReadBatteryLevel request. 
     * 
     * @param parameter [in] the request parameter
     *            Range: a valid object of BLERequestParameter or null object
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the response callback     
     *            Range: a valid object of ResponseCallback or null object
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
        Debug.printD(TAG, "[ReadBatteryLevel]: Request enter ");
        
        AttributeReadRequest request = (AttributeReadRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_READ);

        mCallback = callback;
        
        byte[] btAddress = BLEController.getBDAddress(mContext);
        
        Log.i(TAG, "" + btAddress.length);

        
        if ( null != request ) 
        {
            request.setRemoteBD(new SafetyByteArray(btAddress,CRCTool.generateCRC16(btAddress)));
            
            request.setReadType(new SafetyNumber<Integer>(
                    BlueConstant.WriteType.COMMAND, 
                    -BlueConstant.WriteType.COMMAND));
            
            request.setStartHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MIN_HANDLE, 
                    -BlueConstant.HANDLE.MIN_HANDLE));
            
            request.setEndHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MAX_HANDLE, 
                    -BlueConstant.HANDLE.MAX_HANDLE));
             
            request.setUUID16(new SafetyNumber<Integer>(UUID16.BATTERY_LEVEL,-UUID16.BATTERY_LEVEL));
            
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
// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
