/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.ReadDeviceInfo
 * Brief: This class handles the ReadDeviceInfo process and its responses sequence.
 *
 * Create Date: 2015/7/22
 * $Revision: 23935 $
 * $Author: KiddYeh $
 * $Id: ReadDeviceInfo.java 23935 2015-11-12 06:17:00Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.util.Arrays;

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
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeReadRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeReadResponse;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * This class handles the ReadDeviceInfo process and its responses sequence.
 */
public class ReadDeviceInfo implements IBLERequest
{
    private static final String TAG = "ReadDeviceInfo";
    
    /**
     *  The manufacturer name "Roche" 
     */
    private final byte[] MANUFACTURER_NAME = {0x52, 0x6f, 0x63, 0x68,0x65};
    /**
     * The instance of ReadDeviceInfo class
     */
    private static volatile ReadDeviceInfo mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * The response receiver of ManufacturerName
     */
    private ISubResponseReceiver mReceiver = new ManufacturerName();
    
    /**
     * The response receiver of ModelName
     */
    private ModelNumber mModelName = new ModelNumber();
    
    /**
     * The response receiver of SystemID
     */
    private SystemID mSystemID = new SystemID(); 
    
//    private SerialNumber mSerialNumber = new SerialNumber();
//    private HardwareVersion mHardwareVersion = new HardwareVersion();
//    private SoftwareVersion mSoftwareVersion = new SoftwareVersion();
//    private JSONObject mDeviceInfo = new JSONObject();
    
    /**
     * 
     * The response receiver interface of ReadDeviceInfo 
     *
     */
    interface ISubResponseReceiver
    {
        void onSubResponseReceived(Context context, Intent intent);
    }
    
    
    /**
     * 
     * 
     * Set the certain receiver to handle the response from the broadcast.
     *
     * @param receiver: the certain receiver to handle the specific response.
     *            Range: A valid object of ISubResponseReceiver
     *            Unit: ISubResponseReceiver
     *            Scaling: 1
     * 
     * @return void [out] 
     * 
     * @see mReceiver
     * 
     * @see mReceiver: use this global variable to store the receiver of the
     *      class ISubResponseReceiver
     */
    public void setReceiver(ISubResponseReceiver receiver)
    {
        mReceiver = receiver;
    }
    
    
    /**
     * 
     * 
     * Get the one and only instance of the class ReadDeviceInfo.
     *
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class ReadDeviceInfo
     *         Range: A valid object of ReadDeviceInfo
     *         Unit: ReadDeviceInfo
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class ReadDeviceInfo
     */
    public static ReadDeviceInfo getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new ReadDeviceInfo(context);
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
    private ReadDeviceInfo(Context context)
    {
        mContext = context;
    }

    /**
     * 
     * This method dispatches the response to the certain receiver. 
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: a valid object
     *            Unit: Intent
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mReceiver
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
        mReceiver.onSubResponseReceived(context, intent);
    }
      
    
    private class ManufacturerName implements ISubResponseReceiver
    {
        /**
         * 
         * Get the response from the "RESPONSEPACK" in "CustFrameworkService".
         * It checks Manufacture Name is Roche or not. 
         * If the result is OK, it requests the UUID_MODEL. Otherwise it returns
         * failed via callback function. 
         * 
         * @param context: The Context in which the receiver is running.
         *            Range: a valid object
         *            Unit: Context
         *            Scaling: 1
         * @param intent: The Intent being received.
         *            Range: a valid object
         *            Unit: Intent
         *            Scaling: 1
         *            
         * @return void [out] 
         */
        @Override
        public void onSubResponseReceived(Context context, Intent intent)
        {
            Debug.printD(TAG, "[doProcess]: ManufacturerName ");
            
            ResponsePack pack = intent
                    .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
            
            AttributeReadResponse response= (AttributeReadResponse) pack
                    .getResponse();

            int result = response.getResult().get();
            byte[] message = response.getMessage();
            byte[] manufacturename = response.getData().getByteArray();
            
            if ( CommsConstant.Result.RESULT_OK == result )
            {
                            
                Debug.printD(TAG, "Get ManufacturerName OK!");
                Debug.printD(TAG, "++++++++++++++++++++");
                Debug.printD(TAG, "message");
                Debug.dumpPayload(TAG, message);
                Debug.printD(TAG, "Command =" + response.getCommand().get());
                Debug.printD(TAG, "RemoteBD =" );
                Debug.dumpPayload(TAG, response.getRemoteBD().getByteArray());
                Debug.printD(TAG, "Result =" + response.getResult().get());
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
                  
                if(Arrays.equals(manufacturename, MANUFACTURER_NAME))
                {
                    Debug.printD(TAG, "Get ManufacturerName OK!");
                    setReceiver(mModelName);
                    readDeviceInfo(BlueConstant.DeviceInfo.UUID_MODEL, mCallback);
                }
                else
                {
                    Debug.printD(TAG, "Get ManufacturerName error!");
                    returnResult(SafetyBoolean.FALSE); 
                }
            }
            else
            {
                Debug.printD(TAG,"Get ManufacturerName error!");
                returnResult(SafetyBoolean.FALSE); 

            }   
        }
    }
    
    
    private class ModelNumber implements ISubResponseReceiver
    {
        /**
         * 
         * 
         * Get the response from the "RESPONSEPACK" in "CustFrameworkService"
         * It checks Model Name is 775 or not. 
         * If the result is OK, it requests the SystemID. Otherwise it returns
         * failed via callback function. 
         * 
         * @param context: The Context in which the receiver is running.
         *            Range: a valid object
         *            Unit: Context
         *            Scaling: 1
         * @param intent: The Intent being received.
         *            Range: a valid object
         *            Unit: Intent
         *            Scaling: 1
         *            
         * @return void [out]
         */
        @Override
        public void onSubResponseReceived(Context context, Intent intent)
        {
            Debug.printD(TAG, "[doProcess]: ModelNumber ");
            
            ResponsePack pack = intent
                    .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
            
            AttributeReadResponse response= (AttributeReadResponse) pack
                    .getResponse();
            
            int result = response.getResult().get();
            
            
            
            int mpModelNumber = ReadConfig.getIntegerDataByKey(
                    new SafetyString(ConfigParameter.KEY_MP_MODEL_NAME, CRCTool
                            .generateCRC16(ConfigParameter.KEY_MP_MODEL_NAME
                                    .getBytes()))).get();
            
            
            if ( CommsConstant.Result.RESULT_OK == result )
            {
                String modelNumber = new String(response.getData().getByteArray());
                
                if(modelNumber.equalsIgnoreCase(String.valueOf(mpModelNumber)))    
                {
                    Debug.printD(TAG, "Get ModelNumber OK!");
                    setReceiver(mSystemID);
                    readDeviceInfo(BlueConstant.DeviceInfo.UUID_SYSID, mCallback);
                }
                else
                {
                    Debug.printD(TAG, "Get ModelNumber error!");
                    returnResult(SafetyBoolean.FALSE); 
                }
            }
            else
            {
                Debug.printD(TAG, "Get ModelNumber error!");
                returnResult(SafetyBoolean.FALSE); 

            }   
        }
    }
    
    
    private class SystemID implements ISubResponseReceiver
    {
        /**
         * 
         * 
         * Get the response from the "RESPONSEPACK" in "CustFrameworkService".
         * 
         * @param context: The Context in which the receiver is running.
         *            Range: a valid object
         *            Unit: Context
         *            Scaling: 1
         * @param intent: The Intent being received.
         *            Range: a valid object
         *            Unit: Intent
         *            Scaling: 1
         *            
         * @return void [out]
         */
        @Override
        public void onSubResponseReceived(Context context, Intent intent)
        {
            final int MANUFACTURER_IDENTIFER_PATTERN = 0xFFFE;
            
            Debug.printD(TAG, "[doProcess]: SystemID ");
            
            ResponsePack pack = intent
                    .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
            
            AttributeReadResponse response= (AttributeReadResponse) pack
                    .getResponse();
            
//            int rocheOUI = ReadConfig.getIntegerDataByKey(
//                    new SafetyString(ConfigParameter.KEY_BLE_ROCHE_OUI, CRCTool
//                            .generateCRC16(ConfigParameter.KEY_BLE_ROCHE_OUI
//                                    .getBytes()))).get();
//            
//            Debug.printD(TAG, "OUI");
//            Debug.dumpPayload(TAG, ByteBuffer.allocate(4).putInt(rocheOUI).array());
//            
//            byte[] compareOUI = {0, 0, 0, (byte) MANUFACTURER_IDENTIFER_PATTERN,(byte) (MANUFACTURER_IDENTIFER_PATTERN >> 8)};
//            
//            ByteBuffer tempBuffer = ByteBuffer.allocate(4).putInt(rocheOUI);
//            
//            tempBuffer.position(0);
//            tempBuffer.get();
//            tempBuffer.get(compareOUI, 0, 3);
            
            SafetyBoolean isResult = SafetyBoolean.FALSE;
            
            int result = response.getResult().get();
            
            if ( CommsConstant.Result.RESULT_OK == result )
            {       
                Debug.printD(TAG, "Get SystemID OK!");
                Debug.printD(TAG, "++++++++++++++++++++");  
                Debug.dumpPayload(TAG, response.getData().getByteArray());
//                Debug.dumpPayload(TAG, compareOUI);
                Debug.printD(TAG, "++++++++++++++++++++"); 
                
//                setReceiver(mSerialNumber);
//                readDeviceInfo(BlueConstant.DeviceInfo.UUID_SERIAL, mCallback);  
                isResult = SafetyBoolean.TRUE;
                
            }
            else
            {
                Debug.printD(TAG, "Get SystemID error!");
                returnResult(SafetyBoolean.FALSE); 
            }   
            returnResult(isResult);  
        }
    }

    
//    private class SerialNumber implements ISubResponseReceiver
//    {
//        /**
//         * Get the response from the "RESPONSEPACK" in "CustFrameworkService".
//         * 
//         * @param context: The Context in which the receiver is running.
//         *            Range: a valid object
//         *            Unit: Context
//         *            Scaling: 1
//         * @param intent: The Intent being received.
//         *            Range: a valid object
//         *            Unit: Intent
//         *            Scaling: 1
//         *            
//         * @return void [out]
//         */
//        @Override
//        public void onSubResponseReceived(Context context, Intent intent)
//        {
//            Debug.printD(TAG, "[doProcess]: SerialNumber ");
//            
//            ResponsePack pack = intent
//                    .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
//            
//            AttributeReadResponse response= (AttributeReadResponse) pack
//                    .getResponse();
//            
//            int result = response.getResult().get();
//            
//            if ( CommsConstant.Result.RESULT_OK == result )
//            {     
//                setReceiver(mHardwareVersion);
//                readDeviceInfo(BlueConstant.DeviceInfo.UUID_HW, mCallback);
//                Debug.printD(TAG, "Get SerialNumber OK!");
//                Debug.printD(TAG, "++++++++++++++++++++");  
//                
//                try
//                {
//                    mDeviceInfo.put(BTLEConstants.KEY_SERIAL_NUMBER, new String(response.getData().getByteArray()));
//                }
//                catch (JSONException e)
//                {
//                    e.printStackTrace();
//                }
//                finally
//                {
//                    // Apply to coding standard.
//                }
//                
//                Debug.dumpPayload(TAG, response.getData().getByteArray());
//                Debug.printD(TAG, "++++++++++++++++++++"); 
//            }
//            else
//            {
//                Debug.printD(TAG, "Get SerialNumber error!");
//                
//                returnResult(SafetyBoolean.FALSE); 
//
//            }   
//        }
//    }
//    
//    
//    private class HardwareVersion implements ISubResponseReceiver
//    {
//        /**
//         * Get the response from the "RESPONSEPACK" in "CustFrameworkService".
//         * 
//         * @param context: The Context in which the receiver is running.
//         *            Range: a valid object
//         *            Unit: Context
//         *            Scaling: 1
//         * @param intent: The Intent being received.
//         *            Range: a valid object
//         *            Unit: Intent
//         *            Scaling: 1
//         *            
//         * @return N/A
//         */
//        @Override
//        public void onSubResponseReceived(Context context, Intent intent)
//        {
//            Debug.printD(TAG, "[doProcess]: HardwareVersion ");
//            
//            ResponsePack pack = intent
//                    .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
//            
//            AttributeReadResponse response= (AttributeReadResponse) pack
//                    .getResponse();
//            
//            int result = response.getResult().get();
//            
//            if ( CommsConstant.Result.RESULT_OK == result )
//            {      
//                setReceiver(mSoftwareVersion);
//                readDeviceInfo(BlueConstant.DeviceInfo.UUID_SW, mCallback);
//                Debug.printD(TAG, "Get HardwareVersion OK!");
//                Debug.printD(TAG, "++++++++++++++++++++");  
//                
//                try
//                {
//                    mDeviceInfo.put(BTLEConstants.KEY_HW_VERSION, new String(response.getData().getByteArray()));
//                }
//                catch (JSONException e)
//                {
//                    e.printStackTrace();
//                }
//                finally
//                {
//                    // Apply to coding standard.
//                }
//                
//                Debug.dumpPayload(TAG, response.getData().getByteArray());
//                Debug.printD(TAG, "++++++++++++++++++++"); 
//            }
//            else
//            {
//                Debug.printD(TAG, "Get HardwareVersion error!");
//                
//                returnResult(SafetyBoolean.FALSE); 
//            }   
//        }
//    }
//    
//    
//    private class SoftwareVersion implements ISubResponseReceiver
//    {
//        /**
//         * Get the response from the "RESPONSEPACK" in "CustFrameworkService".
//         * 
//         * @param context: The Context in which the receiver is running.
//         *            Range: a valid object
//         *            Unit: Context
//         *            Scaling: 1
//         * @param intent: The Intent being received.
//         *            Range: a valid object
//         *            Unit: Intent
//         *            Scaling: 1
//         *            
//         * @return @return void [out]
//         */
//        @Override
//        public void onSubResponseReceived(Context context, Intent intent)
//        {
//            Debug.printD(TAG, "[doProcess]: SoftwareVersion ");
//            
//            ResponsePack pack = intent
//                    .getParcelableExtra(ResponseAction.EXTRA_RESPONSEPACK);
//            
//            AttributeReadResponse response= (AttributeReadResponse) pack
//                    .getResponse();
//            SafetyBoolean isResult = SafetyBoolean.FALSE;
//            int result = response.getResult().get();
//            
//            if ( CommsConstant.Result.RESULT_OK == result )
//            {      
//                Debug.printD(TAG, "Get SoftwareVersion OK!");
//                Debug.printD(TAG, "++++++++++++++++++++");  
//                
//                try
//                {
//                    mDeviceInfo.put(BTLEConstants.KEY_SW_VERSION, 
//                            new String(response.getData().getByteArray()));
//                }
//                catch (JSONException e)
//                {
//                    e.printStackTrace();
//                }
//                finally
//                {
//                    // Apply to coding standard.
//                }
//                
//                Debug.dumpPayload(TAG, response.getData().getByteArray());
//                Debug.printD(TAG, "++++++++++++++++++++"); 
//                
//                NugenGeneralModel.setString(mContext, BTLEConstants.KEY_PUMP_DIS, 
//                        new SafetyString(mDeviceInfo.toString(), 
//                                CRCTool.generateCRC16(mDeviceInfo.toString().getBytes())));
//                
//                BLEResponseReceiver.getInstance(context).unregisterReceiver();
//              
//                isResult = SafetyBoolean.TRUE; 
//            }
//            else
//            {
//                Debug.printD(TAG, "Get SoftwareVersion error!");
//            } 
//            returnResult(isResult);
//        }
//    }
    
    
    /**
     * 
     * This method requests the ManufacturerName.
     * 
     * @param callback [in] the callback function of response     
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *            
     * @return void [out] 
     * 
     */
    public void readAllDeviceInfo(ResponseCallback callback)
    {
        setReceiver(new ManufacturerName());
        readDeviceInfo(BlueConstant.DeviceInfo.UUID_MFG, callback);
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
     * 
     * 
     * Send request pack to subsystem to read the device information.
     * 
     * @param uuid16Code: the UUID code
     *            Range: -2^31 to (2^31)-1
     *            Unit: integer
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
     * 
     */
    private void readDeviceInfo(int uuid16Code, ResponseCallback callback)
    {
        Debug.printD(TAG, "[ReadDeviceInfo]: Request enter ");
        
        mCallback = callback;

        AttributeReadRequest request = (AttributeReadRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_READ);
        
        if ( null != request )
        {
            request.setRemoteBD(GlobalTools.MPR.getMpAddress());
            
            request.setReadType(new SafetyNumber<Integer>(
                    BlueConstant.WriteType.COMMAND, 
                    -BlueConstant.WriteType.COMMAND));
            
            request.setStartHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MIN_HANDLE, 
                    -BlueConstant.HANDLE.MIN_HANDLE));
            
            request.setEndHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MAX_HANDLE, 
                    -BlueConstant.HANDLE.MAX_HANDLE));
             
            request.setUUID16(new SafetyNumber<Integer>(uuid16Code,-uuid16Code));
            
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
    
    
    /**
     * 
     * 
     * This method requests all Device Informations. 
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
     * @return N/A
     * 
     * @see mContext
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        readAllDeviceInfo(callback);
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
