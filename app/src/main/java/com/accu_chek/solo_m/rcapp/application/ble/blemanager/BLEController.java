/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.BLEController
 * Brief: The class handles the interface of BLEController
 *
 * Create Date: 2015/7/21
 * $Revision: 25192 $
 * $Author: KiddYeh $
 * $Id: BLEController.java 25192 2015-12-01 02:34:08Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.util.Arrays;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.basal.comms.BLEACT71;
import com.accu_chek.solo_m.rcapp.application.basal.comms.BLEACT_CancelTbrAdj;
import com.accu_chek.solo_m.rcapp.application.basal.comms.BLEACT_ReadIDDDeviceStatus;
import com.accu_chek.solo_m.rcapp.application.basal.comms.BLEACT_SetTbrAdj;
import com.accu_chek.solo_m.rcapp.application.basal.comms.BLESEQ75;
import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.ble.RequestPack;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ScanInfo.IScanInfoListener;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.OpCode;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEACT10;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEACT51;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEACT52;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEACT53;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEACT54;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEACT55;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEACT56;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLECallbackContext;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEKeyExchangeProcedure;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEStartUpState;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEUC01;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.util.BTUtil;

public class BLEController
{
    private static final String TAG = "BLEController";
    
    public static final String BONDED_ADDRESS = "bonded_device_address";
    public static final String CONNECTION_STATE = "connection_state";
    
    public static final int BYTES_OF_INT16 = 2;
    
    private static volatile BLEController mInstance = null;
    private BLERequestHandler mRequestHandler = null;
    private BLECallbackContext mCallbackContext = null;
    
    public interface ResponseProcess
    {
        /**
         * Perform action according to different responses.
         * 
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
         */
        void doProcess(Context context, Intent intent);
    }
    
//    public interface IOnBondStateChangedListener
//    {
//        /**
//         * This method notifies the change of Bonding status
//         * 
//         * 
//         * @param isBonded: the bonding status
//         *            Range: a valid object of SafetyBoolean
//         *            Unit: SafetyBoolean
//         *            Scaling: 1
//         *            
//         * @return void [out]             
//         *            
//         */
//        void onBondStateChanged(SafetyBoolean isBonded);
//    } 
    
    public interface ResponseCallback
    {
        /**
         * This method notifies the result of the request status
         * 
         * 
         * @param result: the result of the request
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return void [out]             
         *            
         */
        void onRequestCompleted(SafetyBoolean result);
    }
    
    public interface ResponseCallbackWithData extends ResponseCallback
    {
        /**
         * This method notifies the result of the request status with response pack
         * 
         * 
         * @param result: the result of the request
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         * 
         * @param pack: the response data of the request
         *            Range: a valid object of ResponsePack
         *            Unit: ResponsePack
         *            Scaling: 1
         *            
         * @return void [out]             
         *            
         */
        void onRequestCompleted(SafetyBoolean result, ResponsePack pack);
    }
    /**
     * The class constructor
     * 
     * @param context :
     *         Range: A valid object of Context
     *         Unit: Context
     *         Scaling: 1 
     * 
     * @return void [out] 
     * 
     * @see mCallbackContext
     * @see mRequestHandler
     * 
     */
    private BLEController(Context context)
    {
        mCallbackContext = new BLECallbackContext(context);
        mRequestHandler = new BLERequestHandler(context, mCallbackContext);
        mCallbackContext.setBLERequestHandler(mRequestHandler);
      
        mRequestHandler.init();
    }
    
    
    /**
     * Get the one and only instance of the class BLEController.
     * 
     * @param context :
     *         Range: A valid object of Context
     *         Unit: Context
     *         Scaling: 1 
     * 
     * @return BLEController : the one and only instance of the class BLEController
     *         Range: A valid object of BLEController
     *         Unit: BLEController
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class BLEController
     */
    public static BLEController getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new BLEController(context);
        }
        else
        {
            // Apply to the coding standard
        }
        
        return mInstance;
    }
    
    
    public static BLEController getInstance()
    {
        return mInstance;
    }
    
    
    /**
     * 
     * This function checks that the BONED_ADDRESS is empty or not to 
     * determine that RC is bonded or not. 
     * 
     *
     * @param N/A
     *
     * @return result : the result is bonded or not
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1 
     *            
     * @see BONDED_ADDRESS            
     * @see mRequestHandler           
     */
    public SafetyBoolean isBonded()
    {
        Context context = mRequestHandler.getContext();
        SafetyBoolean result = SafetyBoolean.FALSE;
        SafetyString address = NugenGeneralModel.getString(context, BONDED_ADDRESS);

        if (null != address)
        {
            result = SafetyBoolean.TRUE;
        }
        else
        {
            result = SafetyBoolean.FALSE;
        }

        Debug.printD("kidd","isBonded xx"+ result);
        return result;
    }
    
    /**
     * 
     * This function checks the CONNECTION_STATE determine that MP is connected or not.
     *
     * @param N/A
     *
     * @return result : the result is bonded or not
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1 
     *            
     * @see CONNECTION_STATE           
     */
    public SafetyBoolean isConnected()
    {
        Context context = mRequestHandler.getContext();
        SafetyBoolean result = NugenGeneralModel.getSafetyBoolean(context, BLEController.CONNECTION_STATE, SafetyBoolean.FALSE);
        Debug.printD("kidd","isConnected xx"+ result);
        return result;
    }
    
    /**
     * This method is triggered by receiving the CommsReadyIndication. It sets the current
     * state to BLEStartUpState.  
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void startup(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEStartUpState(mCallbackContext));
    }
    
    /**
     * This method is triggered by receiving the SyncDateTimeIndication. It sets the current
     * state to BLEACT54.  
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void syncMicroPumpTime(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT54(mCallbackContext, callback));
//    	SyncDateTime.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
      
    
    public void syncEMWR(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT52(mCallbackContext, callback));
    }
    
    public void syncDevcieStatus(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT51(mCallbackContext, callback));
    }
    /**
     * This method is triggered by receiving the updateSoloMConfigIndication. It sets the current
     * state to BLEACT53.  
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void updateSoloMConfig(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT53(mCallbackContext, callback));
    }
    
    /**
     * This method handles the bond request from UI module. It sets the current
     * state to BLE_UC01.  
     * 
     * @param listener [in] 
     *            Range: a valid object of IOnBondStateChangedListener
     *            Unit: IOnBondStateChangedListener
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void bond(ResponseCallback callback)
    {        
        mCallbackContext.setCurrentState(new BLEUC01(mCallbackContext, callback));
    }
    
    
    /**
     * This method handles the connect request from other modules. It sets the current
     * state to BLE_ACT01+BLE_ACT10.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void connect(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT10(mCallbackContext, callback));
    }
    
    
    public void exchangeKey(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEKeyExchangeProcedure(mCallbackContext, callback));
    }
    

    /**
     * This method handles the SoloMCP request from other modules. It calls the SendSoloMCP
     * request.   
     * 
     * @param parameter [in] the parameter of request (UUID,Opcode,....)
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void sendSoloMCP( BLERequestParameter parameter,ResponseCallback callback)
    {           
        SendSoloMCP.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    
    /**
     * This method handles the ReadIDDDeviceStatus request from other modules. It calls the ReadIDDDeviceStatus
     * request.   
     * 
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void readIDDDeviceStatus(ResponseCallbackWithData callback)
    {
    	ReadIDDDeviceStatus.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    /**
     * This method handles the SendIDDCommandCP request from other modules. It calls the SendIDDCommandCP
     * request.   
     * 
     * @param parameter [in] the parameter of request (UUID,Opcode,....)
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void sendIDDCommandCP( BLERequestParameter parameter,ResponseCallback callback)
    {           
        SendIDDCommandCP.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    
    /**
     * This method handles the StartStopPriming request from other modules. It calls the StartStopPriming
     * request.   
     * 
     * @param parameter [in] the parameter of request (UUID,Opcode,....)
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void startStopPriming( BLERequestParameter parameter,ResponseCallback callback)
    {           
        StartStopPriming.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
     
    /**
     * This method handles the IDDStatusReader request from other modules. It calls the SendIDDStatusReader
     * request.   
     * 
     * @param parameter [in] the parameter of request (UUID,Opcode,....)
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void sendIDDStatusReaderCP( BLERequestParameter parameter,ResponseCallback callback)
    {           
        SendIDDStatusReaderCP.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    /**
     * This method handles the Confirmation request from other modules. It calls the GeneralConfirmation
     * request.   
     * 
     * @param parameter [in] the parameter of request 
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void sendGeneralConfirm( BLERequestParameter parameter,ResponseCallback callback)
    {           
        GeneralConfirmation.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    /**
     * This method handles the SendRequest request from other modules. It calls the SendRequest
     * request.   
     * 
     * @param parameter [in] the parameter of request 
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCallbackContext
     */
    public void sendRequest( BLERequestParameter parameter,ResponseCallback callback)
    {           
        SendRequest.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    /**
     * This method handles the scan request from other modules. It sets scan mode
     * and calls the scan request.   
     * 
     * @param mode [in] enable or disable
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1 
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * 
     * @see mRequestHandler
     */
    public void scan( SafetyBoolean mode, ResponseCallback callback)
    {
        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setIsEnable(mode);
        Scan.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    
    /**
     * This method handles the removeBonding request from other modules. It calls 
     * the removeBonding request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void removeBonding( ResponseCallback callback)
    {
    
        RemoveBonding.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    
    /**
     * This method handles the prePowerOffComms request from other modules. It calls 
     * the prePowerOffComms request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void prePowerOffComms(ResponseCallback callback)
    {
        PrePowerOffComms.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    
    /**
     * This method handles the readBatteryLevel request from other modules. It calls 
     * the ReadBatteryLevel request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void readBatteryLevel(ResponseCallback callback)
    {
        ReadBatteryLevel.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    /**
     * This method handles the resetConnection request from other modules. It calls 
     * the ResetConnection request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void resetConnection(ResponseCallback callback)
    {
        ResetConnection.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    /**
     * This method handles the setMPFlightMode request from other modules. It calls 
     * the setFlightMode request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void setMPFlightMode(ResponseCallback callback)
    {
    	final int SHIFT8 = 0x08;
        BLERequestParameter parameter = new BLERequestParameter();
        byte[] data = {(byte)OpCode.IDSCMD_SET_FLIGHT,(byte)(OpCode.IDSCMD_SET_FLIGHT >> SHIFT8)};
        parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
        SetMPFlightMode.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    
    /**
     * This method handles the setFlightMode request from other modules. It calls 
     * the setFlightMode request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void setFlightMode(SafetyBoolean mode, ResponseCallback callback)
    {
        BLERequestParameter Parameter = new BLERequestParameter();
        Parameter.setIsEnable(mode);
        SetFlightMode.getInstance(mRequestHandler.getContext()).request(Parameter,callback);
    }
    
    
    /**
     * This method handles the setFlightModeStatus request from other modules. 
     * It calls the setFlightModeStatus request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void setFlightModeStatus(SafetyBoolean mode)
    {
        BLERequestParameter Parameter = new BLERequestParameter();
        Parameter.setIsEnable(mode);
        SetFlightModeStatus.getInstance(mRequestHandler.getContext()).request(Parameter,null);
    }
    
    
    /**
     * This method handles the setMDIMode request from other modules. 
     * It calls the setMDIMode request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void setMDIMode( SafetyBoolean mode, ResponseCallback callback)
    {
        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setIsEnable(mode);
        SetMDIMode.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    
    /**
     * This method handles the setSystemSync request from other modules. 
     * It calls the setSystemSync request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void setSystemSync(ResponseCallback callback)
    {
        SetSystemSync.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    
    /**
     * This method handles the getErrorLog request from other modules. 
     * It calls the getErrorLog request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void getErrorLog(ResponseCallback callback)
    {
        GetErrorLog.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    
    /**
     * This method handles the updateUIState request from other modules. 
     * It calls the updateUIState request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void updateUIState(SafetyNumber<Integer> state, ResponseCallback callback)
    {
        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setParameter(state.get());
        UpdateUIState.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    
    /**
     * This method handles the makeBeep request from other modules. 
     * It calls the makeBeep request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void makeBeep(ResponseCallback callback)
    {
        MakeBeep.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    
    /**
     * This method handles the SetTimeStamp request from other modules. 
     * It calls the SetTimeStamp request.   
     *
     * @param parameter [in] the parameter of request (UUID,Opcode,....)
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void setTimeStamp(BLERequestParameter parameter, ResponseCallback callback)
    {
        SetTimeStamp.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    
    /**
     * This method handles the setTherapyState request from other modules. 
     * It calls the SetTimeStamp request.   
     *
     * @param parameter [in] the parameter of request (UUID,Opcode,....)
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void setTherapyState(BLERequestParameter parameter, ResponseCallback callback)
    {
        SetTherapyState.getInstance(mRequestHandler.getContext()).request(parameter,callback);
    }
    
    /**
     * This method handles the disconnect request from other modules. 
     * It calls the disconnect request.   
     * 
     * @param callback [in] the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                      
     * @return void [out] 
     * 
     * @see mRequestHandler
     */
    public void disconnect(SafetyByteArray address, ResponseCallback callback)
    {
        Disconnect.getInstance(mRequestHandler.getContext()).request(null,callback);
    }
    
    public void syncBasalRate(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT55(mCallbackContext, callback));
    }
    
    public void syncBolus(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT56(mCallbackContext, callback));
    }
    
    public void BLEACT71(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT71(mCallbackContext, callback));
    }
    
    public void BLEACT_SetTbrAdj(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT_SetTbrAdj(mCallbackContext, callback));
    }
    
    public void BLEACT_CancelTbrAdj(ResponseCallback callback)
    {
        mCallbackContext.setCurrentState(new BLEACT_CancelTbrAdj(mCallbackContext, callback));
    }
    
    public void BLEACT_ReadIDDDeviceStatus(ResponseCallbackWithData callback)
    {
        mCallbackContext.setCurrentState(new BLEACT_ReadIDDDeviceStatus(mCallbackContext, callback));
    } 
    
    public void BLE_SEQ75(ResponseCallbackWithData callback)
    {
        mCallbackContext.setCurrentState(new BLESEQ75(mCallbackContext, callback));
    }    
    
    /**
     * 
     * This method registers the ScanInfo listener
     * 
     * @param scanInfoCB: the listener of scan information
     *            Range: a valid object of IScanInfoListener
     *            Unit: IScanInfoListener
     *            Scaling: 1
     *                      
     * @return void [out] 
     */
    public static void setScanInfoListener(IScanInfoListener scanInfoCB)
    {
        ScanInfo scanInfo = ScanInfo.getInstance();
        scanInfo.registerCallBack(scanInfoCB);
    }
    
    
    /**
     * 
     * This method wraps the request to SafetyByteArray and sends it to the
     * UICommandDispatcher module. 
     *
     * @param context: the context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param request: the request
     *            Range:a valid object of IRequest
     *            Unit: IRequest
     *            Scaling: 1
     *                      
     * @return void [out] 
     */
    public static void sendRequestToComms(Context context, IRequest request)
    {
        SafetyByteArray safeByteArrs = null;
        RequestPack pack = new RequestPack();
        pack.setRequest(request);
        safeByteArrs = BTUtil.wrapperRequest(pack);
        
        BTUtil.sendPackToSubsystem(context, safeByteArrs);
    }
    
    
    /**
     * This method stores BT address in shared preference
     * 
     * @param context: the Context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param remoteBD: the address
     *            Range: a valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     *            
     * @return void [out]  
     * 
     * @see BONDED_ADDRESS         
     */
    public static void storeBDAddress(Context context, byte[] remoteBD)
    {
        final byte MASK = (byte) 0xFF;
        SafetyString sfRemoteBDAddress = null;
        StringBuilder builder = new StringBuilder();
        
        for (byte each : remoteBD)
        {
            builder.append(String.valueOf((int)(each & MASK))).append(",");
        }
        
        sfRemoteBDAddress = new SafetyString(builder.toString(), 
                CRCTool.generateCRC16(builder.toString().getBytes()));
        
        NugenGeneralModel.setString(context, BLEController.BONDED_ADDRESS, 
                sfRemoteBDAddress);
    }
    
    
    /**
     * This method gets BD address in shared preference.
     * 
     * @return byte[]: the BD address or empty 
     *            Range: a valid object of byte[] 
     *            Unit: byte[]
     *            Scaling: 1
     * 
     * @see mContext
     * @see BONDED_ADDRESS 
     */
    public static byte[] getBDAddress(Context context)
    {
        final byte MASK = (byte) 0xFF;
        ByteArrayBuffer retByteArray = new ByteArrayBuffer(0);
        CommonUtils.objectCheck(context);
        
        SafetyString bdaddress = NugenGeneralModel.getString(context, 
                BLEController.BONDED_ADDRESS);
        
        if (null != bdaddress)
        {
            for(String temp : bdaddress.getString().split(","))
            {
                if (!temp.isEmpty())
                {
                    retByteArray.append(Byte.parseByte(temp) & MASK);
                }
                else
                {
                    // Apply to the coding standard
                }
            }     
        }
        else
        {
            // Apply to the coding standard
        }
        
        return retByteArray.toByteArray();
    }
    
    
    /**
     * Return the opposite byte order of original data.
     *
     * @param original : The original data.
     *        Range: Null or a valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     * 
     * @return byte[] [out]: The opposite byte order data.
     *         Range: a valid The length is equal to original.
     *         Unit: byte[]
     *         Scaling: 1.
     */
    public static byte[] makeLittleEndian(byte[] original)
    {
        byte[] result = null;
        int length = -1;
        
        CommonUtils.objectCheck(original);
        
        length = original.length;
        
        result = new byte[length];
        
        for (int i=0; i<length; i++)
        {
            result[i] = original[length - i - 1];
        }
        
        return result;
    }
    
    
    /**
     * Parse the 16 bit integer to byte array.
     *
     * @param value : The input integer value.
     *        Range : -2^31 to (2^31)-1.
     *        Unit : Integer.
     *        Scaling : 1.
     * 
     * @see BYTES_OF_INT16 [in]
     * 
     * @return byte[] [out]: The byte array of input value.
     *         Range:a valid object of byte[] with length 2.
     *         Unit: byte[].
     *         Scaling: 1.
     */
    public static byte[] parseInt16(int value)
    {
        byte[] result = null;        
        byte[] valueInBytes = CRCTool.getBytes(value);
        
        valueInBytes = makeLittleEndian(valueInBytes);
        
        result = Arrays.copyOfRange(valueInBytes, 0, BYTES_OF_INT16);
        
        return result;
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
// (R23998 2015-11-12 06:06:24 KiddYeh)
// ----------------------------------------------------------------------------
// [New Feature] Basal Delivery comms functions
// (R24498 2015-11-20 02:33:55 KiddYeh)
// ----------------------------------------------------------------------------
// [Update] Add readIDDDeviceStatus and GetActveBRD for UISD spec change
