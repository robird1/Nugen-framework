/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEStartUpState
 * Brief: This class is used to handle the StartUp process.
 *
 * Create Date: 2015/7/21
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: BLEStartUpState.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GetCommsInfo;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetConfigration;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetFlightModeStatus;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * This class is used to handle the StartUp process.
 *
 */
public class BLEStartUpState extends AbstractBLEStateHandler
{

    private static final String TAG = "BLEStartUpState";

    /**
     *  The general response callback function of request
     */
    private ResponseCallback mRequestDone = new RequestDone();
    
    /**
     *  The response callback function of SetFlightModeStatus
     */
    private ResponseCallback mFlightModeStatusDone = new FlightModeStatusDone();
    
    /**
     * This method adds the BLEUC02 sequence requests in request list of the 
     * request handler and start to execute the request.
     * 
     * @param callbackContext: the ble state handler
     *            Range: a valid object of BLECallbackContext
     *            Unit: BLECallbackContext
     *            Scaling: 1
     * @param callback: the listener of bondstate
     *            Range: a valid object of IOnBondStateChangedListener
     *            Unit: IOnBondStateChangedListener
     *            Scaling: 1           
     * 
     * @return void [out]
     * 
     */
    public BLEStartUpState(BLECallbackContext callbackContext)
    {
        super(callbackContext);
        
        Context context = callbackContext.getBLERequestHandler().getContext();
        
        NugenGeneralModel.setSafetyBoolean(context, BLEController.CONNECTION_STATE, SafetyBoolean.FALSE);
        // add SetConfigration request in list
        callbackContext.getBLERequestHandler().addRequest(
                SetConfigration.getInstance(context), null, mRequestDone);
        // add GetCommsInfo request in list
        callbackContext.getBLERequestHandler().addRequest(
                GetCommsInfo.getInstance(context), null, mRequestDone);
        
        // add SetFlightModeStatus request in list
        BLERequestParameter flightModeStatusParameter = new BLERequestParameter();
        flightModeStatusParameter.setIsEnable(SafetyBoolean.FALSE);
        callbackContext.getBLERequestHandler().addRequest(
                SetFlightModeStatus.getInstance(context), flightModeStatusParameter, mFlightModeStatusDone);

        // start to execute  
        callbackContext.getBLERequestHandler().startHandleRequest();
    }
    
    
    private class RequestDone implements ResponseCallback
    {
        /**
         * This method is called after the response is received.
         * If the result is failed, it disconnects BD. 
         * 
         * 
         * @param result: the result of request
         *            Range: A valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return void [out]            
         *            
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isResultFailed = 
                    (SafetyBoolean.FALSE.getByte() == result.getByte());
            
            if ( isResultFailed )
            {
                Debug.printD(TAG, "[Request failed ] "); 
            }
            else
            {
                Debug.printD(TAG, "[Request Done ] ");                
            }
        }
    }
    
    private class FlightModeStatusDone implements ResponseCallback
    {
        /**
         * This method is called after the SetFlightModeStatus is done.
         * 
         * 
         * @param result: the result of SetFlightModeStatus request.
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return void [out]
         * 
         * @see mCallbackContext
         * 
         *            
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {          
           mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
        }
    }

    /**
     * This method is called after the ConnectStateChanged notification is received.  
     *
     * @param isConnected [in] the BLE-Device is disconnected or not.      
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1  
     *            
     * @return void [out]
     * 
     */
    @Override
    public void onConnectionStateChanged(int state)
    {

    }
    
    
    /**
     * This method is called after Request-Timeout. It calls disconnect-function. 
     *
     * @param request [in] the request     
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1  
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1 
     *                      
     * @return void [out]
     * 
     */
    @Override
    public void onRequestTimeout(IBLERequest request,
            BLERequestParameter parameter)
    {       
        // CMD timeout, then disconnect BT device      

        Debug.printD(TAG, request.getClass().getSimpleName() + " timeout!");
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R15818 2015-08-31 04:11:59 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
