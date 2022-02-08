/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEACT10
 * Brief: BLEACT10
 *
 * Create Date: 2015/7/21
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: BLEACT10.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.Connect;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.Discovery;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.MakeBeep;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEACT10 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEACT10";

    /**
     * The current state callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     *  The response callback function of Disconnect
     */
    private ResponseCallback mDisconnectDone = new DisconnectDone();
    
    /**
     *  The general response callback function of request
     */
    private ResponseCallback mRequestDone = new RequestDone();
    
    /**
     *  The response callback function of IAS
     */
    private ResponseCallback mBeepDone = new BeepDone();
    
    /**
     * This method adds the BLEACT10 sequence requests in request list of the 
     * request handler and start to execute the request.
     * 
     * @param callbackContext: the ble state handler
     *            Range: a valid object of BLECallbackContext
     *            Unit: BLECallbackContext
     *            Scaling: 1
     * @param callback: the listener of the bond state
     *            Range: a valid object of IOnBondStateChangedListener
     *            Unit: IOnBondStateChangedListener
     *            Scaling: 1           
     * 
     * @return void [out]
     * 
     * @see mCallback
     * @see mCallbackContext
     */
    public BLEACT10(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        super(callbackContext);
        
        Context context = callbackContext.getBLERequestHandler().getContext();
        
        mCallback = callback;
        // add connect request in list
        callbackContext.getBLERequestHandler().addRequest(
                Connect.getInstance(context), null, mRequestDone);
        // add discovery request in list
        callbackContext.getBLERequestHandler().addRequest(
                Discovery.getInstance(context), null, mRequestDone);
        // add IAS request in list
        callbackContext.getBLERequestHandler().addRequest(
                MakeBeep.getInstance(context), null, mBeepDone);
        // start to execute  
        callbackContext.getBLERequestHandler().startHandleRequest();
    }
    
  
    private class DisconnectDone implements ResponseCallback
    {
        /**
         * This method is called after the Disconnect process is done. It removes
         * the Pin code from memory.It returns the failure of BLE_Act10 process 
         * via callback function.
         * 
         * 
         * @param result: the result of Disconnect is done or not.
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return void [out]           
         * @see mCallback          
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            GlobalTools.MPR.setPinCode(null);
            mCallback.onRequestCompleted(SafetyBoolean.FALSE);
        }
    }
    
    
    /**
     * This method calls the disconnect function in BLEController.
     * 
     * @param N/A
     * 
     * @return void [out]
     * 
     */
    private void disconnect()
    {
        SafetyByteArray address = GlobalTools.MPR.getMpAddress();
        BLEController instance = BLEController.getInstance(null);
        if (null != instance)
        {
            instance.disconnect(address, mDisconnectDone);
        }
        else
        {
            // Apply to coding standard
        }
          
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
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isResultFailed = 
                    (SafetyBoolean.FALSE.getByte() == result.getByte());
            
            if ( isResultFailed )
            {
                disconnect();
                Debug.printD(TAG, "[Request failed ] "); 
            }
            else
            {
                Debug.printD(TAG, "[Request Done ] ");  
                
            }
        }
    }
    

    private class BeepDone implements ResponseCallback
    {
        /**
         * This method is called after the IAS response is done. It sets the 
         * current state to waiting-state.
         * 
         * 
         * @param result: the MakeBeep is done or not
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @see mCallback           
         * 
         * @return void [out]            
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            
            boolean isResultFailed = (SafetyBoolean.FALSE.getByte() == result.getByte());
            
            if (isResultFailed)
            {
                disconnect(); 
            }
            
            mCallback.onRequestCompleted(result);
           
            mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
        }
    }
   
    
    /**
     * This method is called after the ConnectStateChanged notification is received. 
     * If it is disconnected, it returns failed via callback function. 
     *
     * @param isConnected [in] the BLE-Device is disconnected or not.      
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1  
     *            
     * @return void [out]
     * @see mCallback
     * 
     */
    @Override
    public void onConnectionStateChanged(int state)
    {
        
    	boolean isNotConnected = (( CommsConstant.BtState.DISCONNECTED == state ) 
    			|| ( CommsConstant.BtState.CONNECTIONLOST == state ));
    	
      
        if (isNotConnected)            
        {
        	Debug.printD(TAG, "[disconnect ] ");
            mCallback.onRequestCompleted(SafetyBoolean.FALSE);  
            mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));                
        }
        else
        {
            // Apply to the coding standard
        }
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
        disconnect(); 
        Debug.printD(TAG, request.getClass().getSimpleName() + " timeout!");
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
// [BT] Fixed Klocwork issue.
