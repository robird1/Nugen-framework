/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEUC02
 * Brief: BLE Control use-case 02
 * 
 * Create Date: 07/30/2015
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: BLEUC02.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.MakeBeep;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.RemoveBonding;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetSystemSync;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEUC02 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEUC02";

    // Context of Use Case
    private Context mUseCaseContext = null;
    
    /**
     *  The BondStateChangedListener callback function
     */
    private ResponseCallback mCallback = null;
    
    /**
     *  The general response callback function of request
     */
    private ResponseCallback mRequestDone = new RequestDone();
     
    /**
     *  The response callback function of IAS
     */
    private ResponseCallback mBeepDone = new BeepDone();
    
    /**
     *  The response callback function of RemoveBonding
     */
    private ResponseCallback mRemoveBondingDone = new RemoveBondingDone();

    private SystemSyncResponseReceiver mSystemSyncResponseReceiver = new SystemSyncResponseReceiver();
    
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
     * @see mCallback
     * @see mCallbackContext
     */
	public BLEUC02(BLECallbackContext callbackContext, ResponseCallback callback)
    {

        super(callbackContext);

        mUseCaseContext = mCallbackContext.getBLERequestHandler().getContext();

        mCallback = callback;
        GlobalTools.MPR.setBondingStatus(SafetyBoolean.TRUE);
        // add SetSystemSync request in list
        mCallbackContext.getBLERequestHandler().addRequest(
                SetSystemSync.getInstance(mUseCaseContext), null, mRequestDone);
        
        IntentFilter filter = new IntentFilter();
        filter.addAction(ResponseAction.CommandResponse.BT_SYSTEM_SYNC);
        
        mUseCaseContext.registerReceiver(mSystemSyncResponseReceiver, filter);
        
        // start to execute
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }

	private class SystemSyncResponseReceiver extends BroadcastReceiver
	{

        @Override
        public void onReceive(Context context, Intent intent)
        {
            String action = intent.getAction();
            Debug.printD(TAG, "action = " + action);
            
            if ( ResponseAction.CommandResponse.BT_SYSTEM_SYNC.equalsIgnoreCase(action))
            {
                
                Debug.printD(TAG, "BT_SYSTEM_SYNC -------------------");
                context.unregisterReceiver(this);
                executeSEQ35(context);
            }
            else
            {
                // Apply to the coding standard
            }
                
        }
        private void executeSEQ35(Context context)
        {
            Debug.printD(TAG, "executeSEQ35");
            // add IAS request in list
            mCallbackContext.getBLERequestHandler().addRequest(
                    MakeBeep.getInstance(context), null, mBeepDone);
            // start to execute  
            mCallbackContext.getBLERequestHandler().startHandleRequest();

        }
	}
	
	private class RemoveBondingDone implements ResponseCallback
    {
        /**
         * This method removes the Micro pump Pin code from memory. It returns the failure 
         * of BLE_UC01 process via callback function. 
         * 
         * 
         * @param result: the result of RemoveBonding process.
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         * 
         * @return void [out]
         * 
         * @see mCallback
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            GlobalTools.MPR.setPinCode(null);
            mCallback.onRequestCompleted(SafetyBoolean.FALSE);
            mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
        }
    }

    private class BeepDone implements ResponseCallback
    {
        /**
         * This method confirms BLE_Device bonded and sets current state to 
         * BLEWaitingState.   
         * 
         * 
         * @param result: the result of IAS (MakeBeep) request.
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         * 
         * @return void [out]
         * 
         * @see mCallbackContext
         * 
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {

            mCallback.onRequestCompleted(result);
            GlobalTools.getInstance().setBLEBondState(true);
            GlobalTools.MPR.setBondingStatus(SafetyBoolean.FALSE);
            Debug.printD(TAG, "[Bonding Done ********************************************************* ] ");
            mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
        }
    }

    private class RequestDone implements ResponseCallback
    {
        /**
         * This method checks the response result of the request. 
         * If the result is failed, then it disconnects BD.
         * 
         * 
         * @param result: the executing result of request.
         *            Range: a valid object of SafetyBoolean
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

            if (isResultFailed)
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
   
    /**
     * This method is called after the ConnectStateChanged notification is received. 
     * If it is auto-disconnect and bonded, then it registers the listener of Service Changed
     * Indication. Otherwise it returns failed via callback function. 
     *
     * @param isConnected [in] the BLE-Device is connected or not.      
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1  
     *            
     * @return void [out]
     * 
     * @see mCallbackContext
     * @see mGotServiceChangedInd
     * 
     */
    @Override
    public void onConnectionStateChanged(int state)
    {
       
        if (CommsConstant.BtState.CONNECTED == state)
        {
        	// Apply to the coding standard
        }
    	else if ( CommsConstant.BtState.DISCONNECTED == state )
        {
    		Debug.printD(TAG, "[disconnect but bonded] ");
            GlobalTools.MPR.setPinCode(null);
            mCallback.onRequestCompleted(SafetyBoolean.FALSE);
            // Set wait state to leave this activity 
            mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
        }
    	else if ( CommsConstant.BtState.CONNECTIONLOST == state )
        {
            Debug.printD(TAG, "Connection Lost but bonded");
            GlobalTools.MPR.setPinCode(null);
            mCallback.onRequestCompleted(SafetyBoolean.FALSE);
            // Set wait state to leave this activity 
            mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
        }
        else
        {
            // Apply to the coding standard
        }
        
    }
    
    /**
     * This method calls the disconnect function in BLEController.
     * 
     * @param N/A
     * 
     * @return void [out]
     * 
     * @see mDisconnectDone
     * 
     */
    private void disconnect()
    {
        GlobalTools.MPR.setBondingStatus(SafetyBoolean.FALSE);
        Debug.printD(TAG, " [Add RemoveBonding  ] ");
        mCallbackContext.getBLERequestHandler().clearRequest();
//        BlankMessageRequest request = (BlankMessageRequest) RequestPayloadFactory
//                .getRequestPayload(CommsConstant.CommandCode.COMM_CLEAR_MEMORY);
//        BLERequestParameter parameter = new BLERequestParameter();
//        parameter.setRequest(request);
//        mCallbackContext.getBLERequestHandler().addRequest(
//                SendRequest.getInstance(mUseCaseContext),parameter,mRequestDone);
        
        mCallbackContext.getBLERequestHandler().addRequest(
                RemoveBonding.getInstance(mUseCaseContext), null, mRemoveBondingDone);
        
        mCallbackContext.getBLERequestHandler().startHandleRequest();    
    }

    /**
     * This method calls disconnect-function after Request is timeout. 
     * 
     * @param request [in] the current executing request 
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
     * 
     */
    @Override
    public void onRequestTimeout(IBLERequest request,
            BLERequestParameter parameter)
    {
        // If the CMD is timeout, it disconnects BT device
        disconnect();
        Debug.printD(TAG, request.getClass().getSimpleName() + " timeout!");
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */

// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Fixed Klocwork issue.
// (R19333 2015-09-22 07:33:58 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
