/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEUC01
 * Brief: BLE control use-case 01
 *
 * Create Date: 2015/7/21
 * $Revision: 25164 $
 * $Author: KiddYeh $
 * $Id: BLEUC01.java 25164 2015-11-30 10:20:58Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.Ble_Seq30;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ConfirmOOB;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.Disconnect;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.Discovery;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ReadDeviceInfo;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ReadIDDFeature;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.RemoveBonding;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetCCCD;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetPairable;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SetSecurity;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID.UUID16;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * 
 * This class is used to handle the process of bonding Remote Control and Micro-Pump.
 * BLE_UC01
 *
 */
public class BLEUC01 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEUC01";
    
    // Context of Use Case
    private Context mUseCaseContext = null;
    
    /**
     *  The BondStateChangedListener callback function
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
     *  The response callback function of Set KES CCCD request
     */
    private ResponseCallback mSetKESCCCDDone = new SetKESCCCDDone();
    
    /**
     *  The response callback function of KES process
     */
    private ResponseCallback mKeyExchangeDone = new KeyExchangeDone();
    
    
    /**
     *  The response callback function of ServiceChangedInd
     */
    private ResponseCallback mGotServiceChangedInd = new GotServiceChangedInd();
    
    /**
     *  The response callback function of IDDFeature
     */
    private ResponseCallback mUC01Done = new UC01Done();
    
    private AbstractBLEStateHandler mSubState = null;
    
    /**
     * This method adds the BLEUC01 sequence requests in request list of the 
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
    public BLEUC01(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        
        super(callbackContext);
        
        mUseCaseContext = mCallbackContext.getBLERequestHandler().getContext();
        
        mCallback = callback;
        
        // add ReadDeviceInfo request in list
        mCallbackContext.getBLERequestHandler().addRequest(
                ReadDeviceInfo.getInstance(mUseCaseContext), null, mRequestDone);
        
        // add Set SERVICE_CHANGE CCCD request in list
        BLERequestParameter serviceChangeParameter = new BLERequestParameter();
        serviceChangeParameter.setUUID(UUID16.SERVICE_CHANGE);
        serviceChangeParameter.setParameter(BlueConstant.WriteType.COMMAND);
        mCallbackContext.getBLERequestHandler().addRequest(
                SetCCCD.getInstance(mUseCaseContext), serviceChangeParameter, mRequestDone);
        
        // add Set KEY_EXCHANGE_CP CCCD request in list
        BLERequestParameter kESParameter = new BLERequestParameter();
        kESParameter.setUUID(UUID16.KEY_EXCHANGE_CP);
        kESParameter.setParameter(BlueConstant.WriteType.COMMAND);
        mCallbackContext.getBLERequestHandler().addRequest(
                SetCCCD.getInstance(mUseCaseContext), kESParameter, mSetKESCCCDDone);
        
//        // add KeyExchange request in list
//        mCallbackContext.getBLERequestHandler().addRequest(
//                new KeyExchange(mUseCaseContext), null, mRequestDone);
//        
//        // add SetPairable request in list
//        mCallbackContext.getBLERequestHandler().addRequest(
//                SetPairable.getInstance(mUseCaseContext), null, mRequestDone);
//        
//        // add SetSecurity request in list
//        mCallbackContext.getBLERequestHandler().addRequest(
//                SetSecurity.getInstance(mUseCaseContext), null, mRequestDone);
//        
//        // add OOBConfirm request in list
//        mCallbackContext.getBLERequestHandler().addRequest(
//                ConfirmOOB.getInstance(mUseCaseContext), null, mRequestDone);
               
        // start to execute 
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }
    
    private class UC01Done implements ResponseCallback
    {

        /**
         * This method sets BLEUC02 State to the current state.
         */
    	@Override
    	public void onRequestCompleted(SafetyBoolean result) 
    	{
    	    boolean isResult = (SafetyBoolean.TRUE.getByte() == result.getByte());
    	    
    	    if(isResult)
    	    {    
    	        mCallbackContext.setCurrentState(new BLEUC02(mCallbackContext, mCallback));	
    	    }
    	    else
    	    {
    	        disconnect();
    	        Debug.printD(TAG, "[Request failed ] "); 
    	    }
    	}
    	
    }
    
    private class DisconnectDone implements ResponseCallback
    {
    	/**
         * This method removes the Pin code from memory.It returns the failure 
         * of BLE_UC01 process via callback function.
         * 
         * 
         * @param result: the result of Disconnect
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
            // Set wait state to leave this activity 
            mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
        }
    }
    
    
    /**
     * This method disconnects BLE-Device.
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
        Debug.printD(TAG, " [UC01 Failed...........  ] ");
        boolean isBonded = 
                ( mCallbackContext.getBLERequestHandler().isBonded().getByte() 
                        == SafetyBoolean.TRUE.getByte());   
        
        mCallbackContext.getBLERequestHandler().clearRequest();
        
      
        if(isBonded)
        {
            Debug.printD(TAG, " [RemoveBonding ......   ] ");
            mCallbackContext.getBLERequestHandler().addRequest(
                    RemoveBonding.getInstance(mUseCaseContext), null, mDisconnectDone);              
        }
        else
        {
            Debug.printD(TAG, " [Disconnect ...... ] ");
            mCallbackContext.getBLERequestHandler().addRequest(
                    Disconnect.getInstance(mUseCaseContext), null, mDisconnectDone); 
        }
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }
    
    
    private class RequestDone implements ResponseCallback
    {
        /**
         * This method checks the response result of the request. 
         * If the result is failed, then it disconnects BD.
         * 
         * 
         * @param result: the result of request.
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
            
            if ( isResultFailed )
            {
                disconnect();
                Debug.printD(TAG, "[Request failed ] "); 
            }
            else
            {
                Debug.printD(TAG, " [Request Done ] ");  
            }
        }
    }
   
    private class SetKESCCCDDone implements ResponseCallback
    {
        /**
         * This method checks the response result of the request. 
         * If the result is failed, then it disconnects BD.
         * 
         * 
         * @param result: the result of request.
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
            
            if ( isResultFailed )
            {
                disconnect();
                Debug.printD(TAG, "[Request failed ] "); 
            }
            else
            {
                Debug.printD(TAG, " [Request Done ] ");  
                Debug.printD(TAG, "[Call BLE KeyExchange ] ");
                
                mSubState = new BLEKeyExchangeProcedure(mCallbackContext, mKeyExchangeDone);
//                BLEController.getInstance(mCallbackContext.getBLERequestHandler().getContext()).exchangeKey(mKeyExchangeDone);
            }
        }
    }
    
    
    private class KeyExchangeDone implements ResponseCallback
    {
        /**
         * This method checks the response result of the request. 
         * If the result is failed, then it disconnects BD.
         * 
         * 
         * @param result: the result of request.
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
            
            mSubState = null;
            
            if ( isResultFailed )
            {
                disconnect();
                Debug.printD(TAG, "[KES failed ] "); 
            }
            else
            {
                Debug.printD(TAG, " [KES Done ] ");  
                            
                // add SetPairable request in list
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetPairable.getInstance(mUseCaseContext), null, mRequestDone);
                
                // add SetSecurity request in list
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetSecurity.getInstance(mUseCaseContext), null, mRequestDone);
                
                // add OOBConfirm request in list
                mCallbackContext.getBLERequestHandler().addRequest(
                        ConfirmOOB.getInstance(mUseCaseContext), null, mRequestDone);
                
                // start to execute 
                mCallbackContext.getBLERequestHandler().startHandleRequest();
            }
        }
    }
    
    
    private class GotServiceChangedInd implements ResponseCallback
    {
        /**
         * This method adds the sequential requests in RequestHandler if the 
         * ServiceChangedIndication is OK. Otherwise it disconnects the connection 
         * of BLE-Device.
         * 
         * @param result: the result of ServiceChangedIndication
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *
         * @return void [out] 
         * 
         * @see mCallbackContext
         * @see mRequestDone 
         *             
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isGotServiceChangedInd = (SafetyBoolean.TRUE.getByte() == result
                    .getByte());
            
            if (isGotServiceChangedInd)
            {
                Debug.printD(TAG, "[Got Service Changed Indication ] ");
               
                mCallbackContext.getBLERequestHandler().addRequest(
                        Discovery.getInstance(mUseCaseContext), null, mRequestDone);
                
                BLERequestParameter iDDStatusChangeParameter = new BLERequestParameter();
                iDDStatusChangeParameter.setUUID(UUID16.IDD_STATUS_CHANGED);
                iDDStatusChangeParameter.setParameter(BlueConstant.WriteType.COMMAND);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), iDDStatusChangeParameter, mRequestDone);
                
                BLERequestParameter iDDStatusReadCPParameter = new BLERequestParameter();
                iDDStatusReadCPParameter.setUUID(UUID16.IDD_STATUS_READER_CP);
                iDDStatusReadCPParameter.setParameter(BlueConstant.WriteType.COMMAND);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), iDDStatusReadCPParameter, mRequestDone);
                
                BLERequestParameter iDDCommandCPParameter = new BLERequestParameter();
                iDDCommandCPParameter.setUUID(UUID16.IDD_COMMAND_CP);
                iDDCommandCPParameter.setParameter(BlueConstant.WriteType.COMMAND);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), iDDCommandCPParameter, mRequestDone);
                
                BLERequestParameter iDDCommandDataParameter = new BLERequestParameter();
                iDDCommandDataParameter.setUUID(UUID16.IDD_COMMAND_DATA);
                iDDCommandDataParameter.setParameter(BlueConstant.WriteType.REQUEST);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), iDDCommandDataParameter, mRequestDone);
                  //V6
                BLERequestParameter recordAccessCPParameter = new BLERequestParameter();
                recordAccessCPParameter.setUUID(UUID16.RECORD_ACCESS_CP);
                recordAccessCPParameter.setParameter(BlueConstant.WriteType.COMMAND);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), recordAccessCPParameter, mRequestDone);
                  //V6
                BLERequestParameter iDDHistoryCPParameter = new BLERequestParameter();
                iDDHistoryCPParameter.setUUID(UUID16.IDD_HISTORY_DATA);
                iDDHistoryCPParameter.setParameter(BlueConstant.WriteType.REQUEST);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), iDDHistoryCPParameter, mRequestDone);
                
                BLERequestParameter soloMStatusChangedParameter = new BLERequestParameter();
                soloMStatusChangedParameter.setUUID(UUID16.SOLOM_STATUS_CHANGED);
                soloMStatusChangedParameter.setParameter(BlueConstant.WriteType.COMMAND);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), soloMStatusChangedParameter, mRequestDone);
                
                BLERequestParameter soloMCPParameter = new BLERequestParameter();
                soloMCPParameter.setUUID(UUID16.SOLOM_CP);
                soloMCPParameter.setParameter(BlueConstant.WriteType.COMMAND);
                mCallbackContext.getBLERequestHandler().addRequest(
                        SetCCCD.getInstance(mUseCaseContext), soloMCPParameter, mRequestDone);
                
                mCallbackContext.getBLERequestHandler().addRequest(
                        ReadIDDFeature.getInstance(mUseCaseContext), null, mUC01Done);
                
                
                mCallbackContext.getBLERequestHandler().startHandleRequest();
            }
            else
            {
                disconnect();
                Debug.printD(TAG, "[Service Changed Indication failed ] ");
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
        Context context = mCallbackContext.getBLERequestHandler().getContext();
        boolean isBonded = 
                ( mCallbackContext.getBLERequestHandler().isBonded().getByte() 
                        == SafetyBoolean.TRUE.getByte());
        
        Debug.printD(TAG, "Connection state =  "+ state );

        
        if (null != mSubState)
        {
        	mSubState.onConnectionStateChanged(state);
        }
        else
        {
        	// Apply to coding standard.
        }
        
        if (CommsConstant.BtState.CONNECTED == state)
        {
        	// Apply to the coding standard
        }
    	else if ( CommsConstant.BtState.DISCONNECTED == state )
        {
    		if(isBonded)
            {  
                Debug.printD(TAG, "[disconnect and bonded ==> Wait for service change  ] ");
               // MP shall disconnect the connection. 
               Debug.printD(TAG, "[Register Service Changed Indication Listener ] ");
               Ble_Seq30.getInstance(context).registerServiceChangedIndicationListener(mGotServiceChangedInd);
            }
            else
            {
                Debug.printD(TAG, "[disconnect and unbound  ] ");
                GlobalTools.MPR.setPinCode(null);
                mCallback.onRequestCompleted(SafetyBoolean.FALSE);
                // Set wait state to leave this activity 
                mCallbackContext.setCurrentState(new BLEWaitingState(mCallbackContext));
            }
        }
    	else if ( CommsConstant.BtState.CONNECTIONLOST == state )
        {
            Debug.printD(TAG, "Connection Lost");
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
     * This method is called after RequestTimeout. It calls disconnect-function. 
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
     * 
     */
    @Override
    public void onRequestTimeout(IBLERequest request,
            BLERequestParameter parameter)
    {       
    	if (null != mSubState)
    	{
    		mSubState.onRequestTimeout(request, parameter);
    	}
    	else
    	{
	        // CMD timeout, then disconnect BT device      
	        disconnect(); 
	        Debug.printD(TAG, request.getClass().getSimpleName() + " timeout!");
    	}
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R18085 2015-09-16 05:09:21 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Fixed Klocwork issue.
