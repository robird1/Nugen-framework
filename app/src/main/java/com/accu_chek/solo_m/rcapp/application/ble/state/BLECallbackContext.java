/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLECallbackContext
 * Brief: BLECallback Context
 *
 * Create Date: 2015/7/21
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: BLECallbackContext.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ConnectionStateObserver;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IOnTimeoutListener;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ConnectionStateObserver.IConnectionListener;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class BLECallbackContext implements IOnTimeoutListener, IConnectionListener
{
    /**
     * The request handler
     */
    private BLERequestHandler mRequestHandler = null;
    
    /**
     * The ble state  handler
     */
    private AbstractBLEStateHandler mCurrentState = null;
    
    /**
     * The class constructor
     * @param context : the application context
     *          Range: a valid object of Context
     *          Unit: Context
     *          Scaling: 1 
     */
    public BLECallbackContext(Context context)
    {
        //register Connection listener
        ConnectionStateObserver.getInstance(context).registerConnectionListener(this);
    }
    
    /**
     * This method sets current state.
     * 
     * 
     * @param state: the ble control state (use-case)
     *            Range: a valid object of AbstractBLEStateHandler
     *            Unit: AbstractBLEStateHandler
     *            Scaling: 1
     *
     * @return void [out]  
     * 
     * @see mCurrentState             
     *            
     */
    public void setCurrentState(AbstractBLEStateHandler state)
    {
        mCurrentState = state;
    }
    /**
     * This method sets request-handler.
     * 
     * 
     * @param handler: the request handler
     *            Range: a valid object of BLERequestHandler
     *            Unit: BLERequestHandler
     *            Scaling: 1
     *
     * @return void [out]  
     * 
     * @see mRequestHandler
     *            
     */
    public void setBLERequestHandler(BLERequestHandler handler)
    {
        mRequestHandler = handler;
    }
    
    /**
     * This method returns request-handler.
     * 
     * 
     * @param N/A
     *
     * @return mRequestHandler : the request handler
     *            Range: a valid object of BLERequestHandler
     *            Unit: BLERequestHandler
     *            Scaling: 1   
     * 
     * @see mRequestHandler            
     *            
     */
    public BLERequestHandler getBLERequestHandler()
    {
        return mRequestHandler;
    }
    
    /**
     * This method notifies the request is timeout in current state. 
     * 
     * @param request: the current request
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1
     * @param parameter: the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1            
     *
     * @return void [out]  
     * 
     * @see mCurrentState 
     *            
     */
    @Override
    public void onRequestTimeout(IBLERequest request,
            BLERequestParameter parameter)
    {
        if (null != mCurrentState)
        {
            mCurrentState.onRequestTimeout(request, parameter);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * This method notifies the connection state in current state.
     * 
     * @param isConnected: the flag of connected indication  
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1
     *                      
     * @return void [out]  
     * 
     * @see mCurrentState  
     *            
     */
    @Override
    public void onConnectionStateChanged(int state)
    {
        if (null != mCurrentState)
        {
            mCurrentState.onConnectionStateChanged(state);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * This method transfers AttWriteResponses to current state.  
     * 
     * @param request: the current request
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1      
     * @param parameter: the parameter of the request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1    
     * @param cause: the cause of the response of the current request
     *            Range: -2^31 to (2^31)-1            
     *            Unit: integer
     *            Scaling: 1 
     *                      
     * @return void [out]  
     * 
     * @see mCurrentState
     */
    public void onWriteResponse(IBLERequest request, BLERequestParameter parameter, int cause)
    {
        if (null != mCurrentState)
        {
            mCurrentState.onWriteResponse(request, parameter, cause);
        }
        else
        {
            // Apply to the coding standard
        }
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
