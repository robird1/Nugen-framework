/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.Connect
 * Brief: This class handles Connect request and its response.
 *
 * Create Date: 2015/8/19
 * $Revision: 25076 $
 * $Author: IvanHuang $
 * $Id: Connect.java 25076 2015-11-30 05:18:15Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ConnectionStateObserver.IConnectionListener;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

/**
* This class handles Connect request and its response.
*/
public class Connect implements IBLERequest
{
    
    /**
     * The instance of Connect class
     */
    private static volatile Connect mInstance = null;
    
    /**
     * This is an application Context. 
     */
    private Context mContext = null;
    
    /**
     * This is a connection-state callback. 
     */
    private ResponseCallback mCallback = null;
    
    /**
     * This is a connection state listener.
     */
    private IConnectionListener mListener = new Listener();

    
    /**
     * This method shall get the one and only instance of the class Connect.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class Connect
     *         Range: A valid object of Connect
     *         Unit: Connect
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class Connect
     */
    public static Connect getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new Connect(context);
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
    protected Connect(Context context)
    {
        mContext = context;
    }

    
    /**
     * This function is useless in this class
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: A valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     * 
     * @return void [out] 
     * 
     * 
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
    }

 
    protected class Listener implements IConnectionListener
    {

        @Override
        public void onConnectionStateChanged(int state)
        {
        	SafetyBoolean isConnected = SafetyBoolean.FALSE;
        	if (CommsConstant.BtState.CONNECTED == state) 
        	{
        		isConnected = SafetyBoolean.TRUE;
        	}
        	else
        	{
        		// Apply to the coding standard
        	}
            mCallback.onRequestCompleted(isConnected);
            ConnectionStateObserver.getInstance(mContext).unregisterConnectionListener(mListener);
        }
        
    }
    
    
    /**
     * This method shaLL handle the Connect BLE-Device request.
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
     * @return N/A
     * 
     * @see mCallback
     * @see mContext 
     * @see mListener
     * 
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        SafetyByteArray address = GlobalTools.MPR.getMpAddress();
        mCallback = callback; 
        ConnectionStateObserver.getInstance(mContext).connect(address, mListener);
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
