/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.comms.ble.Connect
 * Brief: This class handles Connection response/request and registers/unregisters connectionStateChanged listener .
 * 
 * Create Date: 2015/7/21
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: ConnectionStateObserver.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.util.LinkedList;
import java.util.List;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.request.ConnectRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.ConnectionStateIndication;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
* This class handles Connection response and connectionStateChanged listener.
*/
public class ConnectionStateObserver implements ResponseProcess
{
    private static final String TAG = "ConnectionStateObserver";
    
    /**
     * The instance of ConnectionStateObserver class
     */
    private static volatile ConnectionStateObserver mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The list of connection listeners 
     */
    private List<IConnectionListener> mCallback = new LinkedList<IConnectionListener>();
    
    /**
     * The interface of connection listener
     *
     */
    public interface IConnectionListener
    {
        void onConnectionStateChanged(int state);
    }

    
    /**
     * Get the one and only instance of the class ConnectStateObserver.
     * 
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *
     * @return mInstance : the one and only instance of the class ConnectStateObserver
     *         Range: A valid object of ConnectStateObserver
     *         Unit: ConnectStateObserver
     *         Scaling: 1            
     *            
     * @see mInstance: use this global variable to store the instance of the
     *      class ConnectStateObserverery 
     */
    public static ConnectionStateObserver getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new ConnectionStateObserver(context);
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
     * @param context: 
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mContext           
     */
    private ConnectionStateObserver(Context context)
    {
        mContext = context;
    }


    /**
     * Register the Connection listener.
     * 
     * @param callback: IConnectionListener
     *            Range: a valid object of IConnectionListener
     *            Unit: IConnectionListener
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mCallback   
     */  
    public void registerConnectionListener(IConnectionListener callback)
    {
        Debug.printD(TAG, "Register ConnectionListener.");

        mCallback.add(callback);
    }
    
    
    /**
     * Unregister the Connection listener.
     * 
     * @param callback: IConnectionListener 
     *            Range: a valid object of IConnectionListener
     *            Unit: IConnectionListener
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mCallback              
     * 
     */
    public void unregisterConnectionListener(IConnectionListener callback)
    {
        Debug.printD(TAG, "Unregister ConnectionListener.");

        mCallback.remove(callback);
    }

    
    /**
     * Notify the state of Connection by connection listener callback function.
     * 
     * @param isConnected: the BLE-Device is connected or not.
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mCallback               
     * 
     */
    
    private void notifyConnectionState(int state)
    {
        for (IConnectionListener callback : mCallback)
        {
            callback.onConnectionStateChanged(state);
        }
    }

    
    /**
     * This method is called after receiving the indication of connect-status 
     * broadcast.It shall notify the module that has registered it.
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
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
        SafetyByteArray sfBdAddress = null;
        SafetyBoolean isConnected = SafetyBoolean.FALSE;
        int state = -1;
  
        Debug.printD(TAG, "[Connect]: Response ");

        ResponsePack pack = 
                intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        ConnectionStateIndication response = 
                (ConnectionStateIndication) pack.getResponse();

        state = response.getState().get();

        sfBdAddress = response.getRemoteBD();
        
        
        GlobalTools.MPR.setMpAddress(sfBdAddress);

        
        if (CommsConstant.BtState.CONNECTED == state)
        {
            Debug.printD(TAG, "Device Connected");
            Debug.printD(TAG, "isBooded result = "
                            + BLEController.getInstance(context).isBonded());
            
            isConnected = SafetyBoolean.TRUE;

        }
        else if ( CommsConstant.BtState.DISCONNECTED == state )
        {
            Debug.printD(TAG, "Device disonnected");
            Debug.printD(TAG,
                    "isBooded result = "+ BLEController.getInstance(context).isBonded());
        }
        else if ( CommsConstant.BtState.CONNECTIONLOST == state )
        {
            Debug.printD(TAG, "Connection Lost");
        }
        else
        {
            // Apply to the coding standard
        }
        NugenGeneralModel.setSafetyBoolean(context, BLEController.CONNECTION_STATE, isConnected);
        notifyConnectionState(state);
    }

    
    /**
     * This method shall handle the connect request.
     * 
     * @param address [in] thBLE-device address
     *            Range: address.getByteArraye ().length is 6 (Fixed length).
     *            Unit: SafetyByteArray
     *            Scaling: 1
     * 
     * @param callback [in] cthe allback function
     *            Range: a valid object
     *            Unit: N/A
     *            Scaling: 1
     * 
     * @return void [out] 
     * 
     * @see mCallback  
     * @see mContext 
     */

    public void connect(SafetyByteArray address, IConnectionListener callback)
    {
        Debug.printD(TAG, "[connect] request");

        ConnectRequest request = (ConnectRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_CONNECT);

        SafetyNumber<Integer> req_BDType = new SafetyNumber<Integer>(
                BlueConstant.RemoteBDType.BLE_PUBLIC,
                -BlueConstant.RemoteBDType.BLE_PUBLIC);

        mCallback.add(callback);

        if (null != request )
        {
            request.setRemoteBD(address);

            request.setBdType(req_BDType);
            BLEController.sendRequestToComms(mContext, request);
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
 * =========================================================================== */
// (R16679 2015-09-08 05:44:00 VictorChen)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
