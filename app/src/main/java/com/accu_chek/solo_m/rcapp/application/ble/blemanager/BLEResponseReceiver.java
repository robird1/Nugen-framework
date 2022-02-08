/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.BLEResponseReceiver
 * Brief: This class handles the response receiver register function and unregister function. 
 *
 * Create Date: 2015/7/21
 * $Revision: 22298 $
 * $Author: KiddYeh $
 * $Id: BLEResponseReceiver.java 22298 2015-10-22 06:55:59Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEResponseReceiver
{
    private static final String TAG = "BLEResponseReceiver";
    
    public static final String KEY_RESPONSE_PACK = 
            "com.accu_chek.solo_m.rcapp.application.RESPONSEPACK";
    /**
     * The class instance
     */
    private static volatile BLEResponseReceiver mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The map of response receiver
     */
    private Map<String, ResponseProcess> mResponseMap = new HashMap<String, ResponseProcess>();
    
    
    private BroadcastReceiver mReceiver = new BroadcastReceiver()
    {
        /**
         *  Get the received action and check it is in the map or not.
         *  Do the its mapping response. 
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
         * @see mResponseMap
         *            
         */
        @Override
        public void onReceive(Context context, Intent intent)
        {
            Debug.printD(TAG, "action = " + intent.getAction());
            ResponseProcess process = mResponseMap.get(intent.getAction());
            
            Log.i(TAG, process.getClass().getSimpleName()); 
            
            for (Entry<String, ResponseProcess> each : mResponseMap.entrySet())
            {
                Log.i(TAG, each.getKey() + ": " + each.getValue().getClass().getSimpleName());                
            }
            
            if (null != process)
            {
                process.doProcess(context, intent);
            }
            else
            {
                // Apply to the coding standard
            }
        }
    };

    

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
    private BLEResponseReceiver(Context context)
    {
        mContext = context;
    }

    
    /**
     * Get the one and only instance of the class BLEResponseReceiver.
     * 
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return mInstance : the one and only instance of the class BLEResponseReceiver
     *         Range: A valid object of BLEResponseReceiver
     *         Unit: BLEResponseReceiver
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class BLEResponseReceiver
     */
    public static BLEResponseReceiver getInstance(Context context)
    {
        if (null == mInstance)
        {
            synchronized (BLEResponseReceiver.class)
            {
                if (null == mInstance)
                {
                    mInstance = new BLEResponseReceiver(context);
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

        return mInstance;
    }

    
    /**
     * Register the receiver and attach the response callback to the
     * ResonponseMap.
     * 
     * 
     * @param action [in] String 
     *            Range: A valid object of String
     *            Unit: String
     *            Scaling: 1 
     * @param responseProcess [in] ResponseProcess
     *            Range: A valid object of ResponseProcess
     *            Unit: ResponseProcess
     *            Scaling: 1
     *            
     * @return void [out] 
     *            
     * @see mContext
     * @see mResponseMap
     *   
     */
    public void registerReceiver(String action, ResponseProcess responseProcess)
    {
    
        IntentFilter filter = new IntentFilter();

        filter.addAction(action);

        mContext.registerReceiver(mReceiver, filter);

        mResponseMap.put(action, responseProcess);
    }
    
    
    /**
     * Unregister the receiver and clear the ResonponseMap.
     *
     * @param N/A
     *  
     * @return void [out] 
     * 
     * @see mResponseMap
     * @see mContext
     *  
     */
    public void unregisterReceiver()
    {

        mResponseMap.clear();
        mContext.unregisterReceiver(mReceiver);
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
