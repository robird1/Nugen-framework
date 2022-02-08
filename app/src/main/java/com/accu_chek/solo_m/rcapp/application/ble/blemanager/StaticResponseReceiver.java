/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.StaticResponseReceiver
 * Brief: This class handles the broadcast of connection indication notification. 
 *
 * Create Date: 2015/7/21
 * $Revision: 25192 $
 * $Author: KiddYeh $
 * $Id: StaticResponseReceiver.java 25192 2015-12-01 02:34:08Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class StaticResponseReceiver extends BroadcastReceiver
{
    private static final String TAG = "StaticResponseReceiver";
    
    private Context mContext = null;
    
    /**
     * This method receives the Connection Indication broadcast from UICommandDispatcher.
     * It dispatches the broadcast to the certain method to handle it. 
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
    public void onReceive(Context context, Intent intent)
    {
        String action = intent.getAction();

        mContext = context;
        
        Debug.printD(TAG, action);
        
        if (ResponseAction.CommandResponse.BT_CONNECTION_STATE.equalsIgnoreCase(action))
        {
            ConnectionStateObserver.getInstance(context).doProcess(context, intent);
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
// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
