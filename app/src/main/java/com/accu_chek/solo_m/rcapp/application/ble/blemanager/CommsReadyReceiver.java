/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.CommsReadyReceiver
 * Brief: This class triggers the startup process.
 *
 * Create Date: 2015/7/21
 * $Revision: 20558 $
 * $Author: DWYang $
 * $Id: CommsReadyReceiver.java 20558 2015-10-01 14:02:41Z DWYang $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class CommsReadyReceiver extends BroadcastReceiver
{
    /**
     * This method is called after receiving the CommsReadyIndication broadcast.
     * It triggers Startup process.
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
    public void onReceive(Context context, Intent intent)
    {
        BLEController.getInstance(context).startup(null);
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
