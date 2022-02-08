/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SystemSyncReceiver
 * Brief: This class handles the broadcast of SystemSync indication notification. 
 *
 * Create Date: 2015/7/21
 * $Revision: 24261 $
 * $Author: KiddYeh $
 * $Id: SystemSyncReceiver.java 24261 2015-11-17 07:47:29Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.CommandCode;
import com.accu_chek.solo_m.rcapp.application.ble.response.BlankMessageResponse;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SystemSyncReceiver extends BroadcastReceiver
{
    private static final String TAG = "SystemSyncReceiver";
    
    private Context mContext = null;
    
    private BLERequestParameter parameter = null;
    /**
     * This method receives the SystemSync Indication broadcast from UICommandDispatcher.
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
        
       if( BlankMessageResponse.class.getName().equalsIgnoreCase(action)) 
        {
            Debug.printD(TAG, "Nomarl Indication ");
            
            ResponsePack pack = intent
                    .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

            BlankMessageResponse response = (BlankMessageResponse) pack.getResponse();
            
            int command = response.getCommand().get();

            dispatchIndication(context, command);
            
            if ( CommandCode.BT_DEFECT_ALERT == command)
            {
                Debug.printD(TAG, "BT_DEFECT_ALERT"); 
                byte[] data = response.getMessage();
                Debug.dumpPayload(TAG, data);
                if( 0x5E == data[2])
                {
                    // Show EMWR M94_NO_Connection
                    Debug.printD(TAG, "M94, M94_M_RC_COMM_FAILED!");
                    NotifyMessage msg = new NotifyMessage(EMWRList.EMW45607);
                    NotifyProxy.showEMWR(context, msg);
                }
                else
                {
                    // Apply to the coding standard
                }
            }
            else
            {
                // Apply to the coding standard
            }
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    protected void dispatchIndication(Context context, int command)
    {

        switch (command)
        {
        case CommandCode.BT_SYNC_DEVICE_STATUS_IND:
            Debug.printD(TAG, "BT_SYNC_DEVICE_STATUS_IND");
            Debug.printD(TAG, "executeACT51"); 
            BLEController.getInstance(context).syncDevcieStatus(null);
            break;
        case CommandCode.BT_SOLOM_EMWR_IND:
            Debug.printD(TAG, "BT_SOLOM_EMWR_IND");
            Debug.printD(TAG, "executeACT52"); 
            BLEController.getInstance(context).syncEMWR(null);
            break;
        case CommandCode.BT_SOLOM_CONFIG_IND:
            Debug.printD(TAG, "BT_SOLOM_CONFIG_IND");
            Debug.printD(TAG, "executeACT53");
            BLEController.getInstance(context).updateSoloMConfig(null);
            break;
        case CommandCode.BT_TIME_SYNC_IND:
            Debug.printD(TAG, "BT_TIME_SYNC_IND");
            Debug.printD(TAG, "executeACT54");
            BLEController.getInstance(context).syncMicroPumpTime(null);
            break;
        case CommandCode.BT_BASAL_RATE_IND:
            Debug.printD(TAG, "BT_BASAL_RATE_IND");
            Debug.printD(TAG, "executeACT55");
            BLEController.getInstance(context).syncBasalRate(null);
            break;
        case CommandCode.BT_ACTIVE_BOLUS_IND:
            Debug.printD(TAG, "BT_ACTIVE_BOLUS_IND");
            Debug.printD(TAG, "executeACT56"); 
            BLEController.getInstance(context).syncBolus(null);
            break;
        case CommandCode.BT_SYNC_TOTAL_INSULIN_IND:
            Debug.printD(TAG, "BT_SYNC_TOTAL_INSULIN_IND");
            parameter = new BLERequestParameter();
            parameter.setCommandCode(CommsConstant.CommandCode.BT_SYNC_TOTAL_INSULIN_CFM);
            BLEController.getInstance(context).sendGeneralConfirm(parameter, null);
            break;
        default:
            Debug.printD(TAG, "BlankMessageResponse " + command);
            break;       
        }

    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
