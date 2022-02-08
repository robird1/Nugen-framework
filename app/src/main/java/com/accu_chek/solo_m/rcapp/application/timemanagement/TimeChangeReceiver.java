/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: TimeChangeReceiver
 * Brief:Due to the service can not receive time change message directly, so we 
 * create an isolated broadcast receiver class to receive time change message and forward
 * it to TimeManagementService.
 * 
 * Create Date: 05/01/2015
 * $$Revision: 23334 $$
 * $$Author: TerryHsieh $$
 * $$Id: TimeChangeReceiver.java 23334 2015-11-05 05:56:44Z TerryHsieh $$
 */
package com.accu_chek.solo_m.rcapp.application.timemanagement;

import java.util.Calendar;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.timemanagement.TimeManagementService.TimeManagementReceiverInService;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

import android.util.Log;

/**
 * A BroadcastReceiver class to receive date/time/timezone change and forward the message to TimeManagementService,
 * because TimeManagementService can not receive the above broadcast messages directly.
 */
public class TimeChangeReceiver extends BroadcastReceiver
{

    // A tag for debug usage
    private static final String TAG = "[TM]TimeChangeReceiver";

    // flag for debugging
    private static final boolean isDEBUG = true;

    // Activity context
    private Context mContext = null;

    public TimeChangeReceiver(Context context)
    {
        mContext = context;
    }

    /**
     * onReceive function of BroadcastReceiver
     * 
     *
     * @param Context [in] context
     *      Range: valid Context object
     *      Unit: Context
     *      Scaling: 1
     * @param Intent [in] intent
     *      Range: valid Intent object
     *      Unit: Intent
     *      Scaling: 1
     *      
     */
    @Override
    public void onReceive(Context context, Intent intent)
    {
        Intent intn = null;
        String action = null;

        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);

        // Check whether the parameter is null or not
        CommonUtils.objectCheck(intent);

        intn = new Intent(context, TimeManagementReceiverInService.class);
        action = intent.getAction();

        // Check whether the parameter is null or not
        CommonUtils.objectCheck(action);

        //Date change message from system
        if (action.equals(Intent.ACTION_DATE_CHANGED))
        {
            intn.putExtra("msg", "TM DATE CHANGED");
            //Set one short alarm to send this intent to TimeManagementService
            run_am_set(mContext, intn);
        }
        //Time change message from system
        else if (action.equals(Intent.ACTION_TIME_CHANGED))
        {
            intn.putExtra("msg", "TM TIME CHANGED");
            //Set one short alarm to send this intent to TimeManagementService
            run_am_set(mContext, intn);
        }
        //Timezone change message from system
        else if (action.equals(Intent.ACTION_TIMEZONE_CHANGED))
        {
            intn.putExtra("msg", "TM TIMEZONE CHANGED");
            //Set one short alarm to send this intent to TimeManagementService
            run_am_set(mContext, intn);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * calling this function to call
     * AlarmManager.set function for setting alarm timer.
     * 
     * @param Context [in] context
     *      Range: valid Context object
     *      Unit: Context
     *      Scaling: 1
     * @param Intent [in] intent
     *      Range: valid Intent object
     *      Unit: Intent
     *      Scaling: 1
     * 
     * @return void [out]
     */
    protected void run_am_set(Context context, Intent intent)
    {
        Calendar cal = null;
        PendingIntent pi = null;

        AlarmManager am = (AlarmManager) context
                .getSystemService(Context.ALARM_SERVICE);
        // Get current time
        cal = Calendar.getInstance();
        // Send an one shot broadcast immediately
        pi = PendingIntent.getBroadcast(context, 1, intent,
                PendingIntent.FLAG_ONE_SHOT);
        // Set a wakeup alarm
        am.set(AlarmManager.RTC_WAKEUP, cal.getTimeInMillis(), pi);
    }

    /**
     * 
     * Function for test to print message to Logcat or log file
     *
     * @param String [in] str
     *          Range: valid String object
     *          Unit: String
     *          Scaling: 1
     * @return void [out]
     * 
     */
    @SuppressWarnings("unused")
    private static void printI(String str)
    {
        if (isDEBUG)
        {
            Log.i(TAG, str);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
