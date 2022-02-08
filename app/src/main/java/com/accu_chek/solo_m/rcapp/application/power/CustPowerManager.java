/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: CustPowerManager
 * Brief:
 *
 * Create Date: 05/28/2015
 * $Revision: 23075 $
 * $Author: StanleyWu $
 * $Id: CustPowerManager.java 23075 2015-11-03 06:28:57Z StanleyWu $
 */

package com.accu_chek.solo_m.rcapp.application.power;

import android.content.Context;
import android.os.PowerManager;
import android.os.RemoteException;
import android.view.WindowManager;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.PowerManagerConstants;
import com.accu_chek.solo_m.rcapp.application.gui.interfaces.NuGenActionBarActivity;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public final class CustPowerManager extends ICustPowerManager.Stub
{

    static final String TAG = CustPowerManager.class.getSimpleName();

    /**
     * Define max pump battery level
     */
    private static final int MAX_BATTERY_LEVEL = 100;

    /**
     * Define medium pump battery level
     */
    private static final int MEDIUM_BATTERY_LEVEL = 50;

    /**
     * Define low pump battery level
     */
    private static final int LOW_BATTERY_LEVEL = 25;

    /**
     * Define PowerManager object
     */
    private static PowerManager mPm = null;

    /**
     * Define the context object
     */
    private Context mContext = null;

    /**
     * Define the wake lock
     */
    private PowerManager.WakeLock mWakeLock = null;

    /**
     * Define pump battery level
     */
    private String mPumpBatteryLevel = null;

    /**
     * 
     */
    public CustPowerManager()
    {
        Debug.printI(TAG, "CustPowerManager startup");
    }

    /**
     * Get android power service.
     * 
     * @param context Current context object
     *            Range: Valid object
     *            Unit: Context
     *            Scaling: 1
     */
    public CustPowerManager(final Context context)
    {
        Debug.printI(TAG, "CustPowerManager with Context startup");
        mContext = context;

        // get android power manager
        mPm = (PowerManager) mContext.getSystemService(Context.POWER_SERVICE);
    }

    /**
     * 
     * This function is called to shut down the device.
     *
     * @throws RemoteException
     */
    @Override
    public void shutdown() throws RemoteException
    {
        CustJavaFrameworkManager.getRCSystemPropertyService(null).setProperty(
                "ctl.start", "shutdown");

    }

    /**
     * 
     * This function is called to reboot the device.
     *
     * @throws RemoteException
     */
    @Override
    public void reboot() throws RemoteException
    {
        Debug.printI(TAG, "Enter reboot()");
        mPm.reboot(null);
    }

    /**
     * 
     * This function is called to keep the screen on.
     *
     * @param activity Activity to use the FLAG_KEEP_SCREEN_ON.
     *              Range: a valid object of NuGenActionBarActivity 
     *              Unit: NuGenActionBarActivity 
     *              Scaling: 1
     *            
     * return void [out] None
     */
    public static void keepScreenOn(final NuGenActionBarActivity activity)
    {

        // Check input parameter
        CommonUtils.objectCheck(activity);

        final int flag = WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON;
        activity.getWindow().addFlags(flag);
    }

    /**
     * 
     * This function provides the partial wake lock which ensures that the CPU
     * is running; the screen and keyboard back light will be allowed to go off.
     *
     * return void [out] None
     */
    public void newWakeLock()
    {
        mWakeLock = mPm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK,
                "Partial wake lock");
        mWakeLock.acquire();
    }

    /**
     * 
     * Releases the wake lock.
     *
     * return void [out] None
     */
    public void release()
    {
        mWakeLock.release();
    }

    /**
     * 
     * This function returns pump battery status.
     *
     * return void [out]
     */
    public String getPumpBatteryStatus()
    {

        // Send command to get pump battery status
        BLEController.getInstance().readBatteryLevel(new GetBatteryLevel());
        return mPumpBatteryLevel;
    }

    /**
     * 
     * This function is called to turn on the screen
     *
     * @param activity Activity where to turn on the screen
     *            
     * return void [out] None
     */
    public void turnOnScreen(final NuGenActionBarActivity activity)
    {

        // Check input parameter
        CommonUtils.objectCheck(activity);

        final WindowManager.LayoutParams layout = activity.getWindow()
                .getAttributes();
        layout.screenBrightness = 1F;
        activity.getWindow().setAttributes(layout);
    }

    /**
     * Callback function of readBatteryLevel()
     */
    private final class GetBatteryLevel implements ResponseCallback
    {

        /**
         * 
         * Return SafetyBoolean.TRUE when get pump battery level. Return
         * SafetyBoolean.FALSE when no response.
         *
         * @param result Return result.
         */
        public void onRequestCompleted(final SafetyBoolean result)
        {
            if (SafetyBoolean.TRUE.getByte() == result.getByte())
            {
                Debug.printD(TAG, " Kidd already write. ");

                final SafetyNumber<Integer> safePumpBatteryStatus = NugenGeneralModel
                        .getInt(mContext,
                                PowerManagerConstants.PUMP_BATTRY_STATUS);
                final int mPumpBatteryStatus = safePumpBatteryStatus.get();
                Debug.printD(TAG, " pump battery status: " + mPumpBatteryStatus);

                if (mPumpBatteryStatus == MAX_BATTERY_LEVEL)
                {
                    mPumpBatteryLevel = "FULL";
                }
                else if (mPumpBatteryStatus == MEDIUM_BATTERY_LEVEL)
                {
                    mPumpBatteryLevel = "MEDIUM";
                }
                else if (mPumpBatteryStatus == LOW_BATTERY_LEVEL)
                {
                    mPumpBatteryLevel = "LOW";
                }
                else if (mPumpBatteryStatus == 0)
                {
                    mPumpBatteryLevel = "EMPTY";
                }
                else
                {
                    // Empty for code analysis
                }

            }
            else
            {
                Debug.printD(TAG, " Kidd has no response. ");

            }
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
