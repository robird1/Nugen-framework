/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: UsbConnectReceiver
 * Brief: Broadcast receiver for the USB connection
 * 
 * Create Date: 05/04/2015
 * $Revision: 23359 $
 * $Author: IvanHuang $
 * $Id: UsbConnectReceiver.java 23359 2015-11-05 06:48:58Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.usbconnection;

import java.util.HashMap;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.USBConstants;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class UsbConnectReceiver extends BroadcastReceiver
{
    /**
     * Broadcast Action: A sticky broadcast for USB state change events when in
     * device mode
     * Range: "android.hardware.usb.action.USB_STATE"
     * Unit: String
     * Scaling: 1
     */
    public static final String USB_ST = "android.hardware.usb.action.USB_STATE";

    /**
     * USB connection/configuration states listener
     * Range: null, valid object
     * Unit: IUsbListener
     * Scaling: 1
     */
    private static IUsbListener mUsbListener = null;

    /**
     * Sound file path of "Connection": USB_IN.wav
     * Range: "/system/altek/Sounds/USB_IN.wav"
     * Unit: String
     * Scaling: 1
     */
    private static final String USBIN_SOUND = "/system/altek/Sounds/USB_IN.wav";

    /**
     * Sound file path of "Disconnection": USB_OUT.wav
     * Range: "/system/altek/Sounds/USB_OUT.wav"
     * Unit: String
     * Scaling: 1
     */
    private static final String USBOUT_SOUND = "/system/altek/Sounds/USB_OUT.wav";

    /**
     * Boolean extra indicating whether USB is connected or disconnected.
     * Used in extras for the ACTION_USB_STATE broadcast.
     * Range: "configured"
     * Unit: String
     * Scaling: 1
     */
    private static final String USB_CONFIGURED = "configured";

    /**
     * Boolean extra indicating whether USB is configured.
     * Used in extras for the ACTION_USB_STATE broadcast.
     * Range: "connected"
     * Unit: String
     * Scaling: 1
     */
    private static final String USB_CONNECTED = "connected";

    /**
     * Current USB connection state
     * Range: true, false
     * Unit: boolean
     * Scaling: 1
     */
    private static boolean mUsbConnected = false;

    /**
     * Current USB configuration state
     * Range: true, false
     * Unit: boolean
     * Scaling: 1
     */
    private static boolean mUsbConfigured = false;

    /**
     * UsbConnectReceiver context
     * Range: Valid Context
     * Unit: Context
     * Scaling: 1
     */
    private static Context mContext = null;

    enum UsbState
    {
        CONNECTED
        {
            /**
             * Status: FDD/Coding/UT Done
             * 
             * Does the process when USB is connected.
             * 
             * return None
             */
            @Override
            public void doProcess()
            {
                /**
                 * "UsbConnectReceiver", "Usb connected..."
                 */
                mUsbListener = ConnectUtil.INSTANCE.getUsbListener();
                doUsbStateProcess(mUsbListener, CONNECTED);
            }

        },

        DISCONNECTED
        {

            /**
             * Status: FDD/Coding/UT Done
             * 
             * Does the process when USB is disconnected.
             * 
             * return None
             */
            @Override
            public void doProcess()
            {
                /**
                 * "UsbConnectReceiver", "Usb disconnected..."
                 */
                mUsbListener = ConnectUtil.INSTANCE.getUsbListener();
                doUsbStateProcess(mUsbListener, DISCONNECTED);
            }
        },

        CONFIGURED
        {

            /**
             * Status: FDD/Coding/UT Done
             * 
             * Does the process when USB is configured.
             * 
             * return None
             */
            @Override
            public void doProcess()
            {
                /**
                 * "UsbConnectReceiver", "Usb configured..."
                 */
                mUsbListener = ConnectUtil.INSTANCE.getUsbListener();
                doUsbStateProcess(mUsbListener, CONFIGURED);
            }

        },

        UNCONFIGURED
        {

            /**
             * Status: FDD/Coding/UT Done
             * 
             * Does the process when USB is unconfigured.
             * 
             * return None
             */
            @Override
            public void doProcess()
            {
                /**
                 * "UsbConnectReceiver", "Usb unconfigured..."
                 */
                mUsbListener = ConnectUtil.INSTANCE.getUsbListener();
                doUsbStateProcess(mUsbListener, UNCONFIGURED);
            }

        };

        /**
         * For the UsbState elements to override this function
         */
        public void doProcess()
        {
            // For override use
        }
    }

    /**
     * HashMap of the USB connection state
     * Range: Valid object
     * Unit: HashMap<Boolean, UsbState>
     * Scaling: 1
     */
    private static HashMap<Boolean, UsbState> mConnectMap = new HashMap<Boolean, UsbState>();

    /**
     * HashMap of the USB configuration state
     * Range: Valid object
     * Unit: HashMap<Boolean, ConnectedState>
     * Scaling: 1
     */
    private static HashMap<Boolean, UsbState> mConfigureMap = new HashMap<Boolean, UsbState>();

    /**
     * Sets the Connection/Configuration Hash Map
     */
    static
    {
        mConnectMap.put(true, UsbState.CONNECTED);
        mConnectMap.put(false, UsbState.DISCONNECTED);
        mConfigureMap.put(true, UsbState.CONFIGURED);
        mConfigureMap.put(false, UsbState.UNCONFIGURED);
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Receives android.hardware.usb.action.USB_STATE and updates the USB
     * connection & configuration states.
     * Then does the processes depends on the states.
     * 
     * return None
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: Valid Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @param intent: The Intent being received.
     *            Range: Valid Intent
     *            Unit: Intent
     *            Scaling: 1
     */

    @Override
    public void onReceive(Context context, Intent intent)
    {
        String action = null;

        /**
         * Null object check
         */
        CommonUtils.objectCheck(context);
        CommonUtils.objectCheck(intent);

        mContext = context;

        action = intent.getAction();

        /**
         * "UsbConnectReceiver", "Detect USB onReceive"
         */

        if (action != null)
        {
            if (action.equals(USB_ST))
            {
                mUsbConnected = intent.getBooleanExtra(USB_CONNECTED, false);
                mUsbConfigured = intent.getBooleanExtra(USB_CONFIGURED, false);

                // Save the USB connection and configuration states
                saveConnectState(mUsbConnected);
                saveConfigureState(mUsbConfigured);

                // Does the process for the corresponded USB state
                onReceiveUsbProcess(mUsbConnected, mUsbConfigured);

            }
            else
            {
                // Apply to the coding standard
            }
        }
        else
        {
            // Apply to the coding standard
            // The action is not specified.
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Saves the USB connection state
     * 
     * return None
     * 
     * @param isUsbConnected: USB is connected or not.
     *            Range: true (USB is connected), false (USB is disconnected)
     *            Unit: boolean
     *            Scaling: 1
     */
    protected void saveConnectState(boolean isUsbConnected)
    {
        SafetyBoolean sbUsbConnected = null;

        // Null Object Check
        CommonUtils.objectCheck(mContext);

        /**
         * Store the USB connection state in the SharedPreference
         */
        if (isUsbConnected == true)
        {
            // USB is Connected
            sbUsbConnected = SafetyBoolean.TRUE;
        }
        else
        {
            // USB is Disconnected
            sbUsbConnected = SafetyBoolean.FALSE;
        }

        NugenGeneralModel.setSafetyBoolean(mContext,
                USBConstants.KEY_USB_CONNECT, sbUsbConnected);
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Saves the USB configuration state
     * 
     * return None
     * 
     * @param isUsbConfigured: USB is configured or not.
     *            Range: true (USB is configured), false (USB is unconfigured)
     *            Unit: boolean
     *            Scaling: 1
     */
    protected void saveConfigureState(boolean isUsbConfigured)
    {
        SafetyBoolean sbUsbConfigured = null;

        /**
         * Null Object Check
         */
        CommonUtils.objectCheck(mContext);

        // Store the USB configuration state in the SharedPreference
        if (isUsbConfigured)
        {
            // USB is Configured
            sbUsbConfigured = SafetyBoolean.TRUE;
        }
        else
        {
            // USB is Unconfigured
            sbUsbConfigured = SafetyBoolean.FALSE;
        }

        NugenGeneralModel.setSafetyBoolean(mContext,
                USBConstants.KEY_USB_CONFIGURE, sbUsbConfigured);
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Does the process when receiving the USB state
     * 
     * return None
     * 
     * @param isUsbConnected: USB is connected or not.
     *            Range: true (USB is connected), false (USB is disconnected)
     *            Unit: boolean
     *            Scaling: 1
     * 
     * @param isUsbConfigured: USB is configured or not..
     *            Range: true (USB is configured), false (USB is unconfigured)
     *            Unit: boolean
     *            Scaling: 1
     */
    protected void onReceiveUsbProcess(boolean isUsbConnected,
            boolean isUsbConfigured)
    {
        /**
         * Null Object Check
         */
        CommonUtils.objectCheck(mConnectMap);
        CommonUtils.objectCheck(mConfigureMap);
        CommonUtils.objectCheck(mContext);

        /**
         * USB cable is connected and configured.
         */
        if (isUsbConnected && isUsbConfigured)
        {

            /**
             * Does the processes when receive USB connection/configuration
             * state event.
             */
            mConnectMap.get(isUsbConnected).doProcess();
            mConfigureMap.get(isUsbConfigured).doProcess();

            // Play a USB In Sound
            CommonUtils.playSound(USBIN_SOUND, mContext);
        }
        /**
         * USB cable is connected and configured.
         */
        else if (!isUsbConnected && !isUsbConfigured)
        {
            /**
             * Does the processes when receive USB disconnection/unconfiguration
             * state event.
             */
            mConnectMap.get(isUsbConnected).doProcess();
            mConfigureMap.get(isUsbConfigured).doProcess();

            // Play a USB Out Sound
            CommonUtils.playSound(USBOUT_SOUND, mContext);
        }
        else
        {
            // Apply to the coding standard
        }

    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Returns the current USB connection state.
     * 
     * return SafetyBoolean [out]: the current USB connection state
     * Range: SafetyBoolean.TRUE (USB is connected), SafetyBoolean.FALSE (USB is disconnected)
     * Unit: SafetyBoolean
     * Scaling: 1
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: Valid Context
     *            Unit: Context
     *            Scaling: 1
     * 
     */
    public SafetyBoolean getUsbConnectState(Context context)
    {
        SafetyBoolean sbUsbConnected = null;

        // Null Object Check
        CommonUtils.objectCheck(context);

        // Get the USB connection state from the SharedPreference
        sbUsbConnected = NugenGeneralModel.getSafetyBoolean(context,
                USBConstants.KEY_USB_CONNECT);

        return sbUsbConnected;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Returns the current USB configuration state.
     * 
     * return SafetyBoolean [out]: the current USB configuration state
     * Range: SafetyBoolean.TRUE (USB is configured), SafetyBoolean.FALSE (USB is unconfigured)
     * Unit: SafetyBoolean
     * Scaling: 1
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: Valid Context
     *            Unit: Context
     *            Scaling: 1
     * 
     */
    public SafetyBoolean getUsbConfigureState(Context context)
    {
        SafetyBoolean sbUsbConfigured = null;

        // Null Object Check
        CommonUtils.objectCheck(context);

        // Get the USB connection state from the SharedPreference
        sbUsbConfigured = NugenGeneralModel.getSafetyBoolean(context,
                USBConstants.KEY_USB_CONFIGURE);

        return sbUsbConfigured;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Does the process for the specified USB state.
     * 
     * return None
     * 
     * @param listener: USB listener
     *            Range: null, valid object
     *            Unit: IUsbListener
     *            Scaling: 1
     * 
     * @param state: USB state
     *            Range: UsbState
     *            Unit: UsbState
     *            Scaling: 1
     * 
     */
    protected static void doUsbStateProcess(IUsbListener listener,
            UsbState state)
    {
        // Null object check
        CommonUtils.objectCheck(state);

        try
        {
            if (listener != null)
            {
                /**
                 * The USB listener is set up
                 * Does the process for the specified USB state.
                 */
                switch (state)
                {
                // USB is connected
                case CONNECTED:
                    listener.onUsbConnect();
                    break;
                // USB is disconnected
                case DISCONNECTED:
                    listener.onDisConnect();
                    break;
                // USB is configured
                case CONFIGURED:
                    listener.onConfigure();
                    break;
                // USB is unconfigured
                case UNCONFIGURED:
                    listener.onUnConfigure();
                    break;
                default:
                    /**
                     * Invalid USB State
                     */
                    break;
                }
            }
            else
            {
                // Apply to the coding standard

                /**
                 * The USB listener is not set up
                 * No functionality in here
                 */
            }
        }
        catch (RemoteException e)
        {
            errorHandler();
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Error Handler for UsbConnectReceiver
     * 
     * return None
     */
    protected static void errorHandler()
    {
        /**
         * Show EMWR: EMW46602
         */
        NotifyMessage message = new NotifyMessage(EMWRList.EMW46602);

        NotifyProxy.showEMWR(message);
    }
}

/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// [NSIQ-20] (1) Refine USB Connection code (2) Add Auto_Header_Footer_Updater
// (R15068 2015-08-20 01:47:57 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Add Error Handler
// (R15158 2015-08-21 04:13:01 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Refine Code and Comment.
// (R15341 2015-08-24 21:31:21 IvanHuang)
// ----------------------------------------------------------------------------
// 1. Move loadSound() & playSound() to CommonUtils.java
// 2. Add errorHandler() to CommonUtils.java
// (R15468 2015-08-25 23:43:06 IvanHuang)
// ----------------------------------------------------------------------------
// Remove errorHandler() from the default path in the switch-case.
// (R15507 2015-08-26 04:59:41 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Update Comment.
// (R15747 2015-08-28 08:08:51 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Update Comment.
// (R15767 2015-08-30 22:32:59 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Update Comment.
// (R15774 2015-08-31 01:08:18 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Update Comment.
// (R15775 2015-08-31 01:08:43 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Change mConnectMap and mConfigureMap to private.
// (R15811 2015-08-31 03:18:57 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Update comment for the feedback from Technical Write Service.
// (R16684 2015-09-08 05:57:17 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Fix typo in reversion history. (NISQ -> NSIQ)
// (R16747 2015-09-08 22:45:45 IvanHuang)
// ----------------------------------------------------------------------------
// [NSM-2765] Refine comments.
// (R21881 2015-10-19 01:24:51 ivanhuang)
// ----------------------------------------------------------------------------
// [USB Connection] Update comment.
