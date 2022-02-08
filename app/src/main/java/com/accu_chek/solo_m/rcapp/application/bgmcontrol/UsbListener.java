/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.bgmcontrol.UsbListener
 * Brief:
 *
 * Create Date: 2015¦~8¤ë6¤é
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: UsbListener.java 24486 2015-11-20 05:48:56Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.usbconnection.IUsbListener;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class UsbListener extends IUsbListener.Stub
{

    private static final String TAG = "UsbListener";

    /**
     * This method is forced to override the functionality of an existing
     * method of super class and no need to do action in this method.
     * return void [out] None.
     */

    @Override
    public void onUsbConnect() throws RemoteException
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

    /**
     * This method is forced to override the functionality of an existing
     * method of super class and no need to do action in this method.
     * return void [out] None.
     */

    @Override
    public void onDisConnect() throws RemoteException
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

    /**
     * When usb plug in bg measurement process, abort test.
     * in the enable mdi mode, disable mdi mode.
     *
     * @throws RemoteException
     */

    @Override
    public void onConfigure() throws RemoteException
    {

        boolean isBgMeasurement = BgmUtils.getIsbGTestCommand();
        boolean isEnableMdi = BgmUtils.getIsMDIMode();
        Debug.printI(TAG, "isEnableMdi " + isEnableMdi);
        Debug.printI(TAG, "isBgMeasurement " + isBgMeasurement);

        if (isBgMeasurement)
        {
            BgmControl.getInstance(null).abortbGTest();
        }
        else if (isEnableMdi)
        {
            BgmControl.setMdiMode(false);
        }
        else
        {
            // static code for analysis
        }

    }

    /**
     * This method is forced to override the functionality of an existing
     * method of super class and no need to do action in this method.
     * return void [out] None.
     */

    @Override
    public void onUnConfigure() throws RemoteException
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

}
// (R19842 2015-09-25 05:33:00 AdamChen)
// ----------------------------------------------------------------------------
// add header footer.
// (R21340 2015-10-12 07:57:52 VictorChen)
// ----------------------------------------------------------------------------
// Refine bgtestflow constant naming.
// (R24379 2015-11-18 23:07:09 henrytso)
// ----------------------------------------------------------------------------
// Refine code comment.
