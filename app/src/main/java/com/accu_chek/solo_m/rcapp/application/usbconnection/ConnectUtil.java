/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:ConnectTools
 * Brief: Class for setting/getting the USB listener
 * 
 * Create Date: 04/30/2015
 * $Revision: 21881 $
 * $Author: ivanhuang $
 * $Id: ConnectUtil.java 21881 2015-10-19 05:24:51Z ivanhuang $
 */

package com.accu_chek.solo_m.rcapp.application.usbconnection;

import com.accu_chek.solo_m.rcapp.application.usbconnection.IUsbListener;

public class ConnectUtil
{
    /**
     * Instance of ConnectUtil
     * Scaling: 1
     * Range: Valid object
     * Unit: ConnectUtil
     * Scaling: 1
     */
    public static final ConnectUtil INSTANCE = new ConnectUtil();

    /**
     * Object of USB listener
     * Range: Null or valid object
     * Unit: IUsbListener
     * Scaling: 1
     */
    private IUsbListener mUsbListener = null;

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Sets up the USB listener to monitor the USB connection broadcast:
     * If the input listener object is not null,
     * then register the USB listener.
     * Otherwise unregister the USB listener.
     * 
     * return None
     * 
     * @param listener: Input listener
     *            Range: Null or valid object
     *            Unit: IUsbListener
     *            Scaling: 1
     */
    public void setUsbListener(IUsbListener listener)
    {
        mUsbListener = listener;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Gets the USB listener to monitor the USB connection broadcast.
     * The listener is set by setUsbListener().
     * 
     * return IUsbListener [out]: USB listener
     * Range: Null or valid object
     * Unit: IUsbListener
     * Scaling: 1
     */
    public IUsbListener getUsbListener()
    {
        return mUsbListener;
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
// [NSIQ-20] Refine Code and Comment.
// (R15341 2015-08-24 21:31:21 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Update Comment.
// (R15747 2015-08-28 08:08:51 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Update comment for the feedback from Technical Write Service.
// (R16684 2015-09-08 05:57:17 IvanHuang)
// ----------------------------------------------------------------------------
// [NSIQ-20] Fix typo in reversion history. (NISQ -> NSIQ)
// (R16747 2015-09-08 22:45:45 IvanHuang)
// ----------------------------------------------------------------------------
// [NSM-2765] Refine comments.
