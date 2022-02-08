/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmJni
 * Brief:
 *
 * Create Date: 2015¦~8¤ë4¤é
 * $Revision: 24899 $
 * $Author: VictorChen $
 * $Id: BgmJni.java 24899 2015-11-26 05:40:03Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BgmJni
{
    private static final String TAG = "BgmJni";
    /**
     * BgmJni instance
     */
    private static BgmJni mInstance = null;

    /**
     * 
     * Power on Bgm submodule
     *
     * return void [out] None.
     */
    protected void powerOnBgm()
    {
        PowerOnBgm();
    }

    /**
     * 
     * Power off Bgm submodule
     *
     * return void [out] None.
     */
    public void poweroffBgm()
    {
        PowerOffBgm();
    }

    /**
     * 
     * Power on reset Bgm submodule
     *
     * return void [out] None.
     */
    public void resetBgm()
    {
        BgmUtils.setIsPowerOnInterrupt(true);
        Debug.printI("BgmControl", "resetBgm ");
        ResetBgm();
    }

    /**
     * 
     * Wake up Bgm submodule
     *
     * return void [out] None.
     */
    public void wakeUpBgm()
    {
        WakeupBgm();
    }

    /**
     * 
     * Read RDY pin status.
     * Assert: 0xFFFF
     * De-assert: 0x0000
     * 
     * return void [out] None.
     */
    public int checkRdyStatus()
    {
        Debug.printI(TAG, "[checkRdyStatus] enter");
        int pinStatus = CheckRdyPin();
        return pinStatus;
    }

    /**
     * 
     * Get BgmJni instance
     *
     * see mInstance[out]
     * 
     */
    public static synchronized BgmJni getInstance()
    {

        if (null == mInstance)
        {
            mInstance = new BgmJni();
        }
        else
        {

        }
        return mInstance;
    }

    // ---------------------------------------------------------
    // Java native methods
    // --------------------
    /**
     * JNI, power on BGM submodule
     * 
     * return
     */
    private native boolean PowerOnBgm();

    /**
     * JNI, power of BGM submodule
     * 
     * return
     */
    private native boolean PowerOffBgm();

    /**
     * JNI, poewr on reset BGM submodule
     * 
     * return
     */
    private native boolean ResetBgm();

    /**
     * JNI, Wake up BGM submodule
     * 
     * return
     */

    private native boolean WakeupBgm();

    /**
     * JNI, Check BGM RDY pin status
     */
    private native int CheckRdyPin();
}
// (R20520 2015-10-01 07:04:11 DWYang)
// ----------------------------------------------------------------------------
// Refine code comment.
