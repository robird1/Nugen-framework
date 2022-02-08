/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bgmcontrol.
 * BgmRdyTimeout
 * Brief:
 *
 * Create Date: 2015¦~9¤ë3¤é
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: BgmRdyTimeout.java 24486 2015-11-20 05:48:56Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager.LEDTYPE;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.util.AbstractTimeoutTask;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import android.content.Context;

public class BgmRdyTimeout extends AbstractTimeoutTask
{

    /**
     * 
     * BgmRdyTimeout constructor.
     * return void [out] None.
     *
     * @param context[in] BgmControl context
     *            Range: valid object
     *            Unit: context
     *            Scaling: 1
     */
    public BgmRdyTimeout(Context context)
    {
        super(context);
        Debug.printI("BgmRdyTimeout", "BgmRdyTimeout");
    }

    /**
     * When RDY pin assert fail, reset bgm.
     * return void [out] None.
     *
     */

    @Override
    public void onFinish()
    {
        Debug.printI("BgmRdyTimeout", "onFinish");

        LEDManager led = (LEDManager) ICustomizedHWManager
                .getSystemService(ICustomizedHWManager.LED_SERVICE);
        CommonUtils.objectCheck(led);
        try
        {
            led.closeLED(LEDTYPE.STRIP);
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }

        BgmJni.getInstance().resetBgm();
    }

}
// (R19842 2015-09-25 05:33:00 AdamChen)
// ----------------------------------------------------------------------------
// add header footer.
// (R21340 2015-10-12 07:57:52 VictorChen)
// ----------------------------------------------------------------------------
// Refine bgtestflow constant naming.
// (R23451 2015-11-05 07:44:01 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
