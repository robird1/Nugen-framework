/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bgmcontrol.
 * SendCommsCommandTask
 * Brief:
 *
 * Create Date: 2015¦~9¤ë2¤é
 * $Revision: 24487 $
 * $Author: VictorChen $
 * $Id: SendCommsCommandTimeout.java 24487 2015-11-20 05:50:02Z VictorChen $
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

public class SendCommsCommandTimeout extends AbstractTimeoutTask
{

    /**
     * 
     * SendCommsCommandTask constructor.
     * return void [out] None.
     *
     * @param context[in] BgmControl context
     *            Range: valid object
     *            Unit: context
     *            Scaling: 1
     */
    public SendCommsCommandTimeout(Context context)
    {
        super(context);
        Debug.printI("SendCommsCommandTask", "SendCommsCommandTask");
    }

    /**
     * Send comms command timeout. Show EMWR.
     * return void [out] None.
     *
     */

    @Override
    public void onFinish()
    {
        Debug.printI("SendCommsCommandTask", "Time out");
        NotifyMessage msg = new NotifyMessage(EMWRList.EMW46013);
        LEDManager LED = (LEDManager) ICustomizedHWManager
                .getSystemService(ICustomizedHWManager.LED_SERVICE);
        CommonUtils.objectCheck(LED);
        try
        {
            LED.closeLED(LEDTYPE.STRIP);
            NotifyProxy.showEMWR(BgmUtils.getContext(), msg);
            BgmControl.getInstance(null).powerDownBgm();
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }

    }

}
// (R19842 2015-09-25 05:33:00 AdamChen)
// ----------------------------------------------------------------------------
// add header footer.
// (R21340 2015-10-12 07:57:52 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
// (R23050 2015-11-02 08:46:08 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
