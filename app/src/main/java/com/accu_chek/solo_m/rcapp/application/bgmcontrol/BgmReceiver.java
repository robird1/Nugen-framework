package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;

public class BgmReceiver extends BroadcastReceiver
{
    /**
     * System mode in test mode
     */
    private static final String TEST_MODE = "testmode";

    private static final String TAG = "BgmReceiver";

    /**
     * uvevnt name
     */
    private static final String UEVT_NAME = "UEVT_NAME";
    /**
     * MEM_EVENT pin
     */
    private static final String MEM_EVENT = "MEM_EVENT";
    /**
     * RDY pin
     */
    private static final String MEM_RDY = "MEM_RDY";
    /**
     * system mode property path
     */
    private static final String PERSIST_SYS_SYSTEM_MODE = "persist.sys.system.mode";
    /**
     * EventINT interrupt receive flag
     */
    private static boolean isReceiveEventINT = false;
    /**
     * BgmRdyTimeout object
     */
    private static BgmRdyTimeout mTask = null;

    /**
     * Override function. Receive EVENT_INT and RDY broadcast event.
     * Set a timer when receive EVENT_INT interrupt but it does not receive RDY
     * interrupt, then trigger time out.
     * return void [out] None.
     * 
     * @param context, broadcast context
     *            Range: not null
     *            Unit:Context
     *            Scaling: 1
     * @param intent, broadcast intent data
     *            Range: not null
     *            Unit: Intent
     *            Scaling: 1
     */
    @Override
    public void onReceive(Context context, Intent intent)
    {
        String action = intent.getAction();
        String ueventName = intent.getStringExtra(UEVT_NAME);
        IRCSystemPeoperty bpSetProperty = CustJavaFrameworkManager
                .getRCSystemPropertyService(null);
        String property = null;
        boolean isTestMode = false;

        try
        {
            property = bpSetProperty.getProperty(PERSIST_SYS_SYSTEM_MODE, "");
            isTestMode = property.equals(TEST_MODE);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }

        Debug.printI(TAG, "onReceive: action = " + action);
        Debug.printI(TAG, "onReceive: ueventName = " + ueventName);
        Debug.printI(TAG, "onReceive: isTestMode = " + isTestMode);
        boolean isEVENT_Interrupt = MEM_EVENT.equals(ueventName);
        boolean isRDY_Interrupt = MEM_RDY.equals(ueventName);

        if ((true == isEVENT_Interrupt) && (false == isTestMode))
        {
            Debug.printI(TAG, "EVENT enter");
            Debug.dumpPayload("EVENT-INT", (byte) 0x00);
            boolean isInBgtestFlow = BgmUtils.getIsbGTestFlow();
            Debug.printI(TAG, "isInBgtestFlow " + isInBgtestFlow);

            isReceiveEventINT = true;
            if ((isInBgtestFlow == false) && (mTask == null))
            {
                // start timer, until receive RDY pin event.
                mTask = new BgmRdyTimeout(context);
                Debug.printI(TAG, "mTask = " + mTask);
                mTask.start(IBgmConstant.BGM_HARDWARE_PIN_TIMEOUT);
            }
        }
        else
        {
            // Apply to the coding standard
        }

        // receive Even-int and RDY, call handleInterrupt()
        if (isRDY_Interrupt && isReceiveEventINT)
        {
            Debug.printI(TAG, "RDY enter");
            Debug.dumpPayload("MEM_RDY", (byte) 0x00);
            Debug.printI(TAG, "mTask = " + mTask);
            // stop timer
            if (mTask != null)
            {
                mTask.cancel();
                mTask = null;
            }
            isReceiveEventINT = false;
            handleInterrupt(context);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Send broadcast to BGM control module to check strip status. If wake up by
     * bgm module or in bg test flow, ignore the interrupt.
     * On the contrary, call checkBGMInterrupt()
     * 
     * see mIswakeMethod[in]
     * see mIspoweroninterrupt[in]
     * see mIsbgflow[in]
     * return void [out] None.
     */

    protected void handleInterrupt(Context context)
    {
        boolean isWakeMethod = BgmUtils.getIsWakeUpInterrupt();

        if (true == isWakeMethod)
        {
            Debug.printI(TAG, "Receive interrupt mIswakeMethod" + isWakeMethod);
            // do nothing
        }
        else
        {
            // create a thread to check status,
            ExecutorService executor = Executors.newFixedThreadPool(1);
            BgmExecutor bgmExecutor = new BgmExecutor();
            executor.execute(bgmExecutor);
            executor.shutdown();
            // BgmControl.getInstance(null).checkBgmInterrupt();
            Debug.printI(TAG, "Receiver exit!");
        }

    }
}

class BgmExecutor implements Runnable
{
    /**
     * Send check bgm staus command.
     * return void [out] None.
     *
     */
    @Override
    public void run()
    {
        BgmControl.getInstance(null).checkBgmInterrupt();

    }

}
// Refine code and comment.
// Refine bgtestflow constant naming.
// Add flag to check in Bg test flow.
// Refine code comment.
