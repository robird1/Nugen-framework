/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: NuGenActionBarActivity
 * Brief: The base Activity class of the all RCApp UI Activities.
 *
 * Create Date: 02/04/2015
 * $Revision: 25086 $
 * $Author: AdamChen $
 * $Id: NuGenActionBarActivity.java 25086 2015-11-30 06:31:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.RemoteException;
import android.support.v7.app.ActionBarActivity;
import android.view.KeyEvent;
import android.view.WindowManager;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.power.ICustPowerManager;
import com.accu_chek.solo_m.rcapp.application.util.AbstractTimeoutTask;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class NuGenActionBarActivity extends ActionBarActivity
{
    
    // Debug use, there would be removed in the release version
    private static final String TAG = "NuGenActionBarActivity";
    
    // The positive click listener of the Power off dialog instance. 
    private static DialogPositiveButtonClick mPositiveClickListener = null;
    
    // The power off dialog instance.
    private static AlertDialog mConfirmDialog = null;
    
    // Debounce flag for control key debounce
    private volatile boolean mIsKeyDebounce = false;
    
    // Debouncer instance
    private KeyDebouncer mDebouncer = null;
    
    // Initial all class object of this class
    static
    {
        mPositiveClickListener = new DialogPositiveButtonClick();
    }
    
    /**
     * 
     * Called when a key down event has occurred.
     *
     * @param keyCode [in] The value in event.getKeyCode().
     * @param event [in] Description of the key event.
     * @return isRet [out] Return true when handle the event, and return false
     *         when allow the event to be handled by the next receiver.
     */
    @Override
    public boolean onKeyDown(final int keyCode, final KeyEvent event)
    {
//        Debug.printI(TAG, "[onKeyDown] enter");
        boolean isRet = false;
        
        isRet = keyDownDispatcher(keyCode, event);
        
        if (isRet == false)
        {
            isRet = super.onKeyUp(keyCode, event);
        }
       
//        Debug.printI(TAG, "[onKeyDown] isRet = " + isRet);
        return isRet;
    }

    /**
     * 
     * Called when a long press has occurred. Show turn off dialog when long
     * press the power button. Turn off the meter when button is clicked.
     *
     * @param keyCode [in] The value in event.getKeyCode().
     * @param event [in] Description of the key event.
     * @return isRet [out] Return true when handle the event, and return false
     *         when allow the event to be handled by the next receiver.
     */
    public final boolean onLongPressPowerButton(final int keyCode, final KeyEvent event)
    {
        Debug.printI(TAG, "[onKeyLongPress] enter");
        boolean isRet = false;

        if (keyCode == KeyEvent.KEYCODE_POWER)
        {
            Debug.printI(TAG, "keyCode is Power key");
            
            mConfirmDialog = new AlertDialog.Builder(this.getApplicationContext())
                    // .setTitle(R.string.power_off)
                    .setMessage(R.string.turn_off)
                    .setPositiveButton(R.string.txt_yes,
                            mPositiveClickListener)
                    .create();

            mConfirmDialog.getWindow().setType(WindowManager.LayoutParams.TYPE_SYSTEM_ALERT);            
            mConfirmDialog.show();

            isRet = true;
        }

        return isRet;
    }

    /**
     * 
     * Called when a key up event has occurred.
     * 
     * @param keyCode [in] The value in event.getKeyCode().
     * @param event [in] Description of the key event.
     * @return isRet [out] Return true when handle the event, and return false
     *         when allow the event to be handled by the next receiver.
     */
    @Override
    public final boolean onKeyUp(final int keyCode, final KeyEvent event)
    {
        Debug.printI(TAG, "[onKeyUp] enter");
        boolean retValue = false;
        
        mIsKeyDebounce = mDebouncer.getStatus();
        Debug.printI(TAG, "[onKeyUp] mIsKeyDebounce = " + mIsKeyDebounce);
        
        if (mIsKeyDebounce == false)
        {
            retValue = keyUpDispatcher(keyCode, event);
            mDebouncer.enableDebounce();
            mDebouncer.start(250);   //Must read from Configuration Matrix.            
        }
        else
        {
            Debug.printI(TAG, "DEBOUNCING........");
        }
        
        if (retValue == false)
        {
            retValue = super.onKeyUp(keyCode, event);
        }
        
        Debug.printI(TAG, "[onKeyUp] retValue = " + retValue);
        return retValue;
    }

    /**
     * Called when the activity is starting.
     * 
     *
     * @param savedInstanceState[in] : If the activity is being re-initialized after previously 
     *                                 being shut down then this Bundle contains the data 
     *                                 it most recently supplied in onSaveInstanceState(Bundle). 
     *                                 Note: Otherwise it is null.
     *         Range: Valid Bundle object or null
     *         Unit: Bundle
     *         Scale: 1
     */
    @Override
    protected void onCreate(final Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        
        // Set context to dialog positive click listener
        mPositiveClickListener.setContext(getBaseContext());
        
        // Key debouncer instance
        mDebouncer = new KeyDebouncer(this.getApplicationContext());
    }
    
    /**
     * 
     * Handle home button 
     *
     */
    protected void onHomePressed()
    {
        //do nothing in the base class
    }

    /**
     * 
     * Handle next button
     *
     */
    protected void onNextPressed()
    {
        //do nothing in the base class
    }

    /**
     * 
     * Handle Insulin confirm button
     *
     */
    protected void onInsulinConfirmationPressed()
    {
        //do nothing in the base class
    }
    
    /**
     * 
     * Handle key down dispatcher  
     *
     * @param keyCode [in] The value in event.getKeyCode().
     * @param event [in] Description of the key event.
     * @return isRet [out] Return true when handle the event, and return false
     *         when allow the event to be handled by the next receiver.
     */
    private boolean keyDownDispatcher(int keyCode, KeyEvent event)
    {
        boolean isLongPress = false;;
        boolean isRet = false;
//        Debug.printI(TAG, "[keyDownDispatcher]: keyCode = " + keyCode);       
        switch (keyCode)
        {
        case KeyEvent.KEYCODE_POWER:           
            Debug.printI(TAG, "keyCode is Power key");            
            
            // start tracking key event count
            event.startTracking();

            Debug.printI(TAG,
                    "event.getRepeatCount = " + event.getRepeatCount());
            Debug.printI(TAG, "event.isLongPress = " + event.isLongPress());

            isLongPress = event.isLongPress();

            if (isLongPress == true)
            {
                onLongPressPowerButton(keyCode, event);
            }

            isRet = true;
            break;
        case KeyEvent.KEYCODE_BACK :            
            CommonUtils.vibrateTouch(this);
            isRet = true;
            break;
        case KeyEvent.KEYCODE_F1 :
            CommonUtils.vibrateTouch(this);
            isRet = true;
            break;
        case KeyEvent.KEYCODE_F2 :           
            CommonUtils.vibrateTouch(this);
            isRet = true;
            break;
        default :         
            break;
        }
        return isRet;
    }
    
    /**
     * 
     * Handle key up dispatcher 
     *
     * @param keyCode [in] The value in event.getKeyCode().
     * @param event [in] Description of the key event.
     * @return isRet [out] Return true when handle the event, and return false
     *         when allow the event to be handled by the next receiver.
     */
    private boolean keyUpDispatcher(final int keyCode, final KeyEvent event)
    {
        boolean retValue = false;
        Debug.printI(TAG, "[KeyUpDispatcher]: keyCode = " + keyCode);
        switch (keyCode)
        {
        case KeyEvent.KEYCODE_BACK :
            onBackPressed();
            retValue = true;
            break;
        case KeyEvent.KEYCODE_F1 :
            onHomePressed();
            retValue = true;
            break;
        case KeyEvent.KEYCODE_F2 :
            onNextPressed();
            retValue = true;
            break;
        case KeyEvent.KEYCODE_F10 :
            onInsulinConfirmationPressed();
            retValue = true;
            break;
        default :            
            break;
        }
        return retValue;
    }
    
    
    /**
     * Used for click listener interface of the power off dialog 
     */
    private static final class DialogPositiveButtonClick implements DialogInterface.OnClickListener
    {
        
        // The delay time for power off process
        private static final int DELAY_POWER_OFF = 3000;
        
        // Used to get context from other module.
//        private Context mContext = null;
        
        // Customized power manager bonder proxy instance
        private ICustPowerManager mBpPowerManager = null;
        
        /**
         * 
         * The interface for set context
         *
         * @param context [in]: Valid Context object.
         *        Range: Valid object.
         *        Unit: Context.
         *        Scale: 1.
         */
        public void setContext(final Context context)
        {
//            mContext = context;
            
            mBpPowerManager = CustJavaFrameworkManager.getCustPowerManagerService(context);
            
        }
        
        /**
         * 
         * This method will be invoked when a button in the dialog is clicked.
         *
         * @param dialog [in]
         * @param which [in]
         */
        @Override
        public void onClick(final DialogInterface dialog, final int which)
        {
            try
            {
                // Notify Comms meter will be shut down
                // MpAdapter.prePowerOffCommand();
                BLEController.getInstance()
                        .prePowerOffComms(null);
                // Wait 3 seconds then shut down
                CommonUtils.sleep(DELAY_POWER_OFF);
                mBpPowerManager.shutdown();
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Add for static code analysis
            }
            
            // begin Shutdown Sequence
        
            if (mConfirmDialog != null)
            {
                mConfirmDialog = null;
            }            
        }
    
    }
    
    
    /**
     * Used to do key debounce
     */
    private static final class KeyDebouncer extends AbstractTimeoutTask
    {
        
        // Control key debounce status
        private volatile boolean mIsDebouncer = false;
        
        // Used to do lock debounce status.
        private final Object lock = new Object();
        
        /**
         * Custom KeyDebouncer Constructor with Context parameter.
         * @param context [in]
         */
        public KeyDebouncer(Context context)
        {
            super(context);
                      
        }
        
        /**
         * 
         * The interface of the enable debounce function
         *
         */
        public void enableDebounce()
        {
            synchronized (lock)
            {
                mIsDebouncer = true;
            }
           
        }
        
        /**
         * 
         * The interface of the get debounce status function.
         *
         * @return boolean [out]:
         */
        public boolean getStatus()
        {
            synchronized (lock)
            {
                return mIsDebouncer;
            }
                      
        }
        
        /**
         * 
         * This function is invoked when the debounce timer is achieved.
         *
         */        
        @Override
        public void onFinish()
        {
            Debug.printI(TAG, "[onFinish]");
            //disable debounce
            mIsDebouncer = false;
            
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
// [Setting] fix - vibrate when clicking next button, back button, home button
