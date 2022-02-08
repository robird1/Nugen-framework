/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.VibrationManager
 * Brief: 
 * The class provides an interface to modules in RC APP to control Vibration.
 *
 * Create Date: 2015/3/4
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: VibrationManager.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol;

import java.util.Arrays;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.IHaptic;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;


/**
 * The class provides an interface to modules in RC APP to control Vibration.
 */
public class VibrationManager
{
    /**
     * vibration thread name
     */
    private final String VIBRATION_THREAD_NAME = "vibration_thread";
    
    /**
     * Haptic interface
     */
    private IHaptic mService = null;
    
    /**
     * Vibration thread for vibration pattern
     */
    private Thread mVibrationThread = null;
    
    /**
     * Vibration thread for once vibration
     */
    private Thread mShortVibrationThread = null;
    
    /**
     * allowed Haptic types
     */
    public interface VibrationType
    {
        /**
         * feedback for touch
         */
        int TOUCH_FEEDBACK = 0x000F;
        
        /**
         * 0.75s vibration
         */
        int THREE_FORTHS_SECOND = 0x0033;
        
        /**
         * 1s vibration
         */
        int ONE_SECOND = 0x003C;
    }
    
    /**
     * Get Haptic interface that is used by RC APP framework service.
     * The interface is provided RC APP framework service to generate background Haptic service. 
     *
     * @param None [in] 
     * 
     * return IHaptic that is the interface of Haptic. For detail of this object, see Haptic class.
     * Range: Valid IHaptic object
     * Unit: IHaptic
     * Scaling: 1
     */
    public static IHaptic getInterface()
    {
        return new Haptic();
    }

    /**
     * Store the interface of Haptic in local variable.
     * 
     * see mService [in] This global variable is referred for saving Haptic service.
     * 
     * @param service [in] the interface of Haptic. For detail of this object, see Haptic class.
     * Range: Valid IHaptic Object
     * Unit: IHaptic
     * Scaling: 1
     */
    public VibrationManager(IHaptic service)
    {
        mService = service;
    }
    
    /**
     * Short vibration thread
     */
    class ShortVibrationThread extends Thread
    {
        /**
         * vibration type index
         */
        private int mVibrationType = -1;
        
        /**
         * Set initial value to super Thread class. Set vibration type to this object. 
         * 
         * see mVibrationType [in] This global variable in ShortVibrationThread is referred for saving vibration style.
         * 
         * @param threadName [in] Thread name string, VIBRATION_THREAD_NAME.
         * Range: VIBRATION_THREAD_NAME
         * Unit: String
         * Scaling: 1
         * @param vibrationType [in] Vibration type index in VibrationManager::VibrationType
         * Range: TOUCH_FEEDBACK, THREE_FORTHS_SECOND or ONE_SECOND in VibrationType
         * Unit: int
         * Scaling: 1
         */
        ShortVibrationThread(String threadName, int vibrationType)
        {
            super(threadName);
            
            mVibrationType = vibrationType;
        }

        /**
         * If this thread is not newest one, it must not be executed.
         * If it is newest one, it will stop current vibration and play new one
         * according to vibration type.
         * 
         * If any error occurs, EMWR warning is shown; log error message.
         * 
         * see mShortVibrationThread [in] This global variable is referred for saving current short vibration thread.
         * see mService [in] This global variable is referred for getting Haptic service.
         * see mVibrationType [in] This global variable in ShortVibrationThread is referred for getting vibration style.
         * 
         * @param None [in]
         * 
         * return None 
         */
        @Override
        public void run() 
        {
            try
            {
                if(this == mShortVibrationThread)
                {
                    mService.stop();
                
                    mService.play(mVibrationType);
                }
                else
                {
                    // Empty for static code analysis
                }
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
                
                // call emwr warning
                
            }
            catch (IllegalStateException e)
            {
                e.printStackTrace();
                
                // call emwr warning
            }
            finally
            {
                // Empty for static code analysis
            }
        }
        
    }
    
    /**
     * vibration thread with vibration pattern
     */
    class VibrationThread extends Thread
    {
        /**
         * Vibration pattern set item number
         */
        private final int VIBRATION_PATTERN_SET_ITEM_NUMBER = 2;
        /**
         * vibration pattern. First value in the pattern is a vibration type
         * in  VibrationManager::VibrationType. Next value is empty time in millisecond. The rest in the pattern
         * is deduced by analogy.
         */
        private long[] mVibrationPattern = null;
        
        /**
         * Set initial value to super Thread class.
         * Set vibration pattern to this object. 
         * 
         * see mVibrationPattern [in] This global variable in VibrationThread is referred for saving vibration pattern.
         * 
         * @param threadName [in] Thread name string, VIBRATION_THREAD_NAME.
         * Range: VIBRATION_THREAD_NAME
         * Unit: String
         * Scaling: 1
         * @param vibrationPattern [in] a long array. First value in the pattern is a vibration type
         * in  VibrationManager::VibrationType. Next value is empty time in millisecond. The rest in the pattern
         * is deduced by analogy.
         * Even index in vibrationPattern:
         * Range: TOUCH_FEEDBACK, THREE_FORTHS_SECOND or ONE_SECOND in VibrationType
         * Unit: long
         * Scaling: 1
         * Odd index in vibrationPattern:
         * Range: 0 ... max long number
         * Unit: millisecond
         * Scaling: 1
         */
        VibrationThread(String threadName, long[] vibrationPattern)
        {
            super(threadName);
            
            mVibrationPattern = Arrays.copyOf(vibrationPattern, vibrationPattern.length);
        }

        /**
         * If it is newest one, it will stop current vibration and play new one
         * according to vibration pattern. If the item in the pattern is multiplies of 2,
         * this function will play the vibration type in the item. Otherwise,
         * it will sleep a while time in the item. 
         * After playing/sleeping an item in the pattern, the function will check
         * if this thread is not newest one. If it is not, the function must stop.
         * 
         * If any error occurs, EMWR warning is shown.
         * 
         * see mVibrationPattern [in] This global variable in VibrationThread is referred for getting vibration pattern.
         * see mService [in] This global variable is referred for getting Haptic service.
         * see mVibrationThread [in] This global variable is referred for getting current vibration thread.
         * 
         * @param None [in]
         * 
         * return None 
         */
        @Override
        public void run()
        {
            int vibrationIndex = 0;
            
            try
            {
                final int STOP_NUMBER = 1000;
                final int EXIST_CONDITION = mVibrationPattern.length + STOP_NUMBER;
                
                mService.stop();
                
                for(int i=0; i < mVibrationPattern.length; i++)
                {
                    if(this == mVibrationThread)
                    {
                        vibrationIndex = i % VIBRATION_PATTERN_SET_ITEM_NUMBER;
                        
                        if(vibrationIndex == 0)
                        {
                            mService.play((int) mVibrationPattern[i]);
                        }
                        else
                        {
                            mService.stop();
                            
                            CommonUtils.sleep(mVibrationPattern[i]);
                        }
                    }
                    else
                    {
                        i = EXIST_CONDITION;
                    }
                }
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
             
             // call emwr warning
                
            }
            catch (IllegalStateException e)
            {
                e.printStackTrace();
                
             // call emwr warning
                
            }
            finally
            {
                Debug.printI("vibrationIndex", "finish"+mVibrationPattern[0]+" "+mVibrationPattern[1]);
            }
        }
        
    }
    
	/**
	 * Vibrate based on given vibration type.
	 * 
	 * In order to avoid blocking UI, the function will create a thread to play vibration.
	 * If vibration thread is running, short vibration thread is blocked to assure vibration thread work normally.
	 * 
	 * see mVibrationThread [in] This global variable is referred for getting current vibration thread.
	 * see mShortVibrationThread [in] This global variable is referred for saving current short vibration thread.
	 * 
	 * @param vibrationType [in] Vibration type index in VibrationManager::VibrationType
	 * Range: TOUCH_FEEDBACK, THREE_FORTHS_SECOND or ONE_SECOND in VibrationType
     * Unit: int
     * Scaling: 1
	 * 
	 * return None 
	 */
    public void play(final int vibrationType)
    {
        boolean isVibrationThreadAlive = false;
        
        if (mVibrationThread != null)
        {
            isVibrationThreadAlive = mVibrationThread.isAlive();
        }
        else
        {
            // Empty for static code analysis
        }
        
        if(isVibrationThreadAlive == false)
        {
            mShortVibrationThread = new ShortVibrationThread(VIBRATION_THREAD_NAME, vibrationType);
            
            mShortVibrationThread.start();
        }
        else
        {
            // Empty for static code analysis
        }
    }
    
	/**
	 * Vibrate based on given vibration pattern.
	 * 
	 * In order to avoid blocking UI, the function will create a thread to play vibration.
	 * If there has been a vibration thread running, new thread will stop old one first.
	 * 
	 * see mVibrationThread [in] This global variable is referred for saving current vibration thread.
     * see mShortVibrationThread [in] This global variable is referred for setting current short vibration thread.
	 * 
	 * @param pattern [in] a long array. First value in the pattern is a vibration type
	 * in  VibrationManager::VibrationType. Next value is empty time in millisecond. The rest in the pattern
	 * is deduced by analogy.
	 * Even index in pattern:
	 * Range: TOUCH_FEEDBACK, THREE_FORTHS_SECOND or ONE_SECOND in VibrationType
     * Unit: long
     * Scaling: 1
     * Odd index in pattern:
     * Range: 0 ... max long number
     * Unit: millisecond
     * Scaling: 1
	 * 
	 * return None 
	 */
    public void vibrate(final long[] pattern)
    {
        // make short vibration stop
        mShortVibrationThread = null;
        
        mVibrationThread = new VibrationThread(VIBRATION_THREAD_NAME, pattern);
        
        mVibrationThread.start();
    }
    
	/**
	 * Cancel vibration. If any error occurs, EMWR warning is shown; log error message.
	 * 
	 * see mService [in] This global variable is referred for using Haptic service.
	 * 
	 * @param None [in]
	 * 
	 * return None 
	 */
    public void cancel() 
    {
        try
        {
            mService.stop();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            // call emwr warning
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            // call emwr warning
        }
        finally
        {
            // the section does nothing
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
