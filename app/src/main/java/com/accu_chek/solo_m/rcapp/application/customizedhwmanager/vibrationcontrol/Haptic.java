/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.Haptic
 * Brief: 
 * The class provides inner interface to VibrationManager to call the function in JNI layer.
 *
 * Create Date: 2015/3/3
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: Haptic.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.IHaptic;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;

/**
 * The class provides inner interface to VibrationManager to call the function in JNI layer.
 */
class Haptic extends IHaptic.Stub
{
    /**
     * Play specific haptic style based on given haptic type.
     *
     * @param type [in] haptic index in VibrationType
     * Range: TOUCH_FEEDBACK, THREE_FORTHS_SECOND or ONE_SECOND in VibrationType
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw IllegalStateException when vibrating fails.
     */
    public void play(int type) throws IllegalStateException
    {
        try
        {
            playHaptic(type);
        }
        catch(OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    /**
     * Stop haptic.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw IllegalStateException when canceling vibration fails.
     */
    public void stop() throws IllegalStateException
    {
        try
        {
            stopHaptic();
        }
        catch(OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    
    // ---------------------------------------------------------
    // Java native methods 
    // --------------------
    /**
     * Stop haptic.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OprationFailException when canceling vibration fails.
     */
    protected native void stopHaptic() throws OperationFailException;
	
    /**
     * Play specific haptic style based on given haptic type.
     *
     * @param type [in] haptic index in VibrationType
     * Range: TOUCH_FEEDBACK, THREE_FORTHS_SECOND or ONE_SECOND in VibrationType
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OprationFailException when vibrating fails.
     */
    private native final void playHaptic(int type) throws OperationFailException;
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
