package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol;


/**
 * {@hide}
 */
interface IHaptic
{
    //export function
    /**
     * Play based on given haptic type.
     *
     * @param type [in] haptic index in VibrationType
     * Range: TOUCH_FEEDBACK, THREE_FORTHS_SECOND or ONE_SECOND in VibrationType
     * Unit: int
     * Scaling: 1
     * @return None 
     * @throw OperationFailException when vibrating fails.
     */
    void play(in int type);
    
     /**
     * Stop haptic.
     * 
     * @param None [in]
     * @return None 
     */
    void stop();
}
