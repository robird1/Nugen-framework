package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

/**
 * {@hide}
 */
interface ILED
{
    //export function
    /**
     * Turn the LED on based on given LED type.
     * 
     * @param type [in] LED type index in LEDTYPE
     * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OperationFailException if LED type is wrong
     */
    void open(in int ledType);
    
    /**
     * Close LED based on given LED type.
     * 
     * @param type [in] LED type index in LEDTYPE
     * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
     * 
     * return None
     * 
     * throw OperationFailException if LED type is wrong
     */
    void close(in int ledType);
    
    /**
     * Flash LED based on given LED type.
     * 
     * @param frequency [in] LED frequency.
     * Range: Setting in SOLO M EMWR Specification
     * Unit: int
     * Scaling: 1
     * @param ledtype [in] LED type index in LEDTYPE, but only information LED can flash currently.
     * Range: INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
     * 
     * return None
     * 
     * throw OperationFailException if LED type is wrong
     */
    void flash(in SafetyNumber frequency, in int ledType);
}
