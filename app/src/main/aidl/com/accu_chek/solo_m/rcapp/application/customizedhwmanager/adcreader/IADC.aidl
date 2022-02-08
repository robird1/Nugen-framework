package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;

/**
 * {@hide}
 */
interface IADC
{
    //export function
     /**
     * Get a voltage value based on the element.
     *
     * @param element [in] integer number of the element's index in ADCELEMENT.
     * Range: in ADCELEMENT
     * Unit: int
     * Scaling: 1
     * 
     * return SafetyFloat if succeed to read ADC value.
     * Range: refer to NUGEN Hardware Specification Document
     * Unit: SafetyFloat
     * Scaling: 1
     * throws IllegalStateException if any errors occur when read ADC value.
     */
    SafetyFloat getVoltageValue(in int element);
    
    /**
    * Get all voltage value.
    * 
    * return SafetyFloat array if succeed to read interrupt ADC values.
    * SafetyFloat array:
    * Range: 4
    * Unit: SafetyFloat
    * Scaling: 1
    * The value in the SafetyArray:
    * Range: refer to NUGEN Hardware Specification Document
    * Unit: SafetyFloat Array
    * Scaling: 1
    * throws IllegalStateException if any errors occur when read ADC value.
    */
    SafetyFloat[] getInterruptEventData();
}
