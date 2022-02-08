/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader.ADC
 * Brief: The class provides inner interface to ADCManager to call the function in JNI layer.
 *
 * Create Date: 2015/5/14
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: ADC.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader.IADC;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;

final class ADC extends IADC.Stub
{
    /**
     * the number of ADC interrupt data
     */
    static final int ADC_INTERRUPT_DATA_NUMBER = 4; 
    

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
    @Override
    public SafetyFloat getVoltageValue(int element) throws IllegalStateException
    {
        SafetyFloat safetyfloat = null;
        float value = -1;
        
        try
        {
            value = getADCVoltageValue(element);
            safetyfloat = new SafetyFloat(value, String.valueOf(value));
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // Apply to the coding standard
        }
        
        
        return safetyfloat;
    }

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
    @Override
    public SafetyFloat[] getInterruptEventData() throws IllegalStateException
    {
        SafetyFloat[] safetyData = new SafetyFloat[ADC_INTERRUPT_DATA_NUMBER];
        float[] data = new float[ADC_INTERRUPT_DATA_NUMBER];
        
        try
        {
            getADCInterruptEventData(data);
            
            for(int i = 0; i < ADC_INTERRUPT_DATA_NUMBER; i++)
            {
                safetyData[i] = new SafetyFloat(data[i], String.valueOf(data[i]));
            }
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // Apply to the coding standard
        }
        
        return safetyData;
        
    }
    
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
     * Unit: float
     * Scaling: 1
     * throws OperationFailException if any errors occur when read ADC value.
     */
    private native final float getADCVoltageValue(int element) throws OperationFailException;
    /**
     * Get all voltage value.
     * 
     * return float array if succeed to read interrupt ADC values.
     * float array:
     * Range: 4
     * Unit: float
     * Scaling: 1
     * The value in the array:
     * Range: refer to NUGEN Hardware Specification Document
     * Unit: float
     * Scaling: 1
     * throws IllegalStateException if any errors occur when read ADC value.
     */
    private native final void getADCInterruptEventData(float[] data) throws OperationFailException;

}
