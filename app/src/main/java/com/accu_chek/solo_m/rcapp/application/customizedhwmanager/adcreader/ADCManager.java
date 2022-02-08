/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader.ADCManager
 * Brief: The class provides the functions to search some element's voltage on RC meter. 
 *
 * Create Date: 2015/5/14
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: ADCManager.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader.IADC;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;


public class ADCManager
{
    /**
     * ADC service
     */
    private IADC mService = null;
    
    public interface ADCELEMENT
    {
        /**
         * the index of VCCM_FB 
         */
        int VCCM_FB = HammingDistance.SAFETY_NUMBER_VALUE_0001;
        /**
         * the index of VCCN_3V_FB
         */
        int VCCN_3V_FB = HammingDistance.SAFETY_NUMBER_VALUE_0002;
        /**
         * the index of VPROC_SNS
         */
        int VPROC_SNS = HammingDistance.SAFETY_NUMBER_VALUE_0003;
        /**
         * the index of VSYS_SNS
         */
        int VSYS_SNS = HammingDistance.SAFETY_NUMBER_VALUE_0004;
        /**
         * the index of NTC2
         */
        int NTC2 = HammingDistance.SAFETY_NUMBER_VALUE_0005;
        /**
         * the array index of VCCM_FB
         */
        int ARRAY_INDEX_VCCM_FB = 0;
        /**
         * the array index of VCCN_3V_FB
         */
        int ARRAY_INDEX_VCCN_3V_FB = 1;
        /**
         * the array index of VPROC_SNS
         */
        int ARRAY_INDEX_VPROC_SNS = 2;
        /**
         * the array index of VSYS_SNS
         */
        int ARRAY_INDEX_VSYS_SNS = 3;
        
        int ARRAY_INDEX_NTC2 = 4;
    }
    
    /**
     * Get ADC interface that is used by RC APP framework service. 
     * The interface is provided RC APP framework service to generate background
     * ADC service. 
     * 
     * @param None [in] 
     * 
     * return IADC that is the interface of ADC. For detail of this object, see ADC class.
     * Range: Valid IADC object
     * Unit: IADC
     * Scaling: 1
     */
    public static IADC getInterface()
    {
        return new ADC();
    }
    
    /**
     * Get the array index of the element. 
     *
     * @param element [in] integer number of the element's index in ADCELEMENT.
     * Range: in ADCELEMENT
     * Unit: int
     * Scaling: 1
     * 
     * return SafetyChannel<Integer> if the element is correct.
     * throw ArgumentErrorException if the index of the element is wrong.
     */
    private SafetyChannel<Integer> getIndex(int element) throws ArgumentErrorException
    {
        int index = -1;
        SafetyChannel<Integer> safetyIndex = null;
        
        switch(element)
        {
        case ADCELEMENT.VCCM_FB:
            index = ADCELEMENT.ARRAY_INDEX_VCCM_FB;
            break;
        
        case ADCELEMENT.VCCN_3V_FB:
            index = ADCELEMENT.ARRAY_INDEX_VCCN_3V_FB;
            break;
        
        case ADCELEMENT.VPROC_SNS:
            index = ADCELEMENT.ARRAY_INDEX_VPROC_SNS;
            break;
            
        case ADCELEMENT.VSYS_SNS:
            index = ADCELEMENT.ARRAY_INDEX_VSYS_SNS;
            break;
        case ADCELEMENT.NTC2:
            index = ADCELEMENT.ARRAY_INDEX_NTC2;
            break;
        default:
            throw new ArgumentErrorException("Parameter setting is wrong.");
        }
        
        safetyIndex = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(index), CommonUtils.encodeCH2Value(index));
        
        return safetyIndex;
    }
    
    /**
     * Store the interface of ADC in local variable.
     * 
     * see mService [in] This global variable is referred for saving ADC service.
     * 
     * @param service [in] the interface of ADC. For detail of this object, see ADC class.
     * Range: Valid IADC Object
     * Unit: IADC
     * Scaling: 1
     */
    public ADCManager(IADC service)
    {
        mService = service;
    }
    
    /**
     * Get a voltage value based on the element.
     *
     * @param element [in] integer number of the element's index in ADCELEMENT.
     * Range: in ADCELEMENT
     * Unit: int
     * Scaling: 1
     * 
     * throw OperationFailException if there is any errors when get voltage value.
     * throw ArgumentErrorException if the index of the element is wrong.
     * return SafetyFloat if get a voltage value.
     * Range: refer to NUGEN Hardware Specification Document
     * Unit: SafetyFloat
     * Scaling: 1
     */
    public SafetyFloat getVoltage(int element) throws OperationFailException, ArgumentErrorException
    {
        SafetyFloat voltage = null;
        
        getIndex(element);
        
        try
        {
            voltage = mService.getVoltageValue(element);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        finally
        {
            // Apply to the coding standard
        }
        
        return voltage;
    }
    
    /**
     * Get an interrupt voltage value based on the element.
     *
     * @param element [in] integer number of the element's index in ADCELEMENT.
     * Range: in ADCELEMENT
     * Unit: int
     * Scaling: 1
     * 
     * throw OperationFailException if there is any errors when get voltage value.
     * throw ArgumentErrorException if the index of the element is wrong.
     * return SafetyFloat if get an interrupt voltage value.
     * Range: refer to NUGEN Hardware Specification Document
     * Unit: SafetyFloat
     * Scaling: 1
     */
    public SafetyFloat getInterruptVoltage(int element) throws OperationFailException, ArgumentErrorException
    {
        SafetyFloat[] voltages = null;
        SafetyFloat value = null;
        SafetyChannel<Integer> safetyindex = getIndex(element);
        
        try
        {
            int index = CommonUtils.decodeCH1Value(safetyindex.getValueCH1());
            
            voltages = mService.getInterruptEventData();
            value = voltages[index];
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        finally
        {
            // Apply to the coding standard
        }
        
        return value;
    }
    
    /**
     * Get all interrupt voltage values.
     * 
     * throw OperationFailException if there is any errors when get voltage value.
     * return SafetyFloat Array if get all interrupt voltage values.
     * SafetyFloat array:
     * Range: 4
     * Unit: SafetyFloat
     * Scaling: 1
     * The value in SafetyFloat array:
     * Range: refer to NUGEN Hardware Specification Document
     * Unit: SafetyFloat Array
     * Scaling: 1
     */
    public SafetyFloat[] getAllInterruptVoltage() throws OperationFailException
    {
        SafetyFloat[] voltages = null;
        
        try
        {
            voltages = mService.getInterruptEventData();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        finally
        {
            // Apply to the coding standard
        }
        
        return voltages;
    }
}
