/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ConfigurationMatrixWrapper
 * Brief: The class is used to access the Configuration Matrix.
 *
 * Create Date: 10/13/2015
 * $Revision: 23372 $
 * $Author: WilliyChiang $
 * $Id: ConfigurationMatrixWrapper.java 23372 2015-11-05 07:52:14Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.settinginterface;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrix;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.DataTypeMismatchException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

import java.util.NoSuchElementException;

public final class ConfigurationMatrixWrapper
{
    
    /**
     * The initial value of an integer variable.
     */
    private static final int INT_INITIAL_VALUE = -1;
    
    /**
     * The definition is the default values of a user setting.
     */
    private static final String EMPTY_STRING = "";
    
    /**
     * The constructor of ConfigurationMatrixWrapper. To avoid static code analysis issues,
     * the class needs a private constructor.
     * 
     * @param None
     *       
     * @return None
     */
    private ConfigurationMatrixWrapper()
    {
        // Apply to the coding standard
    }
    
    /**
     * Get the SafetyNumber<Long> value of a setting from the Configuration Matrix.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The key of a setting in the Configuration Matrix.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The SafetyNumber<Long> value of a setting.
     *       Range: Valid SafetyNumber<Long> object
     *       Unit: SafetyNumber<Long>
     *       Scaling: 1
     */
    public static SafetyNumber<Long> getLong(final SafetyString key)
    {
        SafetyNumber<Long> result = null;
        
        try
        {
            // Get the value from the Configuration Matrix
            result = MeterParameterMatrix.getMeterParameterMatrixInstance()
                    .getParameterLong(key);
        }
        catch (DataIntegrityException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        catch (NoSuchElementException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        catch (DataTypeMismatchException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        finally
        {
            // Apply to coding standard
        }
        
        // Return the result
        return result;
    }
    
    /**
     * Get the SafetyString value of a setting from the Configuration Matrix.
     *       
     * @param key The key of a setting in the Configuration Matrix.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The SafetyString value of a setting.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    public static SafetyString getString(final Context context, final SafetyString key)
    {   
        SafetyString result = null;
        boolean isEqualToEmptyString = false;
        
        // Get the value from the Production Mode
        result = NugenProductionModel.getString(context, key.getString());
        
        // Compare the value and an empty string
        isEqualToEmptyString = result.getString().equalsIgnoreCase(EMPTY_STRING);
        
        // Is the Value get from the Production Mode equal to an empty string?
        if (true == isEqualToEmptyString)
        {
            try
            {
                // Get the value from the Configuration Matrix
                result = MeterParameterMatrix.getMeterParameterMatrixInstance()
                        .getParameterString(key);
            }
            catch (DataIntegrityException exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            catch (NoSuchElementException exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            catch (DataTypeMismatchException exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            finally
            {
                // Apply to coding standard
            }
        }
        
        // Return the result
        return result;
    }
    
    /**
     * Get the SafetyNumber<Integer> value of a setting from the Configuration Matrix.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The key of a setting in the Configuration Matrix.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The SafetyNumber<Integer> value of a setting.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     */
    public static SafetyNumber<Integer> getInteger(final Context context, final SafetyString key)
    {
        SafetyNumber<Integer> result = null;
        int resultValue = INT_INITIAL_VALUE;
        
        // Get the value from the Production Mode
        result = NugenProductionModel.getInt(context, key.getString());
        
        // Get the value
        resultValue = result.get();
        
        // Is the Value get from the  Production Mode equal to -1?
        if (INT_INITIAL_VALUE == resultValue)
        {
            try
            {
                // Get the value from the Configuration Matrix
                result = MeterParameterMatrix.getMeterParameterMatrixInstance()
                        .getParameterInteger(key);
            }
            catch (DataIntegrityException exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            catch (NoSuchElementException exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            catch (DataTypeMismatchException exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            finally
            {
                // Apply to coding standard
            }        
        }
        
        // Return the result
        return result;
    }
    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
