/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.controller.settings.ReadConfig
 * Brief: This class implemented for providing the interface to other modules access
 * the Configuration Matrix
 *
 * Create Date: 2015/5/7
 * $Revision: 23369 $
 * $Author: WilliyChiang $
 * $Id: ReadConfig.java 23369 2015-11-05 07:40:14Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.config;

import java.util.NoSuchElementException;

import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.DataTypeMismatchException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ReadConfig
{   
    // Error Code Base
    private static final int ERROR_CONFIG_MATRIX_ERR_BASE = 48800;
    // Error Code
    private static final int ERROR_GET_LONG_DATA_INTEGRITY = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 1;
    private static final int ERROR_GET_LONG_NO_SUCH_ELEMENT = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 2;
    private static final int ERROR_GET_LONG_TYPE_MISMATCH = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 3;
    private static final int ERROR_GET_INT_DATA_INTEGRITY = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 4;
    private static final int ERROR_GET_INT_NO_SUCH_ELEMENT = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 5;
    private static final int ERROR_GET_INT_TYPE_MISMATCH = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 6;
    private static final int ERROR_GET_STRING_DATA_INTEGRITY = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 7;
    private static final int ERROR_GET_STRING_NO_SUCH_ELEMENT = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 8;
    private static final int ERROR_GET_STRING_TYPE_MISMATCH = 
                                    ERROR_CONFIG_MATRIX_ERR_BASE + 9;
    
    
    /**
     * Status: Coding/Done
     * 
     * Get Long type data from Configuration Matrix 
     * 
     * @param sKey [in] SafetyString
     * 
     *         ID name of Meter Parameter
     * 
     *         Range: Valid SafetyString object.
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @return SafetyNumber<Long> [out]
     *  
     *         Long type data from Configuration Matrix
     *        
     *         Range: Valid SafetyNumber<Long> object.
     *         Unit: SafetyNumber<Long>
     *         Scaling: 1
     */
    public static SafetyNumber<Long> getLongDataByKey(
            SafetyString sKey)
    {
        // Configuration Matrix Instance
        MeterParameterMatrix mConfigMatrix = 
                MeterParameterMatrix.getMeterParameterMatrixInstance();
        // Returned Value
        SafetyNumber<Long> safetyData = null;
        // Error Code
        int iErrorCode = 0;
        
        // Check whether input parameter is null or not 
        CommonUtils.objectCheck(sKey);
        
        try
        {
            safetyData = mConfigMatrix.getParameterLong(sKey);
            
            iErrorCode = 0; 
        }
        catch (DataIntegrityException e)
        {
            iErrorCode = ERROR_GET_LONG_DATA_INTEGRITY;
        }
        catch (NoSuchElementException e)
        {
            iErrorCode = ERROR_GET_LONG_NO_SUCH_ELEMENT;
        }
        catch (DataTypeMismatchException e)
        {
            iErrorCode = ERROR_GET_LONG_TYPE_MISMATCH;
        }
        finally
        {
            // Apply to coding standard
        }
        
        // if error code is not 0, go to safe state 
        if (iErrorCode != 0)
        {
            safetyData = null;
            
            // Go to safe state with error code
            callEmwr(iErrorCode);
        }
        else
        {
            // Apply to coding standard
        }
        
        return safetyData;
    }
    
    /**
     * Status: Coding/Done
     * 
     * Get Integer type data from Configuration Matrix 
     * 
     * @param sKey [in] SafetyString
     * 
     *         ID name of Meter Parameter
     * 
     *         Range: Valid SafetyString object.
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @return SafetyNumber<Integer> [out]
     *  
     *         Integer type data from Configuration Matrix
     *        
     *         Range: Valid SafetyNumber<Integer> object.
     *         Unit: SafetyNumber<Integer>
     *         Scaling: 1
     */
    public static SafetyNumber<Integer> getIntegerDataByKey(
            SafetyString sKey)
    {
        // Configuration Matrix Instance
        MeterParameterMatrix mConfigMatrix = 
                MeterParameterMatrix.getMeterParameterMatrixInstance();
        // Returned Value
        SafetyNumber<Integer> safetyData = null;
        // Error Code
        int iErrorCode = 0;        
        
        // Check whether input parameter is null or not 
        CommonUtils.objectCheck(sKey);

        try
        {
            safetyData = mConfigMatrix.getParameterInteger(sKey);
            iErrorCode = 0; 
        }
        catch (DataIntegrityException e)
        {
            iErrorCode = ERROR_GET_INT_DATA_INTEGRITY;
        }
        catch (NoSuchElementException e)
        {
            iErrorCode = ERROR_GET_INT_NO_SUCH_ELEMENT;
        }
        catch (DataTypeMismatchException e)
        {
            iErrorCode = ERROR_GET_INT_TYPE_MISMATCH;
        }
        finally
        {
            // Apply to coding standard
        }
        
        // if error code is not 0, go to safe state 
        if (iErrorCode != 0)
        {
            safetyData = null;
            
            // Go to safe state with error code
            callEmwr(iErrorCode);
        }
        else
        {
            // Apply to coding standard
        }
        
        return safetyData;
    }
    
    /**
     * Status: Coding/Done
     * 
     * Get String type data from Configuration Matrix 
     * 
     * @param sKey [in] SafetyString
     * 
     *         ID name of Meter Parameter
     * 
     *         Range: Valid SafetyString object.
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @return SafetyString [out]
     *  
     *         String type data from Configuration Matrix
     *        
     *         Range: Valid SafetyString object.
     *         Unit: SafetyString
     *         Scaling: 1
     */
    public static SafetyString getStringDataByKey(SafetyString sKey)
    {
        // Configuration Matrix Instance
        MeterParameterMatrix mConfigMatrix = 
                MeterParameterMatrix.getMeterParameterMatrixInstance();
        // Returned Value
        SafetyString safetyData = null;
        // Error Code
        int iErrorCode = 0;        
        
        // Check whether input parameter is null or not 
        CommonUtils.objectCheck(sKey);        

        try
        {
            safetyData = mConfigMatrix.getParameterString(sKey);
            iErrorCode = 0; 
        }
        catch (DataIntegrityException e)
        {
            iErrorCode = ERROR_GET_STRING_DATA_INTEGRITY;
        }
        catch (NoSuchElementException e)
        {
            iErrorCode = ERROR_GET_STRING_NO_SUCH_ELEMENT;
        }
        catch (DataTypeMismatchException e)
        {
            iErrorCode = ERROR_GET_STRING_TYPE_MISMATCH;
        }
        finally
        {
            // Apply to coding standard
        }
        
        // if error code is not 0, go to safe state 
        if (iErrorCode != 0)
        {
            safetyData = null;
            
            // Go to safe state with error code
            callEmwr(iErrorCode);
        }
        else
        {
            // Apply to coding standard
        }
        
        return safetyData;
    }
    
    /**
     * Status: Not Ready for review 
     * 
     * Trigger EMWR for error handling
     * 
     * @param ErrorCode [in] int
     * 
     *         Error code of Configuration Matrix
     * 
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     * 
     * @return None
     */
    protected static void callEmwr(int ErrorCode)
    {    
        // TODO : Fix it when EMWR and Log is ready
        // Log error message
//        LogFile logFile = LogFile.getInstance(getApplicationContext());
//        Exception ex = new Exception("Get battery level fail.");
//
//        LogContent content = new LogException(TAG, ex);
//        LogFile.getInstance().log(content);
        
        // Display EWMR 
//        NotifyMessage msg = new NotifyMessage(EMWRList.E57_E_RC_ELECTRONIC);
//        
//        NotifyProxy.showEMWR(context, msg); 
        
        
        
    }    
}
/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
//Add BRP and TBR constants config matrix keys
// (R14458 2015-08-07 03:30:55 JacksonHuang)
// ----------------------------------------------------------------------------
// Remove constant string and functions for specified modules
// (R15034 2015-08-19 04:05:05 JacksonHuang)
// ----------------------------------------------------------------------------
// The method switchToSafeState change the name to callEmwr
