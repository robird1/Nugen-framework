/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: E2ERetryCounter
 * Brief: 
 *
 * Create Date: 11/04/2015
 * $Revision: 24156 $
 * $Author: JacksonHuang $
 * $Id: E2ERetryCounter.java 24156 2015-11-16 05:50:26Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms;

import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;


public class E2ERetryCounter
{    
    // Limit of BLE E2E Retry Times 
    private int mE2ERetryLimit = 5;
    
    // E2E Retry Times
    private int mE2ERetryCount = 0;    
    
    public E2ERetryCounter()
    {
        // Key String
        SafetyString ssCmKey = null;
        
        // Load E2E Retry Limit
        // Get E2E retry limit from Config Matrix
        ssCmKey = new SafetyString(ConfigParameter.KEY_BLE_E2E_RETRY,
           CRCTool.generateCRC16(ConfigParameter.KEY_BLE_E2E_RETRY.getBytes()));
        
        mE2ERetryLimit = ReadConfig.getIntegerDataByKey(ssCmKey).get();
        
        // Reset counter
        mE2ERetryCount = 0;
    }
    
    /**
     * Reset E2E retry counter
     * 
     * @return None
     */ 
    
    public void resetCounter()
    {
        mE2ERetryCount = 0;
    }
    
    /**
     * Add E2E retry counter
     * 
     * @return None
     */ 
    
    public void addCounter()
    {
        mE2ERetryCount++;
    }  
    
    /**
     * Check counter if it is over the limit
     * 
     * @return SafetyBoolean [out]
     * 
     *          SafetyBoolean.TRUE: Counter is over the limit
     *          SafetyBoolean.FALSE: Counter is over the limit
     *          
     *          Range: Valid SafetyBoolean
     *          Unit: SafetyBoolean
     *          Scaling: 1 
     */ 
    
    public SafetyBoolean checkCounter()
    {
        // Check Result
        SafetyBoolean isOver = SafetyBoolean.FALSE;
        
        if (mE2ERetryCount > mE2ERetryLimit)
        {
            isOver = SafetyBoolean.TRUE;
        }
        else
        {
            isOver = SafetyBoolean.FALSE;
        }
        
        return isOver;
    }    
}


/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// [New Feature] Basal Delivery comms functions
// [Update] Modify comments
// [Update] Add WaitDelivery for long time delivery time
