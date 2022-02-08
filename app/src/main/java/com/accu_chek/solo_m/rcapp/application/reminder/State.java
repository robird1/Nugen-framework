/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: State
 * Brief: 
 *
 * Create Date: 11/25/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;

public enum State
{
    INFUSION_SET_ON(HammingValues.HAMMING_HD4_VALUE_0071, true),
    INFUSION_SET_OFF(HammingValues.HAMMING_HD4_VALUE_0072, false),
    BASAL_INJECTION_ON(HammingValues.HAMMING_HD4_VALUE_0073, true),
    BASAL_INJECTION_OFF(HammingValues.HAMMING_HD4_VALUE_0074, false),
    DEFAULT_ON(HammingValues.HAMMING_HD4_VALUE_0065, true),        // TODO DISMISSED: 0064, SNOOZED: 0066
    DEFAULT_OFF(HammingValues.HAMMING_HD4_VALUE_0063, false);      // TODO DISMISSED: 0064, SNOOZED: 0066
    
    private int mCode = -1;
    private boolean mValue = false;
    
    private State(int code, boolean value)
    {
        mCode = code;
        mValue = value;
    }
    
    public int getCode()
    {
        return mCode;
    }
    
    public boolean getValue()
    {
        return mValue;
    }
    
    public static State fromCode(int code)
    {
        State object = State.INFUSION_SET_OFF;
        
        for (State s : State.values())
        {
            int temp = s.getCode();
            
            if (temp == code)
            {
                object = s;
            }
        }
        
        return object;
    }
    
    // TODO This function is tricky.
    public static State fromValue(boolean value)
    {
        State object = State.INFUSION_SET_OFF;

        for (State s : State.values())
        {
            boolean temp = s.getValue();
            
            if (temp == value)
            {
                object = s;
            }
        }
        
        return object;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
