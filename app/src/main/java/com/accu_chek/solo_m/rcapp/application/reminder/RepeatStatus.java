/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: RepeatStatus
 * Brief: 
 *
 * Create Date: 11/25/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public enum RepeatStatus
{
    INFUSION_SET_ONCE(HammingValues.HAMMING_HD4_VALUE_0067, R.string.txt_once, false),     // TODO check Hamming value
    INFUSION_SET_REPEAT(HammingValues.HAMMING_HD4_VALUE_0068, R.string.txt_repeat, true), // TODO check Hamming value
    BASAL_INJECTION_ONCE(HammingValues.HAMMING_HD4_VALUE_0067, R.string.txt_once, false),
    BASAL_INJECTION_REPEAT(HammingValues.HAMMING_HD4_VALUE_0068, R.string.txt_repeat, true),
    ALARM_CLOCK_ONCE(HammingValues.HAMMING_HD4_VALUE_0067, R.string.txt_once, false),
    ALARM_CLOCK_REPEAT(HammingValues.HAMMING_HD4_VALUE_0068, R.string.txt_repeat, true),
    BG_TEST_ONCE(HammingValues.HAMMING_HD4_VALUE_0067, R.string.txt_once, false),
    BG_TEST_REPEAT(HammingValues.HAMMING_HD4_VALUE_0068, R.string.txt_repeat, true),
    MISSED_BOLUS_ONCE(HammingValues.HAMMING_HD4_VALUE_0067, R.string.txt_once, false),
    MISSED_BOLUS_REPEAT(HammingValues.HAMMING_HD4_VALUE_0068, R.string.txt_repeat, true);

    private int mCode = -1;
    private int mTextId = -1;
    private boolean mIsRepeat = false;

    private RepeatStatus(int code, int textId, boolean isRepeat)
    {
        mCode = code;
        mTextId = textId;
        mIsRepeat = isRepeat;
    }
    
    public boolean getStatus()
    {
        return mIsRepeat;
    }

    public int getCode()
    {
        return mCode;
    }
    
    public int getTextId()
    {
        return mTextId;
    }

    public static RepeatStatus fromCode(int code)
    {
        RepeatStatus object = RepeatStatus.BG_TEST_ONCE;
        
        for (RepeatStatus r : RepeatStatus.values())
        {
            int temp = r.getCode();
            
            if (temp == code)
            {
                object = r;
            }
        }
        
        return object;
    }
    
    public static RepeatStatus fromTextId(int textId)
    {
        RepeatStatus object = RepeatStatus.BG_TEST_ONCE;

        for (RepeatStatus r : RepeatStatus.values())
        {
            int temp = r.getTextId();
            
            if (temp == textId)
            {
                object = r;
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
