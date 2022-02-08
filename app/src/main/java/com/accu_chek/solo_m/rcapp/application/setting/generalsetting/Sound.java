/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: Sound
 * Brief: This enum type is for helping obtaining a certain enum constant from a
 * given button index or a given Hamming code.
 * 
 * Create Date: 09/18/2015
 * $Revision: 23722 $
 * $Author: SteveSu $
 * $Id: Sound.java 23722 2015-11-10 03:52:05Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.setting.generalsetting;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;

public enum Sound
{
    // enum constant  
    Loud(HammingValues.HAMMING_HD4_VALUE_0036, 0), 
    // enum constant  
    Normal(HammingValues.HAMMING_HD4_VALUE_0033, 1),
    // enum constant  
    Quiet(HammingValues.HAMMING_HD4_VALUE_0035, 2), 
    // enum constant  
    Vibrate(HammingValues.HAMMING_HD4_VALUE_0034, 3);

    // Hamming code of a certain enum constant
    private int mCode = -1;
    // Button index of a certain enum constant
    private int mBtnIndex = -1;
    
    /**
     * This enum constructor shall initialize the attributes of the enum
     * constant.
     * 
     * @param code : The Hamming code of an enum constant.
     *            Range: HammingValues.HAMMING_HD4_VALUE_0033,
     *            HammingValues.HAMMING_HD4_VALUE_0034,
     *            HammingValues.HAMMING_HD4_VALUE_0035,
     *            HammingValues.HAMMING_HD4_VALUE_0036
     *            Unit: int
     *            Scaling: 1
     * @param buttonIndex : The button index of an enum constant.
     *            Range: 0, 1, 2, 3
     *            Unit: int
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mCode
     * @see mBtnIndex
     */
    private Sound(final int code, final int buttonIndex)
    {
        mCode = code;
        mBtnIndex = buttonIndex;
    }
    
    /**
     * Get Hamming code of a certain enum constant.
     * 
     * @return Hamming code of a certain enum constant.
     *            Range: HammingValues.HAMMING_HD4_VALUE_0033,
     *            HammingValues.HAMMING_HD4_VALUE_0034,
     *            HammingValues.HAMMING_HD4_VALUE_0035,
     *            HammingValues.HAMMING_HD4_VALUE_0036
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mCode
     */
    public int getCode()
    {
        return mCode;
    }
    
    /**
     * Get button index of a certain enum constant.
     * 
     * @return button index of a certain enum constant.
     *         Range: 0, 1, 2, 3
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mBtnIndex
     */
    public int getButtonIndex()
    {
        return mBtnIndex;
    }
    
    /**
     * A util function for obtaining a certain enum constant from a given Hamming code.
     * 
     * @param code : The Hamming code of an enum constant.
     *            Range: HammingValues.HAMMING_HD4_VALUE_0033,
     *            HammingValues.HAMMING_HD4_VALUE_0034,
     *            HammingValues.HAMMING_HD4_VALUE_0035,
     *            HammingValues.HAMMING_HD4_VALUE_0036
     *            Unit: int
     *            Scaling: 1
     * 
     * @return An enum constant.
     *         Range: Valid enum constant.
     *         Unit: Sound
     *         Scaling: 1
     */
    public static Sound fromCode(final int code)
    {
        Sound instance = Sound.Normal;
        
        for (Sound sound : Sound.values())
        {
            final int temp = sound.getCode();
            
            if (temp == code)
            {
                instance = sound;
            }
        }
        
        return instance;
    }

    /**
     * A util function for obtaining a certain enum constant from a given button index.
     * 
     * @param btnIndex : The button index of an enum constant.
     *            Range: 0, 1, 2, 3
     *            Unit: int
     *            Scaling: 1
     * 
     * @return An enum constant.
     *         Range: Valid enum constant.
     *         Unit: Sound
     *         Scaling: 1
     */
    public static Sound fromBtnIndex(final int btnIndex)
    {
        Sound instance = Sound.Normal;

        for (Sound sound : Sound.values())
        {
            final int temp = sound.getButtonIndex();
            
            if (temp == btnIndex)
            {
                instance = sound;
            }
        }
        
        return instance;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// fix build error
//----------------------------------------------------------------------------
// [Setting] update code
// (R19842 2015-09-25 05:33:00 AdamChen)
// ----------------------------------------------------------------------------
// [Setting] 1. fix Klocwork issues
// 2. fix Checkstyle issues
// (R21079 2015-10-06 05:50:12 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] 1. add function comment
// 2. function renaming
// (R21156 2015-10-07 05:02:40 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] Add and update comment
// (R21230 2015-10-08 04:25:28 SteveSu)
// ----------------------------------------------------------------------------
// [Settings] update code based on code review findings
// (R23670 2015-11-09 04:13:59 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update Hamming values defined in CM V8
