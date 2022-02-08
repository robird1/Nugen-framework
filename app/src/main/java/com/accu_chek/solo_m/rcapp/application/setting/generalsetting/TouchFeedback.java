/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: TouchScreen
 * Brief: This enum type is for helping obtaining a certain enum constant from a
 * given Hamming code or given button text ID.
 *
 * Create Date: 09/18/2015
 * $Revision: 23722 $
 * $Author: SteveSu $
 * $Id: TouchFeedback.java 23722 2015-11-10 03:52:05Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.setting.generalsetting;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public enum TouchFeedback
{
    
    // enum constant    
    NONE(HammingValues.HAMMING_HD4_VALUE_0032, R.string.txt_notone),
    
    // enum constant
    TONE(HammingValues.HAMMING_HD4_VALUE_0029, R.string.txt_tone),
    
    // enum constant
    TONE_VIBRATE(HammingValues.HAMMING_HD4_VALUE_0031, R.string.txt_tonevibration),
    
    // enum constant
    VIBRATION(HammingValues.HAMMING_HD4_VALUE_0030, R.string.txt_vibration);

    // Hamming code of a certain enum constant
    private int mCode = 0;
    
    // Text resource ID of a certain enum constant
    private int mTextId = 0;

    /**
     * This enum constructor shall initialize the attributes of the enum
     * constant.
     * 
     * @param code : The Hamming code of enum constant.
     *            Range: HammingValues.HAMMING_HD4_VALUE_0029,
     *            HammingValues.HAMMING_HD4_VALUE_0030,
     *            HammingValues.HAMMING_HD4_VALUE_0031,
     *            HammingValues.HAMMING_HD4_VALUE_0032
     *            Unit: int
     *            Scaling: 1
     * @param textId : The resource ID of enum constant.
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mCode
     * @see mTextId
     */
    private TouchFeedback(final int code, final int textId)
    {
        mCode = code;
        mTextId = textId;
    }

    /**
     * Get Hamming code of a certain enum constant.
     * 
     * @return Hamming code of a certain enum constant.
     *         Range: HammingValues.HAMMING_HD4_VALUE_0029,
     *            HammingValues.HAMMING_HD4_VALUE_0030,
     *            HammingValues.HAMMING_HD4_VALUE_0031,
     *            HammingValues.HAMMING_HD4_VALUE_0032
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
     * Get text resource ID of a certain enum constant.
     * 
     * @return Text resource ID of a certain enum constant.
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mTextId
     */
    public int getTextId()
    {
        return mTextId;
    }

    /**
     * A util function for obtaining a certain enum object from a given Hamming
     * code.
     * 
     * @param value : The Hamming code which corresponds to the feedback type.
     *            Range: HammingValues.HAMMING_HD4_VALUE_0029,
     *            HammingValues.HAMMING_HD4_VALUE_0030,
     *            HammingValues.HAMMING_HD4_VALUE_0031,
     *            HammingValues.HAMMING_HD4_VALUE_0032
     *            Unit: int
     *            Scaling: 1
     * 
     * @return An enum constant.
     *         Range: enum constant of TouchFeedback.
     *         Unit: TouchFeedback
     *         Scaling: 1
     */
    public static TouchFeedback fromCode(final int value)
    {
        TouchFeedback type = TouchFeedback.VIBRATION;
        int length = TouchFeedback.values().length;
        int i = 0;
        int tem = 0;
        
        // less than length and value not equal
        do
        {
            type = TouchFeedback.values()[i];
            tem = type.getCode();
            i++;
        }
        while ((i < length) && (tem != value));
        
        if(tem != value)
        {
            type = TouchFeedback.VIBRATION;
        }
        
        
//        for (TouchFeedback t : TouchFeedback.values())
//        {
//            final int temp = t.getCode();
//            
//            if (temp == value)
//            {
//                type = t;
//            }
//        }

        return type;
    }

    /**
     * A util function for obtaining a certain enum constant from a given text resource
     * ID.
     * 
     * @param value : The text resource ID which corresponds to the feedback type.
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @return An enum constant.
     *         Range: enum constant of TouchFeedback
     *         Unit: TouchFeedback
     *         Scaling: 1
     */
    public static TouchFeedback fromTextId(final int value)
    {
        TouchFeedback type = TouchFeedback.VIBRATION;
        
        for (TouchFeedback t : TouchFeedback.values())
        {
            final int temp = t.getTextId();
            
            if (temp == value)
            {
                type = t;
            }
        }

        return type;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Setting] move code from RCLauncher to framework
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
