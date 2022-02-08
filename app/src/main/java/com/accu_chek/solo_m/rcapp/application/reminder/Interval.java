/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Interval
 * Brief: 
 *
 * Create Date: 11/25/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public enum Interval
{
    ONE_DAY(24, R.string.txt_1dayinterval),
    TWO_DAY(48, R.string.txt_2daysinterval),
    THREE_DAY(72, R.string.txt_3daysinterval);
    
    private int mHour = -1;
    private int mTextId = -1;
    
    private Interval(int hours, int textId)
    {
        mHour = hours;
        mTextId = textId;
    }
    
    public int getHour()
    {
        return mHour;
    }
    
    public int getTextId()
    {
        return mTextId;
    }
    
    public static Interval fromHour(int hour)
    {
        Interval object = Interval.THREE_DAY;
        
        for (Interval i : Interval.values())
        {
            int temp = i.getHour();
            
            if (temp == hour)
            {
                object = i;
            }
        }
        
        return object;
    }

    public static Interval fromTextId(int textId)
    {
        Interval object = Interval.THREE_DAY;
        
        for (Interval i : Interval.values())
        {
            int temp = i.getTextId();
            
            if (temp == textId)
            {
                object = i;
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
