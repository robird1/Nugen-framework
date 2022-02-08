/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker3b_suspension
 * Brief: 
 *
 * Create Date: 10/21/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

public class Picker3b_suspension extends Picker3b
{

    private int mStartHour = 0;
    
    /**
     * @param hours
     * @param minutes
     * @param TimeFormat
     */
    public Picker3b_suspension(int hours, int minutes, int TimeFormat)
    {
        super(hours, minutes, TimeFormat);
        // TODO Auto-generated constructor stub
    }
    
    public void setStartHour(int value)
    {
        mStartHour = value;
    }
    
    @Override
    protected void setHours(int hours)
    {
//        if (hours >= mMaxHours)
//        {
//            hours = hours - mMaxHours;
//            setHours(hours);
//        }
//        else if (hours < 0)
//        {
//            hours = hours + mMaxHours;
//            setHours(hours);
//        }
//        else
//        {
//            setValue(hours);
//        }

    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
