/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker3a_endTime_ss
 * Brief: 
 *
 * Create Date: 10/21/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import java.util.ArrayList;

import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class Picker3a_endTime_ss extends Picker3a
{

    private static final int MAX_HOUR_24H = 24;
    private static final String TAG = Picker3a_endTime_ss.class.getSimpleName();
    private int mStartHour = 0;
    private int mMaxDuration = 12;
    private ArrayList<Integer> mAvailableEndHours = null;
    
    /**
     * @param hours
     * @param minutes
     */
    public Picker3a_endTime_ss(int hours, int minutes, int startHour, int maxDuration)
    {
        super(hours, minutes);
        
        mStartHour = startHour;
        mMaxDuration = maxDuration;
        mAvailableEndHours = getAvailabeEndHours();
    }

//    public void setStartHour(int value)
//    {
//        mStartHour = value;
//        
//        mAvailabeEndHours = getAvailabeEndHours();
//    }
//    
//    public void setMaxDuration(int value)
//    {
//        mMaxDuration = value;
//    }

    private ArrayList<Integer> getAvailabeEndHours()
    {
        Debug.printI(TAG, "[Enter] getAvailabeEndHours()");
        Debug.printI(TAG, "mStartHour: "+ mStartHour);
        Debug.printI(TAG, "mMaxDuration: "+ mMaxDuration);

        ArrayList<Integer> list = new ArrayList<Integer>();
        
        for (int i = mStartHour; i < (mStartHour + mMaxDuration); i++)
        {
            int value = checkHour(i);
            
            list.add(value);
            
            Debug.printI(TAG, "hour: " + value);
        }
        
        Debug.printI(TAG, "list.size(): " + list.size());

        return list;

    }

    @Override
    public void addToHours(int value)
    {
        int hour = checkHour(getValue() + value);
        
        Debug.printI(TAG, "[Enter] addToHours()");
        Debug.printI(TAG, "hour: "+ hour);

        boolean isWithinRange = mAvailableEndHours.contains(hour);
        
        Debug.printI(TAG, "isWithinRange: "+ isWithinRange);

        if (isWithinRange)
        {
            setValue(hour);
        }
        else
        {
            setValue(mStartHour);
        }
        
        refreshValue();
    }

    @Override
    protected void setHours(int hours)
    {
        setValue(hours);
    }

    private int checkHour(int hour)
    {
        if (hour >= MAX_HOUR_24H)
        {
            hour -= MAX_HOUR_24H;
        }
        return hour;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
