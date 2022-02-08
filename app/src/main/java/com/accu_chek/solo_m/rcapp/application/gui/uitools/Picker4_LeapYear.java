/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker4_LeapYear
 * Brief: Provide the interface function of the Picker4_LeapYear UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Picker4_LeapYear.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import java.util.HashMap;
import java.util.Map;


public class Picker4_LeapYear extends Picker4
{
    
    private static final int SELECT_DAY = 29;

    private static final int ZERO = 0;

    private static final int SELECT_MONTH = 2;

    private static final int LEAP_CONDITION1 = 4;
    
    private static final int LEAP_CONDITION2 = 100;
    
    private static final int LEAP_CONDITION3 = 400;
    
    private static Map<Integer, Integer> mDayMap = new HashMap<Integer, Integer>();

    static 
    {
        mDayMap.put(1, 31);
        mDayMap.put(2, 28);
        mDayMap.put(3, 31);
        mDayMap.put(4, 30);
        mDayMap.put(5, 31);
        mDayMap.put(6, 30);
        mDayMap.put(7, 31);
        mDayMap.put(8, 31);
        mDayMap.put(9, 30);
        mDayMap.put(10, 31);
        mDayMap.put(11, 30);
        mDayMap.put(12, 31);
    }
    
    /**
     * 
     * @param day
     * @param month
     * @param year
     */
    public Picker4_LeapYear(int day, int month, int year)
    {
        super(day, month, year);
    }

    /**
     * 
     * 
     *
     * @param month
     */
    @Override
    protected void setMonth(int month)
    {
        super.setMonth(month);
        
        int selectedYear = getYear();
        int maxYear = getMaxYear();
        int minYear = getMinYear();

        boolean isYearValid = (selectedYear <= maxYear) && (selectedYear >= minYear);
        
        // set default year to 2015
        if (! isYearValid)
        {
            setYear(2015);
        }
        
        decideMaxDays();
        
        // update displayed day if necessary
        updateDaysFromMonth();
    }

    /**
     * 
     * 
     *
     * @param year
     */
    @Override
    protected void setYear(int year)
    {
        super.setYear(year);
        
        Integer selectedMonth = mDayMap.get(getMonth());
        
        // set default month to 2
        if (selectedMonth == null)
        {
            setMonth(2);
        }

        decideMaxDays();
        
        // update displayed day if necessary
        updateDaysFromYear();

    }
    
    /**
     * 
     * 
     *
     */
    @Override
    protected void refreshMonth()
    {
        super.refreshMonth();
        
        // update displayed day
        refreshValue();
    }

    /**
     * 
     * 
     *
     */
    @Override
    protected void refreshYear()
    {
        super.refreshYear();
        
        // update displayed day
        refreshValue();
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void decideMaxDays()
    {
        int selectedYear = getYear();
        int selectedMonth = getMonth();
        int decidedMaxDay = -1;
        
        boolean isLeapYear = checkLeapYear(selectedYear);
        
        if (selectedMonth == SELECT_MONTH)
        {
            if (isLeapYear)
            {
                decidedMaxDay = 29;
            }
            else
            {
                decidedMaxDay = 28;
            }
        }
        else
        {
            decidedMaxDay = mDayMap.get(selectedMonth);
        }
        
        setMaxDay(decidedMaxDay);
    }
    
    /**
     * 
     * Function Description
     *
     * @param year
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter
     *         Description
     */
    private boolean checkLeapYear(int year)
    {
        boolean isLeapYear = ((year % LEAP_CONDITION1 == ZERO) && (year
                % LEAP_CONDITION2 != ZERO))
                || (year % LEAP_CONDITION3 == ZERO);
        
        return isLeapYear;
    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void updateDaysFromMonth()
    {
        int selectedMonth = getMonth();
        int currentDayOfMonth = mDayMap.get(selectedMonth);
        int LastDayOfMonth = getDay();
        
        if (currentDayOfMonth < LastDayOfMonth)
        {
            boolean isLeapYear = checkLeapYear(getYear());
            boolean isDayUpdateNeeded = (isLeapYear) && (selectedMonth == SELECT_MONTH);
            
            if (isDayUpdateNeeded)
            {
                currentDayOfMonth = 29;
            }
            
            setDay(currentDayOfMonth);
        }
    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void updateDaysFromYear()
    {
        int selectedYear = getYear();
        boolean isLeapYear = checkLeapYear(selectedYear);
        boolean isDayUpdateNeeded = (!isLeapYear)
                && (getMonth() == SELECT_MONTH) && (getDay() == SELECT_DAY);
        
        if (isDayUpdateNeeded)
        {
            setDay(28);
        }
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
