/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ReminderUtils
 * Brief: 
 *
 * Create Date: 11/27/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class ReminderUtils
{
    
    public static Calendar getCalendarTime(String time)
    {
        Calendar cal = Calendar.getInstance();
        int hour = 23;
        int minute = 59;
        
        if (time != null)
        {
            String[] temp = time.split(":");
            hour = Integer.valueOf(temp[0]);
            minute = Integer.valueOf(temp[1]);
        }

        cal.set(Calendar.HOUR_OF_DAY, hour);
        cal.set(Calendar.MINUTE, minute);
        
        return cal;
    }
    
    public static String getTimeInfo(long time)
    {
        SimpleDateFormat formatter = new SimpleDateFormat("HH:mm:ss MMM dd yyyy");
        Date curDate = new Date(time);
        return formatter.format(curDate);
    }

    public static int getYear(String date)
    {
        return getDateValue(date, 2);
    }

    public static int getMonth(String date)
    {
        return getDateValue(date, 1);
    }

    public static int getDay(String date)
    {
        return getDateValue(date, 0);
    }

    public static int getDateValue(String date, int index)
    {
        int value = 0;
        String[] temp = date.split("-");
        boolean isDateFormatValid = (temp.length == 3);
        
        if (isDateFormatValid)
        {
            value = Integer.valueOf(temp[index]);
        }
        
        return value;
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
// [Reminder] 1. add comment 2. coding rule
