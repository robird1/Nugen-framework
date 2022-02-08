/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractReminderTable
 * Brief: 
 *
 * Create Date: 11/6/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.data.operationhandler;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;


public abstract class AbstractReminderTable extends AbstractTable
{
    
    // Table column name. This column value must not be null when inserting or
    // an SQLiteException will occur.
    public static final String COLUMN_ALARM_REQUEST_CODE = "alarm_request_code";

    
    // TODO move subclass method to here
    public SafetyChannel<Integer> getRecordId()
    {
        return null;
    }

    // TODO move subclass method to here
    public SafetyChannel<Integer> getState()   // 
    {
        return null;
    }
    
    public SafetyString getTime()
    {
        return null;
    }
    
    public SafetyString getDate()   // 
    {
        return null;
    }

    // TODO move subclass method to here
    public SafetyString getTone()   // 
    {
        return null;
    }

    public SafetyChannel<Integer> getRepeatStatus()   // 
    {
        return null;
    }

    // TODO move subclass method to here
    public SafetyChannel<Integer> getAlarmRequestCode()
    {
        return null;
    }

    public SafetyChannel<Integer> getBGThreshold()    //
    {
        return null;
    }

    public SafetyChannel<Integer> getRemindAfterMinute()
    {
        return null;
    }
    
    public SafetyString getName()   // 
    {
        return null;
    }
    
    public SafetyChannel<Integer> getInterval()     //
    {
        return null;
    }

    public SafetyChannel<Integer> getEMWRCode()     //
    {
        return null;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Reminder] fix the bug of canceling alarm
// [Reminder] add screen - missed bolus reminder, basal injection reminder
// [Reminder] 1. add bG after high / low / meal reminders
// 2. add doctor visit and lab test reminders
// [Reminder] add Alarm Clock / Custom reminders
// [Reminder] fix compilation error
