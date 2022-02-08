package com.accu_chek.solo_m.rcapp.application.reminder;

import com.accu_chek.solo_m.rcapp.application.reminder.AlarmData;

interface IReminderAlarm
{
    void set(in AlarmData data);
    void cancel(in AlarmData data);
}