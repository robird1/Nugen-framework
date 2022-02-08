/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SupportedSound
 * Brief: 
 *
 * Create Date: 11/25/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public enum SupportedSound
{
    ROCKY(R.string.txt_tonerocky, "Rocky.wav"),
    VIBE(R.string.txt_tonevibe, "Vibe.wav"),
    CALYPSO(R.string.txt_tonecalypso, "Calypso.wav"),
    JINGLE(R.string.txt_tonejingle, "Jingle.wav"),
    BELL(R.string.txt_tonebell, "Bell.wav"),
    SUNRISE(R.string.txt_tonesunrise, "Sunrise.wav"),
    SMOOTH(R.string.txt_smooth, "Smooth.wav"),
    ENERGETIC(R.string.txt_energetic, "Energetic.wav");

//    WARM_BOOT(R.string.NA, "Warm_Boot.wav"),
//    USB_OUT(R.string.NA, "USB_OUT.wav"),
//    USB_IN(R.string.NA, "USB_IN.wav"),
//    SND_DM_INFO_BEEP(R.string.NA, "SND_DM_INFO_BEEP.wav"),
//    PUMP_ERROR_8K(R.string.NA, "Pump_Error8k.wav"),
//    PUMP_MAINTENANCE_8K(R.string.NA, "Pump_Maintenance8k.wav"),
//    PUMP_WARNING_8K(R.string.NA, "Pump_Warning8k.wav"),
//    PUMP_RMINDER_1(R.string.NA, "Pump_Reminder 1.wav"),
//    PUMP_RMINDER_2(R.string.NA, "Pump_Reminder 2.wav"),
//    PUMP_RMINDER_3(R.string.NA, "Pump_Reminder 3.wav"),
//    PUMP_RMINDER_4(R.string.NA, "Pump_Reminder 4.wav"),
//    PUMP_RMINDER_5(R.string.NA, "Pump_Reminder 5.wav"),
//    PUMP_RMINDER_6(R.string.NA, "Pump_Reminder 6.wav"),
//    PUMP_RMINDER_7(R.string.NA, "Pump_Reminder 7.wav"),
//    PUMP_RMINDER_8(R.string.NA, "Pump_Reminder 8.wav"),
//    PUMP_RMINDER_9(R.string.NA, "Pump_Reminder 9.wav"),
//    PUMP_RMINDER_10(R.string.NA, "Pump_Reminder 10.wav"),
//    METER_ERROR(R.string.NA, "Meter_Error.wav"),
//    METER_MAINTENANCER(R.string.NA, "Meter_Maintenance.wav"),
//    METER_WARNING(R.string.NA, "Meter_Warning.wav"),
//    COMM_COMPLETE(R.string.NA, "Comm_Complete.wav"),
//    COLD_BOOT(R.string.NA, "Cold_Boot.wav"),
//    BUTTON_PRESS(R.string.NA, "Button_Press.wav"),
//    BUTTON_DISABLE(R.string.NA, "Button_Disable.wav"),
//    HELLO(R.string.txt_hello, "Hello.wav"),
//    BEEP(R.string.txt_beep, "Beep.wav");
    

    private int mTextId = -1;
    private String mFileName = null;
    private SupportedSound(int textId, String fileName)
    {
        mTextId = textId;
        mFileName = fileName;
    }
    
    public int getTextId()
    {
        return mTextId;
    }
    
    public String getFileName()
    {
        return mFileName;
    }
    
    public static SupportedSound fromFileName(String name)
    {
        SupportedSound enumConstant = null;
        for (SupportedSound sound : SupportedSound.values())
        {
            String temp = sound.getFileName();
            if (name.equals(temp))
            {
                enumConstant = sound;
            }
        }
        
        return enumConstant;
    }
    
    public static SupportedSound fromTextId(int textId)
    {
        SupportedSound enumConstant = ROCKY;
        for (SupportedSound sound : SupportedSound.values())
        {
            int temp = sound.getTextId();
            if (textId == temp)
            {
                enumConstant = sound;
            }
        }
        
        return enumConstant;
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
