/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker3a
 * Brief: Provide the interface function of the Picker3a UI component
 *
 * Create Date: 10/19/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Picker3a.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Picker3a extends PickerTime 
{

    private static final int MAX_HOUR = 24;
    
    private static final int PICKER_LAYOUT_ID = R.layout.picker3a;
    
    private static final int PICKER_HOUR_ID = R.id.picker3a_title_hours;

    /**
     * @param layoutId
     * @param valueId
     * @param value
     */
    public Picker3a(int hours, int minutes)
    {
        super(PICKER_LAYOUT_ID, PICKER_HOUR_ID);
        setHours(hours);
        setMinutes(minutes);
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    protected int onMaxHour()
    {
        return MAX_HOUR;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    protected int onPlusViewId()
    {
        return R.id.id_picker3a_img2;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    protected int onMinusViewId()
    {
        return R.id.id_picker3a_img1;
    }

    /**
     *
     */
    @Override
    protected int onHourViewId()
    {
        return R.id.picker3a_title_hours;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    protected int onMinuteViewId()
    {
        return R.id.picker3a_title_minutes;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Setting] 1. RCSWSPUI44.8
// 2. GUI code refactoring
