/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker3b
 * Brief: Provide the interface function of the Picker3b UI component
 *
 * Create Date: 10/19/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Picker3b.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Picker3b extends PickerTime
{
    
    private static final int MAX_HOUR = 12;
    
    private static final int PICKER_LAYOUT_ID = R.layout.picker3b;
    
    private static final int PICKER_HOUR_ID = R.id.picker3b_title_hours;
    
    private static final int PICKER_TIME_FORMAT_ID = R.id.picker3b_title_ampm;
    
    private final static int[] PICKER_AMPM_AR = {R.string.txt_12hram, R.string.txt_12hrpm};

    private int mIsAm = 0;
    
    private View mAmPmView = null;
    
    /**
     * @param layoutId
     * @param hourViewId
     */
    public Picker3b(int hours, int minutes, int TimeFormat)
    {
        super(PICKER_LAYOUT_ID, PICKER_HOUR_ID);
        setHours(hours);
        setMinutes(minutes);
        mIsAm = TimeFormat;
    }
    
    /**
     * 
     * 
     *
     * @param activity
     * @param parent
     * @return
     */
    @Override
    public View createView(Activity activity, ViewGroup parent)
    {
        View picker = super.createView(activity, parent);
        
        refreshFormat();
        
        mAmPmView = picker.findViewById(PICKER_TIME_FORMAT_ID);
        mAmPmView.setOnClickListener(new TimeFormatOnClick());
        
        return picker;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getTimeFormat()
    {
        return mIsAm;
    }   

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addToFormat()
    {
        if (mIsAm == 1)
        {
            mIsAm = 0;
        } 
        else
        {
            mIsAm = 1;
        }
        refreshFormat();
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyTimeFormat()
    {
        return CommonUtils.getSafetyChannel(getTimeFormat());
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
        return R.id.id_picker3b_img2;
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
        return R.id.id_picker3b_img1;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    protected int onHourViewId()
    {
        return R.id.picker3b_title_hours;
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
        return R.id.picker3b_title_minutes;
    }
    
    private void refreshFormat() 
    {
        View view = getView();
        if (view != null) 
        {
            // Modified by Henry Tso. To support UIAutomator
            UIHelper.setText(view, PICKER_TIME_FORMAT_ID, PICKER_AMPM_AR[mIsAm], "ampm");
        }
    }

    /**
     * 
     */
    private class TimeFormatOnClick implements View.OnClickListener
    {

        /**
         * 
         * 
         *
         * @param v
         */
        @Override
        public void onClick(final View v)
        {
            addToFormat();
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
// [Setting] 1. RCSWSPUI44.8
// 2. GUI code refactoring
