/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0007c
 * Brief: Provide the function of the LAD0007c layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0007c.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B27;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B30;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0007c extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0007c;

    /**
     * 
     * @param activity
     */
    public LAD0007c(Activity activity)
    {
        super(activity);
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

    /**
     * 
     * Function Description
     *
     * @param slider
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setup(Button_B30 slider)
    {
        setup(slider, null);
    }

    /**
     * 
     * Function Description
     *
     * @param slider
     * @param switcher
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setup(Button_B30 slider, Button_B27 switcher)
    {
        ViewGroup sliderView = findContainerById(R.id.lad007c_slider);
        slider.setView(sliderView);
        ViewGroup switcherView = findContainerById(R.id.lad007c_switcher);

        if (switcher == null)
        {
            switcherView.setVisibility(View.INVISIBLE);
        }
        else
        {
            switcher.setView(switcherView);
        }
    }

    /**
     * Setup layout using default parameters for the action bar (one button:
     * set).
     * 
     * @return Lower Action Bar view
     */
    public View setupLowerActionBar()
    {
        return setupLowerActionBar(R.string.txt_set);
    }

    /**
     * Setup layout using specified parameters for the action bar 1.
     * 
     * @return Lower Action Bar view
     */
    public View setupLowerActionBar(int btn1TextId)
    {
        return addLowerActionBar(R.id.lower_ab, btn1TextId);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */