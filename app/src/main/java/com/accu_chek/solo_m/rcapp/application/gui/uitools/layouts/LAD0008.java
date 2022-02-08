/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0008
 * Brief: Provide the function of the LAD0008 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0008.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import java.util.Arrays;
import java.util.Collection;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B18;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0008 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0008;
    
    private static final int BUTTON_CONTAINER = R.id.lad008_tableLayout;
    
    Boolean mIsFirstButton = true;
    
    // Add to support UIAutomator
    private int mButtonIndex = 0;

    /**
     * 
     * @param activity
     */
    public LAD0008(Activity activity)
    {
        super(activity);
    }

    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

    /**
     * Gets the id for the button container View
     * 
     * @return The button container view id
     */
    protected ViewGroup getButtonContainerId()
    {
        return findContainerById(BUTTON_CONTAINER);
    }

    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        return addLowerActionBar(R.id.lad008_lower_ab, buttonTextId);
    }

    /**
     * Enables or disables lower action bar
     * 
     * @param enabled True to enable, false to disable the action bar.
     */
    public void setLowerActionBarEnabled(boolean enabled)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(R.id.lad008_lower_ab);
        ab.setEnabled(enabled);
    }

    /**
     * 
     * Function Description
     *
     * @param buttons
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addButtons(Button_B18... buttons)
    {
        addButtons(Arrays.asList(buttons));
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(Collection<? extends Button_B18> buttons)
    {
        Activity activity = getActivity();
        ViewGroup buttonContainer = getButtonContainerId();
        for (Button_B18 button : buttons)
        {
            // TODO: specify bottom margin for table rows (or list items) in
            // layout XML file,
            // remove dynamic spacing.
            if (mIsFirstButton)
            {
                mIsFirstButton = false;
                // View space =
                // activity.getLayoutInflater().inflate(R.layout.button_spacing16px,
                // buttonContainer, false);
                // buttonContainer.addView(space);
            }
            else
            {
                View space = activity.getLayoutInflater().inflate(
                        R.layout.button_spacing14px, buttonContainer, false);
                buttonContainer.addView(space);
            }
            // Modified by Henry Tso to support UIAutomator
            buttonContainer.addView(button.createView(activity, null,
                    mButtonIndex++));
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
