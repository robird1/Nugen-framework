/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0007b
 * Brief: Provide the function of the LAD0007b layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0007b.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0007b extends LAD0007a
{

    private static final int LAYOUT_ID = R.layout.lad0007b;
    
    private static final int BUTTON_CONTAINER = R.id.lad007b_tableLayout;

    /**
     * 
     * @param activity
     */
    public LAD0007b(Activity activity)
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
     * Gets the id for the button container View
     * 
     * @return The button container view id
     */
    @Override
    protected int getButtonContainerId()
    {
        return BUTTON_CONTAINER;
    }

    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        return addLowerActionBar(R.id.lad007b_lower_ab, buttonTextId);
    }

    // add by steve {
    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad007b_lower_ab, buttonTextId, button);
    }
    // } add by Steve

    // modify by steve {
    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad007b_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }
    // } modify by steve

    /**
     * Enables or disables lower action bar
     * 
     * @param enabled True to enable, false to disable the action bar.
     */
    public void setLowerActionBarEnabled(boolean enabled)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(R.id.lad007b_lower_ab);
        ab.setEnabled(enabled);
        // Modify by Henry Tso
        // When the footer button is disabled, the scrollview should shows
        // with full screen. Therefore, set the footer button to GONE.
        if (false == enabled)
        {
            ab.setVisibility(ViewGroup.GONE);
        }
        else
        {
            // Do nothing when the flag is true
        }
    }

    /**
     * Adds button divider.
     */
    public void addButtonDividerlad10()
    {
        ViewGroup buttonContainer = getButtonContainer();
        View tr = getLayoutInflater().inflate(R.layout.button_spacing_lad10,
                buttonContainer, false);
        buttonContainer.addView(tr);
        // first Button after divider.
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
