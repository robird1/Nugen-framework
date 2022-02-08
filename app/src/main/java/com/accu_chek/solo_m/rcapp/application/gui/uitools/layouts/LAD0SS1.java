/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0SS1
 * Brief: Provide the function of the LAD0SS1 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0SS1.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import java.util.Arrays;
import java.util.Collection;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0SS1 extends LAD0007a
{

    private static final int LAYOUT_ID = R.layout.lad0ss1;
    
    private static final int LAYOUT_AB = R.id.lad0ss1_ab;
    
    private static final int BUTTON_CONTAINER = R.id.lad0ss1_tableLayout;

    /**
     * Violtae coding rule: R16
     */
    public StatusBG m_stbg;
    
    /**
     * Violtae coding rule: R16
     */
    public StatusReservoir m_strv;

    Boolean mIsFirstButton = true;
    
    // Add to support UIAutomator
    private int mButtonIndex = 0;

    /**
     * 
     * @param activity
     */
    public LAD0SS1(Activity activity)
    {
        super(activity);
        ViewGroup status_bg = (ViewGroup) findViewById(R.id.lad0ss1_status_bg);
        m_stbg = new StatusBG(status_bg);
        ViewGroup res_vg = (ViewGroup) findViewById(R.id.lad0ss1_status_res);
        m_strv = new StatusReservoir(res_vg);
    }

    /**
     * 
     * Function Description
     *
     * @param bg_id
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBGColor(int bg_id)
    {
        ViewGroup buttonContainer = (ViewGroup) findViewById(R.id.lad0ss1_rel_all);
        buttonContainer.setBackgroundColor(bg_id);

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
     * @param color
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBackgroundResource(int color)
    {
        View rel_all = (View) findViewById(R.id.lad0ss1_rel_all);
        rel_all.setBackgroundResource(color);
    }

    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        return addLowerActionBar(LAYOUT_AB, buttonTextId);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id)
    {
        return addLowerActionBar(LAYOUT_AB, buttonText1Id, buttonText2Id);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int icon,
            int buttonText2Id)
    {
        return addLowerActionBar(LAYOUT_AB, buttonText1Id, icon, buttonText2Id);
    }

    /**
     * 
     * Function Description
     *
     * @param first
     * @param second
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLowerClickability(Boolean first, Boolean second)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(LAYOUT_AB);
        View btn1 = (View) ab.findViewById(R.id.btn1);
        btn1.setClickable(first);
        View btn2 = (View) ab.findViewById(R.id.btn2);
        btn2.setClickable(second);
    }

    /**
     * 
     * Function Description
     *
     * @param first
     * @param second
     * @param third
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLowerClickability(Boolean first, Boolean second,
            Boolean third)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(LAYOUT_AB);
        View btn3 = (View) ab.findViewById(R.id.btn3);
        btn3.setClickable(second);
        setLowerClickability(first, third);
    }

    /**
     * Enables or disables lower action bar
     * 
     * @param enabled
     *            True to enable, false to disable the action bar.
     */
    public void setLowerActionBarEnabled(boolean enabled)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(LAYOUT_AB);
        ab.setEnabled(enabled);
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(ButtonBasic... buttons)
    {
        addButtons(Arrays.asList(buttons));
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(Collection<? extends ButtonBasic> buttons)
    {
        Activity activity = getActivity();
        ViewGroup buttonContainer = getButtonContainer();
        for (ButtonBasic button : buttons)
        {
            // TODO: specify bottom margin for table rows (or list items) in
            // layout XML file,
            // remove dynamic spacing.
            if (mIsFirstButton)
                mIsFirstButton = false;
            else
            {
                View space = activity.getLayoutInflater().inflate(
                        R.layout.button_spacing6px, buttonContainer, false);
                buttonContainer.addView(space);
            }
            // Modified by Henry Tso to support UIAutomator
            buttonContainer.addView(button.createView(activity, null,
                    mButtonIndex++));
        }
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
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
