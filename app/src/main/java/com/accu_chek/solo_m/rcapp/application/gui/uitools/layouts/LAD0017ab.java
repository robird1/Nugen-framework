/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0017ab
 * Brief: Provide the function of the LAD0017ab layout
 *
 * Create Date: 01/22/2015
 * $Revision: 25077 $
 * $Author: AdamChen $
 * $Id: LAD0017ab.java 25077 2015-11-30 05:22:16Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0017ab extends LAD0007a
{

    private static final int LAYOUT_ID = R.layout.lad0017ab;

    private static final int BUTTON_CONTAINER = R.id.lad0017ab_tableLayout;
    
    private static final int HEADER_VIEW_CONTAINER = R.id.id_lad17ab_top;
    
    // Add to support UIAutomator
    private int mButtonIndex = 0;

    /**
     * 
     * @param activity
     */
    public LAD0017ab(Activity activity)
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
     * @param button
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addTopButton(ButtonBasic button)
    {
        Activity activity = getActivity();
        ViewGroup buttonContainer = findContainerById(R.id.id_lad17ab_top);
        // modified by Henry
        buttonContainer.addView(button.createView(activity, null, 0));
    }

    // modify by Steve {
    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0017ab_lower_ab, buttonTextId, button);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0017ab_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }
    // } modify by Steve

    /**
     * Enables or disables lower action bar
     * 
     * @param enabled True to enable, false to disable the action bar.
     */
    public void disableLowerActionBarEnabled()
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity
                .findViewById(R.id.lad0017ab_lower_ab);
        ab.setVisibility(View.GONE);
    }

    // add by Steve {
    /**
     * 
     * Function Description
     *
     * @param button
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addHeaderView(ButtonBasic button)
    {
        Activity activity = getActivity();
        ViewGroup headerViewContainer = getHeaderViewContainer();
        headerViewContainer.addView(button.createView(activity, null, 0));
    }

    /**
     * 
     * Function Description
     *
     * @param button
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addDetailBgResultHeaderView(ButtonBasic button)
    {
        Activity activity = getActivity();
        ViewGroup headerViewContainer = getHeaderViewContainer();
        headerViewContainer.addView(button
                .createDatailBgView(activity, null, 0));
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void disableHeaderViewEnabled()
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(R.id.id_lad17ab_top);
        View bGValueView = ab.findViewById(R.id.id_b19_text_result);
        bGValueView.setEnabled(false);
    }
    // } add by Steve
    
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
     * 
     * Function Description
     *
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    protected ViewGroup getHeaderViewContainer()
    {
        ViewGroup headerViewContainer = null;
        
        headerViewContainer = findContainerById(getHeaderViewContainerId());
        
        return headerViewContainer;
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public static int getHeaderViewContainerId()
    {
        return HEADER_VIEW_CONTAINER;
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
