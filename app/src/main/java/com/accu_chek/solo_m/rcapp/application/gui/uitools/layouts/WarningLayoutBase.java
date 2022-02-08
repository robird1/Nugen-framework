/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: WarningLayoutBase
 * Brief: Provide the function of the WarningLayoutBase layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: WarningLayoutBase.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.LowerActionBarClickListener;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class WarningLayoutBase extends LayoutBase
{

    private static final int ONE_BUTTON_INDEX = 0;
    
    private static final int LEFT_BUTTON_INDEX = 1;
    
    private static final int RIGHT_BUTTON_INDEX = 2;

    /**
     * 
     * @param activity
     */
    public WarningLayoutBase(Activity activity)
    {
        super(activity);
    }

    /**
     * Adds Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    @Override
    protected View addLowerActionBar(int containerId, int buttonText1Id,
            int buttonText2Id)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.warning_bar_2);
        UIHelper.setText(actionBar, R.id.warning_btn_1, buttonText1Id);
        UIHelper.setText(actionBar, R.id.warning_btn_2, buttonText2Id);
        return actionBar;
    }

    /**
     * Adds Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    @Override
    protected View addLowerActionBar(int containerId, int buttonTextId)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.warning_bar_1);
        UIHelper.setText(actionBar, R.id.warning_btn_1, buttonTextId);
        return actionBar;
    }
    // modify by Steve {

    /**
     * Adds Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    @Override
    protected View addLowerActionBar(int containerId, int buttonText1Id,
            int buttonText2Id, final IFooterButton button)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.warning_bar_2);
        View leftButtonView = actionBar.findViewById(R.id.warning_btn_1);
        View rightButtonView = actionBar.findViewById(R.id.warning_btn_2);

        UIHelper.setText(actionBar, R.id.warning_btn_1, buttonText1Id);
        UIHelper.setText(actionBar, R.id.warning_btn_2, buttonText2Id);

        leftButtonView.setOnClickListener(new LowerActionBarClickListener(
                LEFT_BUTTON_INDEX, button));

        rightButtonView.setOnClickListener(new LowerActionBarClickListener(
                RIGHT_BUTTON_INDEX, button));

        return actionBar;
    }

    /**
     * Adds Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    @Override
    protected View addLowerActionBar(int containerId, int buttonTextId,
            final IFooterButton button)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.warning_bar_1);
        View footerButtonView = actionBar.findViewById(R.id.warning_btn_1);

        UIHelper.setText(actionBar, R.id.warning_btn_1, buttonTextId);

        footerButtonView.setOnClickListener(new LowerActionBarClickListener(
                ONE_BUTTON_INDEX, button));

        return actionBar;
    }
    // } modify by Steve

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// add Click_Through function.
