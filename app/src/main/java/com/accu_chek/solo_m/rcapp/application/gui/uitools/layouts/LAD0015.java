/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0015
 * Brief: Provide the function of the LAD0015 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0015.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0015 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0015;
    
    private static final int BUTTON_CONTAINER = R.id.lad0015_tableLayout;
    
    private ViewGroup mParent = null;

    /**
     * 
     * @param activity
     */
    public LAD0015(Activity activity)
    {
        super(activity);
        mParent = (ViewGroup) findViewById(R.id.lad0015_relativeLayout_all);

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
     * @return The button container View
     */
    protected int getButtonContainer()
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
        return addLowerActionBar(R.id.lad0015_lower_ab, buttonTextId);
    }

    // modify by Steve {
    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0015_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }
    // } modify by Steve

    /**
     * Set graphic image id
     */
    public LAD0015 setup(int text)
    {
        UIHelper.setText(mParent, R.id.id_lad0015_text_headline, text);
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param number
     * @param text
     * @return
     * @return LAD0015 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0015 setInstruction(String number, int text)
    {
        ViewGroup buttonContainer = findContainerById(getButtonContainer());
        LayoutInflater inflater = getLayoutInflater();
        View tr = inflater.inflate(R.layout.button_lad15, null, false);
        buttonContainer.addView(tr);
        View tr_space = inflater.inflate(R.layout.button_spacing5px, null,
                false); // Secondline has 2px spacing, so 5+2+2=9
        buttonContainer.addView(tr_space);
        UIHelper.setText(tr, R.id.id_lad15_number, number);
        // Modified by Henry Tso. To support UIAutomator
        UIHelper.setText(tr, R.id.id_lad15_text, text, "item_text");
        return this;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
