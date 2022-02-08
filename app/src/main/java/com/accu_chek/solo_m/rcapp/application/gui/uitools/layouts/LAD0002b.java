/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0002b
 * Brief: Provide the function of the LAD0002b layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0002b.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0002b extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0002b;
    
    private static final int CONTAINER_ID = R.id.warning_relativeLayout1;

    /**
     * 
     * @param activity
     */
    public LAD0002b(Activity activity)
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
     * Updates label and value text for first value.
     * 
     * @param labelId
     *            Id for the label string
     * @param valueText
     *            Value text
     */
    public void setValueAndText(int labelId, String valueText, int WarningTxt)
    {
        ViewGroup container = findContainerById(CONTAINER_ID);
        UIHelper.setText(container, R.id.warning_text_1st_line_left, labelId);
        UIHelper.setText(container, R.id.warning_text_1st_line_right, valueText);
        UIHelper.setText(container, R.id.warning_text, WarningTxt);
    }

    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        return addLowerActionBar(R.id.lad002b_lower_ab, buttonTextId);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id)
    {
        return addLowerActionBar(R.id.lad002b_lower_ab, buttonText1Id,
                buttonText2Id);
    }

    /**
     * 
     * Function Description
     *
     * @param infoIconId
     * @param infoTitleId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setup(int infoIconId, int infoTitleId)
    {
        ViewGroup container = findContainerById(CONTAINER_ID);
        UIHelper.setImage(container, R.id.warning_img1, infoIconId);
        UIHelper.setText(container, R.id.warning_title_text, infoTitleId);

    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */