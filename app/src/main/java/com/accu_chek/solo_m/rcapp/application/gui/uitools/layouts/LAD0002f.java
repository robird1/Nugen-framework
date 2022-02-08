/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0002f
 * Brief: Provide the function of the LAD0002f layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0002f.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0002f extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0002f;
    
    private static final int CONTAINER_ID = R.id.warning_relativeLayout1;

    /**
     * 
     * @param activity
     */
    public LAD0002f(Activity activity)
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
     * Setup layout using specified parameters
     * 
     * @param infoIconId
     *            Id for icon used in the dialog body
     * @param infoTitleId
     *            Id for prompt title string
     * @param itemName
     *            Item name
     * @param lowerTextId
     *            Id for the text below the dialog body (green text).
     */
    public void setup(int infoIconId, int infoTitleId, String itemName)
    {

        ViewGroup container = findContainerById(CONTAINER_ID);
        setup(container, infoIconId, infoTitleId, itemName);
    }

    /**
     * Updates label and value text for first value.
     * 
     * @param labelId
     *            Id for the label string
     * @param valueText
     *            Value text
     */
    public void setValue1(int labelId, String valueText)
    {
        setLabelValuePair(R.id.warning_text_1st_line_left, labelId,
                R.id.warning_text_1st_line_right, valueText);
    }

    /**
     * Updates label and value text for second value.
     * 
     * @param labelId
     *            Id for the label string
     * @param valueText
     *            Value text
     */
    public void setValue2(int labelId, String valueText)
    {
        setLabelValuePair(R.id.warning_text1_1, labelId, R.id.warning_text1_2,
                valueText);

    }

    /**
     * Updates label and value text for third value.
     * 
     * @param labelId
     *            Id for the label string
     * @param valueText
     *            Value text
     */
    public void setValue3(int labelId, String valueText)
    {
        setLabelValuePair(R.id.warning_text2_1, labelId, R.id.warning_text2_2,
                valueText);

    }

    // modify by Steve {
    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad002f_lower_ab, buttonTextId, button);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad002f_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }
    // } modify by Steve

    /**
     * 
     * Function Description
     *
     * @param container
     * @param infoIconId
     * @param infoTitleId
     * @param itemName
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setup(ViewGroup container, int infoIconId, int infoTitleId,
            String itemName)
    {
        UIHelper.setImage(container, R.id.warning_img1, infoIconId);
        UIHelper.setText(container, R.id.warning_title_text, infoTitleId);

        // item name caption (placeholder: [insert name])
        UIHelper.setText(container, R.id.warning_text_headline, itemName);
    }

    /**
     * 
     * Function Description
     *
     * @param labelViewId
     * @param labelId
     * @param valueViewId
     * @param valueText
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setLabelValuePair(int labelViewId, int labelId,
            int valueViewId, String valueText)
    {
        ViewGroup container = findContainerById(CONTAINER_ID);
        UIHelper.setText(container, labelViewId, labelId);
        UIHelper.setText(container, valueViewId, valueText);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */