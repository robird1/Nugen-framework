/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0002a
 * Brief: Provide the function of the LAD0002a layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0002a.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0002a extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0002a;

    /**
     * 
     * @param activity
     */
    public LAD0002a(Activity activity)
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
     * @param row_num
     * @param left
     * @param right
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void insertRow(int row_num, int left, String right)
    {
        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
        switch (row_num)
        {
        case 1:
            UIHelper.setText(container, R.id.warning_text_2nd_line_left, left);
            UIHelper.setText(container, R.id.warning_text_2nd_line_right, right);
            break;
        case 2:
            UIHelper.setText(container, R.id.warning_text_3nd_line_left, left);
            UIHelper.setText(container, R.id.warning_text_3nd_line_right, right);
            break;
        case 3:
            UIHelper.setText(container, R.id.warning_text_4nd_line_left, left);
            UIHelper.setText(container, R.id.warning_text_4nd_line_right, right);
            break;
        case 4:
            UIHelper.setText(container, R.id.warning_text_5nd_line_left, left);
            UIHelper.setText(container, R.id.warning_text_5nd_line_right, right);
            break;
        default:
            break;
        }

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
    public void setup(int infoIconId, int infoTitleId, String itemName,
            int lowerTextId)
    {

        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
        setup(container, infoIconId, infoTitleId, itemName, lowerTextId);
    }

    /**
     * Updates label and value text for first value.
     * 
     * @param labelId
     *            Id for the label string
     * @param valueText
     *            Value text
     */
    public void setValue(int labelId, String valueText)
    {
        setLabelValuePair(R.id.warning_text_1st_line_left, labelId,
                R.id.warning_text_1st_line_right, valueText);
    }

    /**
     * 
     * Function Description
     *
     * @param container
     * @param infoIconId
     * @param infoTitleId
     * @param itemName
     * @param lowerTextId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setup(ViewGroup container, int infoIconId, int infoTitleId,
            String itemName, int lowerTextId)
    {
        UIHelper.setImage(container, R.id.warning_img1, infoIconId);
        UIHelper.setText(container, R.id.warning_title_text, infoTitleId);

        // item name caption (placeholder: [insert name])
        UIHelper.setText(container, R.id.warning_text_1st_line_default,
                itemName);
        // green text below
        UIHelper.setText(container, R.id.warning_lower_lines, lowerTextId);
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
        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
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