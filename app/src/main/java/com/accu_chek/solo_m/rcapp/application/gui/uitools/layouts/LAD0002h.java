/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0002h
 * Brief: Provide the function of the LAD0002h layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0002h.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0002h extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0002h;

    /**
     * 
     * @param activity
     */
    public LAD0002h(Activity activity)
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
     * Setup layout using default parameters for the Info-Bar layout id, button
     * 1 text and button 2 text.
     * 
     * @param infoIconId
     *            Id for icon used in the dialog body
     * @param infoTitleId
     *            Id for prompt title string
     * @param infoDescriptionId
     *            Id for prompt description id
     */
    public void setup(int infoIconId, int infoDescriptionId)
    {
        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
        UIHelper.setImage(container, R.id.warning_img1, infoIconId);
        UIHelper.setText(container, R.id.warning_title_text, infoDescriptionId);

    }

    /**
     * 
     * Function Description
     *
     * @param warn
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setWarning(int warn)
    {
        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
        UIHelper.setText(container, R.id.warning_text_1st_line_left, warn);
    }

    /**
     * 
     * Function Description
     *
     * @param warn
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setWarning(String warn)
    {
        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
        UIHelper.setText(container, R.id.warning_text_1st_line_left, warn);
    }

    /**
     * 
     * Function Description
     *
     * @param row_number
     * @param txt_left
     * @param txt_right
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setRow(int row_number, int txt_left, String txt_right)
    {
        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
        switch (row_number)
        {
        case 1:
        {
            UIHelper.setText(container, R.id.warning_text_row1_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row1_right, txt_right);
            break;
        }
        case 2:
        {
            UIHelper.setText(container, R.id.warning_text_row2_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row2_right, txt_right);
            break;
        }
        case 3:
        {
            UIHelper.setText(container, R.id.warning_text_row3_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row3_right, txt_right);
            break;
        }
        case 4:
        {
            UIHelper.setText(container, R.id.warning_text_row4_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row4_right, txt_right);
            break;
        }
        case 5:
        {
            UIHelper.setText(container, R.id.warning_text_row5_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row5_right, txt_right);
            break;
        }
        case 6:
        {
            UIHelper.setText(container, R.id.warning_text_row6_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row6_right, txt_right);
            break;
        }
        case 7:
        {
            UIHelper.setText(container, R.id.warning_text_row7_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row7_right, txt_right);
            break;
        }
        case 8:
        {
            UIHelper.setText(container, R.id.warning_text_row8_left, txt_left);
            UIHelper.setText(container, R.id.warning_text_row8_right, txt_right);
            break;
        }
        default:
            break;

        }

    }

    // modify by Steve {
    /**
     * Setup layout using default parameters for the action bar (two buttons:
     * yes/no).
     * 
     * @return
     */
    public View setupLowerActionBar(IFooterButton button)
    {
        return setupLowerActionBar(R.string.txt_no, R.string.txt_yes, button);
    }

    /**
     * Setup layout using default parameters for the action bar (two buttons:
     * yes/no).
     * 
     * @return
     */
    public View setupLowerActionBar(int btn1TextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad002h_lower_ab, btn1TextId, button);
    }

    /**
     * 
     * Function Description
     *
     * @param btn1TextId
     * @param btn2TextId
     * @param button
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View setupLowerActionBar(int btn1TextId, int btn2TextId,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad002h_lower_ab, btn1TextId, btn2TextId,
                button);
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