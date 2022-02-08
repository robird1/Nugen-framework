/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0001c
 * Brief: Provide the function of the LAD0001c layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0001c.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0001c extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0001c;

    /**
     * 
     * @param activity
     */
    public LAD0001c(Activity activity)
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

    // modify by Steve {
    /**
     * Setup layout using default parameters for the Info-Bar layout id, button
     * 1 text and button 2 text.
     * 
     * @param infoIconId Id for icon used in the dialog body
     * @param infoTitleId Id for prompt title string
     * @param infoDescriptionId Id for prompt description id
     */
    public void setup(int infoIconId, int infoTitleId, int infoDescriptionId,
            int infoTxtleft, String infoTxtRight)
    {
        ViewGroup rl = findContainerById(R.id.warning_relativeLayout1);

        TextView textView3 = (TextView) rl
                .findViewById(R.id.warning_title_text);
        textView3.setText(infoTitleId);

        TextView textView4 = (TextView) rl.findViewById(R.id.warning_text);
        textView4.setText(infoDescriptionId);

        TextView textView6 = (TextView) rl
                .findViewById(R.id.warning_text_1st_line_left);
        textView6.setText(infoTxtleft);

        TextView textView5 = (TextView) rl
                .findViewById(R.id.warning_text_1st_line_right);
        textView5.setText(infoTxtRight);

        ImageView img1 = (ImageView) rl.findViewById(R.id.warning_img1);
        img1.setImageResource(infoIconId);
    }

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
        return addLowerActionBar(R.id.lad001c_lower_ab, btn1TextId, button);
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
        return addLowerActionBar(R.id.lad001c_lower_ab, btn1TextId, btn2TextId,
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
