/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0001b
 * Brief: Provide the function of the LAD0001b layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0001b.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0001b extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0001b;
    
    ViewGroup mViewGroup = null;

    /**
     * 
     * @param activity
     */
    public LAD0001b(Activity activity)
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
    public void setup(int infoIconId, int infoTitleId, int infoDescriptionId)
    {
        mViewGroup = findContainerById(R.id.warning_relativeLayout1);

        TextView textView3 = (TextView) mViewGroup
                .findViewById(R.id.warning_title_text);
        textView3.setText(infoTitleId);

        TextView textView4 = (TextView) mViewGroup.findViewById(R.id.warning_text);
        textView4.setText(infoDescriptionId);

        ImageView img1 = (ImageView) mViewGroup.findViewById(R.id.warning_img1);
        img1.setImageResource(infoIconId);
    }

    /**
     * 
     * Function Description
     *
     * @param infoIconId
     * @param infoTitleId
     * @param infoDescriptionId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setup(int infoIconId, int infoTitleId, String infoDescriptionId)
    {
        mViewGroup = findContainerById(R.id.warning_relativeLayout1);

        TextView textView3 = (TextView) mViewGroup
                .findViewById(R.id.warning_title_text);
        textView3.setText(infoTitleId);

        TextView textView4 = (TextView) mViewGroup.findViewById(R.id.warning_text);
        textView4.setText(infoDescriptionId);

        ImageView img1 = (ImageView) mViewGroup.findViewById(R.id.warning_img1);
        img1.setImageResource(infoIconId);
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
    public void setWarningLine(int row_number, int txt_left, String txt_right)
    {
        switch (row_number)
        {
        case 1:
        {
            TextView textView = (TextView) mViewGroup.findViewById(R.id.txt_r1_col1);
            textView.setText(txt_left);
            textView = (TextView) mViewGroup.findViewById(R.id.txt_r1_col2);
            textView.setText(txt_right);
            break;
        }
        case 2:
        {
            TextView textView = (TextView) mViewGroup.findViewById(R.id.txt_r2_col1);
            textView.setText(txt_left);
            textView = (TextView) mViewGroup.findViewById(R.id.txt_r2_col2);
            textView.setText(txt_right);
            break;
        }
        case 3:
        {
            TextView textView = (TextView) mViewGroup.findViewById(R.id.txt_r3_col1);
            textView.setText(txt_left);
            textView = (TextView) mViewGroup.findViewById(R.id.txt_r3_col2);
            textView.setText(txt_right);
            break;
        }
        case 4:
        {
            TextView textView = (TextView) mViewGroup.findViewById(R.id.txt_r4_col1);
            textView.setText(txt_left);
            textView = (TextView) mViewGroup.findViewById(R.id.txt_r4_col2);
            textView.setText(txt_right);
            break;
        }
        case 5:
        {
            TextView textView = (TextView) mViewGroup.findViewById(R.id.txt_r5_col1);
            textView.setText(txt_left);
            textView = (TextView) mViewGroup.findViewById(R.id.txt_r5_col2);
            textView.setText(txt_right);
            break;
        }
        case 6:
        {
            TextView textView = (TextView) mViewGroup.findViewById(R.id.txt_r6_col1);
            textView.setText(txt_left);
            textView = (TextView) mViewGroup.findViewById(R.id.txt_r6_col2);
            textView.setText(txt_right);
            break;
        }
        case 7:
        {
            TextView textView = (TextView) mViewGroup.findViewById(R.id.txt_r7_col1);
            textView.setText(txt_left);
            textView = (TextView) mViewGroup.findViewById(R.id.txt_r7_col2);
            textView.setText(txt_right);
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
        return addLowerActionBar(R.id.lad001b_lower_ab, btn1TextId, button);
    }

    public View setupLowerActionBar(int btn1TextId, int btn2TextId,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad001b_lower_ab, btn1TextId, btn2TextId,
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
