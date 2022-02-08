/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0001f
 * Brief: Provide the function of the LAD0001f layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0001f.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0001f extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0001f;
    
    ViewGroup mViewGroup = null;

    /**
     * 
     * @param activity
     */
    public LAD0001f(Activity activity)
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
    public void setup(int infoIconId, int infoTitleId)
    {
        mViewGroup = findContainerById(R.id.warning_relativeLayout1);

        TextView textView3 = (TextView) mViewGroup
                .findViewById(R.id.warning_title_text);
        textView3.setText(infoTitleId);

        ImageView img1 = (ImageView) mViewGroup.findViewById(R.id.warning_img1);
        img1.setImageResource(infoIconId);
    }

    /**
     * 
     * Function Description
     *
     * @param row
     * @param img
     * @param txt
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLowerText(int row, int img, int txt)
    {
        switch (row)
        {
        case 1:
        {
            UIHelper.setText(mViewGroup, R.id.warning_text_low_right_1, txt);
            UIHelper.setImage(mViewGroup, R.id.warning_img_low1, img);
            break;
        }
        case 2:
        {
            UIHelper.setText(mViewGroup, R.id.warning_text_low_right_2, txt);
            UIHelper.setImage(mViewGroup, R.id.warning_img_low2, img);
            break;
        }
        case 3:
        {
            UIHelper.setText(mViewGroup, R.id.warning_text_low_right_3, txt);
            UIHelper.setImage(mViewGroup, R.id.warning_img_low3, img);
            break;
        }
        case 4:
        {
            UIHelper.setText(mViewGroup, R.id.warning_text_low_right_4, txt);
            UIHelper.setImage(mViewGroup, R.id.warning_img_low4, img);
            break;
        }
        default:
            break;
        }
    }

    /**
     * Setup layout using default parameters for the action bar (two buttons:
     * yes/no).
     * 
     * @return
     */
    public View setupLowerActionBar()
    {
        return setupLowerActionBar(R.string.txt_no, R.string.txt_yes);
    }

    /**
     * Setup layout using default parameters for the action bar (two buttons:
     * yes/no).
     * 
     * @return
     */
    public View setupLowerActionBar(int btn1TextId)
    {
        return addLowerActionBar(R.id.lad001f_lower_ab, btn1TextId);
    }

    /**
     * 
     * Function Description
     *
     * @param btn1TextId
     * @param btn2TextId
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View setupLowerActionBar(int btn1TextId, int btn2TextId)
    {
        return addLowerActionBar(R.id.lad001f_lower_ab, btn1TextId, btn2TextId);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */