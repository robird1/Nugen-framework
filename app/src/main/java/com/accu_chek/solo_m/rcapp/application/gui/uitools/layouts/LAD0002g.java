/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0002g
 * Brief: Provide the function of the LAD0002g layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0002g.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0002g extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0002g;
    
    private static final int CONTAINER_ID = R.id.warning_relativeLayout1;

    ViewGroup mContainer = null;

    /**
     * 
     * @param activity
     */
    public LAD0002g(Activity activity)
    {
        super(activity);
        mContainer = findContainerById(CONTAINER_ID);
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
     * @param infoIconId
     * @param infoTitleId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setup(int infoIconId, int infoTitleId)
    {
        UIHelper.setImage(mContainer, R.id.warning_img1, infoIconId);
        UIHelper.setText(mContainer, R.id.warning_title_text, infoTitleId);

    }

    /**
     * 
     * Function Description
     *
     * @param warning
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setWarningCode(int warning)
    {
        UIHelper.setText(mContainer, R.id.warning_text_headline, warning);
    }

    /**
     * 
     * Function Description
     *
     * @param description
     * @param value
     * @param info
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setValueAndInfo(int description, String value, int info)
    {
        UIHelper.setText(mContainer, R.id.warning_text_1st_line_left,
                description);
        UIHelper.setText(mContainer, R.id.warning_text_1st_line_right, value);
        UIHelper.setText(mContainer, R.id.warning_text1_1, info);
    }

    /**
     * 
     * Function Description
     *
     * @param buttonTextId
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        return addLowerActionBar(R.id.lad002g_lower_ab, buttonTextId);
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
