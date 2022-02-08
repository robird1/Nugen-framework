/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0013
 * Brief: Provide the function of the LAD0013 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0013.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0013 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0013;
    
    private ViewGroup mParent = null;

    /**
     * 
     * @param activity
     */
    public LAD0013(Activity activity)
    {
        super(activity);
        mParent = (ViewGroup) findViewById(R.id.lad0013_relativeLayout_all);
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
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0013_lower_ab, buttonTextId, button);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0013_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }

    /**
     * 
     * Function Description
     *
     * @param buttonText1Id
     * @param bg_color
     * @param button
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View setupLowerActionBarColor(int buttonText1Id, int bg_color,
            IFooterButton button)
    {
        return addLowerActionBarColored(R.id.lad0013_lower_ab, buttonText1Id,
                bg_color, button);
    }
    // } modify by Steve

    /**
     * Set graphic image id
     */
    public LAD0013 setup(int graphicId, int text)
    {
        UIHelper.setText(mParent, R.id.id_lad0013_text, text);
        UIHelper.setImage(mParent, R.id.id_lad0013_image, graphicId);
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
