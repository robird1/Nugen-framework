/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0010b
 * Brief: Provide the function of the LAD0010b layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0010b.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0010b extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0010b;

    /**
     * 
     * @param activity
     */
    public LAD0010b(Activity activity)
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
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        return addLowerActionBar(R.id.lad0010b_lower_ab, buttonTextId);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id)
    {
        return addLowerActionBar(R.id.lad0010b_lower_ab, buttonText1Id,
                buttonText2Id);
    }

    /**
     * Set graphic image id
     */
    public LAD0010b setGraphic(int graphicId)
    {
        ViewGroup container = findContainerById(R.id.lad0010b_scroll);
        container.setBackgroundResource(graphicId);

        // UIHelper.setImage(container, R.id.lad0010b_scroll, graphicId);
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param tabImg1Id
     * @param tabImag2Id
     * @return
     * @return LAD0010b [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010b createHeader(int tabImg1Id, int tabImag2Id)
    {
        ViewGroup topContainer = findContainerById(R.id.lad0010b_tab2);
        ViewGroup tr = (ViewGroup) getLayoutInflater().inflate(R.layout.tab2,
                topContainer, false);
        topContainer.addView(tr);
        // Modified by Henry Tso. To support UIAutomator
        UIHelper.setImage(tr, R.id.id_tab2_img1, tabImg1Id, "tab1");
        UIHelper.setImage(tr, R.id.id_tab2_img2, tabImag2Id, "tab2");

        // green bar background
        tr.findViewById(R.id.id_tab2_green_right).setBackgroundResource(
                R.color.insulin_button);

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
