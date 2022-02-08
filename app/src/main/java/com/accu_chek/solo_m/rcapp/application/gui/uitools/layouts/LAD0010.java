/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0010
 * Brief: Provide the function of the LAD0010 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0010.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.CollectionPageAdapter;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TabClickListener;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0010 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0010;
    
    private ViewGroup mEntryContainer = null;

    /**
     * 
     * @param activity
     */
    public LAD0010(Activity activity)
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
        return addLowerActionBar(R.id.lad0010_lower_ab, buttonTextId);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id)
    {
        return addLowerActionBar(R.id.lad0010_lower_ab, buttonText1Id,
                buttonText2Id);
    }

    /**
     * 
     * Function Description
     *
     * @param tabImg1Id
     * @param tabImag2Id
     * @return
     * @return LAD0010 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010 createHeader(int tabImg1Id, int tabImag2Id)
    {
        ViewGroup topContainer = findContainerById(R.id.lad0010_tab2);
        ViewGroup tr = (ViewGroup) getLayoutInflater().inflate(R.layout.tab2,
                topContainer, false);
        topContainer.addView(tr);
        UIHelper.setImage(tr, R.id.id_tab2_img1, tabImg1Id);
        UIHelper.setImage(tr, R.id.id_tab2_img2, tabImag2Id);

        // green bar background
        tr.findViewById(R.id.id_tab2_green_left).setBackgroundResource(
                R.color.insulin_button);

        return this;
    }

    // add by Steve {
    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0010_lower_ab, buttonTextId, button);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0010_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }

    /**
     * 
     * Function Description
     *
     * @param tabImg1Id
     * @param tabImag2Id
     * @param adapter
     * @return
     * @return LAD0010 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010 createHeader(int tabImg1Id, int tabImag2Id,
            CollectionPageAdapter adapter)
    {
        ViewGroup topContainer = findContainerById(R.id.lad0010_tab2);
        ViewGroup tr = (ViewGroup) getLayoutInflater().inflate(R.layout.tab2,
                topContainer, false);
        topContainer.addView(tr);
        UIHelper.setImage(tr, R.id.id_tab2_img1, tabImg1Id, "tab1");
        UIHelper.setImage(tr, R.id.id_tab2_img2, tabImag2Id, "tab2");

        // green bar background
        tr.findViewById(R.id.id_tab2_green_left).setBackgroundResource(
                R.color.insulin_button);

        findViewById(R.id.id_tab2_relative_left).setOnClickListener(
                new TabClickListener(adapter));
        findViewById(R.id.id_tab2_relative_right).setOnClickListener(
                new TabClickListener(adapter));

        return this;
    }
    // } add by Steve

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
