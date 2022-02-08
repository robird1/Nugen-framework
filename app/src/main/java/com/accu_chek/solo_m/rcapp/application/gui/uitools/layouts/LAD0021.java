/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0021
 * Brief: Provide the function of the LAD0021 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24929 $
 * $Author: AdamChen $
 * $Id: LAD0021.java 24929 2015-11-26 09:01:08Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

//import com.accu_chek.solo_m.rcapp.application.gui.interfaces.AbstractMyDataTabButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.CollectionPageAdapter;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TabClickListener;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

/**
 * Represents a layout definition.
 */
public class LAD0021 extends LayoutBase
{

    private static final byte SAFETY_BOOLEAN_TRUE = SafetyBoolean.TRUE.getByte();

    private static final int LAYOUT_ID = R.layout.lad0021;
    
    private static final int CONTENT = R.id.lad0021_content;
    
    private static final int DATE = R.id.id_datescroll;

    // === Add by Henry tso ===
    // View group of 2 button tab bar.
    private View m2TabButtonView = null;
    
    // Status bar of left tab button
    private RelativeLayout mLeftStatusBar = null;
    
    // Status bar of right tab button
    private RelativeLayout mRightStatusBar = null;

    /**
     * Constructor. The {@code Activity.setContentView} method is called with
     * the layout id as parameter
     * 
     * @param activity
     *            : The underlying activity
     * 
     */
    public LAD0021(Activity activity)
    {
        super(activity);
    }

    /**
     * Gets the id for the XML layout definition.
     * 
     * @return The layout id
     */
    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void setupBase(DATESCROLL data)
    {
        ViewGroup scrollContainer = findContainerById(getDateScroll());
        UIHelper.setText(scrollContainer, R.id.id_ds_title1, data.title1, "text1");
        UIHelper.setText(scrollContainer, R.id.id_ds_title2, data.title2, "text2");
        UIHelper.setImage(scrollContainer, R.id.id_ds_icon_1, data.icon1, "icon1");
        UIHelper.setImage(scrollContainer, R.id.id_ds_icon_2, data.icon2, "icon2");
        UIHelper.setImage(scrollContainer, R.id.id_ds_icon_3, data.icon3, "icon3");
        UIHelper.setImage(scrollContainer, R.id.id_ds_icon_4, data.icon4, "icon4");

    }

    /**
     * 
     * Function Description
     *
     * @param data
     * @param container
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public static void setupBase(DATESCROLL data, ViewGroup container)
    {
        UIHelper.setText(container, R.id.id_ds_title1, data.title1, "text1");
        UIHelper.setText(container, R.id.id_ds_title2, data.title2, "text2");
        UIHelper.setImage(container, R.id.id_ds_icon_1, data.icon1, "icon1");
        UIHelper.setImage(container, R.id.id_ds_icon_2, data.icon2, "icon2");
        UIHelper.setImage(container, R.id.id_ds_icon_3, data.icon3, "icon3");
        UIHelper.setImage(container, R.id.id_ds_icon_4, data.icon4, "icon4");
    }

    /**
     * 
     * Function Description
     *
     * @param layoutid
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setupContent(int layoutid)
    {
        Activity activity = getActivity();
        ViewGroup content = findContainerById(getContentContainer());
        LayoutInflater inflater = activity.getLayoutInflater();
        View tr = inflater.inflate(layoutid, null, false);
        content.addView(tr);
    }

    /**
     * 
     * Function Description
     *
     * @param tabImg1Id
     * @param adapter
     * @return
     * @return LAD0021 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0021 createHeader(int tabImg1Id, CollectionPageAdapter adapter)
    {
        ViewGroup topContainer = findContainerById(R.id.lad0021_tab2);
        ViewGroup tr = (ViewGroup) getLayoutInflater().inflate(R.layout.tab1,
                topContainer, false);
        topContainer.addView(tr);
        // Modified by Henry Tso. To support UIAutomator
        UIHelper.setImage(tr, R.id.id_tab1_img1, tabImg1Id, "tab1");

        // green bar background
        // tr.findViewById(R.id.id_tab2_green_left).setBackgroundResource(R.color.insulin_button);

        findViewById(R.id.id_tab1_relative_left).setOnClickListener(
                new TabClickListener(adapter));
        // findViewById(R.id.id_tab2_relative_left).setOnClickListener(new
        // TabClickListener(adapter));

        return this;
    }

    // add by Steve {
    /**
     * 
     * Function Description
     *
     * @param tabImg1Id
     * @param tabImag2Id
     * @param adapter
     * @return
     * @return LAD0021 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0021 createHeader(int tabImg1Id, int tabImag2Id,
            CollectionPageAdapter adapter)
    {
        ViewGroup topContainer = findContainerById(R.id.lad0021_tab2);
        ViewGroup tr = (ViewGroup) getLayoutInflater().inflate(R.layout.tab2,
                topContainer, false);
        topContainer.addView(tr);
        // Modified by Henry Tso. To support UIAutomator
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

    // Added by Henry Tso
    /**
     * Call this API to switch the button active highlight (Green bar)
     * 
     * @return void [out] None
     */
    public void setButtonActive(SafetyBoolean bLeftactive)
    {
        byte safetyBooleanByte = bLeftactive.getByte();
        
        if (SAFETY_BOOLEAN_TRUE == safetyBooleanByte)
        {
            mLeftStatusBar.setVisibility(View.VISIBLE);
            mRightStatusBar.setVisibility(View.INVISIBLE);
        }
        else
        {
            mRightStatusBar.setVisibility(View.VISIBLE);
            mLeftStatusBar.setVisibility(View.INVISIBLE);
        }
    }
    
    /**
     * Gets the id for the button container View
     * 
     * @return The button container View
     */
    protected int getContentContainer()
    {
        return CONTENT;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected int getDateScroll()
    {
        return DATE;
    }
    
    /**
     * Violate coding rule: R16
     */
    public static class DATESCROLL
    {
        
        public String title1 = null;
        
        public String title2 = null;
        
        public int icon1 = 0;
        
        public int icon2 = 0;
        
        public int icon3 = 0;
        
        public int icon4 = 0;
    
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */

