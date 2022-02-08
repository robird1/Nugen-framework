/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LayoutBase
 * Brief: Provide the function of the LayoutBase layout
 *
 * Create Date: 01/22/2015
 * $Revision: 25077 $
 * $Author: AdamChen $
 * $Id: LayoutBase.java 25077 2015-11-30 05:22:16Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.LowerActionBarClickListener;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

/**
 * Base class for layout objects. Derived types will wrap layout specific functionality.
 */
public abstract class LayoutBase
{

    private static final int ONE_BUTTON_INDEX = 0;
    
    private static final int LEFT_BUTTON_INDEX = 1;
    
    private static final int RIGHT_BUTTON_INDEX = 2;
    
    private ViewGroup mLowerActionBar = null;
    
    private Activity mContext = null;

    /**
     * @return The R.layout-Id for the layout this object is representing.
     */
    public abstract int getLayoutId();

    /**
     * 
     * @param activity
     */
    public LayoutBase(Activity activity)
    {
        mContext = activity;
        setContentView();
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getFooterButtonView()
    {
        return getLowerActionBar().findViewById(R.id.btn_1);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getFooterLeftButtonView()
    {
        return getLowerActionBar().findViewById(R.id.btn_1);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getFooterRightButtonView()
    {
        return getLowerActionBar().findViewById(R.id.btn_2);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getLowerActionBar()
    {
        return mLowerActionBar;
    }
    // } add by Steve
    
    /**
     * Gets the underlying activity
     * 
     * @return The underlying activity
     */
    protected Activity getActivity()
    {
        return mContext;
    }
    
    /**
     * Gives the layout id from this layout to the underlying activity through
     * Activity.setContentView
     */
    protected void setContentView()
    {
        getActivity().setContentView(getLayoutId());
    }

    /**
     * 
     * Function Description
     *
     * @param containerId
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    protected ViewGroup findContainerById(int containerId)
    {
        return (ViewGroup) findViewById(containerId);
    }

    /**
     * 
     * Function Description
     *
     * @param viewId
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    protected View findViewById(int viewId)
    {
        return getActivity().findViewById(viewId);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return LayoutInflater [out] Delete pre line return if exist. Parameter Description
     */
    protected LayoutInflater getLayoutInflater()
    {
        return getActivity().getLayoutInflater();
    }

    /**
     * Adds Lower Action Bar 3
     * 
     * @return Action Bar View
     */
    protected View addLowerActionBar(int containerId, int buttonText1Id,
            int icon, int buttonText2Id)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.lower_action_bar_3);
        UIHelper.setText(actionBar, R.id.btn1, buttonText1Id);
        UIHelper.setText(actionBar, R.id.btn2, buttonText2Id);
        UIHelper.setImage(actionBar, R.id.btn3, icon);

        mLowerActionBar = actionBar;

        return actionBar;
    }

    /**
     * Adds Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    protected View addLowerActionBar(int containerId, int buttonText1Id,
            int buttonText2Id)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.lower_action_bar_2);
        UIHelper.setText(actionBar, R.id.btn_1, buttonText1Id);
        UIHelper.setText(actionBar, R.id.btn_2, buttonText2Id);

        mLowerActionBar = actionBar;

        return actionBar;
    }

    /**
     * Adds Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    protected View addLowerActionBar(int containerId, int buttonTextId)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.lower_action_bar_1);
        UIHelper.setText(actionBar, R.id.btn_1, buttonTextId);

        mLowerActionBar = actionBar;

        return actionBar;
    }

    /**
     * Adds Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    protected View addLowerActionBarColored(int containerId, int buttonTextId,
            int bg_color, IFooterButton button)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.lower_action_bar_1);
        UIHelper.setText(actionBar, R.id.btn_1, buttonTextId);
        TextView textView_ab = (TextView) actionBar.findViewById(R.id.btn_1);
        textView_ab.setBackgroundResource(bg_color);

        // add by Steve
        textView_ab.setOnClickListener(new LowerActionBarClickListener(
                ONE_BUTTON_INDEX, button));

        mLowerActionBar = actionBar;

        return actionBar;
    }

    /**
     * 
     * Function Description
     *
     * @param containerId
     * @param layoutId
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    protected ViewGroup createLowerActionBar(int containerId, int layoutId)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(containerId);
        ab.removeAllViews();
        ViewGroup ab_v = (ViewGroup) activity.getLayoutInflater().inflate(
                layoutId, ab, false);
        ab.addView(ab_v);
        return ab_v;
    }

    // add by Steve {
    /**
     * Adds Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    protected View addLowerActionBar(int containerId, int buttonText1Id,
            int buttonText2Id, IFooterButton button)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.lower_action_bar_2);
        View leftButtonView = actionBar.findViewById(R.id.btn_1);
        View rightButtonView = actionBar.findViewById(R.id.btn_2);

        UIHelper.setText(actionBar, R.id.btn_1, buttonText1Id);
        UIHelper.setText(actionBar, R.id.btn_2, buttonText2Id);

        leftButtonView.setOnClickListener(new LowerActionBarClickListener(
                LEFT_BUTTON_INDEX, button));

        rightButtonView.setOnClickListener(new LowerActionBarClickListener(
                RIGHT_BUTTON_INDEX, button));

        mLowerActionBar = actionBar;

        return actionBar;
    }

    /**
     * Adds Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    protected View addLowerActionBar(int containerId, int buttonTextId,
            IFooterButton button)
    {
        // ActionBar1
        ViewGroup actionBar = createLowerActionBar(containerId,
                R.layout.lower_action_bar_1);
        View footerButtonView = actionBar.findViewById(R.id.btn_1);

        UIHelper.setText(actionBar, R.id.btn_1, buttonTextId);

        footerButtonView.setOnClickListener(new LowerActionBarClickListener(
                ONE_BUTTON_INDEX, button));

        mLowerActionBar = actionBar;

        return actionBar;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Fixed NSIQ-107]
