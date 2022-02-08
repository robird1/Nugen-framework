/**
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractDetailBGResult
 * Brief: Provide the interface of the AbstractDetailBGResult UI
 *
 * Create Date: 1/20/2015
 * $Revision: 24938 $
 * $Author: AdamChen $
 * $Id: AbstractDetailBGResult.java 24938 2015-11-26 10:04:20Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0017ab;

public abstract class AbstractDetailBGResult implements IActionBar,
        IFooterButton, IList, ILayoutInit
{

    protected ActionBarActivity mActivity = null;

    /**
     * 
     */
    public AbstractDetailBGResult()
    {
        super();
    }

    /**
     * 
     * @param context
     */
    public AbstractDetailBGResult(ActionBarActivity context)
    {
        mActivity = context;
    }

    /**
     * 
     * 
     *
     * @param activity
     * @param data
     */
    @Override
    public final void setupLayout(ActionBarActivity activity, Bundle data)
    {
        LAD0017ab layout = new LAD0017ab(activity);
        int actionBarTextId = getActionBarText();
        int actionBarIconId = getActionBarIcon();
        int footerButtonNumber = getFooterButtonNumber();
        ButtonBasic headerView = getHeaderView(data);
        ButtonBasic[] buttons = getListButtons(data);

        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);

//         layout.addHeaderView(headerView);
        layout.addDetailBgResultHeaderView(headerView);
        layout.disableHeaderViewEnabled();
        layout.addButtons(buttons);

        switch (footerButtonNumber)
        {
        case 1:
            int footerButtonTextId = getFooterButtonTextId();
            layout.setupLowerActionBar(footerButtonTextId, this);
            break;
        case 2:
            int leftButtonTextId = getFooterLeftButtonTextId();
            int rightButtonTextId = getFooterRightButtonTextId();

            layout.setupLowerActionBar(leftButtonTextId, rightButtonTextId,
                    this);
            break;
        default:
            layout.disableLowerActionBarEnabled();
            break;
        }

    }

    /**
     * 
     * 
     *
     * @param activity
     * @param data
     * @param requestCode
     */
    @Override
    public void updateLayout(ActionBarActivity activity, Bundle data,
            int requestCode)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onStart()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onResume()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onPause()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onStop()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onDestroy()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return ButtonBasic [out] Delete pre line return if exist. Parameter Description
     */
    @Deprecated
    protected ButtonBasic getHeaderView()
    {
        return null;
    }

    /**
     * 
     * Function Description
     *
     * @param data
     * @return
     * @return ButtonBasic [out] Delete pre line return if exist. Parameter Description
     */
    protected ButtonBasic getHeaderView(Bundle data)
    {
        return new ButtonBasic();
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
