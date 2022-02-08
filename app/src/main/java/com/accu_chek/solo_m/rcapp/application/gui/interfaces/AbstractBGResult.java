/**
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: AbstractBGResult
 * Brief: Provide the interface of the AbstractBGResult UI
 * 
 * Create Date: 12/28/2014
 * $Revision: 25086 $
 * $Author: AdamChen $
 * $Id: AbstractBGResult.java 25086 2015-11-30 06:31:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0016;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

public abstract class AbstractBGResult implements IActionBar, IFooterButton,
        ILayoutInit
{

    protected ActionBarActivity mActivity = null;

    private SafetyChannel<Integer> mDefaultValue = new SafetyChannel<Integer>();
    
    /**
     * 
     */
    public AbstractBGResult()
    {

    }

    /**
     * 
     * @param context
     */
    public AbstractBGResult(ActionBarActivity context)
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
        int actionBarTextId = getActionBarText();
        int actionBarIconId = getActionBarIcon();
        int footerButtonNumber = getFooterButtonNumber();

        // Modified by Henry Tso. For implement Safety
        String unit = getBgUnit(data).getString();
        String value = null;
        String timeInfoText = getTimeInfoText();

        LAD0016 layout = new LAD0016(activity);
        SafetyString bgValue = null;

        // Modified by Henry Tso. For implement Safety

        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);

        layout.setBgUnit(unit);

        bgValue = getBgValue(data);
        
        if (bgValue != null)
        {
            value = bgValue.getString();
            layout.setBgValue(value);
            layout.setSymbols();
        }
        else
        {
            layout.setSymbols(getBgResult(), getBgResultFlag());
        }
        if (timeInfoText != null)
        {
            layout.setLowerLine(timeInfoText);
        }

        switch (footerButtonNumber)
        {
        case 1:
            int footerButtonTextId = getFooterButtonTextId();

            layout.setupLowerActionBar(footerButtonTextId, this);

            break;
        case 2:
            int footerLeftButtonTextId = getFooterLeftButtonTextId();
            int footerRightButtonTextId = getFooterRightButtonTextId();

            layout.setupLowerActionBar(footerLeftButtonTextId,
                    footerRightButtonTextId, this);

            break;
        default:
            // do nothing
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

    /**
     * 
     * Function Description
     * 
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist.
     *         Parameter Description
     */
    public SafetyChannel<Integer> getBgResult()
    {
        mDefaultValue.set(0, 0);
        
        return mDefaultValue;
    }

    /**
     * 
     * Function Description
     * 
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist.
     *         Parameter Description
     */
    public SafetyChannel<Integer> getBgResultFlag()
    {
        mDefaultValue.set(0, 0);
        
        return mDefaultValue;
    }

    protected abstract String getTimeInfoText();

    protected abstract SafetyString getBgUnit(Bundle data);

    protected abstract SafetyString getBgValue(Bundle data);

    /**
     * 
     * 
     * 
     * @param activity
     */
    @Override
    public void onInsulinConfirmationPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getFooterMiddleButtonIconId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterMiddleButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getActionBarCustomIcon()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onActionBarCustomIconClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getFooterButtonNumber()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getFooterButtonTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getFooterLeftButtonTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getFooterRightButtonTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterLeftButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterRightButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getActionBarIconNumber()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getActionBarIcon()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     * 
     * @return
     */
    @Override
    public int getActionBarText()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
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
    

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// (R14481 2015-08-08 09:09:17 HenryTso)
// ----------------------------------------------------------------------------
// [memory leak issue] update code
// (R14481 2015-08-08 09:09:17 HenryTso)
// ----------------------------------------------------------------------------
// [memory leak issue] update code
// (R15470 2015-08-26 01:31:04 SteveSu)
// ----------------------------------------------------------------------------
// [NSIQ-55] Modify for fixing the possible memory leak issue in TopBar class
// (R23049 2015-11-02 08:40:34 VictorChen)
// ----------------------------------------------------------------------------
// Refine bg value use 2ch.
