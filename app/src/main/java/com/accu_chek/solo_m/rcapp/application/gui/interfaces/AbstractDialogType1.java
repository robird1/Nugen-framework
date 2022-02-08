/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractDialogType1
 * Brief: Provide the interface of the AbstractDialogType1 UI
 *
 * Create Date: 02/13/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractDialogType1.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0001a;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractDialogType1 implements IActionBar, IFooterButton, ILayoutInit
{
    
    protected ActionBarActivity mActivity = null;

    /**
     * 
     */
    public AbstractDialogType1()
    {
        super();
    }

    /**
     * 
     * @param context
     */
	public AbstractDialogType1(ActionBarActivity context)
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
        
        LAD0001a layout = new LAD0001a(activity);
        
        layout.setup(getTitleIconId(), getTitleTextId(), getContentTextId());
        
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, getActionBarText(), getActionBarIcon());

        setupFooterButton(layout);
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
   	public void updateLayout(ActionBarActivity activity, Bundle data, int requestCode) 
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
     * @param activity
     */
    @Override
    public void onBackPressed(ActionBarActivity activity)
    {
        mActivity.finish();

    }

    /**
     * 
     * 
     *
     * @param activity
     */
    @Override
    public void onHomePressed(ActionBarActivity activity)
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
     * @param activity
     */
    @Override
    public void onNextPressed(ActionBarActivity activity)
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
        return 1;
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
        return R.string.txt_scr0066_info_title;
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
        return R.drawable.remote;
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
        return 1;
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
        return R.string.txt_ok;
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
        return R.string.txt_no;
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
        return R.string.txt_yes;
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
    public void onFooterButtonClicked()
    {
        mActivity.finish();
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterLeftButtonClicked()
    {
        mActivity.finish();

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
     * @param savedInstanceState
     */
    @Override
    public void onCreate(Bundle savedInstanceState)
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
    public void onStart()
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
    public void onResume()
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
    public void onPause()
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
    public void onStop()
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
    public void onDestroy()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
    
    protected abstract int getTitleIconId();
    
    protected abstract int getTitleTextId();
    
    protected abstract int getContentTextId();

    /**
     * 
     * Function Description
     *
     * @param layout
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setupFooterButton(LAD0001a layout)
    {
        switch (getFooterButtonNumber())
        {
        case 1:
            
            layout.setupLowerActionBar(getFooterButtonTextId(), this);

            break;
        case 2:

            layout.setupLowerActionBar(getFooterLeftButtonTextId(), getFooterRightButtonTextId(), this);

            break;
        default:
            // do nothing
            break;
        }
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [GUI] update code according to coding guideline
// [Settings] RCSWSPUI33.9
