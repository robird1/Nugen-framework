/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractReplacement
 * Brief: Provide the interface of the AbstractReplacement UI
 *
 * Create Date: 03/10/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractReplacement.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0015;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractReplacement implements IActionBar, IFooterButton,
        ILayoutInit
{
    
    protected ActionBarActivity mActivity = null;
    
	/**
	 * 
	 */
    public AbstractReplacement() 
    {
		super();
	}

    /**
     * 
     * @param context
     */
	public AbstractReplacement(ActionBarActivity context)
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
    public void setupLayout(ActionBarActivity activity, Bundle data)
    {
        int actionBarIconId = getActionBarIcon();
        int actionBarTextId = getActionBarText();
        int footerLeftButtonTextId = getFooterLeftButtonTextId();
        int footerRightButtonTextId = getFooterRightButtonTextId();
        int titleTextId = onTitleText();

        LAD0015 layout = new LAD0015(activity);
        
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);

        layout.setup(titleTextId);
        
        onSetInstructionText(layout);
        
        layout.setupLowerActionBar(footerLeftButtonTextId, footerRightButtonTextId, this);
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
	public void updateLayout(ActionBarActivity activity, Bundle data,int requestCode) 
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
    public int getFooterButtonNumber()
    {
        return 2;
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
        return R.string.txt_help;
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
        return R.string.txt_labeldone;
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

    protected abstract int onTitleText();
    
    protected abstract void onSetInstructionText(LAD0015 layout);

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
