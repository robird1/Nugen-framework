/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractDialogType5
 * Brief: Provide the interface of the AbstractDialogType5 UI
 *
 * Create Date: 02/13/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractDialogType5.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0002h;

public abstract class AbstractDialogType5 implements IActionBar, IFooterButton, ILayoutInit 
{
    
	protected ActionBarActivity mActivity = null;
    
	/**
	 * 
	 */
    public AbstractDialogType5()
    {
        super();
        
    }

    /**
     * 
     * @param context
     */
	public AbstractDialogType5(ActionBarActivity context)
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
        int actionBarIconId = getActionBarIcon();
        int actionBarTextId = getActionBarText();
        int footerButtonNumber = getFooterButtonNumber();
        int contentIconId = getTitleIconId();
        int titleTextId = getTitleTextId();
        String warningNumber = getWarningNumber();
        LAD0002h layout = new LAD0002h(activity);

        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);

        layout.setup(contentIconId, titleTextId);
        layout.setWarning(warningNumber);
        
        switch (footerButtonNumber)
        {
        case 1:
            int footerButtonTextId = getFooterButtonTextId();
            
            layout.setupLowerActionBar(footerButtonTextId, this);

            break;
        case 2:
            int footerLeftButtonTextId = getFooterLeftButtonTextId();
            int footerRightButtonTextId = getFooterRightButtonTextId();

            layout.setupLowerActionBar(footerLeftButtonTextId, footerRightButtonTextId, this);

            break;
        default:
            // do nothing
            break;

        }
        
        setContentInfo(layout);
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
    
    protected abstract String getWarningNumber();
    
    protected abstract void setContentInfo(LAD0002h layout);

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
