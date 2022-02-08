/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractDialogType3
 * Brief: Provide the interface of the AbstractDialogType3 UI
 *
 * Create Date: 02/13/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractDialogType3.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0002a;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractDialogType3 implements IActionBar, ILayoutInit
{

	protected ActionBarActivity mActivity = null;
    
	/**
	 * 	
	 */
    public AbstractDialogType3() {
		super();
	}

    /**
     * 
     * @param context
     */
	public AbstractDialogType3(ActionBarActivity context)
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
        int contentIconId = getTitleIconId();
        int titleTextId = getTitleTextId();
//        String contentInfoTitle = getContentInfoTitle();

        LAD0002a layout = new LAD0002a(activity);
        
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);

        layout.setup(contentIconId, titleTextId, "[Insert Bolus Name]",
                R.string.txt_insulinconfirmationsoftkey);
        
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
//    protected abstract String getContentInfoTitle();
    protected abstract void setContentInfo(LAD0002a layout);

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */