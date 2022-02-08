/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractList
 * Brief: Provide the interface of the AbstractList UI
 *
 * Create Date: 12/25/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: AbstractList.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;

public abstract class AbstractList extends GeneralAbstractList
{
    
    /**
     * 
     */
    public AbstractList()
    {
        super();
    }

    /**
     * 
     * @param context
     */
	public AbstractList(ActionBarActivity context) {
		super(context);
		// TODO Auto-generated constructor stub
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
        super.setupLayout(activity, data);

        int actionBarTextId = getActionBarText();
        int actionBarIconId = getActionBarIcon();         
        
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);
            
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
     * @param activity
     */ 
    @Override
    public void onHomePressed(ActionBarActivity activity)
    {
    	super.onHomePressed(activity);
        
    }

    /**
     * 
     *
     * @param activity
     */   
    @Override
    public void onInsulinConfirmationPressed(ActionBarActivity activity)
    {
        super.onInsulinConfirmationPressed(activity);
        
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
