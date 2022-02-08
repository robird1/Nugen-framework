/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractDialogType9
 * Brief: Provide the interface of the AbstractDialogType9 UI
 *
 * Create Date: 02/13/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractDialogType9.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.View;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0001c;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractDialogType9 implements IFooterButton, ILayoutInit
{
	
    protected ActionBarActivity mActivity = null;
    
	private View mFooterButtonView = null;
	
	/**
	 * 
	 */
    public AbstractDialogType9()
    {
        super();
    }

    /**
     * 
     * @param context
     */
	public AbstractDialogType9(ActionBarActivity context)
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
        int footerButtonNumber = getFooterButtonNumber();
        int infoIconId = getInfoIconId();
        int infoTitleId = getInfoTitleId();
        int infoTxtleftId = getInfoTxtleftId(data);
        String infoTxtRightId = null; 
        int infoDescriptionId = getInfoDescriptionId();
        
        LAD0001c layout = new LAD0001c(activity);
        
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(this.getClass().getName());
        TopActionBar.setup(activity, R.string.txt_information, R.drawable.remote);
        
        infoTxtRightId = getInfoTxtRightId(data);
        layout.setup(infoIconId, infoTitleId, infoDescriptionId, infoTxtleftId,
                infoTxtRightId);
        
        switch (footerButtonNumber)
        {
        case 1:
            int footerButtonTextId = getFooterButtonTextId();

            mFooterButtonView = layout.setupLowerActionBar(footerButtonTextId, this);
            
            break;
        case 2:
            int footerLeftButtonTextId = getFooterLeftButtonTextId();
            int footerRightButtonTextId = getFooterRightButtonTextId();

            mFooterButtonView = layout.setupLowerActionBar(footerLeftButtonTextId, footerRightButtonTextId, this);

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
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    protected View getFooterButtonView()
    {
        return mFooterButtonView;
    }
    

    protected abstract int getInfoIconId();
    
    protected abstract int getInfoTitleId();
    
    protected abstract int getInfoTxtleftId(Bundle data);
    
    protected abstract String getInfoTxtRightId(Bundle data);
    
    protected abstract int getInfoDescriptionId();
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
