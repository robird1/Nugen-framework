/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractStatusScreen
 * Brief: Provide the interface of the AbstractStatusScreen UI
 *
 * Create Date: 12/28/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: AbstractStatusScreen.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.StatusBG;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractStatusScreen implements IFooterButton, ILayoutInit
{
    
    public static final int LAYOUT_THREE_FOOTER_BUTTON = R.layout.lad0ss1;
    
    public static final int LAYOUT_TWO_FOOTER_BUTTON = R.layout.lad0ss2;
    
    protected ActionBarActivity mActivity = null;

    private AbstractStatusScreen mASS = null;
    
    /**
     * 
     */
    public AbstractStatusScreen() {
		super();
		// TODO Auto-generated constructor stub
	}

    /**
     * 
     * Function Description
     *
     * @param Ass
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
	public void setOnListener(AbstractStatusScreen Ass)
    {
        mASS = Ass;
    }
    
    /**
     * 
     * @param context
     */
    public AbstractStatusScreen(ActionBarActivity context)
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
//        android.support.v7.app.ActionBar actionBar = activity.getSupportActionBar();
//        
//        actionBar.hide();
        
        switch (footerButtonNumber)
        {
        case 2:
            activity.setContentView(LAYOUT_TWO_FOOTER_BUTTON);
            
            ViewGroup status_bg = (ViewGroup) activity.findViewById(R.id.lad0ss2_status_bg);         
            StatusBG m_stbg = new StatusBG(status_bg);
            mASS.setupStatusBgInfo(m_stbg);

            LinearLayout status_reservoir = (LinearLayout) activity.findViewById(R.id.lad0ss2_status_res);
            status_reservoir.addView(mASS.getReservoirView()); 

            LinearLayout pumpstop = (LinearLayout) activity.findViewById(R.id.lad0ss2_stop);
            pumpstop.addView(mASS.getContentView()); 

            // populateLowActionBar2(getFooterLeftButtonTextId(), getFooterRightButtonTextId());

            break;
        case 3:
            
            
            break;
        default:
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
    
    protected abstract View setupStatusBgInfo(StatusBG m_stbg);
    
    protected abstract View getReservoirView();
    
    protected abstract View getContentView();

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
