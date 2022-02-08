/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractAboutInfo
 * Brief: Provide the interface of the AbstractAboutInfo UI
 *
 * Create Date: 08/07/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractInfoScreen.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0007a;

public abstract class AbstractInfoScreen implements ILayoutInit
{
    
    protected ActionBarActivity mContext = null;
    
    /**
     * 
     * @param context
     */
    public AbstractInfoScreen(ActionBarActivity context)
    {
        mContext = context;
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
        LAD0007a layout = new LAD0007a(mContext);
        layout.addButtons(getListButtons());

        TopActionBar.setup(mContext, getActionBarTitle(), getActionBarIcon());
    }
    
    /**
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
     */    
    @Override
    public void onDestroy()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
    
    protected abstract ButtonBasic[] getListButtons(); 
    
    protected abstract int getActionBarIcon(); 
    
    protected abstract int getActionBarTitle();

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [test header footer]
