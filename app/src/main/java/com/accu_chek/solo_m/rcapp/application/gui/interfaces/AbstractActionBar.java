/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractActionBar
 * Brief: Provide the interface of the ActionBar UI component
 *
 * Create Date: 05/27/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractActionBar.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractActionBar implements IActionBar, ILayoutInit
{
    
    private static final int ICON_TOTAL_NUMBER_ONE = 1;
    
    private static final int ICON_TOTAL_NUMBER_TWO = 2;
    
    protected ActionBarActivity mContext = null;

    /**
     * 
     */
    public AbstractActionBar()
    {
        super();
    }

    /**
     * 
     * @param context
     */
    public AbstractActionBar(ActionBarActivity context)
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
        setupActionBar(activity);

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
     * @param activity
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setupActionBar(ActionBarActivity activity)
    {

        switch (getActionBarIconNumber())
        {
        case ICON_TOTAL_NUMBER_ONE:

            TopActionBar
                    .setup(activity, getActionBarText(), getActionBarIcon());

            break;
        case ICON_TOTAL_NUMBER_TWO:

            ImageView imageView = (ImageView) activity
                    .findViewById(R.id.actionbar_img2);
            ActionButton buttonListener = new ActionButton();

            TopActionBar.setup(activity, getActionBarText(),
                    getActionBarIcon(), getActionBarCustomIcon());

            imageView.setOnClickListener(buttonListener);

            break;
        default:
            throw new IllegalArgumentException(
                    "invalid action bar icon number...");
        }
    }
    
    /**
     * 
     */
    private class ActionButton implements OnClickListener
    {
        /**
         * 
         * 
         *
         * @param v
         */
        @Override
        public void onClick(View v)
        {
            onActionBarCustomIconClicked();
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
