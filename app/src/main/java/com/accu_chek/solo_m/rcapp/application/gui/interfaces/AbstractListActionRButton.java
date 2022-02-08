/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractListActionRButton
 * Brief: Provide the interface of the AbstractListActionRButton UI
 *
 * Create Date: 03/18/2015
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractListActionRButton.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractListActionRButton extends GeneralAbstractList
{

    private ImageView imageView = null;
    
    /**
     * 
     */
    public AbstractListActionRButton()
    {
        super();
    }

    /**
     * 
     * @param context
     */
    public AbstractListActionRButton(ActionBarActivity context)
    {
        super(context);
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

        ActionBar.textId = getActionBarText();
        ActionBar.iconId = getActionBarIcon();
        ActionBar.iconId_Rbutton = this.getActionBarCustomIcon();

        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, ActionBar.textId, ActionBar.iconId,
                ActionBar.iconId_Rbutton);
        imageView = (ImageView) activity.findViewById(R.id.actionbar_img2);
        ActionButton buttonListener = new ActionButton();
        imageView.setOnClickListener(buttonListener);

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
        super.onBackPressed(activity);
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
        super.onNextPressed(activity);
    }
    
    /**
     * 
     */
    private static abstract class ActionBar
    {
        public static int textId = 0;
        public static int iconId = 0;
        public static int iconId_Rbutton = 0;
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
