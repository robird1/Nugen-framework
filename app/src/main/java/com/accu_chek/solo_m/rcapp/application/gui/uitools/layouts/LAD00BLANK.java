/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD00BLANK
 * Brief: Provide the function of the LAD00BLANK layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD00BLANK.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD00BLANK extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad00blank;

    /**
     * 
     * @param activity
     */
    public LAD00BLANK(Activity activity)
    {
        super(activity);
        // ActionBar tab = activity.getActionBar();
        // tab.hide();
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
