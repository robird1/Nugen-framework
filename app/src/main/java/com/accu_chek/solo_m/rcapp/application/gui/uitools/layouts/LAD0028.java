/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0028
 * Brief: Provide the function of the LAD0028 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0028.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0028 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0028;

    /**
     * 
     * @param activity
     */
    public LAD0028(Activity activity)
    {
        super(activity);
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

    /**
     * Setup layout using default parameters for the action bar (one button:
     * done).
     * 
     * @return Lower Action Bar view
     */
    public View setupLowerActionBar()
    {
        return setupLowerActionBar(R.string.txt_labeldone);
    }

    /**
     * Setup layout using specified parameters for the action bar 1.
     * 
     * @return Lower Action Bar view
     */
    public View setupLowerActionBar(int btn1TextId)
    {
        return addLowerActionBar(R.id.lower_ab, btn1TextId);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
