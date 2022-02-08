/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0012
 * Brief: Provide the function of the LAD0012 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0012.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0012 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0012;
    
    private ViewGroup mParent = null;

    /**
     * 
     * @param activity
     */
    public LAD0012(Activity activity)
    {
        super(activity);
        mParent = (ViewGroup) findViewById(R.id.id_lad0012_rel);
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
     * Set graphic image id
     */
    public LAD0012 setup(int color)
    {
        mParent.setBackgroundColor(color);
        return this;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
