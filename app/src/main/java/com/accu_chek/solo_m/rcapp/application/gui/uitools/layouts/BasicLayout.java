/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BasicLayout
 * Brief: Provide the function of the BasicLayout layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: BasicLayout.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.ViewGroup;

public class BasicLayout
{
    
    int mLayoutID = 0;
    
    ViewGroup mLayout = null;

    /**
     * 
     * @param mLayoutID
     */
    public BasicLayout(int mLayoutID)
    {
        super();
        this.mLayoutID = mLayoutID;
    }

    /**
     * 
     * @param mLayoutID
     * @param group
     */
    public BasicLayout(int mLayoutID, ViewGroup group)
    {
        super();
        this.mLayoutID = mLayoutID;
        mLayout = getLayout(group);
    }

    /**
     * 
     * @param mLayoutID
     * @param activity
     */
    public BasicLayout(int mLayoutID, Activity activity)
    {
        super();
        this.mLayoutID = mLayoutID;
        mLayout = getLayout(activity);
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    public ViewGroup getLayout(ViewGroup group)
    {
        return (ViewGroup) group.findViewById(this.mLayoutID);
    }

    /**
     * 
     * Function Description
     *
     * @param activity
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    public ViewGroup getLayout(Activity activity)
    {
        return (ViewGroup) activity.findViewById(this.mLayoutID);
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */