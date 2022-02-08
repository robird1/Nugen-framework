/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Bolus_Shortcut
 * Brief: Provide the function of the Bolus_Shortcut UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Bolus_Shortcut.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Bolus_Shortcut extends ButtonBasic
{

    private int mIcon = 0;

    /**
     * 
     * @param text
     * @param icon
     * @param listener
     */
    public Bolus_Shortcut(int text, int icon, OnClickListener listener)
    {
        super(text, listener);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param icon
     * @param listener
     */
    public Bolus_Shortcut(String text, int icon, OnClickListener listener)
    {
        super(text, listener);
        mIcon = icon;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getIcon()
    {
        return mIcon;
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
        return R.layout.bolus_shortcut;
    }

    /**
     * 
     * 
     *
     * @param group
     */
    @Override
    public void build(ViewGroup group)
    {
        super.build(group);
        if (mIcon != 0)
        {
            UIHelper.setImage(group, R.id.id_bolus_short_img, mIcon);
        }

    }
    
    /**
     * 
     * 
     *
     * @return
     */
    @Override
    protected int getTextViewId()
    {
        return R.id.id_bolus_short_title;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */

