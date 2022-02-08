/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B5
 * Brief: Provide the function of the Button_B5 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B5.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B5 extends ButtonBasic
{

    private int mIcon = 0;
    
    private Boolean mActive = true;

    /**
     * 
     * @param text
     * @param icon
     * @param listener
     */
    public Button_B5(int text, int icon, OnClickListener listener)
    {
        super(text, listener);
        if (listener == null)
        {
            mActive = false;
        }
        mIcon = icon;
    }

    /**
     * This constructor is defined for construct the Button_B5.
     * 
     * @param text [in] Text resource id
     * @param icon [in] Icon resource id
     * @param listener [in] OnClickListener object
     */
    public Button_B5(String text, int icon, OnClickListener listener)
    {
        super(text, listener);
        if (listener == null)
        {
            mActive = false;
        }
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
        return R.layout.button_b5;
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
        UIHelper.setImage(group, R.id.id_b5_img, mIcon);
        if (mActive == false)
        {
            View clickfield = (View) group
                    .findViewById(R.id.id_button_white_field);
            clickfield.setBackground(null);
            clickfield.setClickable(false);
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
        return R.id.id_b5_text;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// Roll back to 8/4 version
// Restore to 8/4 version
