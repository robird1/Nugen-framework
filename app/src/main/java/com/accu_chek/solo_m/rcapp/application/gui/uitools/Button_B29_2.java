/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B29_2
 * Brief: Provide the function of the Button_B29_2 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B29_2.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B29_2 extends ButtonBasic
{

    private int mIcon;
    
    private ButtonText mText2;

    /**
     * 
     * @param text
     * @param icon
     * @param text2
     * @param listener
     */
    public Button_B29_2(int text, int icon, int text2, OnClickListener listener)
    {
        super(text, listener);
        mIcon = icon;
        mText2 = new ButtonText(text2);
    }

    /**
     * 
     * @param text
     * @param icon
     * @param text2
     * @param listener
     */
    public Button_B29_2(
            int text, int icon, String text2, OnClickListener listener)
    {
        super(text, listener);
        mIcon = icon;
        mText2 = new ButtonText(text2);
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
        return R.layout.button_b29_2;
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
        return R.id.id_b29_text;
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
        UIHelper.setImage(group, R.id.id_b29_image, mIcon);
        mText2.set(group, R.id.id_b29_text2);
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
