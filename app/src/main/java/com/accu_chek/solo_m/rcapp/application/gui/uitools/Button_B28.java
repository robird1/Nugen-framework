/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B28
 * Brief: Provide the function of the Button_B28 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B28.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B28 extends ButtonBasic
{

    private static final int LAYOUT_ID = R.layout.button_b28;
    
    private static final int TEXT_VIEW_ID = R.id.id_b28_text;

    /**
     * 
     * @param mText
     * @param listener
     */
    public Button_B28(int mText, OnClickListener listener)
    {
        super(mText);
        setListener(listener);
    }

    /**
     * 
     * @param mText
     * @param listener
     */
    public Button_B28(String mText, OnClickListener listener)
    {
        super(mText);
        setListener(listener);
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
     * 
     * 
     *
     * @return
     */
    @Override
    protected int getTextViewId()
    {
        return TEXT_VIEW_ID;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
