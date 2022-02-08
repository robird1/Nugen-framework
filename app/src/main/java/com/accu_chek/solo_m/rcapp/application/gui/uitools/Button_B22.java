/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B22
 * Brief: Provide the function of the Button_B22 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B22.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B22 extends ButtonBasic
{

    private static final int LAYOUT_ID = R.layout.button_b22;
    
    private static final int TEXT_VIEW_ID = R.id.id_b22_text;

    /**
     * 
     * @param mText
     * @param listener
     */
    public Button_B22(int mText, OnClickListener listener)
    {
        this(mText);
        setListener(listener);
    }

    /**
     * 
     * @param mText
     * @param listener
     * @param tag
     */
    public Button_B22(int mText, OnClickListener listener, int tag)
    {
        this(mText);
        setListener(listener);
        setTag(tag);
    }

    /**
     * 
     * @param mText
     * @param listener
     */
    public Button_B22(String mText, OnClickListener listener)
    {
        this(mText);
        setListener(listener);
    }

    /**
     * 
     * @param mText
     */
    public Button_B22(int mText)
    {
        super(mText);
    }

    /**
     * 
     * @param mText
     */
    public Button_B22(String mText)
    {
        super(mText);
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
