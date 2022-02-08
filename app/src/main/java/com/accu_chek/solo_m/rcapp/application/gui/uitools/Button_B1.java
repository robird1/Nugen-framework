/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B1
 * Brief: Provide the function of the Button_B1 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B1.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B1 extends ButtonBasic
{
    
    ButtonText mSecondLine = null;

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     * @param listener
     */
    public Button_B1(int mDefault, int mSecondLine, OnClickListener listener)
    {
        super(mDefault, listener);
        this.mSecondLine = new ButtonText(mSecondLine);
    }

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     * @param listener
     */
    public Button_B1(String mDefault, int mSecondLine, OnClickListener listener)
    {
        super(mDefault, listener);
        this.mSecondLine = new ButtonText(mSecondLine);
    }

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     * @param listener
     */
    public Button_B1(int mDefault, String mSecondLine, OnClickListener listener)
    {
        super(mDefault, listener);
        this.mSecondLine = new ButtonText(mSecondLine);
    }

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     * @param listener
     */
    public Button_B1(
            String mDefault, String mSecondLine, OnClickListener listener)
    {
        super(mDefault, listener);
        this.mSecondLine = new ButtonText(mSecondLine);
    }

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     */
    public Button_B1(int mDefault, String mSecondLine)
    {
        super(mDefault);
        this.mSecondLine = new ButtonText(mSecondLine);
        this.setIsClickable(false);
    }

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     */
    public Button_B1(int mDefault, int mSecondLine)
    {
        super(mDefault);
        this.mSecondLine = new ButtonText(mSecondLine);
        this.setIsClickable(false);
    }

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     */
    public Button_B1(String mDefault, int mSecondLine)
    {
        super(mDefault);
        this.mSecondLine = new ButtonText(mSecondLine);
        this.setIsClickable(false);
    }

    /**
     * 
     * @param mDefault
     * @param mSecondLine
     */
    public Button_B1(String mDefault, String mSecondLine)
    {
        super(mDefault);
        this.mSecondLine = new ButtonText(mSecondLine);
        this.setIsClickable(false);
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
        return R.layout.button_b1;
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
        mSecondLine.set(group, getTextView2Id());
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateText2(String value)
    {
        TextView tv = (TextView) getView().findViewById(getTextView2Id());
        tv.setText(value);
    }

    /**
     * 
     * Function Description
     *
     * @param valueId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateText2(int valueId)
    {
        TextView tv = (TextView) getView().findViewById(getTextView2Id());
        tv.setText(valueId);
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
        return R.id.id_b1_text;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected int getTextView2Id()
    {
        return R.id.id_b1_text2;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [GUI] update GUI framework to ClickThrough v0.34
