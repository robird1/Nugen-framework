/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B4
 * Brief: Provide the function of the Button_B4 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B4.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B4 extends ButtonBasic
{
    
    ButtonText mSecondLine = null;
    
    private int mIcon = 0;

    /**
     * 
     * @param text
     * @param icon
     * @param listener
     */
    public Button_B4(int text, int icon, OnClickListener listener)
    {
        super(text, listener);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param mSecondLine
     * @param icon
     * @param listener
     */
    public Button_B4(
            int text, int mSecondLine, int icon, OnClickListener listener)
    {
        super(text, listener);
        this.mSecondLine = new ButtonText(mSecondLine);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param mSecondLine
     * @param icon
     * @param listener
     */
    public Button_B4(
            int text, String mSecondLine, int icon, OnClickListener listener)
    {
        super(text, listener);
        this.mSecondLine = new ButtonText(mSecondLine);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param mSecondLine
     * @param icon
     * @param listener
     */
    public Button_B4(
            String text, String mSecondLine, int icon, OnClickListener listener)
    {
        super(text, listener);
        this.mSecondLine = new ButtonText(mSecondLine);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param icon
     * @param listener
     */
    public Button_B4(String text, int icon, OnClickListener listener)
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
        return R.layout.button_b4;
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
        UIHelper.setImage(group, R.id.id_b4_img, mIcon);
        mSecondLine.set(group, getTextView2Id());

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
        return R.id.id_b4_text;
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
        return R.id.id_b4_text2;
    }

    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Sound setting] add the screen of signal suspension
