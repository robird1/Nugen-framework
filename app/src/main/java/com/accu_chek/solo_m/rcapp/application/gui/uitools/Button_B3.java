/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B3
 * Brief: Provide the function of the Button_B3 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Button_B3.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.content.res.Resources;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B3 extends ButtonBasic
{

    private static final int ID_TEXT2 = R.id.id_b3_text2;
    
    private ButtonText mText2 = null;
    
    private Boolean mActive = true;
    
    private Boolean mWhiteBG = false;

    /**
     * 
     * @param text1
     * @param text2
     * @param listener
     */
    public Button_B3(int text1, String text2, OnClickListener listener)
    {
        this(text1, listener);
        mText2 = new ButtonText(text2);
        if (listener == null)
        {
            mActive = false;
        }
    }

    /**
     * This constructor is defined for construct the Button_B3.
     * 
     * @param text [in] Text resource id
     * @param icon [in] Icon resource id
     * @param listener [in] OnClickListener object
     */
    public Button_B3(int text1, int text2, OnClickListener listener)
    {
        this(text1, listener);
        this.mText2 = new ButtonText(text2);
        if (listener == null)
        {
            mActive = false;
        }
    }

    /**
     * 
     * @param text1
     * @param listener
     */
    private Button_B3(int text1, OnClickListener listener)
    {
        super(text1, listener);
        if (listener == null)
        {
            mActive = false;
        }
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
        return R.layout.button_b3;

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
        TextView tv = (TextView) getView().findViewById(ID_TEXT2);
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
        TextView tv = (TextView) getView().findViewById(ID_TEXT2);
        tv.setText(valueId);
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void disable()
    {
        Resources res = getView().getContext().getResources();
        {
            TextView textView = (TextView) getView().findViewById(
                    R.id.id_b3_text);
            textView.setTextColor(res.getColor(R.color.main_grey_40));
        }
        {
            TextView textView = (TextView) getView().findViewById(ID_TEXT2);
            textView.setTextColor(res.getColor(R.color.main_grey_40));
        }
        this.setListener(null);
        this.setIsClickable(false);
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void disableWhiteBG()
    {
        mWhiteBG = true;
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
        mText2.set(group, ID_TEXT2);
        if ((mActive == false) && (mWhiteBG == false))
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
        return R.id.id_b3_text;
    }


}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
