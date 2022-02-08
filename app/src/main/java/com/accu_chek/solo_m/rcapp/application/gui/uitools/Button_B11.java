/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B11
 * Brief: Provide the function of the Button_B11 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24767 $
 * $Author: SteveSu $
 * $Id: Button_B11.java 24767 2015-11-25 07:11:47Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.content.res.Resources;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B11 extends ButtonBasic
{

    private static final int LAYOUT_ID = R.layout.button_b11;
    
    private static final int TEXT_VIEW_ID = R.id.id_b11_text;
    
    private Boolean mActive = true;

    /**
     * 
     * @param mText
     * @param listener
     */
    public Button_B11(int mText, OnClickListener listener)
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
    public Button_B11(int mText, OnClickListener listener, int tag)
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
    public Button_B11(String mText, OnClickListener listener)
    {
        this(mText);
        setListener(listener);
    }

    /**
     * 
     * @param mText
     * @param active
     */
    public Button_B11(int mText, Boolean active)
    {
        super(mText);
        mActive = active;
    }

    /**
     * 
     * @param mText
     * @param active
     */
    public Button_B11(String mText, Boolean active)
    {
        super(mText);
        mActive = active;
    }

    /**
     * 
     * @param mText
     */
    public Button_B11(int mText)
    {
        super(mText);
    }

    /**
     * 
     * @param mText
     */
    public Button_B11(String mText)
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
     * @param group
     */
    @Override
    public void build(ViewGroup group)
    {
        super.build(group);
        if (mActive == false)
        {
            View clickfield = (View) group
                    .findViewById(R.id.id_button_white_field);
            clickfield.setBackground(null);
            clickfield.setClickable(false);
        }
    }
    
    // add by Steve {
    public void disable() 
    {
        Resources res = getView().getContext().getResources();
        View clickfield = (View) getView().findViewById(R.id.id_button_white_field);
        TextView textView = (TextView) getView().findViewById(R.id.id_b11_text);
        textView.setTextColor(res.getColor(R.color.main_grey_40));
        clickfield.setClickable(false);
    }
    // } add by Steve

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

// (R24175 2015-11-16 05:33:09 AdamChen)
// ----------------------------------------------------------------------------
// [Reminder] update the functionality of adding reminder
