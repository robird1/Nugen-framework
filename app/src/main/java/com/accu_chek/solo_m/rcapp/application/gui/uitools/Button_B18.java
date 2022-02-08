/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B18
 * Brief: Provide the function of the Button_B18 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B18.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;

public class Button_B18 extends ButtonBasic
{
    
    int mHeadline = 0;
    
    String left_txt = null;
    
    String right_txt = null;
    
    String head_txt = null;
    
    OnClickListener mListener_left = null;
    
    OnClickListener mListener_right = null;

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return R.layout.button_b18;

    }

    /**
     * 
     * @param headline
     * @param left
     * @param right
     */
    public Button_B18(int headline, String left, String right)
    {
        super();
        mHeadline = headline;
        left_txt = left;
        right_txt = right;
    }

    /**
     * 
     * @param headline_Str
     * @param left
     * @param right
     */
    public Button_B18(String headline_Str, String left, String right)
    {
        super();
        head_txt = headline_Str;
        left_txt = left;
        right_txt = right;
    }

    /**
     * 
     * Function Description
     *
     * @param left_listener
     * @param right_listener
     * @return
     * @return Button_B18 [out] Delete pre line return if exist. Parameter Description
     */
    public Button_B18 setListener(OnClickListener left_listener,
            OnClickListener right_listener)
    {
        mListener_left = left_listener;
        mListener_right = right_listener;
        return this;
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
        // Added by Henry Tso. To support UIAutomator
        String index = String.format("Item_%d", mItemIndex++);

        if (head_txt != null)
            UIHelper.setText(group, R.id.id_b18_text, head_txt);
        else
            UIHelper.setText(group, R.id.id_b18_text, mHeadline, index);
        UIHelper.setText(group, R.id.id_b18_box_left, left_txt);
        UIHelper.setText(group, R.id.id_b18_box_right, right_txt);

        if (mListener_left != null)
        {
            View view = (View) group.findViewById(R.id.id_b18_box_left);
            view.setOnClickListener(mListener_left);
        }
        if (mListener_right != null)
        {
            View view = (View) group.findViewById(R.id.id_b18_box_right);
            view.setOnClickListener(mListener_right);
        }
    }

    /**
     * 
     * 
     *
     * @param group
     * @return
     */
    @Override
    public ViewGroup add(ViewGroup group)
    {
        // TODO Auto-generated method stub
        return null;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
