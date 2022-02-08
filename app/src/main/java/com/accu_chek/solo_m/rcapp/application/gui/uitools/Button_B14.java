/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B14
 * Brief: Provide the function of the Button_B14 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B14.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;

public class Button_B14 extends ButtonBasic
{
    
    int mHeadline = 0;
    
    int mIcon = 0;
    
    String left_txt = null;
    
    String right_txt = null;
    
    OnClickListener mListener = null;

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return R.layout.button_b14;

    }

    /**
     * 
     * @param headline
     * @param icon
     * @param left
     * @param right
     * @param listener
     */
    public Button_B14(
            int headline, int icon, String left, String right,
            OnClickListener listener)
    {
        super();
        mHeadline = headline;
        mIcon = icon;
        left_txt = left;
        right_txt = right;
        mListener = listener;
    }

    /**
     * 
     * 
     *
     * @param listener
     */
    public void setListener(OnClickListener listener)
    {
        mListener = listener;
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
        UIHelper.setText(group, R.id.id_b14_text, mHeadline);
        UIHelper.setText(group, R.id.id_b14_text2, left_txt);
        UIHelper.setText(group, R.id.id_b14_text3, right_txt);
        UIHelper.setImage(group, R.id.id_b14_img, mIcon);

        if (mListener != null)
        {
            View view = (View) group.findViewById(R.id.id_button_white_field);
            view.setClickable(true);
            view.setOnClickListener(mListener);
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
