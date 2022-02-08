/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B32
 * Brief: Provide the function of the Button_B32 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Button_B32.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import android.view.ViewGroup;
import android.view.View.OnClickListener;

public class Button_B32 extends ButtonBasic
{
    
    OnClickListener mListener = null;
    
    private B32Container container;
    
    /**
     * 
     * @param values
     * @param listener
     */
    public Button_B32(B32Container values, OnClickListener listener)
    {
        super();
        container = values;
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
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return R.layout.button_b32;
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
        UIHelper.setText(group, R.id.id_b32_title, container.title);

        UIHelper.setImage(group, R.id.id_b32_icon_1, container.title_icon1);
        UIHelper.setImage(group, R.id.id_b32_icon_2, container.title_icon2);
        UIHelper.setImage(group, R.id.id_b32_icon_3, container.title_icon3);

        UIHelper.setText(group, R.id.id_b32_value1, container.value1);
        UIHelper.setText(group, R.id.id_b32_value2, container.value2);
        UIHelper.setText(group, R.id.id_b32_value3, container.value3);

        // LinearLayout view =
        // (LinearLayout)group.findViewById(R.id.id_button_b23);
        // if(mListener != null) {
        //
        // view.setOnClickListener(mListener);
        // } else {
        // view.setBackgroundResource(R.color.background);
        // }
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
    
    /**
     * Violate coding rule: R16
     */
    public static class B32Container
    {
        
        public String title;

        public int title_icon1;
        
        public int title_icon2;
        
        public int title_icon3;

        public String value1;
        
        public String value2;
        
        public String value3;
    
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
