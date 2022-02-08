/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Status_Slider
 * Brief: Provide the interface function of the Status_Slider UI component
 *
 * Create Date: 10/16/2015
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Status_Slider.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.ImageView;

public class Status_Slider extends ButtonBasic
{
    String mTitle = null;
    
    String mBGUnit = null;
    
    String mUnittxt = null;
    
    int mIntTitle = 0;
    
    int mBGUnit_int = 0;
    
    OnClickListener mListener = null;

    /**
     * 
     * @param title
     * @param unit
     * @param unittxt
     * @param listener
     */
    public Status_Slider(
            String title, String unit, String unittxt, OnClickListener listener)
    {
        super();
        mTitle = title;
        mBGUnit = unit;
        mUnittxt = unittxt;
        mListener = listener;
    }

    /**
     * 
     * @param title
     * @param unit
     * @param unittxt
     * @param listener
     */
    public Status_Slider(
            String title, int unit, String unittxt, OnClickListener listener)
    {
        super();
        mTitle = title;
        mBGUnit_int = unit;
        mUnittxt = unittxt;
        mListener = listener;
    }

    /**
     * 
     * @param title
     * @param unit
     * @param unittxt
     * @param listener
     */
    public Status_Slider(
            int title, String unit, String unittxt, OnClickListener listener)
    {
        super();
        mIntTitle = title;
        mBGUnit = unit;
        mUnittxt = unittxt;
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
        return R.layout.status_slider;
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
        if (mIntTitle != 0)
        {
            UIHelper.setText(group, R.id.id_status_slider_title, mIntTitle);
        }
        else
        {
            UIHelper.setText(group, R.id.id_status_slider_title, mTitle);
        }
        
        if (mBGUnit_int != 0)
        {
            UIHelper.setText(group, R.id.id_status_slider_value, mBGUnit_int);
        }
        else
        {
            UIHelper.setText(group, R.id.id_status_slider_value, mBGUnit);
        }
        UIHelper.setText(group, R.id.id_status_slider_info, mUnittxt);
        View view = (View) group.findViewById(R.id.id_button_white_field);
        
        if (mListener != null)
        {
            view.setOnClickListener(mListener);
            view.setBackgroundResource(R.drawable.bg_background_to_brown_click);
            view.setClickable(true);
            
        }
        else
        {
        view.setClickable(false);
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

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void removeSlider()
    {
        ImageView img = (ImageView) getView().findViewById(
                R.id.id_status_slider_img);
        img.setImageDrawable(null);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
