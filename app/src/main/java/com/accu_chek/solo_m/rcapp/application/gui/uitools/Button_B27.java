/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B27
 * Brief: Provide the function of the Button_B27 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Button_B27.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B27 extends ButtonBasic
{

    // The string "on" where is used to display in Content Description field.
    // For UIAutomator
    private static final String TOGGLE_ON = "on";
    
    // The string "off" where is used to display in Content Description field.
    // For UIAutomator
    private static final String TOGGLE_OFF = "off";

    private static final int LAYOUT_ID = R.layout.button_b27;
    
    private static final int TEXT_ID = R.id.id_b27_text;
    
    private static final int IMG_ID = R.id.id_b27_img;
    
    private boolean mIsOn;
    
    private OnSwitchingListener mListener;

    /**
     * 
     * @param text
     * @param isOn
     * @param listener
     */
    public Button_B27(int text, boolean isOn, OnSwitchingListener listener)
    {
        super(text);
        mIsOn = isOn;
        mListener = listener;
        setListener(defaultListener);
    }

    /**
     * 
     * @param text
     * @param isOn
     * @param listener
     */
    public Button_B27(String text, boolean isOn, OnSwitchingListener listener)
    {
        super(text);
        mIsOn = isOn;
        mListener = listener;
        setListener(defaultListener);
    }

    /**
     * 
     * @param text
     * @param isOn
     */
    public Button_B27(int text, boolean isOn)
    {
        super(text);
        mIsOn = isOn;
        setListener(defaultListener);
    }

    /**
     * 
     * @param text
     * @param isOn
     */
    public Button_B27(String text, boolean isOn)
    {
        super(text);
        mIsOn = isOn;
        setListener(defaultListener);
    }

    /**
     * 
     * @param text
     * @param isOn
     * @param listener
     */
    public Button_B27(int text, boolean isOn, OnClickListener listener)
    {
        super(text, listener);
        mIsOn = isOn;
    }

    /**
     * 
     * @param text
     * @param isOn
     * @param listener
     */
    public Button_B27(String text, boolean isOn, OnClickListener listener)
    {
        super(text, listener);
        mIsOn = isOn;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter Description
     */
    public boolean isOn()
    {
        return mIsOn;
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
        UIHelper.setImage(group, IMG_ID, chooseIcon(mIsOn));
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void toggle()
    {
        toggle(getView());
    }

    /**
     * 
     * Function Description
     *
     * @param isOn
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateIsOn(boolean isOn)
    {
        mIsOn = isOn;
        setIsOn(getView(), mIsOn);
    }

    /**
     * 
     * 
     *
     * @param view
     */
    @Override
    public void setView(ViewGroup view)
    {
        super.setView(view);
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
        return TEXT_ID;
    }
    
    /**
     * 
     * Function Description
     *
     * @param isOn
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected static int chooseIcon(boolean isOn)
    {
        return isOn ? R.drawable.on_slider : R.drawable.off_slider;
    }

    /**
     * 
     * Function Description
     *
     * @param buttonView
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void toggle(View buttonView)
    {
        mIsOn = !mIsOn;
        setIsOn(buttonView, mIsOn);
    }
    
    /**
     * 
     * Function Description
     *
     * @param buttonView
     * @param isOn
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public static void setIsOn(View buttonView, boolean isOn)
    {
        String description = TOGGLE_OFF;
        ImageView image = (ImageView) buttonView.findViewById(IMG_ID);
        image.setImageResource(chooseIcon(isOn));
        // Added by Henry Tso. To support UIAutomator
        if (true == isOn)
        {
            description = TOGGLE_ON;
        }
        image.setContentDescription(description);
    }

    /**
     * 
     */
    private OnClickListener defaultListener = new OnClickListener()
    {
        /**
         * 
         * 
         *
         * @param v
         */
        @Override
        public void onClick(View v)
        {
            boolean onSwitching = false;

            if (mListener != null)
            {
                onSwitching = mListener.onSwitching(Button_B27.this, mIsOn);
                if (onSwitching)
                    toggle(v);
            }
            else
            {
                toggle(v);
        }
        }
    };
    
    /**
     * 
     */
    public interface OnSwitchingListener
    {
        /**
         * @param currentValue Value before the switch
         * @return If true, switch will go trough, otherwise it will be
         *         cancelled.
         */
        public boolean onSwitching(Button_B27 sender, boolean currentValue);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
