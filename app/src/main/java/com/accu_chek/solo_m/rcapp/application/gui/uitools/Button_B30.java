/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B30
 * Brief: Provide the function of the Button_B30 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B30.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.ViewGroup;
import android.widget.SeekBar;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B30 extends ButtonBasic
{

    private static final int LAYOUT_ID = R.layout.button_b30;
    
    private static final int TEXT_VIEW_ID = R.id.id_slider_title;
    
    private int progress = 0;

    /**
     * 
     * @param mText
     * @param value
     */
    public Button_B30(int mText, int value)
    {
        this(mText);
        progress = value;
    }

    /**
     * 
     * @param mText
     * @param value
     */
    public Button_B30(String mText, int value)
    {
        this(mText);
        progress = value;
    }

    /**
     * 
     * @param mText
     */
    public Button_B30(int mText)
    {
        super(mText);
    }

    /**
     * 
     * @param mText
     */
    public Button_B30(String mText)
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
     * Function Description
     *
     * @param newProgress
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateProgress(int newProgress)
    {
        progress = newProgress;
        refreshProgress(getView());
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getProgress()
    {
        SeekBar slider = (SeekBar) getView().findViewById(R.id.slider);
        progress = slider.getProgress();
        return progress;
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
     * @param group
     */
    @Override
    public void build(ViewGroup group)
    {
        super.build(group);
        refreshProgress(group);
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

    /**
     * 
     * Function Description
     *
     * @param group
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void refreshProgress(ViewGroup group)
    {
        SeekBar slider = (SeekBar) group.findViewById(R.id.slider);
        slider.setProgress(progress);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
