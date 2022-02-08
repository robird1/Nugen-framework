/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B8
 * Brief: Provide the function of the Button_B8 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24617 $
 * $Author: HenryTso $
 * $Id: Button_B8.java 24617 2015-11-23 08:28:19Z HenryTso $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.view.ViewGroup;
import android.view.View.OnClickListener;

public class Button_B8 extends Button_B2
{

    private int rightIcon = 0;
    
    /**
     * @param text
     * @param isRadio
     * @param listener
     */
    public Button_B8(
            int text, boolean isRadio, int rightIcon, OnClickListener listener)
    {
        super(text, isRadio, listener);
        this.rightIcon = rightIcon;
    }

    /**
     * @param text
     * @param isRadio
     * @param listener
     */
    public Button_B8(
            String text, boolean isRadio, int rightIcon,
            OnClickListener listener)
    {
        super(text, isRadio, listener);
        this.rightIcon = rightIcon;
    }

    /**
     * @param text
     * @param isRadio
     */
    public Button_B8(int text, boolean isRadio, int rightIcon)
    {
        super(text, isRadio);
        this.rightIcon = rightIcon;
    }

    /**
     * @param text
     * @param isRadio
     */
    public Button_B8(String text, boolean isRadio, int rightIcon)
    {
        super(text, isRadio);
        this.rightIcon = rightIcon;
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
        String index = String.format("icon_%d", mItemIndex);
        
        super.build(group);
        UIHelper.setImage(group, R.id.id_b8_img2, rightIcon, index);
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
        boolean result = isRadio();
        int id = 0;
        
        if (true == result)
        {
            id = R.layout.button_b8_radio;
        }
        else
        {
            id = R.layout.button_b8;
        }
        return id;
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
        return R.id.id_b8_text;
    }

    /**
     * 
     * 
     *
     * @return
     */
    protected int getSelectorId()
    {
        return R.id.id_b8_chkb;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
