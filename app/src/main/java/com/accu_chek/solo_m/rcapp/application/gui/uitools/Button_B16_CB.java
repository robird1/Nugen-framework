/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B16_CB
 * Brief: Provide the function of the Button_B16_CB UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: Button_B16_CB.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.text.TextUtils;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class Button_B16_CB extends ButtonBasic
{

    ButtonText mSecondLine = null;
    
    private int mIcon = 0;
    
    // Added by Henry Tso. For keeping the state of the Checkbox
    private SafetyBoolean bIsChecked = SafetyBoolean.FALSE;

    /**
     * 
     * @param text
     * @param second
     * @param icon
     * @param listener
     */
    public Button_B16_CB(
            int text, int second, int icon, OnClickListener listener)
    {
        super(text, listener);
        this.mSecondLine = new ButtonText(second);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param second
     * @param icon
     * @param listener
     */
    public Button_B16_CB(
            String text, int second, int icon, OnClickListener listener)
    {
        super(text, listener);
        this.mSecondLine = new ButtonText(second);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param second
     * @param icon
     * @param listener
     */
    public Button_B16_CB(
            int text, String second, int icon, OnClickListener listener)
    {
        super(text, listener);
        this.mSecondLine = new ButtonText(second);
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param second
     * @param icon
     * @param listener
     */
    public Button_B16_CB(
            String text, String second, int icon, OnClickListener listener)
    {
        super(text, listener);
        this.mSecondLine = new ButtonText(second);
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
        return R.layout.button_b16;
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
        return R.id.id_b16_text;
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
        // Don't call super.build() at here to bypress the default action.
        // Implement the own action for checkbox button right here.
        // super.build(group);
        GlobalTools gt = null;

        setButtonText(group, getTextViewId());
        addCheckboxOnClickListener(group, DEFAULT_CLICK_ID, R.id.id_b16_chkb);
        updateClicable(group);

        if (GlobalTools.ReplaceLenghtWithDots)
        {
            TextView tv = (TextView) group.findViewById(getTextViewId());

            tv.setEllipsize(TextUtils.TruncateAt.END);
        }

        UIHelper.setImage(group, R.id.id_b16_img2, mIcon);
        mSecondLine.set(group, R.id.id_b16_text2);
    }

    /**
     * 
     * Function Description
     *
     * @param isSelected
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setSelected(SafetyBoolean isSelected)
    {
        bIsChecked = isSelected;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyBoolean [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyBoolean isSelected()
    {
        return bIsChecked;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// Fix the checkbox issue in Health event selection screen.
