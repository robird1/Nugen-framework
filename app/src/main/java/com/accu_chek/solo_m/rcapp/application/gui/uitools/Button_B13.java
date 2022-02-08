/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B13
 * Brief: Provide the function of the Button_B13 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B13.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.CompoundButton;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class Button_B13 extends ButtonBasic
{

    private boolean isRadio = false;
    
    private int mIcon = 0;
    
    // This flag is defined for keeping the status of the check box.
    // Add by Henry Tso
    private SafetyBoolean mIsSelected = SafetyBoolean.FALSE;

    /**
     * 
     * @param text
     * @param icon
     * @param isRadio
     * @param listener
     */
    public Button_B13(
            int text, int icon, boolean isRadio, OnClickListener listener)
    {
        super(text, listener);
        this.isRadio = isRadio;
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param icon
     * @param isRadio
     * @param listener
     */
    public Button_B13(
            String text, int icon, boolean isRadio, OnClickListener listener)
    {
        super(text, listener);
        this.isRadio = isRadio;
        mIcon = icon;
    }

    /**
     * 
     * @param text
     * @param isRadio
     */
    public Button_B13(int text, boolean isRadio)
    {
        super(text);
        this.isRadio = isRadio;
    }

    /**
     * 
     * @param text
     * @param isRadio
     */
    public Button_B13(String text, boolean isRadio)
    {
        super(text);
        this.isRadio = isRadio;
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
        return isRadio ? R.layout.button_b13_radio : R.layout.button_b13;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter Description
     */
    public boolean isRadio()
    {
        return isRadio;
    }

    /**
     * 
     * 
     *
     * @param selected
     */
    public void setSelected(boolean selected)
    {
        ViewGroup container = getView();
        CompoundButton btn = (CompoundButton) container
                .findViewById(getSelectorId());

        btn.setChecked(selected);
        // Add by Henry Tso
        if (selected)
        {
            mIsSelected = SafetyBoolean.TRUE;
        }
        else
        {
            mIsSelected = SafetyBoolean.FALSE;
        }
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter Description
     */
    public boolean isSelected()
    {
        ViewGroup container = getView();
        CompoundButton btn = (CompoundButton) container
                .findViewById(getSelectorId());
        return btn.isChecked();
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
        UIHelper.setImage(group, R.id.id_b13_img2, mIcon);

    }

    /**
     * Call this API for getting the status of the check box
     * Added by Henry Tso
     * 
     * @return SafetyBoolean [out] Check box status
     *         Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean getSelected()
    {
        return mIsSelected;
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
        // TODO, use same id for both layouts
        return isRadio ? R.id.id_b13_radio_text : R.id.id_b13_text;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected int getSelectorId()
    {
        return R.id.id_b13_chkb;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
