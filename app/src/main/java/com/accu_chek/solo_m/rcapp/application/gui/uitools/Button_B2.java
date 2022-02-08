/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B2
 * Brief: Provide the function of the Button_B2 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B2.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.CompoundButton;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B2 extends ButtonBasic
{

    private boolean isRadio = false;

    /**
     * 
     * @param text
     * @param isRadio
     * @param listener
     */
    public Button_B2(int text, boolean isRadio, OnClickListener listener)
    {
        super(text, listener);
        this.isRadio = isRadio;
    }

    /**
     * This constructor is defined for construct the Button_B2.
     * 
     * @param text [in] Text resource id of the button content
     *            Range: Valid Resource ID
     *            Unit: int
     *            Scaling: 1
     * @param isRadio [in] Flag for enabling the Radio Button function
     *            Range: TRUE or FALSE
     *            Unit: Boolean
     *            Scaling: 1
     * @param listener [in] OnClickListener object
     *            Range: Valid OnClickListener object
     *            Unit: OnClickListener
     *            Scaling: 1
     */
    public Button_B2(String text, boolean isRadio, OnClickListener listener)
    {
        super(text, listener);
        this.isRadio = isRadio;
    }

    /**
     * 
     * @param text
     * @param isRadio
     */
    public Button_B2(int text, boolean isRadio)
    {
        super(text);
        this.isRadio = isRadio;
    }

    /**
     * This constructor is defined for construct the Button_B2.
     * 
     * @param text [in] Text of the button content
     *            Range: Valid String object
     *            Unit: String
     *            Scaling: 1
     * @param isRadio [in] Flag for enabling the Radio Button function
     *            Range: TRUE or FALSE
     *            Unit: Boolean
     *            Scaling: 1
     */
    public Button_B2(String text, boolean isRadio)
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
        return isRadio ? R.layout.button_b2_radio : R.layout.button_b2;
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
        return isRadio ? R.id.id_b2_radio_text : R.id.id_b2_text;
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
        return R.id.id_b2_chkb;
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
    }

    /**
     * Call this function for setting the item index.
     * This item index is reserved for UIAutomator usage.
     * Added by Henry Tso
     * 
     * @param nItemIndex [in] Item index of the current button object
     *            Range: 0 ~ n (n is the maximum of the ListView item)
     *            Unit: Integer
     *            Scaling: 1
     * @return None
     */
    public void setItemIndex(int nItemIndex)
    {
        setBaseItemIndex(nItemIndex);
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
