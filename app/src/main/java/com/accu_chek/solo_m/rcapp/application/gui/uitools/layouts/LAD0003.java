/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0003
 * Brief: Provide the function of the LAD0003 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0003.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.PickerBase;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

/**
 * Represents a layout definition.
 */
public class LAD0003 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0003;

    /**
     * Constructor. The {@code Activity.setContentView} method is called with
     * the layout id as parameter
     * 
     * @param activity
     *            : The underlying activity
     * 
     */
    public LAD0003(Activity activity)
    {
        super(activity);
    }

    /**
     * Gets the id for the XML layout definition.
     * 
     * @return The layout id
     */
    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

    /**
     * Setups layout
     */
    public void setup(PickerBase picker)
    {
        ViewGroup pickerContainer = findContainerById(R.id.lad003_picker);
        pickerContainer.addView(picker.createView(getActivity(),
                pickerContainer));
    }

    /**
     * Setups default lower action bar: single button with text (set)
     * 
     */
    public void setupLowerActionBar()
    {

        setupLowerActionBar(R.string.txt_set);

    }

    // modify by steve {
    /**
     * Setups lower action bar
     * 
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        View view = addLowerActionBar(R.id.lad003_lower_ab, buttonTextId);

        return view;
    }
    // } modify by steve

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
