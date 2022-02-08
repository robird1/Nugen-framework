/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker1
 * Brief: Provide the interface function of the Picker1 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 25064 $
 * $Author: AdamChen $
 * $Id: Picker1.java 25064 2015-11-30 02:13:27Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Picker1 extends PickerIntBase
{

    final static int PICKER_LAYOUT_ID = R.layout.picker1;
    
    final static int PICKER_UNIT_ID = R.id.picker1_unit;
    
    final static int PICKER_TITLE_ID = R.id.picker1_title;
    
    private int mUnitId = 0;

    private ImageView mPlus = null;
    
    private ImageView mMinus = null;

    /**
     * Initializes picker with default (percentage) unit
     * @param value
     */
    public Picker1(int value)
    {
        this(value, R.string.txt_percentsign);
    }

    /**
     * 
     * @param value
     * @param unitValueId
     */
    public Picker1(int value, int unitValueId)
    {
        super(PICKER_LAYOUT_ID, PICKER_TITLE_ID, value);
        mUnitId = unitValueId;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getPickerUnitViewId()
    {
        return PICKER_UNIT_ID;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getPickerUnitValueId()
    {
        return mUnitId;
    }

    /**
     * 
     * 
     *
     * @param activity
     * @param parent
     * @return
     */
    @Override
    public View createView(Activity activity, ViewGroup parent)
    {
        View picker = super.createView(activity, parent);
        TextView textView2 = (TextView) picker
                .findViewById(getPickerUnitViewId());
        textView2.setText(getPickerUnitValueId());
        mPlus = (ImageView) picker.findViewById(R.id.id_picker1_img2);
        mMinus = (ImageView) picker.findViewById(R.id.id_picker1_img1);
        setListeners(mPlus, mMinus);
        
        return picker;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyValue()
    {
        return CommonUtils.getSafetyChannel(getValue());
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyUnitId()
    {
        return CommonUtils.getSafetyChannel(getPickerUnitValueId());
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
