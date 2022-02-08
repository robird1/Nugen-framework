/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker5
 * Brief: Provide the interface function of the Picker5 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Picker5.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Picker5 extends PickerIntBase
{

    static final int PICKER_LAYOUT_ID = R.layout.picker5;
    
    static final int PICKER_UNIT_ID = R.id.picker5_unit;
    
    static final int PICKER_TITLE_ID = R.id.picker5_title;
    
    private int mUnitId = 0;
    
    private int mIncretmentValue = 0;
    
    private OnClickListener mTabRightClick = null;
    
    private OnClickListener mTabLeftClick = null;

    private ImageView mPlus = null;
    
    private ImageView mMinus = null;

    /**
     * Initializes picker with default (percentage) unit
     */
    public Picker5(int value)
    {
        this(value, R.string.txt_percentsign);
    }

    /**
     * 
     * @param value
     * @param unitValueId
     */
    public Picker5(int value, int unitValueId)
    {
        super(PICKER_LAYOUT_ID, PICKER_TITLE_ID, value);
        mUnitId = unitValueId;
    }

    /**
     * Will not be used. For fixing compilation error.
     */
    @Deprecated
    public Picker5(
            int value, int unitValueId, SafetyBoolean isTabActionBarEnabled,
            int leftTabIconId, int rightTabIconId)
    {
        this(value, unitValueId);
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
     * @param value
     */
    public void setIncrementValue(int value)
    {
        mIncretmentValue = value;
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
        mPlus = (ImageView) picker.findViewById(R.id.id_picker5_img2);
        mMinus = (ImageView) picker.findViewById(R.id.id_picker5_img1);
        setListeners(mPlus, mMinus);
        return picker;
    }

    /**
     * 
     * Function Description
     *
     * @param leftHander
     * @param rightHander
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setTabOnClickHandler(OnClickListener leftHander,
            OnClickListener rightHander)
    {
        mTabLeftClick = leftHander;
        mTabRightClick = rightHander;
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

