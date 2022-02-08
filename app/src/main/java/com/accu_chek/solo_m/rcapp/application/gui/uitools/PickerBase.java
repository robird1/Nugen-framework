/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: PickerBase
 * Brief: Provide the interface function of the PickerBase UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: PickerBase.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public abstract class PickerBase
{

    protected final int mPickerLayoutId;
    
    protected final int mPickerValueId;
    
    protected View mView = null;
    
    // Input type defined for number picker user interface
    // Add by Henry Tso
    protected SafetyNumber<Integer> mInputType = null;

    /**
     * Default key for passing values by using bundles.
     */
    public static final String PICKER_VALUE_KEY = "PickerValue";

    /**
     * 
     * @param layoutId
     * @param valueId
     */
    protected PickerBase(int layoutId, int valueId)
    {
        mPickerLayoutId = layoutId;
        mPickerValueId = valueId;
    }

    /**
     * Call this API with given the Input type to define the input style of the
     * Keypad
     * 
     * @param activity [in] Current Activity object
     *            Range: Valid Activity object
     *            Unit: Activity
     *            Scaling: 1
     * @param parent [in] ViewGroup object of the UI parent
     *            Range: Valid ViewGroup object
     *            Unit: ViewGroup
     *            Scaling: 1
     * @param nInputType [in] Input Type (Integer or Float)
     *            Range: Valid SafetyNumber<Integer> object
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1
     * @return View [out] Sub view object
     *         Range: Valid View object
     *         Unit: View
     *         Scaling: 1
     */
    public View createView(Activity activity, ViewGroup parent,
            SafetyNumber<Integer> nInputType)
    {
        mInputType = nInputType;
        mView = activity.getLayoutInflater().inflate(getPickerLayoutId(),
                parent, false);
        refreshValue();
        return mView;
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getPickerLayoutId()
    {
        return mPickerLayoutId;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getPickerValueId()
    {
        return mPickerValueId;
    }

    /**
     * 
     * Function Description
     *
     * @param activity
     * @param parent
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View createView(Activity activity, ViewGroup parent)
    {
        mView = activity.getLayoutInflater().inflate(getPickerLayoutId(),
                parent, false);
        refreshValue();
        return mView;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    protected View getView()
    {
        return mView;
    }

    /**
     * 
     * Function Description
     *
     * @param mView
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setView(View mView)
    {
        this.mView = mView;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected abstract void refreshValue();

    /**
     * Helper method to retrieve integer value from bundle or default value when
     * not available
     * 
     * @param data
     * @param defaultValue
     * @return integer value from bundle with key from
     *         PickerBase.PICKER_VALUE_KEY or default value when not available
     */
    public static int getPickerValue(Bundle data, int defaultValue)
    {
        int value = defaultValue;
        boolean containsKey = data.containsKey(PICKER_VALUE_KEY);
        
        if ((data != null) && containsKey)
        {
            value = data.getInt(PICKER_VALUE_KEY);
        }
        return value;
    }

    /**
     * Helper method to retrieve integer value from bundle or default value when
     * not available
     * 
     * @param data
     * @param defaultValue
     * @return integer value from bundle with key from
     *         PickerBase.PICKER_VALUE_KEY or default value when not available
     */
    public static double getPickerValueDouble(Bundle data, double defaultValue)
    {
        double value = defaultValue;
        if (data != null && data.containsKey(PICKER_VALUE_KEY))
        {
            value = data.getDouble(PICKER_VALUE_KEY);
        }
        return value;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
