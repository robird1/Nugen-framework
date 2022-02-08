/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker6
 * Brief: Provide the interface function of the Picker6 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Picker6.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

public class Picker6 extends PickerBase
{

    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();

    private static final int FLOAT_FACTOR = 100;

    // Added by Henry Tso
    public static final int INTEGER_TYPE = 0x0F;
    
    public static final int FLOAT_TYPE = 0x33;
    
    public static final int MAX_INTEGER = 999;
    
    public static final float MAX_FLOAT = 99.9f;

    private static final int PICKER_LAYOUT_ID = R.layout.picker6;
    
    private static final int PICKER_VALUE_FIELD = R.id.picker6_edit;
    
    private int mHintId = 0;
    
    private TextView mTv = null;
    
    private View picker = null;
    
    private Context mContext = null;

    private int mMinIntegerValue = 0;
    
    private int mMaxIntegerValue = 0;
    
    private float mMinFloatValue = 0.0f;
    
    private float mMaxFloatValue = 0.0f;
    
    // Added by Henry. For storing the View object that will be disabled.
    private View mViewForDisable = null;

    /**
     * Initializes picker with default (percentage) unit
     */
    public Picker6()
    {
        super(PICKER_LAYOUT_ID, PICKER_VALUE_FIELD);
    }

    /**
     * 
     * @param hint_id
     */
    public Picker6(int hint_id)
    {
        super(PICKER_LAYOUT_ID, PICKER_VALUE_FIELD);
        mHintId = hint_id;
    }

    /**
     * Will not be used. For fix compile error.
     */
    @Deprecated
    public Picker6(
            int hint_id, int tab_left_icon, int tab_right_icon, int active_tab)
    {
        this(hint_id);
    }

    // modify by Henry {
    /**
     * 
     * 
     *
     * @param activity
     * @param parent
     * @param nInputType
     * @return
     */
    @Override
    public View createView(Activity activity, ViewGroup parent,
            SafetyNumber<Integer> nInputType)
    {
        picker = super.createView(activity, parent, nInputType);
        mTv = (TextView) picker.findViewById(PICKER_VALUE_FIELD);
        mContext = activity;
        initOnClickListeners();
        if (mHintId != 0)
        {
            TextView title = (TextView) picker.findViewById(R.id.picker6_edit);
            title.setHint(mHintId);
        }
        return picker;
    }
    // } modify by Henry

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void initOnClickListeners()
    {
        initOnClickListeners(num1, R.id.picker6_num1);
        initOnClickListeners(num2, R.id.picker6_num2);
        initOnClickListeners(num3, R.id.picker6_num3);
        initOnClickListeners(num4, R.id.picker6_num4);
        initOnClickListeners(num5, R.id.picker6_num5);
        initOnClickListeners(num6, R.id.picker6_num6);
        initOnClickListeners(num7, R.id.picker6_num7);
        initOnClickListeners(num8, R.id.picker6_num8);
        initOnClickListeners(num9, R.id.picker6_num9);
        initOnClickListeners(num0, R.id.picker6_num0);
        initOnClickListeners(numPoint, R.id.picker6_numPoint);
        initOnClickListeners(numDel, R.id.picker6_numDel);
    }

    /**
     * 
     * Function Description
     *
     * @param numpicker
     * @param field_id
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void initOnClickListeners(OnClickListener numpicker, int field_id)
    {
        TextView num_txt = (TextView) picker.findViewById(field_id);
        num_txt.setOnClickListener(numpicker);
    }

    /**
     * 
     * Function Description
     *
     * @param num
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setValue(String num)
    {
        mTv.setText(num);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public String getValue()
    {
        return mTv.getText().toString();
    }

    // add by Steve {
    /**
     * Will not be used. For fix compile error.
     */
    @Deprecated
    public void setOnClickHandler(OnClickListener left, OnClickListener right)
    {

    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyString [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyString getSafetyValue()
    {
        SafetyString value = new SafetyString(getValue(),
                CRCTool.generateCRC16(getValue().getBytes()));

        return value;
    }
    // } add by Steve

    // add by Henry Tso {
    /**
     * Call this API for setting the minimum and maximum value can be input.
     * 
     * @param minValue [in] Minimum value, if it is float then divid by 100
     *            Range: Depends on Configuration Matrix and input purpose
     *            Unit: SafetyNumber<Integer> object
     *            Scaling: 1
     * 
     * @param maxValue [in] Maximum value, if it is float then divid by 100
     *            Range: Depends on Configuration Matrix and input purpose
     *            Unit: SafetyNumber<Integer> object
     *            Scaling: 1
     * 
     * @param isFloat [in] TRUE: The input value represented by FIXPOINT
     *            Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *            Unit: SafetyBoolean object
     *            Scaling: 1
     * 
     * @return void [out] None
     */
    public void setMinMaxValue(SafetyNumber<Integer> minValue,
            SafetyNumber<Integer> maxValue, SafetyBoolean isFloat)
    {
        byte byte1 = isFloat.getByte();
        
        if (SAFETY_TRUE == byte1)
        {
            mMinFloatValue = minValue.get() / FLOAT_FACTOR;
            mMaxFloatValue = maxValue.get() / FLOAT_FACTOR;
        }
        else
        {
            mMinIntegerValue = minValue.get();
            mMaxIntegerValue = maxValue.get();
        }
    }

    /**
     * Call this API for setting the View object. This view object will be
     * disabled when the input value is out of range
     * 
     * @param view [in] Target view object to be disabled
     *            Range: U/I view object. Should be the Footer button
     *            Unit: View object
     *            Scaling: 1
     * 
     * @return void [out] None
     */
    public void setDisableView(View view)
    {
        mViewForDisable = view;
    }
    // } add by Henry Tso
    
    /**
     * 
     * 
     *
     */
    @Override
    protected void refreshValue()
    {

        // TODO Auto-generated method stub

    }
    
    // modify by Henry {
    /**
     * 
     * Function Description
     *
     * @param num
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setNum(String num)
    {
        // Get the current value to do filter
        String sValue = (String) mTv.getText();
        int nValue = 0;
        String sFinal = "";

        Integer integer = mInputType.get();
        
        if (INTEGER_TYPE == integer)
        {
            int newValue = 0;

            nValue = Integer.parseInt(sValue + num);
            if ((nValue < mMinIntegerValue) || (nValue > mMaxIntegerValue))
            {
                num = "";
            }
            sFinal = sValue + num;
        }
        else
        // For floating point input
        {
            float fValue = 0;

            boolean contains = sValue.contains(".");
            int compareTo = num.compareTo(".");
            
            if (contains && (compareTo == 0))
            {
                num = "";
            }
            
            fValue = Float.parseFloat(sValue + num);
            if ((fValue < mMinFloatValue) || (fValue > mMaxFloatValue))
            {
                num = "";
            }
            sFinal = sValue + num;
        }
        mTv.setText(sFinal);
    }
    // } modify by Henry
    
    /**
     * 
     */
    private OnClickListener num1 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("1");
        }
    };

    /**
     * 
     */
    private OnClickListener num2 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("2");
        }
    };

    /**
     * 
     */
    private OnClickListener num3 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("3");
        }
    };

    /**
     * 
     */
    private OnClickListener num4 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("4");
        }
    };

    /**
     * 
     */
    private OnClickListener num5 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("5");
        }
    };

    /**
     * 
     */
    private OnClickListener num6 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("6");
        }
    };

    /**
     * 
     */
    private OnClickListener num7 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("7");
        }
    };

    /**
     * 
     */
    private OnClickListener num8 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("8");
        }
    };

    /**
     * 
     */
    private OnClickListener num9 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("9");
        }
    };

    /**
     * 
     */
    private OnClickListener num0 = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            setNum("0");
        }
    };

    /**
     * 
     */
    private OnClickListener numPoint = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            // Modified by Henry Tso. Only support dot in Float
            // input
            Integer integer = mInputType.get();
            
            if (FLOAT_TYPE == integer)
            {
                setNum(".");
            }
        }
    };

    /**
     * 
     */
    private OnClickListener numDel = new OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            CharSequence txt = mTv.getText();
            String str = txt.toString();
            int length = str.length();
            if (length != 0)
            {
                txt = str.substring(0, length - 1);
                mTv.setText(txt);
            }
        }
    };
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [NSIQ-73]: Workaround solution. block the user input the value over the range.
