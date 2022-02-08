/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker7
 * Brief: Provide the interface function of the Picker7 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Picker7.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

public class Picker7 extends PickerBase
{

    private static final int PICKER_LAYOUT_ID = R.layout.picker7;
    
    private static final int PICKER_VALUE_FIELD = R.id.picker7_edit;
    
    private int mTitleId = 0;
    
    private TextView mTv = null;
    
    private View mPicker = null;

    /**
     * Initializes picker with default (percentage) unit
     */
    public Picker7()
    {
        super(PICKER_LAYOUT_ID, PICKER_VALUE_FIELD);
    }

    /**
     * 
     * @param title_id
     */
    public Picker7(int title_id)
    {
        super(PICKER_LAYOUT_ID, PICKER_VALUE_FIELD);
        mTitleId = title_id;
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
        mPicker = super.createView(activity, parent);
        mTv = (TextView) mPicker.findViewById(PICKER_VALUE_FIELD);
        initOnClickListeners();
        if (mTitleId != 0)
        {
            TextView title = (TextView) mPicker.findViewById(R.id.picker7_title);
            title.setText(mTitleId);
        }
        return mPicker;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void initOnClickListeners()
    {
        // All modified by Henry Tso to support UIAutomator
        initOnClickListeners(num1, R.id.picker7_num1, CommonConstants.INDEX_01);
        initOnClickListeners(num2, R.id.picker7_num2, CommonConstants.INDEX_02);
        initOnClickListeners(num3, R.id.picker7_num3, CommonConstants.INDEX_03);
        initOnClickListeners(num4, R.id.picker7_num4, CommonConstants.INDEX_04);
        initOnClickListeners(num5, R.id.picker7_num5, CommonConstants.INDEX_05);
        initOnClickListeners(num6, R.id.picker7_num6, CommonConstants.INDEX_06);
        initOnClickListeners(num7, R.id.picker7_num7, CommonConstants.INDEX_07);
        initOnClickListeners(num8, R.id.picker7_num8, CommonConstants.INDEX_08);
        initOnClickListeners(num9, R.id.picker7_num9, CommonConstants.INDEX_09);
        initOnClickListeners(num0, R.id.picker7_num0, CommonConstants.INDEX_00);
        initOnClickListeners(numPoint, R.id.picker7_numPoint,
                CommonConstants.INDEX_10);
        initOnClickListeners(numDel, R.id.picker7_numDel,
                CommonConstants.INDEX_11);
    }

    /**
     * 
     * Function Description
     *
     * @param numpicker
     * @param field_id
     * @param buttonIndex
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void initOnClickListeners(OnClickListener numpicker, int field_id,
            int buttonIndex)
    {
        // Add by Henry Tso for support UIAutomator
        String index = "";
        TextView num_txt = (TextView) mPicker.findViewById(field_id);
        num_txt.setOnClickListener(numpicker);
    }

    /**
     * 
     * Function Description
     *
     * @param num
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setNum(String num)
    {
        mTv.setText(mTv.getText() + num);
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
    
    /**
     * 
     * 
     *
     */
    @Override
    protected void refreshValue()
    {

        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }
    
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

    /*
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
            setNum(".");
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

