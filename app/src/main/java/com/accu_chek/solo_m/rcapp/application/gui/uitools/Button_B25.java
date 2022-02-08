/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B25
 * Brief: Provide the function of the Button_B25 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B25.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.LinearLayout;

public class Button_B25 implements UIBuilder
{
    
    int mDefault = 0;
    
    Double mValueD = null;
    
    String mValueS = null;
    
    int mUnit = 0;
    
    OnClickListener mListener = null;

    /**
     * 
     * 
     *
     * @param group
     */
    @Override
    public void build(ViewGroup group)
    {
        UIHelper.setText(group, R.id.id_b25_text, mDefault);
        if (mValueD != null)
        {
            UIHelper.setText(group, R.id.id_b25_insulin,
                    String.valueOf(mValueD));
        }
        else
        {
            if (mValueS != null)
            {
                UIHelper.setText(group, R.id.id_b25_insulin, mValueS);
            }
        }
        UIHelper.setText(group, R.id.id_b25_value, mUnit);
        if (mListener != null)
        {
            LinearLayout view = (LinearLayout) group
                    .findViewById(R.id.id_button_b25);
            view.setOnClickListener(mListener);
        }
    }

    /**
     * 
     * @param mDefault
     * @param mValueD
     */
    public Button_B25(int mDefault, Double mValueD)
    {
        super();
        this.mDefault = mDefault;
        this.mValueD = mValueD;
        this.mUnit = R.string.txt_insulinunit;
    }

    /**
     * 
     * @param mDefault
     * @param mValueD
     */
    public Button_B25(int mDefault, String mValueD)
    {
        super();
        this.mDefault = mDefault;
        this.mValueS = mValueD;
        this.mUnit = R.string.txt_insulinunit;
    }

    /**
     * 
     * @param mDefault
     * @param mValueD
     * @param mUnit
     */
    public Button_B25(int mDefault, Double mValueD, int mUnit)
    {
        super();
        this.mDefault = mDefault;
        this.mValueD = mValueD;
        this.mUnit = mUnit;
    }

    /**
     * 
     * @param mDefault
     * @param mValueS
     * @param mUnit
     */
    public Button_B25(int mDefault, String mValueS, int mUnit)
    {
        super();
        this.mDefault = mDefault;
        this.mValueS = mValueS;
        this.mUnit = mUnit;
    }

    /**
     * 
     * Function Description
     *
     * @param listener
     * @return
     * @return Button_B25 [out] Delete pre line return if exist. Parameter Description
     */
    public Button_B25 setListener(OnClickListener listener)
    {
        mListener = listener;
        return this;
    }

    /**
     * 
     * 
     *
     * @param group
     * @return
     */
    @Override
    public ViewGroup add(ViewGroup group)
    {
        // TODO Auto-generated method stub
        return null;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
