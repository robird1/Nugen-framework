/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: PickerDoubleBase
 * Brief: Provide the interface function of the PickerDoubleBase UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 25213 $
 * $Author: AdamChen $
 * $Id: PickerDoubleBase.java 25213 2015-12-01 06:11:39Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import java.util.Locale;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;

import android.os.Bundle;
import android.os.Handler;
import android.view.MotionEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

public class PickerDoubleBase extends PickerBase
{

    private double mValue = 0.0;
    
    private String stringFormat = "%.2f";

    private boolean mAutoIncrement = false;
    
    private boolean mAutoDecrement = false;
    
    private Handler repeatUpdateHandler = new Handler();

    private int mSlowDelay = 250;
    
    private int mFastDelay = 25;
    
    private int mSlowCounter = 2000 / mSlowDelay;
    
    private int mActCount = 0;

    // modify by Steve
    private double mMaxUpper = 10.0;
    
    private double mMaxLower = 0.0;
    
    private double mIncrement = 0.1;

    /**
     * 
     * @param layoutId
     * @param valueId
     * @param value
     */
    public PickerDoubleBase(int layoutId, int valueId, double value)
    {
        this(layoutId, valueId);
        mValue = value;
        mFastDelay = GlobalTools.Picker_Delay;
    }

    /**
     * 
     * @param layoutId
     * @param valueId
     */
    public PickerDoubleBase(int layoutId, int valueId)
    {
        super(layoutId, valueId);
        mFastDelay = GlobalTools.Picker_Delay;
    }

    /**
     * 
     * Function Description
     *
     * @param data
     * @param defaultValue
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public static int getPickerValue(Bundle data, int defaultValue)
    {
        int value = defaultValue;
        boolean containsKey = data.containsKey(PICKER_VALUE_KEY);
        if ((data != null) && (containsKey))
        {
            value = data.getInt(PICKER_VALUE_KEY);
        }
        return value;
    }

    /**
     * 
     * Function Description
     *
     * @param newValue
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateValue(double newValue)
    {
        mValue = newValue;
        refreshValue();
    }

    /**
     * 
     * 
     *
     */
    protected void refreshValue()
    {
        TextView txtValue = (TextView) mView.findViewById(getPickerValueId());
        String format = getStringFormat();
        txtValue.setText(String.format(Locale.getDefault(), format, mValue));
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return double [out] Delete pre line return if exist. Parameter Description
     */
    public double getValue()
    {
        return mValue;
    }

    /**
     * 
     * Function Description
     *
     * @param mValue
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setValue(double mValue)
    {
        this.mValue = mValue;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void add(double value)
    {
        mValue = mValue + value;
        if (mValue >= mMaxUpper)
        {
            mValue = mMaxUpper;
        }
        
        if (mValue <= mMaxLower)
        {
            mValue = mMaxLower;
        }
        
        refreshValue();
    }

    /**
     * @return the stringFormat
     */
    public String getStringFormat()
    {
        return stringFormat;
    }

    /**
     * @param stringFormat the stringFormat to set
     */
    public void setStringFormat(String stringFormat)
    {
        this.stringFormat = stringFormat;
    }

    // modify by Steve {
    /**
     * 
     * Function Description
     *
     * @param mPlus
     * @param mMinus
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setListeners(ImageView mPlus, ImageView mMinus)
    {
        // mPlus.setOnClickListener(new View.OnClickListener()
        // {
        // @Override
        // public void onClick(View v)
        // {
        // add(INCREMENT);
        // }
        // });

        mPlus.setOnLongClickListener(new View.OnLongClickListener()
        {
            public boolean onLongClick(View arg0)
            {
                mAutoIncrement = true;
                repeatUpdateHandler.post(new RptUpdater());
                return false;
            }
        });

        mPlus.setOnTouchListener(new View.OnTouchListener()
        {
            @Override
            public boolean onTouch(View v, MotionEvent event)
            {
                if ((event.getAction() == MotionEvent.ACTION_UP || event
                        .getAction() == MotionEvent.ACTION_CANCEL))
                {
                    mAutoIncrement = false;
                    mActCount = 0;
                    add(mIncrement);
                }
                return false;
            }

        });

        // mMinus.setOnClickListener(new View.OnClickListener()
        // {
        // @Override
        // public void onClick(View v)
        // {
        // add(-INCREMENT);
        // }
        // });

        mMinus.setOnLongClickListener(new View.OnLongClickListener()
        {
            public boolean onLongClick(View arg0)
            {
                mAutoDecrement = true;
                repeatUpdateHandler.post(new RptUpdater());
                return false;
            }
        });

        mMinus.setOnTouchListener(new View.OnTouchListener()
        {
            @Override
            public boolean onTouch(View v, MotionEvent event)
            {
                if ((event.getAction() == MotionEvent.ACTION_UP || event
                        .getAction() == MotionEvent.ACTION_CANCEL))
                {
                    mAutoDecrement = false;
                    mActCount = 0;
                    add(-mIncrement);
                }
                return false;
            }

        });
    }
    // modify by Steve

    // add by Steve {
    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setMax(double value)
    {
        mMaxUpper = value;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setMin(double value)
    {
        mMaxLower = value;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setIncrementValue(double value)
    {
        mIncrement = value;
    }
    // } add by Steve
    
    /**
     * 
     */
    class RptUpdater implements Runnable
    {
        
        /**
         * 
         * 
         *
         */
        @Override
        public void run()
        {
            if (mAutoIncrement)
            {
                add(mIncrement);
                repeatUpdateHandler.postDelayed(new RptUpdater(), mFastDelay);

            }
            else if (mAutoDecrement)
            {
                add(-mIncrement);
                repeatUpdateHandler.postDelayed(new RptUpdater(), mFastDelay);
            }
        }
        
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
