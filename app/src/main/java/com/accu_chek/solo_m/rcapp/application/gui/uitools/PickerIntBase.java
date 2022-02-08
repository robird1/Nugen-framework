/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: PickerIntBase
 * Brief: Provide the interface function of the PickerIntBase UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 25213 $
 * $Author: AdamChen $
 * $Id: PickerIntBase.java 25213 2015-12-01 06:11:39Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;

import android.os.Handler;
import android.view.MotionEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

public class PickerIntBase extends PickerBase
{

    private int mValue = 0;
    
    private boolean mAutoIncrement = false;
    
    private boolean mAutoDecrement = false;
    
    private Handler mRepeatUpdateHandler = new Handler();

    private int mActCount = 0;
    
    private int mMaxValue = 0;
    
    private int mMinValue = 0;

    private int mMaxUpper = 200;
    
    private int mMaxLower = 0;
    
    private int mIncrement = 1;
    
    private int mFastDelay = 75;

    /**
     * 
     * Function Description
     *
     * @param min
     * @param max
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLimit(int min, int max)
    {
        mMaxUpper = max;
        mMaxLower = min;
    }

    /**
     * 
     * @param layoutId
     * @param valueId
     * @param value
     */
    public PickerIntBase(int layoutId, int valueId, int value)
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
    public PickerIntBase(int layoutId, int valueId)
    {
        super(layoutId, valueId);
        mFastDelay = GlobalTools.Picker_Delay;
    }

    /**
     * 
     * Function Description
     *
     * @param newValue
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateValue(int newValue)
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
        txtValue.setText(Integer.toString(mValue));
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getValue()
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
    protected void setValue(int mValue)
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
    public void add(int value)
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
        // add(1);
        // }
        // });

        mPlus.setOnLongClickListener(new View.OnLongClickListener()
        {
            public boolean onLongClick(View arg0)
            {
                mAutoIncrement = true;
                mRepeatUpdateHandler.post(new RptUpdater());
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
                    mIncrement = 1;
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
        // add(-1);
        // }
        // });

        mMinus.setOnLongClickListener(new View.OnLongClickListener()
        {
            public boolean onLongClick(View arg0)
            {
                mAutoDecrement = true;
                mRepeatUpdateHandler.post(new RptUpdater());
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

    // the following methods add by Steve {
    /**
     * Use add(-value) instead.
     * 
     */
    @Deprecated
    public void minus(int value)
    {
        mValue = mValue - value;
        refreshValue();
    }

    /**
     * Use setLimit(int min, int max) instead.
     * 
     */
    @Deprecated
    public void setMax(int value)
    {
        mMaxValue = value;
    }

    /**
     * Use setLimit(int min, int max) instead.
     * 
     */
    @Deprecated
    public void setMin(int value)
    {
        mMinValue = value;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setIncrementValue(int value)
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
                mRepeatUpdateHandler.postDelayed(new RptUpdater(), mFastDelay);

            }
            else if (mAutoDecrement)
            {
                add(-mIncrement);
                mRepeatUpdateHandler.postDelayed(new RptUpdater(), mFastDelay);
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