/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: PickerTime
 * Brief: Provide the interface function of the PickerTime UI component
 *
 * Create Date: 10/16/2015
 * $Revision: 25213 $
 * $Author: AdamChen $
 * $Id: PickerTime.java 25213 2015-12-01 06:11:39Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

import android.app.Activity;
import android.os.Handler;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

public abstract class PickerTime extends PickerIntBase
{

    private static final String VALUE_FORMAT = "%02d";
    
    private static final int MAX_MINUTES = 60;
    
    private static final int INCREMENT_VALUE = 1;
    
    private int mFastDelay = 75;

    private int mMaxHours = onMaxHour();
    
    private int minutes = 0;
    
    private int mSelected = 0;
    
    private TextView mHourView = null;
    
    private TextView mMinuteView = null;
    
    private ImageView mPlusView = null;
    
    private ImageView mMinusView = null;
    
    private boolean mAutoIncrement = false;
    
    private boolean mAutoDecrement = false;
    
    private Handler mRepeatUpdateHandler = new Handler();
    
    /**
     * 
     * @param layoutId
     * @param hourViewId
     */
    public PickerTime(int layoutId, int hourViewId) 
    {
        super(layoutId, hourViewId);
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

        mPlusView = (ImageView) picker.findViewById(onPlusViewId());
        mMinusView = (ImageView) picker.findViewById(onMinusViewId());
        mHourView = (TextView) picker.findViewById(onHourViewId());
        mMinuteView = (TextView) picker.findViewById(onMinuteViewId());

        mHourView.setSelected(true);

        // refreshValue() has been called in the super class. Therefore, only
        // refreshMinute() will be called here.
        refreshMinute();

        setListeners();

        return picker;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setListeners()
    {
        mPlusView.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                addToSelected(1);
            }
        });

        mPlusView.setOnLongClickListener(new View.OnLongClickListener()
        {
            public boolean onLongClick(View arg0)
            {
                mAutoIncrement = true;
                mRepeatUpdateHandler.post(new RptUpdater());
                return false;
            }
        });

        mMinusView.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                addToSelected(-1);
            }
        });

        mPlusView.setOnTouchListener(new View.OnTouchListener()
        {
            @Override
            public boolean onTouch(View v, MotionEvent event)
            {
                int action = event.getAction();
                if (((action == MotionEvent.ACTION_UP) || (action == MotionEvent.ACTION_CANCEL))
                        && mAutoIncrement)
                {
                    mAutoIncrement = false;
                }
                return false;
            }

        });

        mMinusView.setOnLongClickListener(new View.OnLongClickListener()
        {
            public boolean onLongClick(View arg0)
            {
                mAutoDecrement = true;
                mRepeatUpdateHandler.post(new RptUpdater());
                return false;
            }
        });

        mMinusView.setOnTouchListener(new View.OnTouchListener()
        {
            @Override
            public boolean onTouch(View v, MotionEvent event)
            {
                int action = event.getAction();
                if (((action == MotionEvent.ACTION_UP) || (action == MotionEvent.ACTION_CANCEL))
                        && mAutoDecrement)
                {
                    mAutoDecrement = false;
                }
                return false;
            }

        });

        mHourView.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                setSelected(view, 0);
            }
        });

        mMinuteView.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                setSelected(view, 1);
            }
        });

    }
    
    /**
     * 
     * Function Description
     *
     * @param view
     * @param select
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setSelected(View view, int select)
    {
        resetselected();
        view.setSelected(true);
        mSelected = select;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addToHours(int value)
    {
        setHours(getValue() + value);
        refreshValue();
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addToSelected(int value)
    {
        if (mSelected == 1)
        {
            addToMinutes(value);
        }
        else
        {
            addToHours(value);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addToMinutes(int value)
    {
        setMinutes(getMinutes() + value);
        refreshMinute();
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getHours()
    {
        return getValue();
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getMinutes()
    {
        return minutes;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getMaxHours() 
    {
        return mMaxHours;
    }

    /**
     * @param maxHours
     *            24 or 12
     */
    public void setMaxHours(int maxHours) 
    {
        this.mMaxHours = maxHours;
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyHours()
    {
        return CommonUtils.getSafetyChannel(getHours());
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyMinutes()
    {
        return CommonUtils.getSafetyChannel(getMinutes());
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void refreshMinute()
    {
        View view = getView();
        if (view != null)
        {
            TextView txtValue = (TextView) view.findViewById(onMinuteViewId());
            txtValue.setText(String.format(VALUE_FORMAT, getMinutes()));
        }
    }

    /**
     * 
     * 
     *
     */
    @Override
    protected void refreshValue()
    {
        View view = getView();
        if (view != null)
        {
            TextView txtValue = (TextView) view.findViewById(getPickerValueId());
            txtValue.setText(String.format(VALUE_FORMAT, getValue()));
        }
    }

    /**
     * 
     * Function Description
     *
     * @param hours
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setHours(int hours)
    {
        if (hours >= mMaxHours)
        {
            hours = hours - mMaxHours;
            setHours(hours);
        }
        else if (hours < 0)
        {
            hours = hours + mMaxHours;
            setHours(hours);
        }
        else
        {
            setValue(hours);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param minutes
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setMinutes(int minutes)
    {
        if (minutes >= MAX_MINUTES)
        {
            minutes = minutes - MAX_MINUTES;
            addToHours(1);
            setMinutes(minutes);
        }
        else if (minutes < 0)
        {
            minutes = minutes + MAX_MINUTES;
            addToHours(-1);
            setMinutes(minutes);
        }
        else
        {
            this.minutes = minutes;
        }
    }

    protected abstract int onMaxHour();
    
    protected abstract int onPlusViewId();
    
    protected abstract int onMinusViewId();
    
    protected abstract int onHourViewId();
    
    protected abstract int onMinuteViewId();
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void resetselected() 
    {
        mHourView.setSelected(false);
        mMinuteView.setSelected(false);
    }

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
                addToSelected(INCREMENT_VALUE);
                mRepeatUpdateHandler.postDelayed(new RptUpdater(), mFastDelay);

            }
            else if (mAutoDecrement)
            {
                addToSelected(-INCREMENT_VALUE);
                mRepeatUpdateHandler.postDelayed(new RptUpdater(), mFastDelay);
            }
            else
            {
                // do nothing
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
// [Setting] 1. RCSWSPUI44.8
// 2. GUI code refactoring
// [Reminder] add Reminder module
// [Reminder] add Reminder module
// [Reminder] add Reminder module
