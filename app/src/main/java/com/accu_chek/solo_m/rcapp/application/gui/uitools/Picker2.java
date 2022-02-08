/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker2
 * Brief: Provide the interface function of the Picker2 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 25213 $
 * $Author: AdamChen $
 * $Id: Picker2.java 25213 2015-12-01 06:11:39Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.os.Handler;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class Picker2 extends PickerIntBase
{

    private static final String VALUE_FORMAT = "%02d";
    
    private static final int PICKER_LAYOUT_ID = R.layout.picker2;
    
    private static final int PICKER_HOUR_ID = R.id.picker2_title_row1;
    
    private static final int PICKER_MINUTE_ID = R.id.picker2_title_row3;
    
    private static final int MAX_MINUTES = 60;
    
    private int mMaxHours = 24;
    
    private int mMinutes = 0;

    private int mSlowDelay = 250;
    
    private int mFastDelay = 25;
    
    private int mSlowCounter = 2000 / mSlowDelay;
    
    private int mActCount = 0;
    
    private int mSelected = 0;

    private Handler mRepeatUpdateHandler = new Handler();

    private boolean mAutoIncrement = false;
    
    private boolean mAutoDecrement = false;

    private ImageView mPlus = null;
    
    private ImageView mMinus = null;
    
    private TextView mHour = null;
    
    private TextView mMin = null;

    /**
     * Initializes picker with default (percentage) unit
     */
    public Picker2(int hours, int minutes)
    {
        super(PICKER_LAYOUT_ID, PICKER_HOUR_ID);
        setHours(hours);
        setMinutes(minutes);
        mFastDelay = GlobalTools.Picker_Delay;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getPickerMinuteViewId()
    {
        return PICKER_MINUTE_ID;
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
        refreshMinute();
        mPlus = (ImageView) picker.findViewById(R.id.id_picker2_img2);
        mMinus = (ImageView) picker.findViewById(R.id.id_picker2_img1);
        mHour = (TextView) picker.findViewById(R.id.picker2_title_row1);
        mMin = (TextView) picker.findViewById(R.id.picker2_title_row3);
        mHour.setSelected(true);
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
        mHour.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                setSelected(view, 0);
            }
        });

        mMin.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                setSelected(view, 1);
            }
        });

        mPlus.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                addToSelected(1);
            }
        });

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
                int action = event.getAction();

                if (((action == MotionEvent.ACTION_UP) || (action == MotionEvent.ACTION_CANCEL))
                        && mAutoIncrement)
                {
                    mAutoIncrement = false;
                    mActCount = 0;
                }
                return false;
            }

        });

        mMinus.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                addToSelected(-1);
            }
        });

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
                int action = event.getAction();

                if (((action == MotionEvent.ACTION_UP) || (action == MotionEvent.ACTION_CANCEL))
                        && mAutoDecrement)
                {
                    mAutoDecrement = false;
                    mActCount = 0;
                }
                return false;
            }

        });
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
    public void addToSelected(int value)
    {
        if (mSelected == 1)
            addToMinutes(value);
        else
            addToHours(value);
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
        return mMinutes = 0;
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
            TextView txtValue = (TextView) view
                    .findViewById(getPickerMinuteViewId());
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
            TextView txtValue = (TextView) view
                    .findViewById(getPickerValueId());
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
            setValue(hours);
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
            this.mMinutes = minutes;
    }
    
    /**
     * @param maxHours
     *            24 or 12
     */
    protected void setMaxHours(int maxHours)
    {
        this.mMaxHours = maxHours;
    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void resetselected()
    {
        mHour.setSelected(false);
        mMin.setSelected(false);
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
        public void run()
        {
            if (mAutoIncrement)
            {
                addToSelected(1);
                mRepeatUpdateHandler.postDelayed(new RptUpdater(), mFastDelay);
        
    }
            else if (mAutoDecrement)
            {
                addToSelected(-1);
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

