/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Picker4
 * Brief: Provide the interface function of the Picker4 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 25213 $
 * $Author: AdamChen $
 * $Id: Picker4.java 25213 2015-12-01 06:11:39Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

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

public class Picker4 extends PickerIntBase
{

    private static final int PICKER_LAYOUT_ID = R.layout.picker4;
    
    private static final int PICKER_DAY_ID = R.id.picker4_title_row1;
    
    private static final int PICKER_MONTH_ID = R.id.picker4_title_row2;
    
    private static final int PICKER_YEAR_ID = R.id.picker4_title_row3;
    
    private static final int[] PICKER_MONTHS_AR = { 0, R.string.txt_mon01,
            R.string.txt_mon02, R.string.txt_mon03, R.string.txt_mon04,
            R.string.txt_mon05, R.string.txt_mon06, R.string.txt_mon07,
            R.string.txt_mon08, R.string.txt_mon09, R.string.txt_mon10,
            R.string.txt_mon11, R.string.txt_mon12 };

    private int maxDays = 31;
    
    private int maxMonths = 12;
    
    private int mMonth, mYear, mDay;
    
    private int mSelected = 0;

    private int mMaxYear = 2150;
     
    private int mMinYear = 2014;

    private int mSlowDelay = 250;
    
    private int mFastDelay = 25;
    
    private int mSlowCount = 2000 / mSlowDelay;
    
    private int mActCount = 0;

    private Handler mRepeatUpdateHandler = new Handler();

    private boolean mAutoIncrement = false;
    
    private boolean mAutoDecrement = false;

    private ImageView mPlus = null;
    
    private ImageView mMinus = null;

    private TextView mDayView = null;
    
    private TextView mMonthView = null;
    
    private TextView mYearView = null;

    /**
     * Initializes picker with default (percentage) unit
     */
    public Picker4(int day, int month, int year)
    {
        super(PICKER_LAYOUT_ID, PICKER_DAY_ID);
        setDay(day);
        setMonth(month);
        setYear(year);
        mFastDelay = GlobalTools.Picker_Delay;
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
        refreshMonth();
        refreshYear();

        mPlus = (ImageView) picker.findViewById(R.id.id_picker4_img2);
        mMinus = (ImageView) picker.findViewById(R.id.id_picker4_img1);
        mDayView = (TextView) picker.findViewById(R.id.picker4_title_row1);
        mMonthView = (TextView) picker.findViewById(R.id.picker4_title_row2);
        mYearView = (TextView) picker.findViewById(R.id.picker4_title_row3);
        resetselected();
        mDayView.setSelected(true);
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

        mDayView.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                setSelected(view, 0);
            }
        });

        mMonthView.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                setSelected(view, 1);
            }
        });

        mYearView.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                setSelected(view, 2);
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
    public void addToDay(int value)
    {
        Boolean checkDate = false;
        mDay = mDay + value;
        if (mDay > maxDays)
        {
            mDay = 0;
        }
        
        if (mDay < 0)
        {
            mDay = maxDays;
        }
        
        checkDate = checkDate();
        if (checkDate == false)
        {
            addToDay(value);
        }

        setDay(mDay);
        refreshValue();
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
    public void addToMonth(int value)
    {
        Boolean checkDate = false;
        setMonth(mMonth + value);
        refreshMonth();
        
        checkDate = checkDate();
        
        if (checkDate == false)
        {
            addToDay(-1);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addToYear(int value)
    {
        Boolean checkDate = false;
        
        mYear = mYear + value;
        
        if (mYear >= mMaxYear)
        {
            mYear = mMaxYear;
        }
        else if (mYear <= mMinYear)
        {
            mYear = mMinYear;
        }
        
        setYear(mYear);
        refreshYear();
        checkDate = checkDate();
        if (checkDate == false)
        {
            addToDay(-1);
        }
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
        if (mSelected == 0)
            addToDay(value);
        else if (mSelected == 1)
            addToMonth(value);
        else
            addToYear(value);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getDay()
    {
        return mDay;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getMonth()
    {
        return mMonth;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getYear()
    {
        return mYear;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getMonthStringId()
    {
        return PICKER_MONTHS_AR[mMonth];
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getMaxYear()
    {
        return mMaxYear;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getMinYear()
    {
        return mMinYear;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyDay()
    {
        return CommonUtils.getSafetyChannel(getDay());
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyMonth()
    {
        return CommonUtils.getSafetyChannel(getMonth());
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyMonthStringId()
    {
        return CommonUtils.getSafetyChannel(getMonthStringId());
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return SafetyChannel<Integer> [out] Delete pre line return if exist. Parameter Description
     */
    public SafetyChannel<Integer> getSafetyYear()
    {
        return CommonUtils.getSafetyChannel(getYear());
    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void refreshMonth()
    {
        View view = getView();
        if (view != null)
        {
            // Modified by Henry Tso. To support UIAutomator
            UIHelper.setText(view, PICKER_MONTH_ID, PICKER_MONTHS_AR[mMonth],
                    "month");
        }
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void refreshYear()
    {
        View view = getView();
        if (view != null)
        {
            UIHelper.setText(view, PICKER_YEAR_ID, String.valueOf(mYear));
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

            UIHelper.setText(view, PICKER_DAY_ID, String.valueOf(mDay));

        }
    }
    
    /**
     * 
     * Function Description
     *
     * @param day
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setDay(int day)
    {
        if (day > maxDays)
        {
            day = day - maxDays;
            setDay(day);
        }
        else if (day <= 0)
        {
            day = maxDays;
            setDay(day);
        }
        else
            mDay = day;

        setValue(day);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return Boolean [out] Delete pre line return if exist. Parameter Description
     */
    protected Boolean checkDate()
    {

        // String tmpTagesdatum = "02.02.2008";
        String tmpTagesdatum = String.format("%02d.%02d.%04d", mDay, mMonth,
                mYear);

        // String newTagesdatum = tmpTagesdatum + " 11:11:11";

        try
        {
            SimpleDateFormat sdfToDate = new SimpleDateFormat("dd.MM.yyyy");
            sdfToDate.setLenient(false);
            Date date1 = sdfToDate.parse(tmpTagesdatum);
            return true;
        }
        catch (ParseException ex2)
        {
            ex2.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
        return false;
    }

    /**
     * 
     * Function Description
     *
     * @param month
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setMonth(int month)
    {
        if (month > maxMonths)
        {
            month = month - maxMonths;
            setMonth(month);
        }
        else if (month <= 0)
        {
            month = month + maxMonths;
            setMonth(month);
        }
        else
            mMonth = month;
        setValue(month);
    }

    /**
     * 
     * Function Description
     *
     * @param year
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setYear(int year)
    {
        mYear = year;
        setValue(year);
    }
    
    /**
     * 
     * Function Description
     *
     * @param day
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setMaxDay(int day)
    {
        maxDays = day;
    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void resetselected()
    {
        mDayView.setSelected(false);
        mMonthView.setSelected(false);
        mYearView.setSelected(false);
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

