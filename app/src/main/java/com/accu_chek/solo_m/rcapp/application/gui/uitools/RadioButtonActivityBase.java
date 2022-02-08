/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: RadioButtonActivityBase
 * Brief: RadioButtonActivityBase is the activity which handle Radio button
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: RadioButtonActivityBase.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.TableLayout;

import com.accu_chek.solo_m.rcapp.application.gui.buildingblocks.SoloMActionBarActivity;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public abstract class RadioButtonActivityBase extends SoloMActionBarActivity
{

    public static final String RADIO_VALUE_KEY = "RadioValue";

    RadioGroup mRadioGroup = null;
    
    private boolean mIsFirstButton = true;
    
    private boolean mOverwriteListener = false;
    
    private Boolean mDualclicklistener = false;
    
    private int mChkbId = 0;
            
    private int mTlId = 0;

    private View mSelected = null;
    
    // Added by Henry Tso. For more easy getting the current
    // selected item.
    private int mSelectedIndex = 0;
    
    // Add to support UIAutomator
    private int mButtonIndex = 0;

    /**
     * 
     */
    public RadioButtonActivityBase()
    {
        super();
    }

    /**
     * 
     */
    View.OnClickListener radio_handler = new View.OnClickListener()
    {      
        /**
         * 
         * 
         *
         * @param v
         */
        @Override
        public void onClick(View v)
        {
            // Added by Henry Tso. Find out which item is selected
            int nIndex = 0;
            RadioButton btn = (RadioButton) v.findViewById(mChkbId);

            for (int i = 0; i < mRadioGroup.getChildCount(); i++)
            {
                View child = mRadioGroup.getChildAt(i);
                RadioButton bnall = (RadioButton) child.findViewById(mChkbId);

                if (bnall != null)
                {
                    bnall.setChecked(false);
                    // Added by Henry Tso. Find out which item is selected
                    if (bnall.equals(btn))
                    {
                        mSelectedIndex = nIndex;
                    }
                    nIndex++;
                }
            }
            mRadioGroup.clearCheck();
            btn.setChecked(true);
            mSelected = v;
            // Added by Henr Tso. For supporting the UIAutomator function
        }
    };

    // Modified by Henry Tso
    /**
     * Call this API to create the Radio button list with initial selected
     * item index
     * 
     * @param chkbid [in] Radio button resource Id
     *            Range: Valid Resource Id.
     *            Unit: int
     *            Scaling: 1
     * @param tlid [in] Layout resource Id
     *            Range: Valid Resource Id.
     *            Unit: int
     *            Scaling: 1
     * @param index [in] Initial selected item index (Zero base)
     *            Range: Valid SafetyNumber<Integer> object
     *            0 ~ 6
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1
     * @param buttons [in] Button_B13 object list represent the button items
     *            Range: Valid ButtonBasic object
     *            Unit: ButtonBasic
     *            Scaling: 1
     * @return void [out] None
     */
    public void addRadioButtons(int chkbid, int tlid,
            SafetyNumber<Integer> index, ButtonBasic... buttons)
    {
        mChkbId = chkbid;
        mTlId = tlid;
        mSelectedIndex = index.get();
        addButtons(Arrays.asList(buttons));
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(Collection<? extends ButtonBasic> buttons)
    {
        // Added by Henry Tso.
        int nIndex = 0;
        String sIndex = "";
        OnClickListener listener = null;

        for (ButtonBasic button : buttons)
        {
            // TODO: specify bottom margin for table rows (or list items) in
            // layout XML file,
            // remove dynamic spacing.
            if (mIsFirstButton)
                mIsFirstButton = false;
            else
            {
                View space = getLayoutInflater().inflate(
                        R.layout.button_spacing6px, mRadioGroup, false);
                mRadioGroup.addView(space);
            }
            ViewGroup gp = button.createView(this, null, mButtonIndex++);
            mRadioGroup.addView(gp);

            // Added by Henry Tso. Set the initial selected item
            if (nIndex == mSelectedIndex)
            {
                button.setSelected(true);
            }
            nIndex++;

            CompositeOnClickListener groupListener = new CompositeOnClickListener();
            groupListener.addOnClickListener(radio_handler);
            
            listener = button.getListener();
            if (listener != null)
            {
                groupListener.addOnClickListener(listener);
            }

            View white_field = gp.findViewById(R.id.id_button_white_field);
            white_field.setOnClickListener(groupListener);
            // button.setIsClickable(true);

        }
        TableLayout tl = (TableLayout) findViewById(mTlId);
        tl.addView(mRadioGroup);

    }

    /**
     * 
     */
    private class CompositeOnClickListener implements View.OnClickListener
    {
        List<View.OnClickListener> listeners;

        public CompositeOnClickListener()
        {
            listeners = new ArrayList<View.OnClickListener>();
        }

        public void addOnClickListener(View.OnClickListener listener)
        {
            listeners.add(listener);
        }

        @Override
        public void onClick(View v)
        {
            for (View.OnClickListener listener : listeners)
            {
                listener.onClick(v);
            }
        }
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getSelectedButton()
    {
        return mSelected;
    }

    /**
     * 
     * Function Description
     *
     * @param view
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void b2_click(View view)
    {

    }

    /**
     * Call this API for getting the current selected index of the item
     * Added by Henry Tso
     * 
     * @return int [out] Item index
     *         Range: 0 ~ (2^31)-1
     *         Unit: int
     *         Scaling: 1
     */
    public int getSelectedIndex()
    {
        return mSelectedIndex;
    }
    
    /**
     * 
     * 
     *
     * @param savedInstanceState
     */
    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        mRadioGroup = new RadioGroup(this);

    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */

