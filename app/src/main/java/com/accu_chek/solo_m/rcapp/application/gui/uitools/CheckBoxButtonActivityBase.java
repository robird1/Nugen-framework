/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: CheckBoxButtonActivityBase
 * Brief: The CheckBoxButtonActivityBase is the activity which have check box
 * button
 * 
 * Create Date: 12/09/2014
 * $Revision: 24958 $
 * $Author: AdamChen $
 * $Id: CheckBoxButtonActivityBase.java 24958 2015-11-27 01:02:55Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import java.util.Arrays;
import java.util.Collection;

import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.TableLayout;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.gui.buildingblocks.SoloMActionBarActivity;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public abstract class CheckBoxButtonActivityBase extends SoloMActionBarActivity
{

    private static final byte SAFETY_BOOLEAN_TRUE = SafetyBoolean.TRUE.getByte();

    // Default max selected item
    private static final int DEFAULT_MAX = 100;
    
    TableLayout mTableLayout = null;
   
    private boolean isFirstButton = true;
    
    private int chkb_id, tl_id;

    // Set the maximum selected item to 100 as default. (No limit)
    private int mMaxSelectItem = DEFAULT_MAX;

    // Counter of the selected item.
    private int mTotalSelected = 0;
    
    // Defined for keeping the button list. Used in onClick event
    // for calculating which items is selected
    SafetyBoolean[] mIsChecked = null;
    
    // Add to support UIAutomator
    private int mButtonIndex = 0;

    /**
     * 
     */
    public CheckBoxButtonActivityBase()
    {
        super();

    }

    /**
     * 
     */
    View.OnClickListener cb_handler = new View.OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            // Added by Henry Tso.
            int nIndex = 0;
            CheckBox chkb = (CheckBox) v.findViewById(chkb_id);
            View child = null;
            CheckBox checkBox = null;
            boolean equals = false;
            boolean checked = false;
            boolean checked2 = false;
            int childCount = mTableLayout.getChildCount();

            // Add by Henry Tso.
            for (int i = 0; i < childCount; i++)
            {
                child = mTableLayout.getChildAt(i);
                checkBox = (CheckBox) child.findViewById(chkb_id);
                if (checkBox != null)
                {
                    equals = checkBox.equals(chkb);
                    
                    if (equals)
                    {
                        checked = checkBox.isChecked();
                        
                        if (checked)
                        {
                            mIsChecked[nIndex] = SafetyBoolean.FALSE;
                        }
                        else
                        {
                            mIsChecked[nIndex] = SafetyBoolean.TRUE;
                        }
                        break;
                    }
                    nIndex++;
                }
            }

            checked2 = chkb.isChecked();
            
            if (checked2)
            {
                chkb.setChecked(false);
                // Modified by Henry Tso. Add select limit.
                mTotalSelected--;
            }
            // Modified by Henry Tso. Add select limit.
            else
            {
                if (mMaxSelectItem > mTotalSelected)
                {
                    chkb.setChecked(true);
                    mTotalSelected++;
                }
                else
                {
                    chkb.setChecked(false);
                }
            }
        }
    };

    /**
     * 
     */
    View.OnClickListener cbicon_handler = new View.OnClickListener()
    {
        @Override
        public void onClick(View v)
        {
            int nIndex = 0;
            CheckBox checkBox = null;
            boolean equals = false;
            boolean checked = false;

            int childCount = mTableLayout.getChildCount();
            
            for (int i = 0; i < childCount; i++)
            {
                View child = mTableLayout.getChildAt(i);
                checkBox = (CheckBox) child.findViewById(chkb_id);
                
                if (checkBox != null)
                {
                    equals = checkBox.equals(v);
                    
                    if (equals)
                    {
                        checked = checkBox.isChecked();
                        
                        if (checked)
                        {
                            mIsChecked[nIndex] = SafetyBoolean.TRUE;
                        }
                        else
                        {
                            mIsChecked[nIndex] = SafetyBoolean.FALSE;
                        }
                        break;
                    }
                    nIndex++;
                }
            }

            if (!checked)
            {
                mTotalSelected--;
            }
            // Modified by Henry Tso. Add select limit.
            else
            {
                if (mMaxSelectItem > mTotalSelected)
                {
                    mTotalSelected++;
                }
                else
                // More than the maximum items then set uncheck back
                {
                    checkBox.setChecked(false);
                }
            }
        }
    };

    /**
     * 
     * Function Description
     *
     * @param chkbid
     * @param tlid
     * @param buttons
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addCBButtons(int chkbid, int tlid, ButtonBasic... buttons)
    {
        chkb_id = chkbid;
        tl_id = tlid;
        // Added by Henry Tso
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
        // Added by Henry Tso
        int nCount = 0;
        int nIndex = 0;
        CheckBox chkb = null;
        Button_B16_CB checkboxButton = null;
        byte isSelected = SafetyBoolean.FALSE.getByte();
        // Modified by Henry Tso
        mTableLayout = (TableLayout) findViewById(tl_id);
        for (ButtonBasic button : buttons)
        {
            // TODO: specify bottom margin for table rows (or list items) in
            // layout XML file,
            // remove dynamic spacing.
            if (isFirstButton)
            {
                isFirstButton = false;
            }
            else
            {
                View space = getLayoutInflater().inflate(
                        R.layout.button_spacing6px, null, false);
                mTableLayout.addView(space);
            }
            button.setListener(cb_handler);

            // Added by Henry Tso.
            button.setCheckBoxIconListener(cbicon_handler);

            ViewGroup gp = button.createView(this, null, mButtonIndex++);
            // Added by Henry Tso
            chkb = (CheckBox) gp.findViewById(chkb_id);
            checkboxButton = (Button_B16_CB) button;
            isSelected = checkboxButton.isSelected().getByte();
            if (SAFETY_BOOLEAN_TRUE == isSelected)
            {
                chkb.setChecked(true);
            }
            mTableLayout.addView(gp);
            // Added by Henry Tso
            // Calcaulate the total item of the checkbox list
            nCount++;
        }
        // Added by Henry Tso
        nIndex = 0;
        mIsChecked = new SafetyBoolean[nCount];
        for (ButtonBasic button : buttons)
        {
            checkboxButton = (Button_B16_CB) button;
            mIsChecked[nIndex++] = checkboxButton.isSelected();
        }

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

    }
    
    /**
     * Call this API to set up the maximum item can be selected.
     * 
     * @param nMax [in] Maximum of the selected item
     *            Range: Valid SafetyNumber<Integer> object
     *            0 ~ 100
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1
     */
    public void setMaxSelectedItem(SafetyNumber<Integer> nMax)
    {
        Integer integer = nMax.get();
        
        if ((integer < 0) || (integer > DEFAULT_MAX))
        {
            mMaxSelectItem = DEFAULT_MAX;
        }
        else
        {
            mMaxSelectItem = integer;
        }
    }

    /**
     * Call this API to set up the current Activity is allow how
     * many items can be selected.
     * 
     * @param nMax [in] Total allowed item for the selection.
     *            Range: Valid SafetyNumber<Integer> object
     *            0 ~ 100
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1
     */
    public void setTotalSelectedItem(SafetyNumber<Integer> nTotal)
    {
        Integer integer = nTotal.get();
        
        if ((integer < 0) || (integer > mMaxSelectItem))
        {
            mTotalSelected = 0;
        }
        else
        {
            mTotalSelected = integer;
        }
    }

    /**
     * Call this API to get the list of item check status.
     * 
     * @return SafetyBoolean[] [out] Array of check status.
     *         Range: Valid SafetyBoolean array object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean[] getSelectedState()
    {
        return mIsChecked;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// Fix the checkbox issue in Health event selection screen.
