/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0023
 * Brief: Provide the function of the LAD0023 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: LAD0023.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalContainer;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B24;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B25;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B26;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.StringHelper;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0023 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0023;
    
    Button_B26 mBGInsulin = null;
    
    Button_B26 mCarbsInsuin = null;
    
    String mBgValue = null;
    
    String mActiveInsulin = null;
    
    int mCarbsHeadline = 0;
    
    String mCarbsValue = null;
    
    String mInstruction = null;
    
    Button_B25 mInsulinToltal = null;
    
    Button_B24 mInsulinType = null;

    /**
     * 
     * @param activity
     */
    public LAD0023(Activity activity)
    {
        super(activity);
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

    /**
     * 
     * Function Description
     *
     * @param mBGInsulin
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setBGInsulin(Button_B26 mBGInsulin)
    {
        this.mBGInsulin = mBGInsulin;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param mCarbsInsuin
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setCarbsInsuin(Button_B26 mCarbsInsuin)
    {
        this.mCarbsInsuin = mCarbsInsuin;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param mBgValue
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setBgValue(String mBgValue)
    {
        this.mBgValue = mBgValue;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param bgValue
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setBgValue(double bgValue)
    {
        this.mBgValue = StringHelper.formatMgDl((int) bgValue, getActivity()
                .getApplicationContext());
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param activeInsulin
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setActiveInsulin(double activeInsulin)
    {
        this.mActiveInsulin = StringHelper.formatInsulinUnit(activeInsulin,
                getActivity().getApplicationContext());
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param mActiveInsulin
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setActiveInsulin(String mActiveInsulin)
    {
        this.mActiveInsulin = mActiveInsulin;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param mCarbsHeadline
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setCarbsHeadline(int mCarbsHeadline)
    {
        this.mCarbsHeadline = mCarbsHeadline;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param mCarbsValue
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setCarbsValue(String mCarbsValue)
    {
        this.mCarbsValue = mCarbsValue;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param carbsValue
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setCarbsValue(double carbsValue)
    {
        // todo: add switch between different carb units
        this.mCarbsValue = StringHelper.formatCarbsGram(carbsValue,
                getActivity().getApplicationContext());
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param mInsulinToltal
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setInsulinToltal(Button_B25 mInsulinToltal)
    {
        this.mInsulinToltal = mInsulinToltal;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param mInsulinType
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setInsulinType(Button_B24 mInsulinType)
    {
        this.mInsulinType = mInsulinType;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param instruction
     * @return
     * @return LAD0023 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0023 setInstructionTxt(String instruction)
    {
        this.mInstruction = instruction;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setup()
    {
        ViewGroup group1 = findContainerById(R.id.id_lad0023);
        UIHelper.setText(group1, R.id.id_lad0023_bg_value, mBgValue);
        UIHelper.setText(group1, R.id.id_lad0023_active_insulin, mActiveInsulin);
        UIHelper.setText(group1, R.id.id_lad0023_carbs_title, mCarbsHeadline);
        UIHelper.setText(group1, R.id.id_lad0023_carbs_value, mCarbsValue);
        UIHelper.setText(group1, R.id.instruction_text, mInstruction);

        GlobalTools g = GlobalTools.getInstance();
        GlobalContainer gc = GlobalContainer.getInstance();
        // Added by Henry Tso. To support UIAutomator
        ImageView imageView = (ImageView) group1
                .findViewById(R.id.id_lad0023_circle_icon);
        UIHelper.setImage(group1, R.id.id_lad0023_circle_icon,
                g.getCircleDrawable(imageView, GlobalContainer.BGMeasuredResult));

        mBGInsulin.build((ViewGroup) group1
                .findViewById(R.id.id_lad0023_insulin_value_bg));
        mCarbsInsuin.build((ViewGroup) group1
                .findViewById(R.id.id_lad0023_insulin_value_carbs));
        mInsulinToltal.build((ViewGroup) group1
                .findViewById(R.id.id_lad0023_total_bolus));
        mInsulinType.build((ViewGroup) group1
                .findViewById(R.id.id_lad0023_bolus_type));

    }

    /**
     * Setup layout using default parameters for the action bar (two buttons:
     * yes/no).
     * 
     * @return
     */
    public View setupLowerActionBar()
    {
        return setupLowerActionBar(R.string.txt_no, R.string.txt_yes);
    }

    /**
     * Setup layout using default parameters for the action bar (two buttons:
     * yes/no).
     * 
     * @return
     */
    public View setupLowerActionBar(int btn1TextId)
    {
        return addLowerActionBar(R.id.lad0023_lower_ab, btn1TextId);
    }

    public View setupLowerActionBar(int btn1TextId, int btn2TextId)
    {
        return addLowerActionBar(R.id.lad0023_lower_ab, btn1TextId, btn2TextId);
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
