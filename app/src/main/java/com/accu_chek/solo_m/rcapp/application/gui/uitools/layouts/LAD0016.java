/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0016
 * Brief: Provide the function of the LAD0016 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24929 $
 * $Author: AdamChen $
 * $Id: LAD0016.java 24929 2015-11-26 09:01:08Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.content.Context;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalContainer;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0016 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0016;
    
    private static final int CONTAINER = R.id.lad0016_rel_result;
    
    private TextView mResult = null;
    
    private TextView mUnit = null;
    
    private TextView mLowerline = null;
    
    private ImageView mCircle = null;
    
    private ImageView mHyper = null;
    
    private String mBgUnit = null;
    
    private String mBgValue = null;
    
    private Context mContext = null;

    /**
     * 
     * @param activity
     */
    public LAD0016(Activity activity)
    {
        super(activity);
        mResult = (TextView) findViewById(R.id.id_lad0016_text_result);
        mUnit = (TextView) findViewById(R.id.id_lad0016_text_unit);
        mLowerline = (TextView) findViewById(R.id.id_lad0016_text_baseline);
        mCircle = (ImageView) findViewById(R.id.id_lad0016_image);
        mHyper = (ImageView) findViewById(R.id.id_lad0016_hyper);
        // setSymbols();
        mContext = activity;
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

    // modify by Steve {
    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton footerButton)
    {
        return addLowerActionBar(R.id.lad0016_lower_ab, buttonTextId,
                footerButton);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton footerButton)
    {
        return addLowerActionBar(R.id.lad0016_lower_ab, buttonText1Id,
                buttonText2Id, footerButton);
    }
    // } modify by Steve

    // modify by Steve {
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setSymbols()
    {
        GlobalContainer gc = GlobalContainer.getInstance();
        GlobalTools g = GlobalTools.getInstance();
        // if (gc.BGMeasuredResult.length() == 0)
        // return;

        String value = g.setAndCheckValue(mBgValue);
        mResult.setText(value);
        if (g.isHighLow(value) == false)
        {
            mUnit.setText(mBgUnit);

            mCircle.setImageResource(g.getCircleDrawable(mCircle, mBgValue));
            mHyper.setImageDrawable(g.getHyperHypoSymbol(
                    Integer.parseInt(mBgValue), false));
        }
    }
    // } modify by Steve

    /**
     * 
     * Function Description
     *
     * @param SafeValue
     * @param SafeFlag
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setSymbols(SafetyChannel<Integer> SafeValue,
            SafetyChannel<Integer> SafeFlag)
    {
        GlobalTools g = GlobalTools.getInstance();
        // LO flag bit is 2
        final int LO = 4;
        // LO flag bit is 3
        final int HI = 8;

        int HILOFalgPosition = 1000;
        int flag = 0;

        boolean isHI = false;
        boolean isLO = false;
        // get channel1 value
        int encodeBgValueCh1 = 0;
        // get channel2 value
        int encodeBgValueCh2 = 0;
        // get channel1 value
        int encodeBgFlagCh1 = SafeFlag.getValueCH1();
        // get channel2 value
        int encodeBgFlagCh2 = SafeFlag.getValueCH2();

        // get decode channel1
        int decodeBgValueCh1 = 0;
        // get decode channel2
        int decodeBgValueCh2 = 0;
        // compare channel value
        SafeFlag.set(encodeBgFlagCh1, encodeBgFlagCh2);

        flag = CommonUtils.getOriginValue(encodeBgFlagCh1, encodeBgFlagCh2);

        Debug.printI("test", "flag " + flag);
        // flag is 0000, LSB is HI LO flag
        isHI = ((flag % HILOFalgPosition) & HI) == HI;
        isLO = ((flag % HILOFalgPosition) & LO) == LO;
        Debug.printI("test", "isHI " + isHI);
        Debug.printI("test", "isLO " + isLO);
        if (isHI)
        {
            mResult.setText(mContext.getResources().getString(R.string.txt_hi));
        }
        else if (isLO)
        {
            mResult.setText(mContext.getResources().getString(R.string.txt_lo));
        }
        else
        {
            // get channel1 value
            encodeBgValueCh1 = SafeValue.getValueCH1();
            // get channel2 value
            encodeBgValueCh2 = SafeValue.getValueCH2();
            SafeValue.set(encodeBgValueCh1, encodeBgValueCh2);

            // get decode channel1
            decodeBgValueCh1 = CommonUtils.decodeCH1Value(encodeBgValueCh1);
            // get decode channel2
            decodeBgValueCh2 = CommonUtils.decodeCH2Value(encodeBgValueCh2);

            mResult.setText(String.valueOf(decodeBgValueCh1));

            mUnit.setText(mBgUnit);

            mCircle.setImageResource(g.getCircleDrawable(mCircle,
                    decodeBgValueCh2));
            mHyper.setImageDrawable(g.getHyperHypoSymbol(decodeBgValueCh2,
                    false));
        }
    }

    /**
     * 
     * Function Description
     *
     * @param txt
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLowerLine(String txt)
    {
        mLowerline.setText(txt);
    }

    // add by Steve {
    /**
     * 
     * Function Description
     *
     * @param unit
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBgUnit(String unit)
    {
        Debug.printI("test", "0016 bgunit " + unit);
        mBgUnit = unit;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBgValue(String value)
    {
        mBgValue = value;
    }
    // } add by Steve

    /**
     * Gets the id for the button container View
     * 
     * @return The button container View
     */
    protected int getContainer()
    {
        return CONTAINER;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
