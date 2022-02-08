/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B19
 * Brief: Provide the function of the Button_B19 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: Button_B19.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalContainer;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B19 extends ButtonBasic
{
    
    // Modified by Henry Tso. Add initial value
    String mValue = "";
    
    // Modified by Henry Tso. Add initial value
    String mDate = "";
    
    // Modified by Henry Tso.
    String mUnit = "";

    SafetyChannel<Integer> mBgResultValue = null;

    SafetyChannel<Integer> mBgResultFlag = null;

    /**
     * 
     * @param mValue
     */
    public Button_B19(String mValue)
    {
        super();
        this.mValue = mValue;
    }

    /*
     * public Button_B19(String mValue, String Date, String sUnit)
     * {
     * super();
     * GlobalTools g = GlobalTools.getInstance();
     * this.mValue = g.setAndCheckValue(mValue);
     * this.mDate = Date;
     * // Added by Henry Tso
     * this.mUnit = sUnit;
     * 
     * mBgResultFlag = new SafetyChannel<Integer>(
     * CommonUtils.encodeCH1Value(0), CommonUtils.encodeCH2Value(0));
     * 
     * }
     */
    public Button_B19(
            SafetyChannel<Integer> bgResultValue,
            SafetyChannel<Integer> bgResultFlag, String Date, String sUnit)
    {
        mBgResultValue = bgResultValue;
        mBgResultFlag = bgResultFlag;
        this.mDate = Date;
        this.mUnit = sUnit;
    }

    /**
     * 
     * @param bgResultValue
     * @param bgResultFlag
     * @param Date
     * @param sUnit
     */
    public Button_B19(
            SafetyChannel<Integer> bgResultValue, String Date, String sUnit)
    {
        mBgResultValue = bgResultValue;

        this.mDate = Date;
        this.mUnit = sUnit;
    }

    /**
     * 
     */
    public Button_B19()
    {
        super();
//        GlobalContainer gc = GlobalContainer.getInstance();
//        GlobalTools g = GlobalTools.getInstance();
//        this.mValue = g.setAndCheckValue(gc.BGMeasuredResult);
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
        return R.layout.button_b19;

    }

    /**
     * 
     * 
     *
     * @param group
     */
    @Override
    public void build(ViewGroup group)
    {
        TextView tvResult = (TextView) group
                .findViewById(R.id.id_b19_text_result);
        TextView tvDate = (TextView) group.findViewById(R.id.id_b19_text_below);
        TextView tvUnit = (TextView) group.findViewById(R.id.id_b19_text_unit);
        ImageView circle = (ImageView) group.findViewById(R.id.id_b19_img1);
        ImageView hypohyper = (ImageView) group.findViewById(R.id.id_b19_img2);

        GlobalContainer gc = GlobalContainer.getInstance();
        GlobalTools g = GlobalTools.getInstance();
        g.init(group.getContext());
        this.mValue = g.setAndCheckValue(GlobalContainer.BGMeasuredResult);
        tvResult.setText(mValue);
        circle.setVisibility(View.VISIBLE);
        hypohyper.setVisibility(View.VISIBLE);
        // Marked by Henry Tso
        // if (mDate == null) {
        // tvDate.setText(g.getDateFormatted());
        // } else {
        tvDate.setText(mDate);
        // }

        if (g.isHighLow(mValue))
        {
            tvUnit.setText(null);
            hypohyper.setImageDrawable(null);
            circle.setVisibility(View.GONE);
            hypohyper.setVisibility(View.GONE);
        }
        else
        {
            tvUnit.setText(mUnit);
            hypohyper.setImageDrawable(g.getHyperHypoSymbol(
                    Integer.parseInt(mValue), false));
            // Modified by Henry Tso. To support UIAutomator
            circle.setImageResource(g.getCircleDrawable(circle, mValue));
        }
    }

    /**
     * 
     * 
     *
     * @param group
     */
    @Override
    public void buildDetailBgResult(ViewGroup group)
    {
        TextView tvResult = (TextView) group
                .findViewById(R.id.id_b19_text_result);
        TextView tvDate = (TextView) group.findViewById(R.id.id_b19_text_below);
        TextView tvUnit = (TextView) group.findViewById(R.id.id_b19_text_unit);
        ImageView circle = (ImageView) group.findViewById(R.id.id_b19_img1);
        ImageView hypohyper = (ImageView) group.findViewById(R.id.id_b19_img2);

        GlobalContainer gc = GlobalContainer.getInstance();
        GlobalTools g = GlobalTools.getInstance();
        g.init(group.getContext());
        this.mValue = g.setAndCheckValue(GlobalContainer.BGMeasuredResult);
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
        int encodeBgFlagCh1 = 0;
        // get channel2 value
        int encodeBgFlagCh2 = 0;

        // get decode channel1
        int decodeBgValueCh1 = 0;
        // get decode channel2
        int decodeBgValueCh2 = 0;

        // get flag
        if (this.mBgResultFlag != null)
        {
            encodeBgFlagCh1 = this.mBgResultFlag.getValueCH1();
            encodeBgFlagCh2 = this.mBgResultFlag.getValueCH2();
            this.mBgResultFlag.set(encodeBgFlagCh1, encodeBgFlagCh2);

            flag = CommonUtils.getOriginValue(encodeBgFlagCh1, encodeBgFlagCh2);

            // flag is 0000, LSB is HI LO flag
            isHI = ((flag % HILOFalgPosition) & HI) == HI;
            isLO = ((flag % HILOFalgPosition) & LO) == LO;

        }

        tvDate.setText(mDate);
        if (isHI)
        {
            tvResult.setText(group.getContext().getResources()
                    .getString(R.string.txt_hi));
            tvUnit.setText(null);
        }
        else if (isLO)
        {
            tvResult.setText(group.getContext().getResources()
                    .getString(R.string.txt_lo));
            tvUnit.setText(null);
        }
        else
        {
            // get channel1 value
            encodeBgValueCh1 = this.mBgResultValue.getValueCH1();
            // get channel2 value
            encodeBgValueCh2 = this.mBgResultValue.getValueCH2();
            this.mBgResultValue.set(encodeBgValueCh1, encodeBgValueCh2);

            // get decode channel1
            decodeBgValueCh1 = CommonUtils.decodeCH1Value(encodeBgValueCh1);
            // get decode channel2
            decodeBgValueCh2 = CommonUtils.decodeCH2Value(encodeBgValueCh2);

            tvResult.setText(String.valueOf(decodeBgValueCh1));

            tvUnit.setText(mUnit);

            circle.setImageResource(g.getCircleDrawable(circle,
                    decodeBgValueCh2));
            hypohyper.setImageDrawable(g.getHyperHypoSymbol(decodeBgValueCh2,
                    false));
        }

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
        return group;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */// (R24175 2015-11-16 05:33:09 AdamChen)
// ----------------------------------------------------------------------------
// add Button_B19() constructor.
