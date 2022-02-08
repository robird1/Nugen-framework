/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B34
 * Brief: Provide the function of the Button_B34 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24958 $
 * $Author: AdamChen $
 * $Id: Button_B34.java 24958 2015-11-27 01:02:55Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;

import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.TextView;

public class Button_B34 extends ButtonBasic
{
    
    String mValue = "42";
    
    int mUnit = R.string.txt_mgdl;
    
    TextView tvResult = null;
    
    TextView tvUnit = null;
    
    ImageView circle = null;
    
    //ImageView hypohyper = null;

    /**
     * 
     * @param mValue
     */
    public Button_B34(String mValue)
    {
        super();
        this.mValue = mValue;
    }

    /**
     * 
     * @param mValue
     * @param listener
     */
    public Button_B34(String mValue, OnClickListener listener)
    {
        super(mValue, listener);
        this.mValue = mValue;
    }

    /**
     * 
     */
    public Button_B34()
    {
        super();

        // for measured Results
        // this.mValue = g.setAndCheckValue(gc.BGMeasuredResult);
    }

//    /**
//     * 
//     * Function Description
//     *
//     * @param value
//     * @return void [out] Delete pre line return if exist. Parameter Description
//     */
//    public void setValue(String value)
//    {
//        this.mValue = value;
//        setColorAndIcons();
//    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return R.layout.button_b34;

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
        tvResult = (TextView) group.findViewById(R.id.id_b34_text_result);
        tvUnit = (TextView) group.findViewById(R.id.id_b34_text_unit);
        circle = (ImageView) group.findViewById(R.id.id_b34_img1);
        //hypohyper = (ImageView) group.findViewById(R.id.id_b34_img2);
        setColorAndIcons(group);

        addOnClickListener(group);
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
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setColorAndIcons(ViewGroup group)
    {
        GlobalTools g = GlobalTools.getInstance();
        g.init(group.getContext());
        tvResult.setText(mValue);
        if (g.isHighLow(mValue))
        {
            tvUnit.setText(null);
            tvUnit.setVisibility(View.GONE);
            //hypohyper.setImageDrawable(null);
            circle.setImageResource(0);
        }
        else
        {
            tvUnit.setText(mUnit);
            //hypohyper.setImageDrawable(g.getHyperHypoSymbol(
            //        Integer.parseInt(mValue), false));
            circle.setImageResource(g.getCircleDrawable(circle, mValue));
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