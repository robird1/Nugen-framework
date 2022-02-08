/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0SS2
 * Brief: Provide the function of the LAD0SS2 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0SS2.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0SS2 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0ss2;
    
    private static final int LAYOUT_AB = R.id.lad0ss2_ab;

    /**
     * Violtae coding rule: R16
     */
    public StatusBG m_stbg;
    
    /**
     * Violtae coding rule: R16
     */
    public StatusReservoir m_strv;

    /**
     * 
     * @param activity
     */
    public LAD0SS2(Activity activity)
    {
        super(activity);
        ViewGroup status_bg = (ViewGroup) findViewById(R.id.lad0ss2_status_bg);
        m_stbg = new StatusBG(status_bg);
        ViewGroup res_vg = (ViewGroup) findViewById(R.id.lad0ss2_status_res);
        m_strv = new StatusReservoir(res_vg);
        insertPumpStop();

    }

    /**
     * 
     * Function Description
     *
     * @param bg_id
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBGColor(int bg_id)
    {
        ViewGroup buttonContainer = (ViewGroup) findViewById(R.id.lad0ss2_rel_all);
        buttonContainer.setBackgroundColor(bg_id);

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
     * @param color
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBackgroundResource(int color)
    {
        View rel_all = (View) findViewById(R.id.lad0ss2_rel_all);
        rel_all.setBackgroundResource(color);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id)
    {
        return addLowerActionBar(LAYOUT_AB, buttonText1Id, buttonText2Id);
    }

    /**
     * 
     * Function Description
     *
     * @param first
     * @param second
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLowerClickability(Boolean first, Boolean second)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(LAYOUT_AB);
        View btn1 = (View) ab.findViewById(R.id.btn1);
        btn1.setClickable(first);
        View btn2 = (View) ab.findViewById(R.id.btn2);
        btn2.setClickable(second);
    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void insertPumpStop()
    {
        LinearLayout pumpstop = (LinearLayout) findViewById(R.id.lad0ss2_stop);
        View pumpstop_v = getLayoutInflater().inflate(R.layout.pumpstopped,
                null, false);
        pumpstop.addView(pumpstop_v);

        TextView txt1 = (TextView) findViewById(R.id.pump_txt1);
        txt1.setText(R.string.txt_pumpstopped);

        TextView txt2 = (TextView) findViewById(R.id.pump_txt2);
        txt2.setText(R.string.txt_tap2restart);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
