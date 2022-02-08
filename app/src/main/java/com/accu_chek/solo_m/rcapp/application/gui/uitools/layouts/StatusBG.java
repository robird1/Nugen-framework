/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: StatusBG
 * Brief: Provide the function of the StatusBG
 *
 * Create Date: 01/22/2015
 * $Revision: 24929 $
 * $Author: AdamChen $
 * $Id: StatusBG.java 24929 2015-11-26 09:01:08Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.content.Context;
import android.content.res.Resources;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class StatusBG
{

    Context mStatus_ct = null;
    
    ViewGroup m_container = null;
    
    android.support.v7.app.ActionBar mTab_actionbar = null;
    
    TextView mT_date = null;
    
    TextView mT_value = null;
    
    TextView mT_unit = null;
    
    ImageView mT_image = null;
    
    String m_unit = "mg/dL";
    
    Boolean mIsTransparent = false;
    
    Boolean mIsInverted = false;

    final int[] mBg_color = { R.color.background, R.drawable.bg_hypo_color,
            R.drawable.bg_low_color, R.drawable.bg_target_color,
            R.drawable.bg_high_color };

    /**
     * 
     * Function Description
     *
     * @param is_mgdl
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void isMgDl(boolean is_mgdl)
    {
        if (!is_mgdl)
        {
            m_unit = "mmol";
        }
    }

    /**
     * 
     * @param container
     */
    public StatusBG(ViewGroup container)
    {
        m_container = container;
        mStatus_ct = container.getContext();
        LayoutInflater mInflater = LayoutInflater.from(mStatus_ct);
        View status_v = mInflater.inflate(R.layout.status_bg, null, false);
        container.addView(status_v);

        mT_date = (TextView) container.findViewById(R.id.id_status_bg_date);
        mT_unit = (TextView) container.findViewById(R.id.id_status_bg_unit);
        mT_value = (TextView) container.findViewById(R.id.id_status_bg_res);
        mT_image = (ImageView) container.findViewById(R.id.id_status_bg_image);
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setValueAndColor(int value)
    {
        GlobalTools g = GlobalTools.getInstance();
        String val = g.setAndCheckValue(String.valueOf(value));
        mT_value.setText(val);
        Resources res = mStatus_ct.getResources();
        if (g.isHighLow(val))
        {
            mT_unit.setText(null);
        }
        else
        {
            mT_unit.setText(m_unit);
        }

        if (mIsTransparent == false)
        {
            setBGColor(value);
        }

        if (mIsInverted == true)
        {
            mT_value.setTextColor(mStatus_ct.getResources().getColor(
                    R.color.main_grey));
            mT_date.setTextColor(mStatus_ct.getResources().getColor(
                    R.color.main_grey));
            mT_unit.setTextColor(mStatus_ct.getResources().getColor(
                    R.color.main_grey));
            mT_image.setImageDrawable(g.getHyperHypoSymbol(value, false));
            if (mIsTransparent == false)
            {
                whiteShadow();
            }
        }
        else
        {
            mT_value.setTextColor(mStatus_ct.getResources().getColor(
                    R.color.white));
            mT_date.setTextColor(mStatus_ct.getResources()
                    .getColor(R.color.white));
            mT_unit.setTextColor(mStatus_ct.getResources()
                    .getColor(R.color.white));
            mT_image.setImageDrawable(g.getHyperHypoSymbol(value, true));
            greyShadow();
        }
        mT_date.setText(g.getDateFormatted());
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void whiteShadow()
    {
        mT_value.setShadowLayer(1, -1, 1,
                m_container.getResources().getColor(R.color.white));
        mT_date.setShadowLayer(1, -1, 1,
                m_container.getResources().getColor(R.color.white));
        mT_unit.setShadowLayer(1, -1, 1,
                m_container.getResources().getColor(R.color.white));
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void greyShadow()
    {
        mT_value.setShadowLayer(1, -1, 1,
                m_container.getResources().getColor(R.color.main_grey));
        mT_date.setShadowLayer(1, -1, 1,
                m_container.getResources().getColor(R.color.main_grey));
        mT_unit.setShadowLayer(1, -1, 1,
                m_container.getResources().getColor(R.color.main_grey));
    }

    /**
     * 
     * Function Description
     *
     * @param is_transparent
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setTransparent(Boolean is_transparent)
    {
        mIsTransparent = is_transparent;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setValue(int value)
    {
        if (mIsTransparent)
        {
            mIsInverted = true;
        }
        setValueAndColor(value);

    }
    
    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setBGColor(int value)
    {

        Resources res = mStatus_ct.getResources();
        mIsInverted = false;
        int bgLowValue = res.getInteger(R.integer.bg_low);
        int bgHypoValue = res.getInteger(R.integer.bg_hypo);
        int bgTragetValue = res.getInteger(R.integer.bg_target);
        int bgHyperValue = res.getInteger(R.integer.bg_hyper);
        int bgHighValue = res.getInteger(R.integer.bg_high);
        
        
        if (value <= bgLowValue)
        {
            m_container.setBackgroundResource(mBg_color[0]);
            mIsInverted = true;
        }
        else if (value <= bgHypoValue)
        {
            m_container.setBackgroundResource(mBg_color[1]);
        }
        else if (value <= bgTragetValue)
        {
            m_container.setBackgroundResource(mBg_color[2]);
            mIsInverted = true;
        }
        else if (value <= bgHyperValue)
        {
            m_container.setBackgroundResource(mBg_color[3]);
        }
        else if (value <= bgHighValue)
        {
            m_container.setBackgroundResource(mBg_color[4]);
        }
        else
        {
            m_container.setBackgroundResource(mBg_color[0]);
            mIsInverted = true;
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
