/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: StatusReservoir
 * Brief: Provide the function of the StatusReservoir
 *
 * Create Date: 01/22/2015
 * $Revision: 24929 $
 * $Author: AdamChen $
 * $Id: StatusReservoir.java 24929 2015-11-26 09:01:08Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.content.Context;
import android.content.res.Resources;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class StatusReservoir
{

    private static final int TOTAL_NUMBER = 4;

    private Context mStatus_ct = null;
    
    private ViewGroup m_container = null;

    private TextView mT_resval = null;
    
    private ImageView mT_resimg = null;
    
    private String mUnit_sign = "U";
    
    private Boolean mTransparent = true;

    private int[] mRes_ar = { R.drawable.reservoir_50u_58x51px,
            R.drawable.reservoir_100u_58x51px,
            R.drawable.reservoir_150u_58x51px,
            R.drawable.reservoir_200u_58x51px };
    
    private int[] mRes_ar_inv = { R.drawable.reservoir_50u_58x51px_locked,
            R.drawable.reservoir_100u_58x51px_locked,
            R.drawable.reservoir_150u_58x51px_locked,
            R.drawable.reservoir_200u_58x51px_locked };

    private int[] active_res = null;

    /**
     * 
     * @param container
     */
    public StatusReservoir(ViewGroup container)
    {
        m_container = container;
        mStatus_ct = container.getContext();
        LayoutInflater mInflater = LayoutInflater.from(mStatus_ct);
        View status_v = mInflater.inflate(R.layout.status_reservoir, null,
                false);
        container.addView(status_v);

        mT_resval = (TextView) container.findViewById(R.id.id_status_res_txt);
        mT_resimg = (ImageView) container.findViewById(R.id.id_status_res_img);
    }

    /**
     * 
     * Function Description
     *
     * @param is_trans
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setTransparent(Boolean is_trans)
    {
        mTransparent = is_trans;
        if (is_trans == false)
        {
            mT_resval.setBackgroundResource(R.drawable.bg_white_to_brown_click);
            mT_resval.setClickable(true);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param bat_value
     * @param res_value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setValue(int bat_value, int res_value)
    {
        active_res = new int[TOTAL_NUMBER];

        if (mTransparent == false)
        {
            m_container.setBackgroundResource(R.color.white);
            active_res = mRes_ar.clone();
        }
        else
        {
            m_container.setBackground(null);
            active_res = mRes_ar_inv.clone();
        }

        setResValue(res_value);
    }
    
    /**
     * 
     * Function Description
     *
     * @param res_value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setResValue(int res_value)
    {
        mT_resval.setText(String.valueOf(res_value) + mUnit_sign);
        setResImg(res_value);

    }

    /**
     * 
     * Function Description
     *
     * @param res_value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setResImg(int res_value)
    {
        Resources res = mStatus_ct.getResources();
        int res50Value = res.getInteger(R.integer.res_50);
        int res100Value = res.getInteger(R.integer.res_100);
        int res150Value = res.getInteger(R.integer.res_150);
        int res200Value = res.getInteger(R.integer.res_200);
        
        
        if (res_value <= res50Value)
        {
            mT_resimg.setImageDrawable(res.getDrawable(active_res[0]));
        }
        else if (res_value <= res100Value)
        {
            mT_resimg.setImageDrawable(res.getDrawable(active_res[1]));
        }
        else if (res_value <= res150Value)
        {
            mT_resimg.setImageDrawable(res.getDrawable(active_res[2]));
        }
        else if (res_value <= res200Value)
        {
            mT_resimg.setImageDrawable(res.getDrawable(active_res[3]));
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
// [GUI] update GUI framework to ClickThrough v0.34
