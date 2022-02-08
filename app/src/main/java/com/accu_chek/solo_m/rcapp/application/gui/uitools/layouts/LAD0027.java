/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0027
 * Brief: Provide the function of the LAD0027 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0027.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0027 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0027;

    /**
     * 
     * @param activity
     */
    public LAD0027(Activity activity)
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
     * @param title
     * @param firstline
     * @param secondline
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setTitle(String title, String firstline, String secondline)
    {
        {
            TextView t = (TextView) findViewById(R.id.id_lad27_title);
            t.setText(title);
        }
        {
            TextView t = (TextView) findViewById(R.id.id_lad27_text1);
            t.setText(firstline);
        }
        {
            TextView t = (TextView) findViewById(R.id.id_lad27_text2);
            t.setText(secondline);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param message
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setText(String message)
    {
        TextView t = (TextView) findViewById(R.id.id_lad27_message);
        t.setText(message);
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
