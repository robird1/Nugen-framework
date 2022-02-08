/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0014
 * Brief: Provide the function of the LAD0014 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0014.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0014 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0014;
    
    private ViewGroup mParent = null;

    /**
     * 
     * @param activity
     */
    public LAD0014(Activity activity)
    {
        super(activity);
        mParent = (ViewGroup) findViewById(R.id.lad0014_relativeLayout_all);
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
     * @param graphicId
     * @param text
     * @param handler
     * @return
     * @return LAD0014 [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0014 setup(int graphicId, int text, View.OnClickListener handler)
    {
        UIHelper.setText(mParent, R.id.id_lad0014_text, text);
        UIHelper.setImage(mParent, R.id.id_lad0014_image, graphicId);
        if (handler != null)
        {
            ImageView img1 = (ImageView) findViewById(R.id.id_lad0014_image);
            img1.setOnClickListener(handler);
        }
        return this;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
