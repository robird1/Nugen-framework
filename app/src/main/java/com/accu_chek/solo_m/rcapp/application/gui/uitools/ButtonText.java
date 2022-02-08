/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ButtonText
 * Brief: Provide the interface function of the ButtonText
 *
 * Create Date: 12/09/2014
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: ButtonText.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;

import android.text.TextUtils;
import android.view.View;
import android.widget.TextView;

public class ButtonText
{
    
    int mId = 0;
    
    String mString = null;

    /**
     * 
     * @param id
     */
    public ButtonText(int id)
    {
        super();
        mId = id;
        mString = null;
    }

    /**
     * 
     * @param string
     */
    public ButtonText(String string)
    {
        super();
        this.mString = string;
    }

    /**
     * 
     * Function Description
     *
     * @param view
     * @param id
     * @return
     * @return TextView [out] Delete pre line return if exist. Parameter Description
     */
    public TextView set(View view, int id)
    {
        TextView textView = (TextView) view.findViewById(id);
        if (mString == null)
        {
            textView.setText(this.mId);
        }
        else
        {
            textView.setText(this.mString);
        }

        if (GlobalTools.ReplaceLenghtWithDots)
        {
            textView.setEllipsize(TextUtils.TruncateAt.END);
        }
        return textView;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */

