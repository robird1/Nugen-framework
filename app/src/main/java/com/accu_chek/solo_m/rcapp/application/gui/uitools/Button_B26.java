/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B26
 * Brief: Provide the function of the Button_B26 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B26.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.TextView;

public class Button_B26 extends ButtonBasic
{
    
    OnClickListener mListener = null;

    /**
     * 
     * @param stringText
     */
    public Button_B26(String stringText)
    {
        super(stringText);

    }

    /**
     * 
     * @param mText
     */
    public Button_B26(int mText)
    {
        super(mText);
    }

    /**
     * 
     * 
     *
     * @param listener
     */
    @Override
    public void setListener(OnClickListener listener)
    {
        mListener = listener;
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
        super.build(group);
        if (mListener != null)
        {
            TextView view = (TextView) group.findViewById(R.id.id_button_text);
            if (view != null)
            {
                view.setOnClickListener(mListener);
            }
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
