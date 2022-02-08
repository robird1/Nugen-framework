/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B24
 * Brief: Provide the function of the Button_B24 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: Button_B24.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.LinearLayout;

public class Button_B24 implements UIBuilder
{
    
    int mHeadline = 0;
    
    int mTypePicture = 0;
    
    int mTypeText = 0;
    
    String mTypeTextString = null;
    
    OnClickListener mListener = null;

    /**
     * 
     * @param typePicture
     * @param typeText
     */
    public Button_B24(int typePicture, int typeText)
    {
        super();
        mHeadline = R.string.txt_labeltype;
        mTypePicture = typePicture;
        mTypeText = typeText;

    }

    /**
     * 
     * @param typePicture
     * @param typeText
     */
    public Button_B24(int typePicture, String typeText)
    {
        super();
        mHeadline = R.string.txt_labeltype;
        mTypePicture = typePicture;
        mTypeTextString = typeText;
    }

    /**
     * 
     * @param headline
     * @param typePicture
     * @param typeText
     */
    public Button_B24(int headline, int typePicture, int typeText)
    {
        super();
        mHeadline = headline;
        mTypePicture = typePicture;
        mTypeText = typeText;
    }

    /**
     * 
     * Function Description
     *
     * @param listener
     * @return
     * @return Button_B24 [out] Delete pre line return if exist. Parameter Description
     */
    public Button_B24 setListener(OnClickListener listener)
    {
        mListener = listener;
        return this;
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
        UIHelper.setText(group, R.id.id_b24_headline, mHeadline);
        if (mTypeTextString == null)
        {
            UIHelper.setText(group, R.id.id_b24_value, mTypeText);
        }
        else
        {
            UIHelper.setText(group, R.id.id_b24_value, mTypeTextString);
        }
        UIHelper.setImage(group, R.id.id_b24_picture, mTypePicture);
        if (mListener != null)
        {
            LinearLayout view = (LinearLayout) group
                    .findViewById(R.id.id_button_b24);
            view.setOnClickListener(mListener);
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
        // TODO Auto-generated method stub
        return null;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
