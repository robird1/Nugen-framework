/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B12
 * Brief: Provide the function of the Button_B12 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Button_B12.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.content.Context;
import android.text.Editable;
import android.text.InputType;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B12 extends Button_B5
{

    private ButtonText mText2 = null;
    
    private Context mCtx = null;
    
    private EditText mEdtxt = null;

    // Add by Henry Tso.
    // This TextView object is used update the button text
    private TextView mSecondTextView = null;

    /**
     * @param text First line text id
     * @param secondLineText Second line text id
     * @param icon Icon
     * @param listener event listener
     */
    public Button_B12(
            int text, int secondLineText, int icon, OnClickListener listener)
    {
        super(text, icon, listener);
        mText2 = new ButtonText(secondLineText);
    }

    /**
     * This constructor is defined for construct the Button_B12 with
     * the item index. This index is designed for UIAutomator usage
     * only
     * Added by Henry Tso
     * 
     * @param text [in] Text resource ID of the first line
     * @param secondLineText [in] Text resource Id of the second line
     * @param icon [in] Icon resource Id
     * @param listener [in] onClickListener object
     */
    public Button_B12(
            String text, int secondLineText, int icon, OnClickListener listener)
    {
        super(text, icon, listener);
        mText2 = new ButtonText(secondLineText);
    }

    /**
     * This constructor is defined for construct the Button_B12 with
     * the item index. This index is designed for UIAutomator usage
     * only
     * Added by Henry Tso
     * 
     * @param text [in] Text of the first line
     *            Range: Valid String object
     *            Unit: String
     *            Scaling: 1
     * @param secondLineText [in] Text resource Id of the second line
     *            Range: Valid Resource ID
     *            Unit: int
     *            Scaling: 1
     * @param icon [in] Icon resource Id
     *            Range: Valid Resource ID
     *            Unit: int
     *            Scaling: 1
     * @param listener [in] onClickListener object
     */
    public Button_B12(
            String text, String secondLineText, int icon,
            OnClickListener listener)
    {
        super(text, icon, listener);
        mText2 = new ButtonText(secondLineText);
    }

    /**
     * @param text First line text
     * @param secondLineText Second line text
     * @param icon Icon
     * @param listener event listener
     */
    public Button_B12(
            int text, String secondLineText, int icon, OnClickListener listener)
    {
        super(text, icon, listener);
        mText2 = new ButtonText(secondLineText);
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
        return R.layout.button_b12;

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
        setButtonText(group, R.id.id_b12_text);
        UIHelper.setImage(group, R.id.id_b12_img, getIcon());
        mSecondTextView = mText2.set(group, R.id.id_b12_text2);
        addOnClickListener(group);
    }

    /**
     * 
     * Function Description
     *
     * @param visible
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void cursorVisible(Boolean visible)
    {
        mEdtxt.setCursorVisible(visible);
    }

    /**
     * 
     */
    TextWatcher txw = new TextWatcher()
    {

        /**
         * 
         * 
         *
         * @param s
         */
        @Override
        public void afterTextChanged(Editable s)
        {
            boolean equals = false;
            int length = s.length();

            for (int i = length; i > 0; i--)
            {
                equals = s.subSequence(i - 1, i).toString().equals("\n");
                if (equals)
            {
                    s.replace(i - 1, i, "");
                }

            }
            cursorVisible(false);
        }

        /**
         * 
         * 
         *
         * @param s
         * @param start
         * @param count
         * @param after
         */
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count,
                int after)
        {
            int indexOf = s.toString().indexOf("\n");

            if (-1 != indexOf)
            {
                InputMethodManager imm = (InputMethodManager) mCtx
                        .getSystemService(Context.INPUT_METHOD_SERVICE);
                imm.hideSoftInputFromWindow(mEdtxt.getWindowToken(), 0);
                cursorVisible(false);
            }

        }

        /**
         * 
         * 
         *
         * @param s
         * @param start
         * @param before
         * @param count
         */
        @Override
        public void onTextChanged(CharSequence s, int start, int before,
                int count)
        {
            int indexOf = s.toString().indexOf("\n");
            
            if (-1 != indexOf)
            {
                InputMethodManager imm = (InputMethodManager) mCtx
                        .getSystemService(Context.INPUT_METHOD_SERVICE);
                imm.hideSoftInputFromWindow(mEdtxt.getWindowToken(), 0);
                cursorVisible(false);
            }
        }

    };

    /**
     * 
     * Function Description
     *
     * @param applicationContext
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void isEditText(Context applicationContext)
    {
        mCtx = applicationContext;
        InputMethodManager imm = (InputMethodManager) applicationContext
                .getSystemService(Context.INPUT_METHOD_SERVICE);
        getView().findViewById(R.id.id_b12_text2).setVisibility(View.GONE);
        mEdtxt = (EditText) getView().findViewById(R.id.id_b12_text2_edit);
        mEdtxt.setText(mText2.mId);
        mEdtxt.setVisibility(View.VISIBLE);
        mEdtxt.setInputType(InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS);
        mEdtxt.addTextChangedListener(txw);
        cursorVisible(false);
    }

    /**
     * Call this API to update the second line text in the button
     * Added by Henry Tso
     * 
     * @param sData [in] The text for updating the second text in the button
     * 
     * @return void [out] None
     */
    public void setText2(String sData)
    {
        mSecondTextView.setText(sData);
    }
    
    /**
     * 
     * 
     *
     * @return
     */
    @Override
    protected int getTextViewId()
    {
        return R.id.id_b12_text;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
