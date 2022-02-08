/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B10
 * Brief: Provide the function of the Button_B10 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24607 $
 * $Author: SteveSu $
 * $Id: Button_B10.java 24607 2015-11-23 06:51:18Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.text.InputType;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.EditText;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B10 extends Button_B5
{

    private static final int ID_LAYOUT = R.layout.button_b10;
    
    private static final int ID_TEXT = R.id.id_b10_text;
    
    private static final int ID_IMAGE = R.id.id_b10_img;
    
    boolean mEditable = false;

    /**
     * 
     * @param text
     * @param icon
     * @param listener
     */
    public Button_B10(int text, int icon, OnClickListener listener)
    {
        super(text, icon, listener);
    }

    /**
     * 
     * @param text
     * @param icon
     * @param listener
     */
    public Button_B10(String text, int icon, OnClickListener listener)
    {
        super(text, icon, listener);
    }

    /**
     * 
     * @param text
     * @param icon
     * @param editable
     */
    public Button_B10(int text, int icon, boolean editable)
    {
        this(text, icon, null);
        if (editable)
        {
            setListener(editableListener);
        }
    }

    /**
     * 
     * @param text
     * @param icon
     * @param editable
     */
    public Button_B10(String text, int icon, boolean editable)
    {
        this(text, icon, null);
        if (editable)
        {
            setListener(editableListener);
        }
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
        return ID_LAYOUT;
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
        setButtonText(group, ID_TEXT);
        UIHelper.setImage(group, ID_IMAGE, getIcon());
        addOnClickListener(group);
    }

    public String getText()
    {
        return mStringText;
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
        return ID_TEXT;
    }

    /**
     * 
     */
    private OnClickListener editableListener = new OnClickListener()
    {
        /**
         * 
         * 
         *
         * @param v
         */
        @Override
        public void onClick(View v)
        {
            Context context = v.getContext();
            // v.findViewById(ID_TEXT).setVisibility(View.INVISIBLE);
            AlertDialog.Builder builder = new AlertDialog.Builder(context);
            AlertDialog alert;
            // Set an EditText view to get user input
            final EditText input = new EditText(context);
            input.setText(mStringText);
            input.requestFocus();
            input.setInputType(InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS);

            final TextView output = (TextView) v.findViewById(ID_TEXT);
            builder.setView(input);

            builder.setPositiveButton(R.string.txt_labeldone,
                    new DialogInterface.OnClickListener()
                    {
                        @Override
                        public void onClick(DialogInterface dialog,
                                int whichButton)
                        {
                            mStringText = input.getText().toString();
                            output.setText(mStringText);
                        }
                    });
            alert = builder.create();
            alert.getWindow().setSoftInputMode(
                    WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
            alert.show();

        }
    };

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// (R24175 2015-11-16 05:33:09 AdamChen)
// ----------------------------------------------------------------------------
// [Reminder] add Alarm Clock / Custom reminders
