/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractSoftKeyboard
 * Brief: Provide the interface of the AbstractSoftKeyboard UI
 *
 * Create Date: 12/28/2014
 * $Revision: 24640 $
 * $Author: AdamChen $
 * $Id: AbstractSoftKeyboard.java 24640 2015-11-23 11:27:22Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.text.InputFilter;
import android.view.KeyEvent;
import android.view.View;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.TextView.OnEditorActionListener;

import com.accu_chek.solo_m.rcapp.application.gui.buildingblocks.SoloMActionBarActivity;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class AbstractSoftKeyboard extends SoloMActionBarActivity
{
    
    private static final Pattern KEYCODE_PATTERN = Pattern
            .compile("KEYCODE_(\\w)");
    
    private EditText mEditText = null;
    
    /**
     * 
     * 
     *
     * @param keyCode
     * @param event
     * @return
     */
    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) 
    {
//        if (keyCode == KeyEvent.KEYCODE_ENTER)
//        {
//            finish();
//        }
        String key = KeyEvent.keyCodeToString(keyCode);
        // key will be something like "KEYCODE_A" - extract the "A"

        // use pattern to convert int keycode to some character
        Matcher matcher = KEYCODE_PATTERN.matcher(key);
        if (matcher.matches())
        {
            // append character to textview
            mEditText.append(matcher.group(1));
        }
        
        // let the default implementation handle the event
        return super.onKeyDown(keyCode, event);
    }

    /**
     * 
     * Function Description
     *
     * @param view
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void textbox_click(View view) 
    {
//        InputMethodManager imm = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
//        imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0);
    }
    
    /**
     * 
     * 
     *
     * @param savedInstanceState
     */
    @Override
    protected void onCreate(Bundle savedInstanceState) 
    {
        boolean noteKeyboard = false;
        InputFilter[] filter = { new InputFilter.LengthFilter(
                getTextLimitLength()) };

        super.onCreate(savedInstanceState);
        
        noteKeyboard = isNoteKeyboard();
        
        if (noteKeyboard == false)
        {
            setContentView(R.layout.lad0009_steve);

//            this.getActionBar().hide();

        }
        else
        {
            setContentView(R.layout.lad0009);
            
            TopActionBar.setup(this, R.string.txt_scr0091_note_title,
                    R.drawable.keyboard_75x75, "SCR0091_note");
        }
        

        this.getWindow().setSoftInputMode(
                WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
        
        mEditText = (EditText) findViewById(R.id.id_lad0009_text);
        
        // set the limit of text length
        mEditText.setFilters(filter);
        
        mEditText.setSingleLine();
        
        mEditText.setOnEditorActionListener(new EditorListener());
        
    }
    
    /**
     * 
     * 
     *
     */
    @Override
    protected void onNextPressed()
    {
        doAction();
    }
    
    /**
     * Return true if current screen is a note keyboard.
     * 
     * @return boolean: true if current screen is a note keyboard.
     */
    protected boolean isNoteKeyboard()
    {
        // override by subclass
        return false;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    protected String getInput()
    {
        return mEditText.getText().toString();
    }

    /**
     * Do the action after user presses "Done" button of the keyboard or the
     * "Next" button of the device.
     */
    protected abstract void doAction();
    
    protected abstract int getTextLimitLength();
    
    /**
     * 
     */
    private class EditorListener implements OnEditorActionListener
    {

        /**
         * 
         * 
         *
         * @param v
         * @param actionId
         * @param event
         * @return
         */
        @Override
        public boolean onEditorAction(TextView v, int actionId, KeyEvent event) 
        {
            boolean ret = false;
            // When user clicks the "Done" button of the keyboard
            if (actionId == EditorInfo.IME_ACTION_DONE) 
            {
                doAction();

                ret = true;
            }
            return ret;
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
