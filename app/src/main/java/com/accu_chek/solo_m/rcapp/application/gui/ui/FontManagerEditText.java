/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: FontManagerEditText
 * Brief: Used to handle font style
 *
 * Create Date: 22/01/2015
 * $Revision: 24628 $
 * $Author: AdamChen $
 * $Id: FontManagerEditText.java 24628 2015-11-23 10:31:58Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.ui;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.widget.EditText;

import java.util.HashMap;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class FontManagerEditText extends EditText
{

    //The hash map of the type and string
    private static final HashMap<String, Typeface> cache = new HashMap<String, Typeface>();

    /**
     * Custom FontManagerEditText constructor: Construct FontManager object
     * 
     * @param context
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     * @param attrs
     *            Range: Null, Valid reference of the AttributeSet
     *            Unit: AttributeSet
     *            Scaling: 1
     * @param defStyle
     *            Range: -2^31 ~ 2^31 -1
     *            Unit: int
     *            Scaling: 1
     */
    public FontManagerEditText(Context context, AttributeSet attrs, int defStyle)
    {
        super(context, attrs, defStyle);
        init(attrs, context);
    }

    /**
     * Custom FontManagerEditText constructor: Construct FontManager object
     * 
     * @param context
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     * @param attrs
     *            Range: Null, Valid reference of the AttributeSet
     *            Unit: AttributeSet
     *            Scaling: 1
     */
    public FontManagerEditText(Context context, AttributeSet attrs)
    {
        super(context, attrs);
        init(attrs, context);
    }

    /**
     * Custom FontManagerEditText constructor: Construct FontManager object
     * 
     * @param context
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     */
    public FontManagerEditText(Context context)
    {
        super(context);
        init(null, context);
    }
    
    /**
     * 
     * Handle a key event before it is processed by any input method associated
     * with the view hierarchy.
     * 
     * @param keyCode
     *            Range: The valid key code
     *            Unit: int
     *            Scaling: 1
     * @param event
     *            Range: Null, Valid reference of the KeyEvent
     *            Unit: KeyEvent
     *            Scaling: 1
     * @return
     *         Range: true, false
     *         Unit: boolean
     *         Scaling: 1
     */
    @Override
    public boolean onKeyPreIme(int keyCode, KeyEvent event)
    {
        boolean ret = super.onKeyPreIme(keyCode, event);       
        int getKeyCode = event.getKeyCode();
        
        if (getKeyCode == KeyEvent.KEYCODE_BACK)
        {
            dispatchKeyEvent(event);
            this.setCursorVisible(false);
            ret = false;
        }
        return ret;
    }
    
    /**
     * 
     * Set font type by resource.
     * 
     * @param attrs [in]
     *            Range: Null, Valid reference of the AttributeSet
     *            Unit: AttributeSet
     *            Scaling: 1
     * @param context [in]
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     * @return void [out]
     * 
     */
    private void init(AttributeSet attrs, Context context)
    {
        if (!isInEditMode())
        {
            if (attrs != null)
            {
                TypedArray a = getContext().obtainStyledAttributes(attrs,
                        R.styleable.FontManager);
                String fontName = a.getString(R.styleable.FontManager_fontName);
                if (fontName != null)
                {
                    Typeface myTypeface = get(fontName);
                    setTypeface(myTypeface);
                }
                a.recycle();
            }
        }
    }

    /**
     * Used to return the typeface object by the fontName key in the hash map.
     * 
     * @param fontName [in]
     *            Range: Null, Valid reference of the String
     *            Unit: String
     *            Scaling: 1
     * @return Typeface [out]
     *         Range: Null, Valid reference of the Typeface
     *         Unit: Typeface
     *         Scaling: 1
     * 
     */
    private Typeface get(String fontName)
    {
        synchronized (cache)
        {
            if (!cache.containsKey(fontName))
            {
                Typeface t = Typeface.createFromAsset(getContext().getAssets(),
                        "fonts/" + fontName);
                cache.put(fontName, t);
            }
        }

        return cache.get(fontName);
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
// This file is useful, do not remove
