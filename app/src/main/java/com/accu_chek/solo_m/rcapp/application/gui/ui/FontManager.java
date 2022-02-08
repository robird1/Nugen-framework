/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: FontManager
 * Brief: Used to handle font style
 *
 * Create Date: 22/01/2015
 * $Revision: 24602 $
 * $Author: AdamChen $
 * $Id: FontManager.java 24602 2015-11-23 05:44:22Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.ui;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.widget.TextView;

import java.util.HashMap;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class FontManager extends TextView
{

    //The hash map of the type and string
    private static final HashMap<String, Typeface> cache = new HashMap<String, Typeface>();

    /**
     * Custom FontManager constructor: Construct FontManager object.
     * 
     * @param context [in]
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     * @param attrs [in]
     *            Range: Null, Valid reference of the AttributeSet
     *            Unit: AttributeSet
     *            Scaling: 1
     * @param defStyle [in]
     *            Range: -2^31 ~ 2^31 -1
     *            Unit: int
     *            Scaling: 1
     */
    public FontManager(Context context, AttributeSet attrs, int defStyle)
    {
        super(context, attrs, defStyle);
        
        //initial the attribute and style of the font
        init(attrs, context);
    }

    /**
     * Custom FontManager constructor: Construct FontManager object.
     * 
     * @param context [in]
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     * @param attrs [in]
     *            Range: Null, Valid reference of the AttributeSet
     *            Unit: AttributeSet
     *            Scaling: 1
     */
    public FontManager(Context context, AttributeSet attrs)
    {
        super(context, attrs);
        
        //initial the attribute and style of the font
        init(attrs, context);
    }

    /**
     * Custom FontManager constructor: Construct FontManager object.
     * 
     * @param context [in]
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     * 
     */
    public FontManager(Context context)
    {
        super(context);
        
        //initial the attribute and style of the font
        init(null, context);
    }

    /**
     * 
     * Set font type by resource.
     * 
     * @param attrs
     *            Range: Null, Valid reference of the AttributeSet
     *            Unit: AttributeSet
     *            Scaling: 1
     * @param context
     *            Range: Null, Valid reference of the Context
     *            Unit: Context
     *            Scaling: 1
     * @return void [out] Delete pre line return if exist. Parameter Description
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
// [NSIQ-55] Disable "FrontManager" function to solve the memory leak problem.
// [NSIQ-121] Fix memory leak issue.
// [GUI] update GUI framework to ClickThrough v0.34
