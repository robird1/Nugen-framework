/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD00MM
 * Brief: Provide the function of the LAD00MM layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: LAD00MM.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD00MM extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad00mm;

    /**
     * 
     * @param activity
     */
    public LAD00MM(Activity activity)
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
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId)
    {
        return addLowerActionBar(R.id.lad00mm_lower_ab, buttonTextId);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id)
    {
        return addLowerActionBar(R.id.lad00mm_lower_ab, buttonText1Id,
                buttonText2Id);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int icon,
            int buttonText2Id)
    {
        return addLowerActionBar(R.id.lad00mm_lower_ab, buttonText1Id, icon,
                buttonText2Id);
    }

    /**
     * 
     * Function Description
     *
     * @param first
     * @param second
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLowerClickability(Boolean first, Boolean second)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(R.id.lad00mm_lower_ab);
        View btn1 = (View) ab.findViewById(R.id.btn1);
        btn1.setClickable(first);
        View btn2 = (View) ab.findViewById(R.id.btn2);
        btn2.setClickable(second);
    }

    /**
     * 
     * Function Description
     *
     * @param first
     * @param second
     * @param third
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLowerClickability(Boolean first, Boolean second,
            Boolean third)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(R.id.lad00mm_lower_ab);
        View btn3 = (View) ab.findViewById(R.id.btn3);
        btn3.setClickable(second);
        setLowerClickability(first, third);
    }

    /**
     * Enables or disables lower action bar
     * 
     * @param enabled True to enable, false to disable the action bar.
     */
    public void setLowerActionBarEnabled(boolean enabled)
    {
        Activity activity = getActivity();
        ViewGroup ab = (ViewGroup) activity.findViewById(R.id.lad00mm_lower_ab);
        ab.setEnabled(enabled);
    }

    /**
     * 
     * Function Description
     *
     * @param pos
     * @param imgid
     * @param txt
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void insertItemAndText(int pos, int imgid, int txt)
    {
        Activity activity = getActivity();
        TextView textView = null;
        boolean isDefault = false;
        
        switch (pos)
        {
        case 1:
            textView = (TextView) activity.findViewById(R.id.menu_icon1);
            break;
        case 2:
            textView = (TextView) activity.findViewById(R.id.menu_icon2);
            break;
        case 3:
            textView = (TextView) activity.findViewById(R.id.menu_icon3);
            break;
        case 4:
            textView = (TextView) activity.findViewById(R.id.menu_icon4);
            break;
        case 5:
            textView = (TextView) activity.findViewById(R.id.menu_icon5);
            break;
        case 6:
            textView = (TextView) activity.findViewById(R.id.menu_icon6);
            break;
        case 7:
            textView = (TextView) activity.findViewById(R.id.menu_icon7);
            break;
        case 8:
            textView = (TextView) activity.findViewById(R.id.menu_icon8);
            break;
        case 9:
            textView = (TextView) activity.findViewById(R.id.menu_icon9);
            break;
        case 10:
            textView = (TextView) activity.findViewById(R.id.menu_icon10);
            break;
        case 11:
            textView = (TextView) activity.findViewById(R.id.menu_icon11);
            break;
        // case 12:
        // textView = (TextView) activity.findViewById(R.id.menu_icon12);
        // break;
        default:
            isDefault = true;

        }
        
        if (isDefault == false)
        {
            Drawable top = activity.getResources().getDrawable(imgid);
            textView.setText(txt);
            textView.setCompoundDrawablesWithIntrinsicBounds(null, top, null, null);

            if (GlobalTools.ReplaceLenghtWithDots)
            {
                textView.setEllipsize(TextUtils.TruncateAt.END);
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
