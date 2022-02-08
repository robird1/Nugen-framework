/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0010a
 * Brief: Provide the function of the LAD0010a layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: LAD0010a.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.CollectionPageAdapter;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TabClickListener;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0010a extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0010a;
    
    private ViewGroup mEntryContainer = null;
    
    // Add to support UIAutomator
    private int mButtonIndex = 0;

    /**
     * 
     * @param activity
     */
    public LAD0010a(Activity activity)
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

    // modify by Steve {
    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0010a_lower_ab, buttonTextId, button);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad0010a_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }
    // } modify by Steve

    /**
     * 
     * Function Description
     *
     * @param tabImg1Id
     * @param tabImag2Id
     * @return
     * @return LAD0010a [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010a createHeader(int tabImg1Id, int tabImag2Id)
    {
        ViewGroup topContainer = findContainerById(R.id.lad0010a_tab2);
        ViewGroup tr = (ViewGroup) getLayoutInflater().inflate(R.layout.tab2,
                topContainer, false);
        topContainer.addView(tr);
        // Modified by Henry Tso. To support UIAutomator
        UIHelper.setImage(tr, R.id.id_tab2_img1, tabImg1Id, "tab1");
        UIHelper.setImage(tr, R.id.id_tab2_img2, tabImag2Id, "tab2");

        // green bar background
        tr.findViewById(R.id.id_tab2_green_left).setBackgroundResource(
                R.color.insulin_button);

        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param txt1
     * @param txt2
     * @param txt3
     * @param button
     * @return
     * @return LAD0010a [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010a createHeadline(int txt1, int txt2, int txt3,
            ButtonBasic button)
    {

        ViewGroup container = findContainerById(R.id.lad0010a_nonscroll);
        UIHelper.setText(container, R.id.id_lad0010a_text1, txt1);
        UIHelper.setText(container, R.id.id_lad0010a_text2, txt2);
        UIHelper.setText(container, R.id.id_lad0010a_text3, txt3);

        ViewGroup buttonContainer = (ViewGroup) container
                .findViewById(R.id.lad0010a_b10);
        // Modified by Henry Tso
        buttonContainer.addView(button.createView(getActivity(),
                buttonContainer, 0));
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param str_id
     * @param str_id2
     * @param str_id3
     * @return
     * @return LAD0010a [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010a createLadb10Entry(String str_id, String str_id2,
            String str_id3)
    {

        if (mEntryContainer == null)
        {
            mEntryContainer = findContainerById(R.id.lad0010a_tableLayout);
        }

        LayoutInflater inflater = getLayoutInflater();
        ViewGroup tr = (ViewGroup) inflater.inflate(R.layout.button_lad10a,
                mEntryContainer, false);
        mEntryContainer.addView(tr);
        View tr_space = inflater.inflate(R.layout.button_spacing_lad10,
                mEntryContainer, false);
        mEntryContainer.addView(tr_space);
        UIHelper.setText(tr, R.id.id_lad10a_text, str_id);
        UIHelper.setText(tr, R.id.id_lad10a_text2, str_id2);
        UIHelper.setText(tr, R.id.id_lad10a_text3, str_id3);

        return this;
    }

//    /**
//     * 
//     * Function Description
//     *
//     * @param bp
//     * @return void [out] Delete pre line return if exist. Parameter Description
//     */
//    public void collectValues(BasalProfile bp)
//    {
//        if (mEntryContainer == null)
//        {
//            mEntryContainer = findContainerById(R.id.lad0010a_tableLayout);
//        }
//
//        TextView title = (TextView) findViewById(R.id.id_b10_text);
//        bp.name = title.getText().toString();
//        bp.isactive = false;
//        int childCount = mEntryContainer.getChildCount();
//        
//        for (int i = 0; i < childCount; i++)
//        {
//            View v = mEntryContainer.getChildAt(i);
//            TextView tv1 = (TextView) v.findViewById(R.id.id_lad10a_text);
//            if (tv1 != null)
//            {
//                BasalEntry basal_e = bp.new BasalEntry();
//                basal_e.start = tv1.getText().toString();
//
//                TextView tv2 = (TextView) v.findViewById(R.id.id_lad10a_text2);
//                basal_e.stop = tv2.getText().toString();
//
//                TextView tv3 = (TextView) v.findViewById(R.id.id_lad10a_text3);
//
//                String value = tv3.getText().toString();
//                Number val = 0;
//                
//                try
//                {
//                    val = NumberFormat.getNumberInstance().parse(value);
//                }
//                catch (ParseException e)
//                {
//                    val = 0;
//                    e.printStackTrace();
//                }
//                finally
//                {
//                    // Apply to the coding standard
//                }
//                
//                basal_e.unit = val.doubleValue();
//                bp.be.add(basal_e);
//            }           
//        }
//    }

    /**
     * 
     * Function Description
     *
     * @param str_id
     * @param str_id2
     * @param str_id3
     * @param stop
     * @param unit
     * @return
     * @return LAD0010a [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010a createLadb10Entry(String str_id, String str_id2,
            String str_id3, View.OnClickListener stop, View.OnClickListener unit)
    {

        if (mEntryContainer == null)
            mEntryContainer = findContainerById(R.id.lad0010a_tableLayout);

        LayoutInflater inflater = getLayoutInflater();
        ViewGroup tr = (ViewGroup) inflater.inflate(R.layout.button_lad10a,
                mEntryContainer, false);
        mEntryContainer.addView(tr);
        View tr_space = inflater.inflate(R.layout.button_spacing_lad10,
                mEntryContainer, false);
        mEntryContainer.addView(tr_space);
        UIHelper.setText(tr, R.id.id_lad10a_text, str_id);
        UIHelper.setText(tr, R.id.id_lad10a_text2, str_id2);
        UIHelper.setText(tr, R.id.id_lad10a_text3, str_id3);
        {
            TextView textView = (TextView) tr
                    .findViewById(R.id.id_lad10a_text2);
            textView.setOnClickListener(stop);
        }
        {
            TextView textView = (TextView) tr
                    .findViewById(R.id.id_lad10a_text3);
            textView.setOnClickListener(unit);
        }
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @param str_id
     * @param str_id2
     * @param str_id3
     * @param stop
     * @param unit
     * @return
     * @return LAD0010a [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010a createLadb10Entry(String str_id, String str_id2,
            double str_id3, View.OnClickListener stop, View.OnClickListener unit)
    {

        if (mEntryContainer == null)
        {
            mEntryContainer = findContainerById(R.id.lad0010a_tableLayout);
        }

        LayoutInflater inflater = getLayoutInflater();
        ViewGroup tr = (ViewGroup) inflater.inflate(R.layout.button_lad10a,
                mEntryContainer, false);
        mEntryContainer.addView(tr);
        View tr_space = inflater.inflate(R.layout.button_spacing_lad10,
                mEntryContainer, false);
        mEntryContainer.addView(tr_space);
        UIHelper.setText(tr, R.id.id_lad10a_text, str_id);
        UIHelper.setText(tr, R.id.id_lad10a_text2, str_id2);
        UIHelper.setText(tr, R.id.id_lad10a_text3, str_id3);
        {
            TextView textView = (TextView) tr
                    .findViewById(R.id.id_lad10a_text2);
            textView.setOnClickListener(stop);
        }
        {
            TextView textView = (TextView) tr
                    .findViewById(R.id.id_lad10a_text3);
            textView.setOnClickListener(unit);
        }
        return this;
    }

    // add by Steve {
    /**
     * 
     * Function Description
     *
     * @param tabImg1Id
     * @param tabImag2Id
     * @param adapter
     * @return
     * @return LAD0010a [out] Delete pre line return if exist. Parameter Description
     */
    public LAD0010a createHeader(int tabImg1Id, int tabImag2Id,
            CollectionPageAdapter adapter)
    {
        ViewGroup topContainer = findContainerById(R.id.lad0010a_tab2);
        ViewGroup tr = (ViewGroup) getLayoutInflater().inflate(R.layout.tab2,
                topContainer, false);
        topContainer.addView(tr);
        // Modified by Henry Tso. To support UIAutomator
        UIHelper.setImage(tr, R.id.id_tab2_img1, tabImg1Id, "tab1");
        UIHelper.setImage(tr, R.id.id_tab2_img2, tabImag2Id, "tab2");

        // green bar background
        tr.findViewById(R.id.id_tab2_green_left).setBackgroundResource(
                R.color.insulin_button);

        findViewById(R.id.id_tab2_relative_left).setOnClickListener(
                new TabClickListener(adapter));
        findViewById(R.id.id_tab2_relative_right).setOnClickListener(
                new TabClickListener(adapter));

        return this;
    }
    // } add by Steve
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
