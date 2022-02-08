/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0008
 * Brief: Provide the function of the LAD0010_TB_content
 *
 * Create Date: 01/22/2015
 * $Revision: 25077 $
 * $Author: AdamChen $
 * $Id: LAD0010_TB_content.java 25077 2015-11-30 05:22:16Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0010_TB_content
{

    private static final int FACTOR = 2;

    private static final int LAYOUT_ID = R.layout.lad0010a_content;

    private ViewGroup mEntryContainer = null;

    private View mRootView = null;

    LayoutInflater mInflater = null;

    // Added by Henry Tso. To support UIAutomator
    private int mItemIndex = 0;

    /**
     * 
     * Function Description
     * 
     * @param inflater
     * @param container
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void inflate10a(LayoutInflater inflater, ViewGroup container)
    {
        mRootView = inflater.inflate(R.layout.lad0010a_content, container,
                false);
        mEntryContainer = (ViewGroup) mRootView
                .findViewById(R.id.lad0010a_tableLayout);
        mInflater = inflater;
    }

    /**
     * 
     * Function Description
     * 
     * @param inflater
     * @param container
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void inflate10b(LayoutInflater inflater, ViewGroup container)
    {
        mRootView = inflater.inflate(R.layout.lad0010b_content, container,
                false);
        mEntryContainer = (ViewGroup) mRootView
                .findViewById(R.id.lad0010b_scroll);
        mInflater = inflater;
    }

    /**
     * Set graphic image id
     * JUST USED FOR TESTING STAGE
     */
    public LAD0010_TB_content setGraphic(int graphicId)
    {
        ViewGroup test = (ViewGroup) mRootView
                .findViewById(R.id.lad0010b_scroll2);
        // remove default padding
        test.setPadding(0, 0, 0, 0);

        mEntryContainer.setBackgroundResource(graphicId);
        // remove default rotation
        mEntryContainer.setRotation(0);
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
     * @return LAD0010_TB_content [out] Delete pre line return if exist.
     *         Parameter Description
     */
    public LAD0010_TB_content createHeadline(int txt1, int txt2, int txt3,
            ButtonBasic button)
    {
        // Modified by Henry Tso. To support UIAutomator
        mItemIndex = 0; // Reset item index
        UIHelper.setText(mRootView, R.id.id_lad0010a_text1, txt1, "title1");
        UIHelper.setText(mRootView, R.id.id_lad0010a_text2, txt2, "title2");
        UIHelper.setText(mRootView, R.id.id_lad0010a_text3, txt3, "title3");

        ViewGroup buttonContainer = (ViewGroup) mRootView
                .findViewById(R.id.lad0010a_b10);
        buttonContainer.addView(button.createView(mInflater, buttonContainer));
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
     * @return LAD0010_TB_content [out] Delete pre line return if exist.
     *         Parameter Description
     */
    public LAD0010_TB_content createLadb10Entry(String str_id, String str_id2,
            String str_id3)
    {
        ViewGroup tr = (ViewGroup) mInflater.inflate(R.layout.button_lad10a,
                mEntryContainer, false);
        mEntryContainer.addView(tr);
        View tr_space = mInflater.inflate(R.layout.button_spacing_lad10,
                mEntryContainer, false);
        mEntryContainer.addView(tr_space);
        UIHelper.setText(tr, R.id.id_lad10a_text, str_id);
        UIHelper.setText(tr, R.id.id_lad10a_text2, str_id2);
        UIHelper.setText(tr, R.id.id_lad10a_text3, str_id3);
        return this;
    }

    // /**
    // *
    // * Function Description
    // *
    // * @param bp
    // * @return void [out] Delete pre line return if exist. Parameter
    // Description
    // */
    // public void collectValues(BasalProfile bp)
    // {
    // TextView title = (TextView) mRootView.findViewById(R.id.id_b10_text);
    // bp.name = title.getText().toString();
    // bp.isactive = false;
    // int childCount = mEntryContainer.getChildCount();
    //
    // for (int i = 0; i < childCount; i++)
    // {
    // View v = mEntryContainer.getChildAt(i);
    // TextView tv1 = (TextView) v.findViewById(R.id.id_lad10a_text);
    // if (tv1 != null)
    // {
    // BasalEntry basal_e = bp.new BasalEntry();
    // basal_e.start = tv1.getText().toString();
    //
    // TextView tv2 = (TextView) v.findViewById(R.id.id_lad10a_text2);
    // basal_e.stop = tv2.getText().toString();
    //
    // TextView tv3 = (TextView) v.findViewById(R.id.id_lad10a_text3);
    //
    // String value = tv3.getText().toString();
    // Number val;
    // try
    // {
    // val = NumberFormat.getNumberInstance().parse(value);
    // }
    // catch (ParseException e)
    // {
    // val = 0;
    // e.printStackTrace();
    // }
    // finally
    // {
    // // Apply to the coding standard
    // }
    //
    // basal_e.unit = val.doubleValue();
    // bp.be.add(basal_e);
    // }
    // }
    //
    // }

    /**
     * Call this API for creating total 10 time block item into Scrollable view.
     * Modified by Henry Tso. Add one more parameter for recording the item
     * index
     * 
     * @param str_id
     * @param str_id2
     * @param str_id3
     * @param stop
     * @param unit
     * @param itemIndex [in] Given to create the Content Description. (Support
     *            UIAutomator)
     * 
     * @return LAD0010_TB_content [out] Delete pre line return if exist.
     *         Parameter Description
     */
    public LAD0010_TB_content createLadb10Entry(String str_id, String str_id2,
            String str_id3, View.OnClickListener stop, View.OnClickListener unit)
    {
        // Added by Henry Tso. To support UIAutomator
        String index = String.format("Item_%d", ++mItemIndex);

        ViewGroup tr = (ViewGroup) mInflater.inflate(R.layout.button_lad10a,
                mEntryContainer, false);
        mEntryContainer.addView(tr);
        View tr_space = mInflater.inflate(R.layout.button_spacing_lad10,
                mEntryContainer, false);
        mEntryContainer.addView(tr_space);
        UIHelper.setText(tr, R.id.id_lad10a_text, str_id, index);
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
     * @return LAD0010_TB_content [out] Delete pre line return if exist.
     *         Parameter Description
     */
    public LAD0010_TB_content createLadb10Entry(String str_id, String str_id2,
            double str_id3, View.OnClickListener stop, View.OnClickListener unit)
    {
        // Added by Henry Tso. To support UIAutomator
        String index = String.format("Item_%d", ++mItemIndex);

        ViewGroup tr = (ViewGroup) mInflater.inflate(R.layout.button_lad10a,
                mEntryContainer, false);
        mEntryContainer.addView(tr);
        View tr_space = mInflater.inflate(R.layout.button_spacing_lad10,
                mEntryContainer, false);
        mEntryContainer.addView(tr_space);
        UIHelper.setText(tr, R.id.id_lad10a_text, str_id, index);
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
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getView()
    {
        return mRootView;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getEntryView()
    {
        return mEntryContainer;
    }

    /**
     * 
     * Function Description
     *
     * @param index
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View getChildAt(int index)
    {
        View v = mEntryContainer.getChildAt(index * FACTOR);
        return v;
    }

    /**
     * 
     * Function Description
     * 
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public static int getLayoutId()
    {
        return LAYOUT_ID;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
