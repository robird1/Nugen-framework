/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0020LISTVIEW
 * Brief: Provide the function of the LAD0020LISTVIEW layout
 *
 * Create Date: 01/22/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.content.Context;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

/**
 * Represents a layout definition.
 */
public class LAD0020LISTVIEW extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0020_listview;
    
    // Add to support UIAutomator
    private ListView mListView = null;
    
    private Context mContext = null;

    /**
     * Constructor. The {@code Activity.setContentView} method is called with
     * the layout id as parameter
     * 
     * @param activity
     *            : The underlying activity
     * 
     */
    public LAD0020LISTVIEW(Activity activity)
    {
        super(activity);
        mContext = activity;
        mListView = (ListView) findViewById(R.id.listView1);
    }

    /**
     * Gets the id for the XML layout definition.
     * 
     * @return The layout id
     */
    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

    /**
     * 
     * Function Description
     *
     * @param uiadapter
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addadapter(ArrayAdapter uiadapter)
    {
        mListView.setAdapter(uiadapter);
    }

    /**
     * 
     * Function Description
     *
     * @param itemListener
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addclickiistener(AdapterView.OnItemClickListener itemListener)
    {
        mListView.setOnItemClickListener(itemListener);
    }

    // added by Henry Tso {
    /**
     * Call this API to return the TextView object of the bG Unit
     * 
     * @return TextView [out] TextView object of the bG Unit
     *         Range: Valid TextView object
     *         Unit: TextView
     *         Scaling: 1
     */
    public TextView getbGUnitTextView()
    {
        TextView bGUnitView = (TextView) findViewById(R.id.id_lad0020_unit1);

        return bGUnitView;
    }

    /**
     * Call this API to return the TextView object of the Carbs Unit
     * 
     * @return TextView [out] TextView object of the Carbs Unit
     *         Range: Valid TextView object
     *         Unit: TextView
     *         Scaling: 1
     */
    public TextView getCarbsUnitTextView()
    {
        TextView carbsUnitView = (TextView) findViewById(R.id.id_lad0020_unit3);

        return carbsUnitView;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
