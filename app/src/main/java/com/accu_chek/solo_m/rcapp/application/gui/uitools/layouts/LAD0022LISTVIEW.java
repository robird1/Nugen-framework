/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0022LISTVIEW
 * Brief: Provide the function of the LAD0022LISTVIEW layout
 *
 * Create Date: 01/22/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

/**
 * Represents a layout definition.
 */
public class LAD0022LISTVIEW extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0022_listview;
    
    private static final int BUTTON_CONTAINER = R.id.lad0022_tableLayout;
    
    // Add to support UIAutomator
    private int mButtonIndex = 0;

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
    public LAD0022LISTVIEW(Activity activity)
    {
        super(activity);
        mContext = activity;
        mListView = (ListView) findViewById(R.id.listView2);
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
     * Adds button divider.
     */
    public void addButtonDivider()
    {
        // TODO check divider (layout, margin, width)
        ViewGroup buttonContainer = findContainerById(getButtonContainer());
        View tr = getLayoutInflater().inflate(R.layout.lad22_divider,
                buttonContainer, false);
        buttonContainer.addView(tr);
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void addSpacing()
    {
        // TODO check divider (layout, margin, width)
        ViewGroup buttonContainer = findContainerById(getButtonContainer());
        View tr = getLayoutInflater().inflate(R.layout.button_spacing26px,
                buttonContainer, false);
        buttonContainer.addView(tr);
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void add6pxSpacing()
    {
        // TODO check divider (layout, margin, width)
        ViewGroup buttonContainer = findContainerById(getButtonContainer());
        View tr = getLayoutInflater().inflate(R.layout.button_spacing6px,
                buttonContainer, false);
        buttonContainer.addView(tr);
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(ButtonBasic... buttons)
    {
        Activity activity = getActivity();
        ViewGroup buttonContainer = findContainerById(getButtonContainer());
        for (ButtonBasic button : buttons)
        {
            // TODO: specify bottom margin for table rows (or list items) in
            // layout XML file,
            // remove dynamic spacing.
            // Modified by Henry Tso to support UIAutomator
            buttonContainer.addView(button.createView(activity, null,
                    mButtonIndex++));

        }

    }

    /**
     * Adds Instruction Text.
     * 
     * @param instructionTextId
     *            : Instruction string id
     */
    public void addInstruction(int instructionTextId)
    {
        // TODO: add container in layout (FrameLayout) with 19px Margin
        Activity activity = getActivity();
        ViewGroup buttonContainer = (ViewGroup) activity
                .findViewById(getButtonContainer());
        LayoutInflater inflater = activity.getLayoutInflater();
        ViewGroup instruction = (ViewGroup) inflater.inflate(
                R.layout.instruction, buttonContainer, false);
        UIHelper.setText(instruction, R.id.instruction_text, instructionTextId);
        View margin = new View(activity);
        margin.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, 19));
        buttonContainer.addView(margin);
        buttonContainer.addView(instruction);

    }

    /**
     * Gets the id for the button container View
     * 
     * @return The button container View
     */
    protected static int getButtonContainer()
    {
        return BUTTON_CONTAINER;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
