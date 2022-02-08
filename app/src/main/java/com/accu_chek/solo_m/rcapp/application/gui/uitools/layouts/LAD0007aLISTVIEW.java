/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0007aLISTVIEW
 * Brief: Provide the function of the LAD0007aLISTVIEW layout
 *
 * Create Date: 01/22/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import java.util.Arrays;
import java.util.Collection;

import android.app.Activity;
import android.content.Context;
import android.util.SparseArray;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

/**
 * Represents a layout definition.
 */
public class LAD0007aLISTVIEW extends LayoutBase
{

    private static final int FACTOR = 2;

    private static final int LAYOUT_ID = R.layout.lad0007a_listview;
    
    private static final int BUTTON_CONTAINER = R.id.lad007_tableLayout;
    
    private ViewGroup mButtonContainer = null;
    
    private boolean isFirstButton = true;
    
    private int mButtonIndex = 0; // Added by Henry Tso to support UIAutomator

    private ListView mListView = null;
    
    private Context mContext = null;

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
     * Constructor. The {@code Activity.setContentView} method is called with
     * the layout id as parameter
     * 
     * @param activity
     *            : The underlying activity
     * 
     */
    public LAD0007aLISTVIEW(Activity activity)
    {
        super(activity);

        mContext = activity;
        mListView = (ListView) findViewById(R.id.listView2);
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

    /**
     * Gets the id for the button container View
     * 
     * @return The button container View
     */
    protected int getButtonContainerId()
    {
        return BUTTON_CONTAINER;
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
     * Adds button divider.
     */
    public void addButtonDivider()
    {
        Activity activity = getActivity();
        ViewGroup buttonContainer = getButtonContainer();
        View tr = activity.getLayoutInflater().inflate(R.layout.divider,
                buttonContainer, false);
        buttonContainer.addView(tr);
        // first Button after divider.
        isFirstButton = true;
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(ButtonBasic... buttons)
    {
        addButtons(Arrays.asList(buttons));
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(Collection<? extends ButtonBasic> buttons)
    {
        Activity activity = getActivity();
        ViewGroup buttonContainer = getButtonContainer();

        for (ButtonBasic button : buttons)
        {
            // TODO: specify bottom margin for table rows (or list items) in
            // layout XML file,
            // remove dynamic spacing.
            if (isFirstButton)
                isFirstButton = false;
            else
            {
                View space = activity.getLayoutInflater().inflate(
                        R.layout.button_spacing6px, buttonContainer, false);
                buttonContainer.addView(space);
            }
            // Modified by Henry Tso to support UIAutomator
            buttonContainer.addView(button.createView(activity, null,
                    mButtonIndex++));
        }

    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(SparseArray<? extends ButtonBasic> buttons)
    {
        Activity activity = getActivity();
        ViewGroup buttonContainer = getButtonContainer();
        int size = buttons.size();
        
        for (int i = 0; i < size; i++)
        {
            // TODO: specify bottom margin for table rows (or list items) in
            // layout XML file,
            // remove dynamic spacing.
            if (isFirstButton)
                isFirstButton = false;
            else
            {
                View space = activity.getLayoutInflater().inflate(
                        R.layout.button_spacing6px, buttonContainer, false);
                buttonContainer.addView(space);
            }
            // Modified by Henry Tso to support UIAutomator
            buttonContainer.addView(buttons.valueAt(i).createView(activity,
                    null, mButtonIndex++));
        }

    }

    // modified by Steve {
    /**
     * 
     * Function Description
     *
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    public ViewGroup getButtonContainer()
    {
        if (mButtonContainer == null)
        {
            mButtonContainer = findContainerById(getButtonContainerId());
        }
        
        return mButtonContainer;
    }
    // } modified by Steve

    /**
     * 
     * Function Description
     *
     * @param view
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int indexOf(View view)
    {
        ViewGroup buttonContainer = getButtonContainer();
        // remove spacing
        return (buttonContainer.indexOfChild((View) view.getParent())) / FACTOR;
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
        ViewGroup buttonContainer = getButtonContainer();
        LayoutInflater inflater = activity.getLayoutInflater();
        ViewGroup instruction = (ViewGroup) inflater.inflate(
                R.layout.instruction, buttonContainer, false);
        UIHelper.setText(instruction, R.id.instruction_text, instructionTextId);
        View margin = new View(activity);
        margin.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, 19));
        buttonContainer.addView(margin);
        buttonContainer.addView(instruction);

    }

}


// [GUI] update GUI framework to ClickThrough v0.34
