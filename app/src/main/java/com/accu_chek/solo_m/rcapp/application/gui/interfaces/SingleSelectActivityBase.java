/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SingleSelectActivityBase
 * Brief: The Activity class provide single select function
 *
 * Create Date: 02/04/2015
 * $Revision: 25086 $
 * $Author: AdamChen $
 * $Id: SingleSelectActivityBase.java 25086 2015-11-30 06:31:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.view.View;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.gui.buildingblocks.SoloMActionBarActivity;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B13;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B2;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0007b;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.util.ArrayList;

public abstract class SingleSelectActivityBase extends SoloMActionBarActivity
{

    public static final String KEY_VALUE = "SelectedValue";
    
    public static final String KEY_RESOURCE_ID = "resource_id";
   
    private static final String TAG = "SingleSelectActivityBase";
   
    protected LAD0007b mLayout = null;
    
    private int selectedIndex = -1;
       
    private Button_B2 mButton_B2 = null;
       
    // private SparseArray<ButtonBasic> options = new
    // SparseArray<ButtonBasic>();
    
    private ArrayList<ButtonBasic> mList = new ArrayList<ButtonBasic>();

    /**
     * 
     */
    public SingleSelectActivityBase()
    {
        super();
    }

    /**
     * 
     * Function Description
     *
     * @param btnTextId
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getButtonIndex(int btnTextId)
    {
        int index = 0;
        int textId = 0;

        int size = mList.size();
        
        for (int i = 0; i < size; i++)
        {
            textId = mList.get(i).getTextId();
            
            if (btnTextId == textId)
            {
                index = i;
            }
        }

        return index;
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onBackPressed()
    {
        // Intent intent = getIntent();
        // this.setResult(RESULT_OK, intent);
        this.finish();
    }
    
    /**
     * 
     * Function Description
     *
     * @param view
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void b13_click(View view)
    {
        onSelectionChanged(view);
    }

    /**
     * 
     * Function Description
     *
     * @param view
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void b2_click(View view)
    {
        onSelectionChanged(view);
    }
    
    /**
     * 
     * Function Description
     *
     * @param view
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void lowerClick_btn1(View view)
    {

        int index = getSelectedIndex();

        if (index != -1)
        {
            goToBackPreviousUI();
        }
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getSelectedValueId()
    {
        // return options.keyAt(selectedIndex);
        return mList.get(selectedIndex).getTextId();
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
        int btnIndex = 0;
        super.onCreate(savedInstanceState);

        createOptions();
        // set layout
        mLayout = new LAD0007b(this);
        mLayout.addButtons(getOptions());

        setupTopActionBar();
        setupLowerActionBar();

        // modify by Steve {

        btnIndex = getSelectedBtnIndex();

        setSelectedIndex(btnIndex);
        refreshView();

        // } modify by Steve

    }

    protected abstract void setupTopActionBar();

    protected abstract void createOptions();

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setupLowerActionBar()
    {
        mLayout.setupLowerActionBar(R.string.txt_labeldone);
    }

    /**
     * 
     * Function Description
     *
     * @param optionKeys
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void createOptions(int... optionKeys)
    {
        for (int optionKey : optionKeys)
        {
            createOption(optionKey);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param optionKey
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void createOption(int optionKey)
    {
        // options.put(optionKey, createButton(optionKey));
        mList.add(createButton(optionKey));
    }
    
    /**
     * 
     * Function Description
     *
     * @param optionKey
     * @return
     * @return Button_B2 [out] Delete pre line return if exist. Parameter Description
     */
    protected Button_B2 createButton(int optionKey)
    {
        mButton_B2 = new Button_B2(optionKey, true);
        
        return mButton_B2;
    }

    /**
     * 
     * Function Description
     *
     * @param textId
     * @param iconId
     * @param isRadio
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void createOptionsForBolusType(int textId, int iconId,
            boolean isRadio)
    {
        Button_B13 button = new Button_B13(textId, iconId, isRadio, null);

        // options.put(textId, button);
        mList.add(button);
    }

    /**
     * 
     * Function Description
     *
     * @param view
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void onSelectionChanged(View view)
    {
        selectedIndex = mLayout.indexOf(view);
        refreshView();
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void refreshView()
    {
        // for (int i = 0; i < options.size(); i++) {
        // ButtonBasic btn = options.valueAt(i);
        // btn.setSelected(i == selectedIndex);
        // }
        int size = mList.size();
        
        for (int i = 0; i < size; i++)
        {
            mList.get(i).setSelected(i == selectedIndex);
            ;
        }
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected int getSelectedIndex()
    {
        return selectedIndex;
    }

    /**
     * 
     * Function Description
     *
     * @param selectedIndex
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setSelectedIndex(int selectedIndex)
    {
        Debug.printI(TAG, "[setSelectedIndex]: selectedIndex = "
                + selectedIndex);
        this.selectedIndex = selectedIndex;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return LAD0007b [out] Delete pre line return if exist. Parameter Description
     */
    protected LAD0007b getLayout()
    {
        return mLayout;
    }


    /**
     * 
     * Function Description
     *
     * @return
     * @return ButtonBasic[] [out] Delete pre line return if exist. Parameter Description
     */
    protected ButtonBasic[] getOptions()
    {
        ButtonBasic[] button = new ButtonBasic[mList.size()];
        button = mList.toArray(button);

        return button;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected int getDefaultValue()
    {
        return 0;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected int getDefaultResourceId()
    {
        return -1;
    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void goToBackPreviousUI()
    {
        Debug.printI(TAG, "[goToBackPreviousUI]: enter");

        Bundle bundle = getIntent().getBundleExtra(
                NugenFrameworkConstants.KEY_BUNDLE_DATA_FOR_UPDATE_ACTIVITY);

        // Modify by Henry Tso. Change to return item index.
        bundle.putInt(KEY_VALUE, getSelectedIndex());

        // add by Steve
        bundle.putInt(KEY_RESOURCE_ID, getSelectedValueId());

        getIntent().putExtras(bundle);

        this.setResult(RESULT_OK, getIntent());
        finish();
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    private int getSelectedBtnIndex()
    {
        int btnIndex = 0;
        Bundle bundle = getIntent().getBundleExtra(
                NugenFrameworkConstants.KEY_BUNDLE_DATA_FOR_UPDATE_ACTIVITY);

        if (bundle != null)
        {
            int buttonTextId = -1;

            btnIndex = bundle.getInt(KEY_VALUE, getDefaultValue());

            buttonTextId = bundle.getInt(KEY_RESOURCE_ID,
                    getDefaultResourceId());

            // use the button index based on the provided button text ID
            if (buttonTextId != -1)
            {
                btnIndex = getButtonIndex(buttonTextId);
            }

        }
        else
        {
            int buttonTextId = getDefaultResourceId();

            if (buttonTextId != -1) // use the button index provided by subclass
            {
                btnIndex = getButtonIndex(getDefaultResourceId());
            }
            else
            {
                btnIndex = 0;
            }
        }
        return btnIndex;
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// [Setting] add MDI mode
// [Setting] add function comment
// [Reminder] add Reminder module
// [Reminder] add Reminder module
// [Reminder] add Reminder module
