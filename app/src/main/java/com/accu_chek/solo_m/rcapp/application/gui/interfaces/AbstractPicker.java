/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractPicker
 * Brief: Provide the interface of the AbstractPicker UI
 *
 * Create Date: 12/26/2014
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractPicker.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.View;
import android.view.View.OnClickListener;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.PickerBase;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0003;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractPicker implements IActionBar, IFooterButton,
        ITabBar, ILayoutInit
{
    
    protected ActionBarActivity mActivity = null;
    
    private PickerBase mPicker = null;
    
    private static final int LOWER_BUTTON_ID = R.id.btn_1;
        
    private Bundle mBundle = new Bundle(); //prevent not need bundle
    
    private BottomButtonListener mButtonListener =  null;

    /**
     * 
     */
    public AbstractPicker() {
		super();
		
	}

    /**
     * 
     * @param context
     */
	public AbstractPicker(ActionBarActivity context)
    {
    	mActivity = context;
    	mButtonListener = new BottomButtonListener();
    }
    
    /**
     * 
     * 
     *
     * @param activity
     * @param data
     */
    @Override
    public void setupLayout(ActionBarActivity activity, Bundle data)
    {
        LAD0003 layout = new LAD0003(activity);
        View groupView = null;
        View lowerButtonView = null;
        int actionBarTextId = getActionBarText();
        int actionBarIconId = getActionBarIcon();
        int buttonTextId = getFooterButtonTextId();              

        CheckBundleData(data);
        
        mPicker = getPicker(mBundle);        

        layout.setup(mPicker);
        
        groupView = layout.setupLowerActionBar(buttonTextId);
        lowerButtonView = groupView.findViewById(LOWER_BUTTON_ID);
        lowerButtonView.setOnClickListener(mButtonListener);
        
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);
                
    }

    /**
     * 
     * 
     *
     * @param activity
     * @param data
     * @param requestCode
     */
    @Override
	public void updateLayout(ActionBarActivity activity, Bundle data, int requestCode) 
    {
		/**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
		
	}
   
    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterButtonNumber()
    {
        return 1;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterButtonTextId()
    {
        return R.string.txt_set;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterLeftButtonTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterRightButtonTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterMiddleButtonIconId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterLeftButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterRightButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterMiddleButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public SafetyBoolean isTabActionBarEnabled()
    {
        return SafetyBoolean.FALSE;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getTabLeftIconID()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getTabRightIconID()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public OnClickListener getLeftTabHandler()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return null;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public OnClickListener getRightTabHandler()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return null;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getActionBarIconNumber()
    {
        return 1;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getActionBarCustomIcon()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onActionBarCustomIconClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     * @param activity
     */
    @Override
    public void onBackPressed(ActionBarActivity activity)
    {
        mActivity.finish();
    }

    /**
     * 
     *
     * @param activity
     */ 
    @Override
    public void onHomePressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     *
     * @param activity
     */    
    @Override
    public void onNextPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     *
     * @param activity
     */   
    @Override
    public void onInsulinConfirmationPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
    
    /**
     * 
     * 
     *
     * @param savedInstanceState
     */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onStart()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onResume()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onPause()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onStop()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onDestroy()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
    
    public abstract PickerBase getPicker(Bundle data);
    
    /**
     * 
     * Function Description
     *
     * @param data
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void CheckBundleData(Bundle data) 
    {
        if(data != null)
        {
            mBundle = data;
        }
    }
    
    /**
     * 
     */
    private class BottomButtonListener implements OnClickListener
    {
        
        @Override
        public void onClick(View v) {
            onFooterButtonClicked(); 
            
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
