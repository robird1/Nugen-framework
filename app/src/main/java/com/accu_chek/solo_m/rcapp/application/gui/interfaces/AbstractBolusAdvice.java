/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractBolusAdvice
 * Brief: Provide the interface of the AbstractBolusAdvice UI
 *
 * Create Date: 12/26/2014
 * $Revision: 24474 $
 * $Author: AdamChen $
 * $Id: AbstractBolusAdvice.java 24474 2015-11-20 03:16:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.View;
import android.view.View.OnClickListener;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B24;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B25;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B26;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0023;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractBolusAdvice implements IActionBar, IFooterButton, ILayoutInit
{
    
	private static final int LOWER_BUTTON_ID = R.id.btn_1;
	
	private Button_B26 mButton_bGInsulin = null;
    
	private Button_B26 button_carbInsulin = null;
    
    private BottomButtonListener mButtonListener =  null;
	
    /**
     * 
     */
    public AbstractBolusAdvice()
    {
    	
    }
    
    /**
     * 
     * @param context
     */
    public AbstractBolusAdvice(ActionBarActivity context)
    {
    	mButtonListener = new BottomButtonListener();
    }
    
    /**
     * 
     * 
     *
     * @param activity
     * @param pickerData
     */
    @Override
    public final void setupLayout(ActionBarActivity activity, Bundle pickerData)
    {
    	View groupView = null;
    	View lowerButtonView = null;
        int actionBarTextId = getActionBarText();
        int actionBarIconId = getActionBarIcon();
        int footerButtonTextId = getFooterButtonTextId();              
        int bgValue = getBgValue(); 
        int carbValue = getCarbValue(); 
        float activeInsulinValue = getActiveInsulinValue(); 
        String bGInsulin = getBgInsulin(pickerData);
        String carbInsulin = getCarbInsulin(pickerData);
        float totalInsulinValue = getTotalInsulinValue(pickerData); 
        int insulinTypeIconId = getInsulinIconId(pickerData);
        int insulinTypeId = getInsulinTypeId(pickerData);
        int instructionTextId = getInstructionTextId();
        
        View.OnClickListener bGInsulinButtonHandler = getBGInsulinButtonHandler();
        View.OnClickListener carbInsulinButtonHandler = getCarbInsulinButtonHandler();
        View.OnClickListener totalInsulinButtonHandler = getTotalInsulinButtonHandler();
        View.OnClickListener bolusTypeButtonHandler = getbolusTypeButtonHandler();
        
        LAD0023 layout = new LAD0023(activity);        
        
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);
        
        layout.setBgValue(bgValue + " mg/dL");
        layout.setActiveInsulin(activeInsulinValue);
        layout.setCarbsHeadline(R.string.txt_labelcarbs);
        layout.setCarbsValue(carbValue);
        
        mButton_bGInsulin = new Button_B26(bGInsulin);
        mButton_bGInsulin.setListener(bGInsulinButtonHandler);
        layout.setBGInsulin(mButton_bGInsulin);

        button_carbInsulin = new Button_B26(carbInsulin);
        button_carbInsulin.setListener(carbInsulinButtonHandler);
        layout.setCarbsInsuin(button_carbInsulin);
        
        layout.setInsulinToltal(new Button_B25(R.string.txt_totalamount,
                (double) totalInsulinValue).setListener(totalInsulinButtonHandler));
        
        layout.setInsulinType(new Button_B24(insulinTypeIconId,
                insulinTypeId).setListener(bolusTypeButtonHandler));

        if (instructionTextId != 0)
        {
            String instructionText = activity.getResources().getString(
                    instructionTextId);
            int healthPercentage = getHealthPercentage();
            
            layout.setInstructionTxt(instructionText + " " + healthPercentage + "%");

        }        
        
        groupView = layout.setupLowerActionBar(footerButtonTextId);
        lowerButtonView = groupView.findViewById(LOWER_BUTTON_ID);
        lowerButtonView.setOnClickListener(mButtonListener);
        layout.setup();        
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
    public int getFooterButtonTextId()
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
    public void onFooterButtonClicked()
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
    public int getActionBarIconNumber()
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
    public int getActionBarIcon()
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
    public int getActionBarText()
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
    
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onStart()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onResume()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onPause()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onStop()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }

    @Override
    public void onDestroy()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
    }
        
    protected abstract int getBgValue();
    
    protected abstract int getCarbValue();
    
    protected abstract float getActiveInsulinValue();
    
    protected abstract String getBgInsulin(Bundle pickerData);
    
    protected abstract String getCarbInsulin(Bundle pickerData);
    
    protected abstract float getTotalInsulinValue(Bundle pickerData);   
    
    protected abstract int getInsulinIconId(Bundle pickerData);
    
    protected abstract int getInsulinTypeId(Bundle pickerData);
    
    protected abstract int getInstructionTextId();
    
    protected abstract int getHealthPercentage();
    
    protected abstract View.OnClickListener getBGInsulinButtonHandler();
    
    protected abstract View.OnClickListener getCarbInsulinButtonHandler();
    
    protected abstract View.OnClickListener getTotalInsulinButtonHandler();
    
    protected abstract View.OnClickListener getbolusTypeButtonHandler();
    
    /**
     * 
     */
    private class BottomButtonListener implements OnClickListener
    {

        /**
         * 
         * 
         *
         * @param v
         */
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
