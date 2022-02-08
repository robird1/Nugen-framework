/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: GeneralAbstractList
 * Brief: Provide the interface of the GeneralAbstractList UI
 *
 * Create Date: 12/25/2014
 * $Revision: 25086 $
 * $Author: AdamChen $
 * $Id: GeneralAbstractList.java 25086 2015-11-30 06:31:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0007b;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class GeneralAbstractList implements IActionBar, IFooterButton, IList, ILayoutInit
{
    
    protected ActionBarActivity mActivity = null;
    
    private static final int ONE_BUTTON = 1;
    
    private static final int TWO_BUTTON = 2;
    
    private static final int THREE_BUTTON = 3;
    
    private LAD0007b mLayout = null;
    
    private ButtonBasic[] mButtons = null;
    
    /**
     * 
     */
    public GeneralAbstractList() 
    {
		super();
	}

    /**
     * 
     * @param context
     */
	public GeneralAbstractList(final ActionBarActivity context)
    {
    	mActivity = context;
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
        int instructionTextId = 0;
        mLayout = new LAD0007b(activity);        
        mButtons = getListButtons(data);
               
        // Modified by Henry Tso. It is possible equal to null. (Nothing items
        // on the list view
        if (mButtons != null)
        {
            mLayout.addButtons(mButtons);
        }

        instructionTextId = getInstructionTextId();
        
        if (instructionTextId != 0)
        {
            mLayout.addInstruction(instructionTextId);
        }
        
        switch (getFooterButtonNumber())
        {
        case ONE_BUTTON:
            int buttonTextId = getFooterButtonTextId();              

            mLayout.setupLowerActionBar(buttonTextId, this);
            
            break;
        case TWO_BUTTON:
            int buttonLeftTextId = getFooterLeftButtonTextId();              
            int buttonRightTextId = getFooterRightButtonTextId();              

            mLayout.setupLowerActionBar(buttonLeftTextId, buttonRightTextId, this);
            
            break;
        case THREE_BUTTON:
            break;
        default:
            mLayout.setLowerActionBarEnabled(false);
            break;
        }
        
    }
    
    protected ActionBarActivity getContext()
    {
        return mActivity;
    }
    
    protected LAD0007b getLayout()
    {
        return mLayout;
    }
    
    /**
     * 
     * Add a specified button at the end of the list view. 
     * 
     * @param button : the button object to be added
     *            Range: valid object
     *            Unit: ButtonBasic
     *            Scaling: 1
     *            
     * @return None
     * 
     * @see mButtons
     * @see mLayout
     */
    protected void addButton(ButtonBasic button)
    {
        ButtonBasic[] temp = new ButtonBasic[mButtons.length + 1]; 
        
        CommonUtils.objectCheck(button);
        
        for (int i = 0; i < mButtons.length; i++)
        {
            temp[i] = mButtons[i];
        }
        
        temp[mButtons.length] = button;
        
        mButtons = temp;
                
        mLayout.addButtons(button);
    }
    
    /**
     * 
     * Delete a button at the specified index of the list view. 
     * 
     * @param index : button index on the list view
     *            Range: 0, 1, 2, ..., (max button number - 1)
     *            Unit: int
     *            Scaling: 1
     *            
     * @return None
     * 
     * @see mButtons
     * @see mLayout
     */
    protected void deleteButton(int index)
    {
        if (index < mButtons.length)
        {
            int size = 0;
            ButtonBasic[] temp = new ButtonBasic[mButtons.length - 1];
            List<ButtonBasic> buttonList = new ArrayList<ButtonBasic>();

            mLayout.getButtonContainer().removeAllViews();

            buttonList.addAll(Arrays.asList(mButtons));

            buttonList.remove(index);

            size = buttonList.size();
            
            for (int i = 0; i < size; i++)
            {
                temp[i] = buttonList.get(i);
            }

            mButtons = temp;

            mLayout.addButtons(mButtons);
        }
    }

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
    public void onInsulinConfirmationPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

	@Override
    public void onBackPressed(ActionBarActivity activity)
    {
        mActivity.finish();

    }

    @Override
    public void onNextPressed(ActionBarActivity activity)
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


    @Override
    public int getInstructionTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }


    @Override
    public int getDisableButtonNumber()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    @Override
    public int getFooterButtonNumber()
    {
        return 1;
    }

    @Override
    public int getFooterButtonTextId()
    {
        return R.string.txt_labeldone;
    }

    @Override
    public void onFooterButtonClicked()
    {
        mActivity.finish();
    }

    @Override
    public int getFooterLeftButtonTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }


    @Override
    public int getFooterRightButtonTextId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }


    @Override
    public int getFooterMiddleButtonIconId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }


    @Override
    public void onFooterLeftButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }


    @Override
    public void onFooterRightButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }


    @Override
    public void onFooterMiddleButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }


    @Override
    public int getActionBarIconNumber()
    {
        return 1;
    }


    @Override
    public int getActionBarCustomIcon()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }


    @Override
    public void onActionBarCustomIconClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
        
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// (R14997 2015-08-19 01:09:40 SteveSu)
// ----------------------------------------------------------------------------
// [Fixed NSIQ-41]
