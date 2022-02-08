/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractSingleImageView
 * Brief: Provide the interface of the AbstractSingleImageView UI
 *
 * Create Date: 12/28/2014
 * $Revision: 24226 $
 * $Author: AdamChen $
 * $Id: AbstractSingleImageView.java 24226 2015-11-17 02:51:17Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import java.util.HashMap;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.View;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0013;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0014;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LayoutBase;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public abstract class AbstractSingleImageView implements IActionBar, IFooterButton, ILayoutInit
{
    private static final int LAYOUT_ID = R.layout.lad0014;
    private static final int LAYOUT_ID_WITH_FOOTER_BUTTON = R.layout.lad0013;
    private LAD0013 mLayout = null;
    
    private View.OnClickListener mHandler = new View.OnClickListener() 
    {
        @Override
        public void onClick(View v) 
        {
            onImageClicked();
        }
    };
    
    private HashMap<Integer, Integer> mLayoutIdMap = new HashMap<Integer, Integer>();
    {
        int layoutIdNoButton = LAYOUT_ID;
        int layoutIdWithButton = LAYOUT_ID_WITH_FOOTER_BUTTON;

        mLayoutIdMap.put(0, layoutIdNoButton);
        mLayoutIdMap.put(1, layoutIdWithButton);
        mLayoutIdMap.put(2, layoutIdWithButton);
    }

    
    protected ActionBarActivity mActivity = null;
    
    public AbstractSingleImageView() {
		super();
		// TODO Auto-generated constructor stub
	}

	public AbstractSingleImageView(ActionBarActivity context)
    {
    	mActivity = context;
    }
    
    @Override
    public void setupLayout(ActionBarActivity activity, Bundle data)
    {

        int actionBarTextId = getActionBarText();
        int actionBarIconId = getActionBarIcon();
        int footerButtonNumber = getFooterButtonNumber();
        int imageResourceId = getContentImage();
        int textResourceId = getContentText();
        int footerBtnColorId = onFooterBtnColorId();
        int layoutId = mLayoutIdMap.get(footerButtonNumber);
                
        if (layoutId == LAYOUT_ID_WITH_FOOTER_BUTTON)
        {
            mLayout = new LAD0013(activity);
            mLayout.setup(imageResourceId, textResourceId);
            
            switch (footerButtonNumber)
            {
            case 1:
                int buttonId = getFooterButtonTextId();
                
                if (footerBtnColorId != 0)
                {
                    mLayout.setupLowerActionBarColor(buttonId, footerBtnColorId, this);
                }
                else
                {
                    mLayout.setupLowerActionBar(buttonId, this);
                }

                break;
            case 2:
                int leftButtonId = getFooterLeftButtonTextId();
                int rightButtonId = getFooterRightButtonTextId();
                
                mLayout.setupLowerActionBar(leftButtonId, rightButtonId, this);
                
                break;
                
            default:
                throw new IllegalArgumentException("the input footer button number is invalid!");
            }
        }
        else
        {
            LAD0014 layout = new LAD0014(activity);
            layout.setup(imageResourceId, textResourceId, mHandler);
        }
                
        // Added by Henry Tso. To support UIAutomator
        TopActionBar.setScreenId(getScreenId());
        TopActionBar.setup(activity, actionBarTextId, actionBarIconId);
    }
    
    @Override
	public void updateLayout(ActionBarActivity activity, Bundle data, int requestCode) 
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
    
    @Override
    public int getFooterButtonNumber()
    {
        return 1;
    }

    @Override
    public int getFooterButtonTextId()
    {
        return R.string.txt_labelnext;
    }

    @Override
    public int getFooterLeftButtonTextId()
    {
        return R.string.txt_help;
    }

    @Override
    public int getFooterRightButtonTextId()
    {
        return R.string.txt_labeldone;
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
    public void onFooterButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

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
    public void onBackPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

    }

    @Override
    public void onHomePressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */

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
    public void onInsulinConfirmationPressed(ActionBarActivity activity)
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
    
    public int onFooterBtnColorId()
    {
        // override by subclasses
        return 0;
    }
    
    protected LayoutBase getLayout()
    {
        return mLayout;
    }
        
    protected abstract int getContentImage();
    protected abstract int getContentText();
    protected abstract void onImageClicked();

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// (R14481 2015-08-08 09:09:17 HenryTso)
// ----------------------------------------------------------------------------
// [Fixed NSIQ-107]
// (R16209 2015-09-02 23:46:35 SteveSu)
// ----------------------------------------------------------------------------
// [Fixed NSIQ-107]
