/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LowerActionBarClickListener
 * Brief: Provide the LowerActionBarClickListener function
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LowerActionBarClickListener.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;

import android.view.View;
import android.view.View.OnClickListener;

public class LowerActionBarClickListener implements OnClickListener
{
    
    private static final int ONE_BUTTON_INDEX = 0;
    
    private static final int LEFT_BUTTON_INDEX = 1;
    
    private static final int RIGHT_BUTTON_INDEX = 2;
    
    private IFooterButton mButton = null;
    
    private int mButtonIndex = -1;
    
    /**
     * 
     * @param buttonIndex
     * @param button
     */
    public LowerActionBarClickListener(int buttonIndex, IFooterButton button)
    {
        mButtonIndex = buttonIndex;
        mButton = button;
    }

    /**
     * 
     * 
     *
     * @param v
     */
    @Override
    public void onClick(View v)
    {
        switch (mButtonIndex)
        {
        case ONE_BUTTON_INDEX:
            mButton.onFooterButtonClicked();
            
            break;
        case LEFT_BUTTON_INDEX:
            mButton.onFooterLeftButtonClicked();
            
            break;
        case RIGHT_BUTTON_INDEX:
            mButton.onFooterRightButtonClicked();

            break;
        default:
            // do nothing
            break;
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

