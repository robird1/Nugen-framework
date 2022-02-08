/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: NuGenFragmentActivity
 * Brief: The base Activity class of the all RCApp UI Activities.
 *
 * Create Date: 02/04/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: NuGenFragmentActivity.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.support.v4.app.FragmentActivity;
import android.view.KeyEvent;

public class NuGenFragmentActivity extends FragmentActivity
{

    /**
     * 
     * 
     *
     * @param keyCode
     * @param event
     * @return
     */
    @Override
    public final boolean onKeyUp(int keyCode, KeyEvent event)
    {
        boolean retValue;
        switch (keyCode)
        {
        case KeyEvent.KEYCODE_BACK:
            onBackPressed();
            retValue = true;
            break;
        case KeyEvent.KEYCODE_F1:
            onHomePressed();
            retValue = true;
            break;
        case KeyEvent.KEYCODE_F2:
            onNextPressed();
            retValue = true;
            break;
        case KeyEvent.KEYCODE_F10:
            onInsulinConfirmationPressed();
            retValue = true;
            break;
        default:
            retValue = super.onKeyUp(keyCode, event);
            break;
        }
        return retValue;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void onHomePressed()
    {

    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void onNextPressed()
    {

    }
    
    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void onInsulinConfirmationPressed()
    {

    }
    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */