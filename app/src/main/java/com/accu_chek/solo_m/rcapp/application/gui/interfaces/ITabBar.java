/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ITabBar
 * Brief: Provide the interface of the ITabBar
 *
 * Create Date: 12/28/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: ITabBar.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.view.View;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public interface ITabBar
{
    
    SafetyBoolean isTabActionBarEnabled();
    
    int getTabLeftIconID();
    
    int getTabRightIconID();
    
    View.OnClickListener getLeftTabHandler();
    
    View.OnClickListener getRightTabHandler();

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
