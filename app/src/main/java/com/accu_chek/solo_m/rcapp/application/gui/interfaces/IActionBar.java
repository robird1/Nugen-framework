/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IActionBar
 * Brief: Provide the interface of the IActionBar
 *
 * Create Date: 12/22/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: IActionBar.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

public interface IActionBar
{
    
    int getActionBarIconNumber();
    
    int getActionBarIcon();
    
    int getActionBarText();
    
    int getActionBarCustomIcon();
    
    void onActionBarCustomIconClicked();
    
    String getScreenId();
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
