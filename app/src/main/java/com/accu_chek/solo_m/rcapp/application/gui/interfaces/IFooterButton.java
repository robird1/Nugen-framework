/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.demo.nugendemo.IFooterButton
 * Brief: Provide the interface of the IFooterButton
 *
 * Create Date: 2014/12/28
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: IFooterButton.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

public interface IFooterButton
{
    
    int getFooterButtonNumber();
    
    int getFooterButtonTextId();
    
    int getFooterLeftButtonTextId();
    
    int getFooterRightButtonTextId();
    
    int getFooterMiddleButtonIconId(); 
    
    void onFooterButtonClicked();
    
    void onFooterLeftButtonClicked();
    
    void onFooterRightButtonClicked();
    
    void onFooterMiddleButtonClicked();

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
