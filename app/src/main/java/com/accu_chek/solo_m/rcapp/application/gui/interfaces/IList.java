/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IList
 * Brief: Provide the interface of the IList
 *
 * Create Date: 01/14/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: IList.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;

import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;

public interface IList
{
    
    int getInstructionTextId();
    
    int getDisableButtonNumber();
    
    ButtonBasic[] getListButtons(Bundle pickerData);

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
