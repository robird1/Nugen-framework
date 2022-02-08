/** 
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ILayoutInit
 * Brief: Provide the interface of the ILayoutInit
 *
 * Create Date: 12/29/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: ILayoutInit.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

public interface ILayoutInit
{
    
    void setupLayout(ActionBarActivity activity, Bundle data);
    
    void updateLayout(ActionBarActivity activity, Bundle data, int requestCode);
    
    void onBackPressed(ActionBarActivity activity);
    
    void onHomePressed(ActionBarActivity activity);
    
    void onNextPressed(ActionBarActivity activity);
    
    void onInsulinConfirmationPressed(ActionBarActivity activity);
    
    void onCreate(Bundle savedInstanceState);  
    
    void onStart();
    
    void onResume();
    
    void onPause();
    
    void onStop();
    
    void onDestroy();

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
