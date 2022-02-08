/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: FragmentChangeListener
 * Brief: The listener interface of the fragment change
 *
 * Create Date: 05/27/2015
 * $Revision: 22792 $
 * $Author: AdamChen $
 * $Id: FragmentChangeListener.java 22792 2015-10-29 02:56:04Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.globaltools;

public interface FragmentChangeListener
{
    // This function is invoked when fragment change
    public void onFragmentChanged(int position);

    //Set fragment content
    public void setFC();

    //Get fragment content
    public void getFC();
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
