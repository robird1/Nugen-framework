/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: UIBuilder
 * Brief: Provide the interface function of the UIBuilder UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: UIBuilder.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.ViewGroup;

public interface UIBuilder
{

    public void build(ViewGroup group);

    public ViewGroup add(ViewGroup group);
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */