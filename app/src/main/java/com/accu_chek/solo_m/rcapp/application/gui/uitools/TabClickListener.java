/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: TabClickListener
 * Brief: Provide the interface function of the TabClickListener UI component
 *
 * Create Date: 10/16/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: TabClickListener.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.view.View;
import android.view.View.OnClickListener;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class TabClickListener implements OnClickListener
{
    
    CollectionPageAdapter mAdapter = null;
    
    /**
     * 
     * @param adapter
     */
    public TabClickListener(CollectionPageAdapter adapter)
    {
        mAdapter = adapter;
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
        int viewId = v.getId();
        
        if (viewId == R.id.id_tab2_relative_left)
        {
            mAdapter.setLeftActive(true);
        }
        else if (viewId == R.id.id_tab2_relative_right)
        {
            mAdapter.setLeftActive(false);
        }
        else
        {
            // do nothing
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
