/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Debuncer
 * Brief: This class is used to do debounce process
 *
 * Create Date: 10/27/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.util;

import android.content.Context;

public class Debouncer extends AbstractTimeoutTask
{
    
    // Control key debounce status
    private volatile boolean mIsDebouncer = false;
    
    // Used to do lock debounce status.
    private final Object lock = new Object();

    /**
     * Custom Debuncer Constructor with Context parameter.
     * @param context [in]
     */
    public Debouncer(Context context)
    {
        super(context);
        
    }
    
    /**
     * 
     * The interface of the enable debounce function
     *
     */
    public void enableDebounce()
    {
        synchronized (lock)
        {
            mIsDebouncer = true;
        }
       
    }
    
    /**
     * 
     * The interface of the get debounce status function.
     *
     * @return boolean [out]:
     */
    public boolean getStatus()
    {
        synchronized (lock)
        {
            return mIsDebouncer;
        }
                  
    }

    /**
     * 
     * This function is invoked when the debounce timer is achieved.
     *
     */
    @Override
    public void onFinish()
    {
        //disable debounce
        mIsDebouncer = false;
        
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// 存取被拒。
// 存取被拒。
// 存取被拒。
// 存取被拒。
