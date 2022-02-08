/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: TimeSyncListener
 * Brief: Provide an interface to callback the time sensitive activity who
 * register to TimeManagementService in the beginning.
 * 
 * Create Date: 05/01/2015
 * $$Revision: 23334 $$
 * $$Author: TerryHsieh $$
 * $$Id: TimeSyncListener.java 23334 2015-11-05 05:56:44Z TerryHsieh $$
 */
package com.accu_chek.solo_m.rcapp.application.timemanagement;

public interface TimeSyncListener
{

    /**
     * A callback override function to add code for pre-process of time
     * synchronization.
     * 
     * @param void [in]
     * 
     * @return void [out]
     */
    public void onBeforeTimeSyncEvent();

    /**
     * A callback override function to add code for post-process of time
     * synchronization.
     * 
     * @param void [in]
     * 
     * @return void [out]
     */
    public void onAfterTimeSyncEvent();
    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
