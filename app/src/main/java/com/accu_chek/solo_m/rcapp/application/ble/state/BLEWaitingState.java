/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.state.BLEWaitingState
 * Brief: BLE control waiting state
 *
 * Create Date: 2015/7/21
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: BLEWaitingState.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

/**
 * This class is only used to wait for other modules to change current state 
 * handling their requests
 * 
 */
public class BLEWaitingState extends AbstractBLEStateHandler
{
    /**
     * The constructor of BLEWaitingState 
     * 
     * @param callbackContext
     *        Range: a valid object of BLECallbackContext
     *        Unit: BLECallbackContext
     *        Scaling: 1
     */
    public BLEWaitingState(BLECallbackContext callbackContext)
    {
        super(callbackContext);
    }
    
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R15818 2015-08-31 04:11:59 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
