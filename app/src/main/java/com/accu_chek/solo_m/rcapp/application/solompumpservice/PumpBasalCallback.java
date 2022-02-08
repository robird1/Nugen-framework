/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.emwrservice.OnNotifyClickListener
 * Brief: 
 *
 * Create Date: 2015/6/1
 * $Revision: 20513 $
 * $Author: DWYang $
 * $Id: EMWRButtonCallback.java 20513 2015-10-01 10:25:24Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.solompumpservice;

public interface PumpBasalCallback
{
    /**
     * The callback of EMWR button
     *
     * return None
     */
    public void onReturn( boolean result );
}
