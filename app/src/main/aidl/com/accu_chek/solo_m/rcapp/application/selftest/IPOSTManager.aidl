/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IPOSTManager
 * Brief: Provide interfaces for POST flow control  
 *
 * Create Date: 10/06/2015
 * $Revision: 21298 $
 * $Author: JamesLee $
 * $Id: IPOSTManager.aidl 21298 2015-10-12 03:04:06Z JamesLee $
 */
 
package com.accu_chek.solo_m.rcapp.application.selftest;

import com.accu_chek.solo_m.rcapp.application.selftest.IPOSTResultListener;

/**
 * {@hide}
 */
interface IPOSTManager
{
    void initial();
 
    void runTest();
 
    void setCommsReadyState(int commsReadyResult);   

    void registerPOSTResultCB(IPOSTResultListener resultCB);
}

