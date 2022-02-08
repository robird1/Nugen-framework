/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IPOSTResultListener
 * Brief: Do callback function when finish POST's flow.  
 *
 * Create Date: 10/06/2015
 * $Revision: 21298 $
 * $Author: JamesLee $
 * $Id: IPOSTResultListener.aidl 21298 2015-10-12 03:04:06Z JamesLee $
 */
 
package com.accu_chek.solo_m.rcapp.application.selftest;

interface IPOSTResultListener
{
    void onFinish();
}