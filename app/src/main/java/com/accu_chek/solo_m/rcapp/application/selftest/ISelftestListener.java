/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IselftestListener
 * Brief: Provided POST or Run-Time to set self-test items interface
 *
 * Create Date: 10/06/2015
 * $Revision: 21298 $
 * $Author: JamesLee $
 * $Id: ISelftestListener.java 21298 2015-10-12 03:04:06Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.selftest;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public interface ISelftestListener
{
    
    /**
     * Provide interface to implement self-test items.
     * Both POST and run time self-test will implement this interface.
     *    
     * 
     * @return SafetyBoolean [out] the result of the assigned self-test item
     *         Range :valid SafetyBoolean object
     *         Unit: SafetyBoolean 
     *         Scaling: 1
     */
    SafetyBoolean doAction();

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */