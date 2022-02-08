/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IDispatchCommand
 * Brief: Provide an interface after complete dispatch command.
 *
 * Create Date: 10/06/2015
 * $Revision: 21298 $
 * $Author: JamesLee $
 * $Id: ISelftestListener.java 21298 2015-10-12 03:04:06Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.flight;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public interface IDispatchCommand
{
    
    /**
     * If finished to dispatch command , do this method.
     * 
     * @param isResultOK [in] Set the result is okay or not.
     *            Range: Valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling:1
     * 
     * @return None
     */
    void onCompleted(SafetyBoolean isResultOK);

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Commit for setting the SVN keyword
// Add file header and footer comment block.
