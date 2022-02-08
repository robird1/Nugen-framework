/*
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 */
/*
 * Class name: ISafetyFlowMonitorState
 * Brief:
 * Create Date: 05/14/2015
 * $Workfile: ISafetyFlowMonitorState.java
 * $Revision: 1
 * $Author: James Lee
 */
package com.accu_chek.solo_m.rcapp.application.sfm;

public interface ISafetyFlowMonitorState
{
    /**
     * Status: FDD/Coding
     * 
     * Return the monitor id represent the correspond sequence flow state.
     * 
     * @return int [out] the monitor id represent the correspond sequence flow
     *         state. the return value is HammingDistance id.
     *         Range: 0 - 2^31-1 
     *         Unit: int
     *         Scaling: 1
     */
    public int getMonitorId();
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */