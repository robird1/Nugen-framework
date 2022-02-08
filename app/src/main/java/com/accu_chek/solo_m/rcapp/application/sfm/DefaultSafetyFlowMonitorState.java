/*
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 */
/*
 * Class name: DefaultSafetyFlowMonitorState
 * Brief:
 * Create Date: 05/14/2015
 * $Workfile: DefaultFlowMonitorState.java
 * $Revision: 1
 * $Author: James Lee
 */

package com.accu_chek.solo_m.rcapp.application.sfm;

import android.content.Context;

public class DefaultSafetyFlowMonitorState implements ISafetyFlowMonitorState
{


     // The monitor id that represent this monitor sequence state.
    private int mMonitorId;

    /**
     * Status: FDD/Coding
     * 
     * Initial the monitor id .
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @param nMonitorId [in] the monitor id that represent this monitor state
     *            Range: 0 - 2^31-1
     *            Unit: int
     *            Scaling: 1
     */
    public DefaultSafetyFlowMonitorState(Context context, int nMonitorId)
    {

        mMonitorId = nMonitorId;
    }

    /**
     * Status: FDD/Coding
     * 
     * Return the Monitor id represent the correspond monitor sequence state.
     * 
     * @return int [out] he monitor id represent the correspond sequence flow
     *         state. the return value is HammingDistance id. 
     *         Range: 0 - 2^31-1
     *         Unit: int
     *         Scaling: 1
     */
    @Override
    public int getMonitorId()
    {
        return mMonitorId;
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */