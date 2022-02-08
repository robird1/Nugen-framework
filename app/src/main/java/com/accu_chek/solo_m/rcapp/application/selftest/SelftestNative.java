/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SelftestNative
 * Brief: Get CPU test result and get insulin's key state from JNI interface.
 *
 * Create Date: 07/02/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.selftest;

public class SelftestNative
{
    
    private static SelftestNative mInstance = null;

    /**
     * Get the instance object of the SelftestNative
     *
     * @see mInstance [out]
     * 
     * @return SelftestNative [out] the instance object.
     *         Range :valid POST object Unit:
     *         SelftestNative
     *         Scaling: 1
     * 
     */
    public static synchronized SelftestNative getInstance()
    {
        if (null == mInstance)
        {
            mInstance = new SelftestNative();
        }
        else
        {
            // Apply to the coding standard
        }

        return mInstance;
    }

    /**
     * Peek insulin key button state
     * 
     * @return int [out] Return the key state of insulin button
     *         Range: -2^31-1 ~ 2^31-1
     *         Unit: int
     *         Scaling:1
     */
    public static native int peekInsulinKeyState();

    /**
     * To do ALU test callback implement Challenge.ICPUTest
     * 
     * @param paraAdd
     *            [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * 
     * @param paraSub
     *            [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * 
     * @param paraMul
     *            [in] : ALU parameter.
     *            Range : -2^31-1 ~ 2^31-1
     *            Unit:SafetyChannel<Integer>
     *            Scaling:1
     * 
     * @return int [out] Return calculated result of ALU
     *         Range: -2^31-1 ~ 2^31-1
     *         Unit: SafetyChannel<Integer>
     *         Scaling:1
     */
    public static native int cpuTest(final int paraAdd, final int paraSub, final int paraMul);

}
