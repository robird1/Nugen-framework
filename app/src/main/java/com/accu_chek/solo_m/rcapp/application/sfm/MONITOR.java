/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: MONITOR
 * Brief: Build a table to record monitoring points of all critical path
 * 
 * Create Date: 2015/08/12
 * $Revision: 25273 $
 * $Author: JamesLee $
 * $Id: MONITOR.java 25273 2015-12-01 12:55:41Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.sfm;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public enum MONITOR
{

    POST_START(HammingDistance.SAFETY_NUMBER_VALUE_0050, SafetyBoolean.FALSE),
    POST_END(HammingDistance.SAFETY_NUMBER_VALUE_0051, SafetyBoolean.TRUE),
    SCR0086_insert_test_strip(HammingDistance.SAFETY_NUMBER_VALUE_0052,
        SafetyBoolean.FALSE),
    SCR0378_waiting_1(HammingDistance.SAFETY_NUMBER_VALUE_0053,
        SafetyBoolean.FALSE),
    SCR0088_apply_blood(HammingDistance.SAFETY_NUMBER_VALUE_0054,
        SafetyBoolean.FALSE),
    SCR0378_waiting_2(HammingDistance.SAFETY_NUMBER_VALUE_0055,
        SafetyBoolean.FALSE),
    SCR0090_bg_result(HammingDistance.SAFETY_NUMBER_VALUE_0056,
        SafetyBoolean.FALSE),
    SCR0204_detailed_bg_result(HammingDistance.SAFETY_NUMBER_VALUE_0057,
        SafetyBoolean.FALSE),
    SCR0043_fitting_steps(HammingDistance.SAFETY_NUMBER_VALUE_0058,
        SafetyBoolean.FALSE),
    SCR0044_picker_reservoir_amount(HammingDistance.SAFETY_NUMBER_VALUE_0059,
        SafetyBoolean.FALSE),
    SCR0045_locating(HammingDistance.SAFETY_NUMBER_VALUE_0060,
        SafetyBoolean.FALSE),
    SCR0053_device_authentication(HammingDistance.SAFETY_NUMBER_VALUE_0061,
        SafetyBoolean.FALSE),
    SCR0295_scan(HammingDistance.SAFETY_NUMBER_VALUE_0062, SafetyBoolean.FALSE),
    SCR0093_warning_15_ready_prime(HammingDistance.SAFETY_NUMBER_VALUE_0063,
        SafetyBoolean.FALSE),
    SCR0056_MP_priming(HammingDistance.SAFETY_NUMBER_VALUE_0064,
        SafetyBoolean.FALSE),
    SCR0052_select_MP(HammingDistance.SAFETY_NUMBER_VALUE_0065,
        SafetyBoolean.FALSE),
    SCR0055_pump_code(HammingDistance.SAFETY_NUMBER_VALUE_0066,
        SafetyBoolean.FALSE),
    SCR0074_replace_part_r(HammingDistance.SAFETY_NUMBER_VALUE_0067,
        SafetyBoolean.FALSE),
    SCR0067_remove_old_parts(HammingDistance.SAFETY_NUMBER_VALUE_0068,
        SafetyBoolean.FALSE),
    SCR0078_fitting_two_steps_only_rp(HammingDistance.SAFETY_NUMBER_VALUE_0069,
        SafetyBoolean.FALSE),
    SCR0081_fitting_one_steps_only_RP(HammingDistance.SAFETY_NUMBER_VALUE_0070,
        SafetyBoolean.FALSE),
    SCR0059_attach_mp_to_i(HammingDistance.SAFETY_NUMBER_VALUE_0071,
        SafetyBoolean.FALSE),
    SCR0282_main_menu_startpump(HammingDistance.SAFETY_NUMBER_VALUE_0072,
        SafetyBoolean.FALSE),
    SCR0282_main_menu_stoppump(HammingDistance.SAFETY_NUMBER_VALUE_0073,
        SafetyBoolean.FALSE),
    SCR0066_info_41_stop_insulin(HammingDistance.SAFETY_NUMBER_VALUE_0074,
        SafetyBoolean.FALSE),
    SCR0065_status_stopped(HammingDistance.SAFETY_NUMBER_VALUE_0075,
        SafetyBoolean.FALSE),
    SCR0061_start_insulin(HammingDistance.SAFETY_NUMBER_VALUE_0076,
        SafetyBoolean.FALSE),
    SCR0004_time_date(HammingDistance.SAFETY_NUMBER_VALUE_0077,
        SafetyBoolean.FALSE),
    SCR0306_picker_clock(HammingDistance.SAFETY_NUMBER_VALUE_0078,
        SafetyBoolean.FALSE),
    SCR0005_picker_time_24h(HammingDistance.SAFETY_NUMBER_VALUE_0079,
        SafetyBoolean.FALSE),
    SCR0005_picker_time_12h(HammingDistance.SAFETY_NUMBER_VALUE_0080,
        SafetyBoolean.FALSE),
    SCR0006_picker_date(HammingDistance.SAFETY_NUMBER_VALUE_0081,
        SafetyBoolean.FALSE),
    SCR0305_warning_limits(HammingDistance.SAFETY_NUMBER_VALUE_0082,
        SafetyBoolean.FALSE),
    WarningLimitPicker(HammingDistance.SAFETY_NUMBER_VALUE_0083,
        SafetyBoolean.FALSE),
    SCR0356_automatic_off_1(HammingDistance.SAFETY_NUMBER_VALUE_0084,
        SafetyBoolean.FALSE),
    SCR0356_automatic_off_2(HammingDistance.SAFETY_NUMBER_VALUE_0085,
        SafetyBoolean.FALSE),
    SCR0219_picker_duration_hrs_wl(HammingDistance.SAFETY_NUMBER_VALUE_0086,
        SafetyBoolean.FALSE),
    SCR0250_mdi_insulin_increment_settings(
        HammingDistance.SAFETY_NUMBER_VALUE_0087, SafetyBoolean.FALSE),
    SCR0011_picker_max_bolus_mdi(HammingDistance.SAFETY_NUMBER_VALUE_0088,
        SafetyBoolean.FALSE),
    SCR0109_basal_menu(HammingDistance.SAFETY_NUMBER_VALUE_0089,
        SafetyBoolean.FALSE),
    SCR0110_tbr_menu(HammingDistance.SAFETY_NUMBER_VALUE_0090,
        SafetyBoolean.FALSE),
    SCR0111_basic_tbr_detail(HammingDistance.SAFETY_NUMBER_VALUE_0091,
        SafetyBoolean.FALSE),
    SCR0062_status(HammingDistance.SAFETY_NUMBER_VALUE_0092,
        SafetyBoolean.FALSE),
    SCR0027_picker_percentage_basic_tbr(
        HammingDistance.SAFETY_NUMBER_VALUE_0087, SafetyBoolean.FALSE),
    SCR0219_picker_duration_tbr(HammingDistance.SAFETY_NUMBER_VALUE_0093,
        SafetyBoolean.FALSE),
    SCR0115_deliver_start_tbr(HammingDistance.SAFETY_NUMBER_VALUE_0094,
        SafetyBoolean.FALSE),
    SCR0064_info_83_tbr_segments_max(HammingDistance.SAFETY_NUMBER_VALUE_0095,
        SafetyBoolean.FALSE),
    SCR0066_info_67_override_tbr(HammingDistance.SAFETY_NUMBER_VALUE_0096,
        SafetyBoolean.FALSE),
    SCR0064_info_89_tbr_less_frequent(HammingDistance.SAFETY_NUMBER_VALUE_0097,
        SafetyBoolean.FALSE),
    SCR0116_info_tbr_cancel(HammingDistance.SAFETY_NUMBER_VALUE_0098,
        SafetyBoolean.FALSE),
    SCR0118_custom_tbr(HammingDistance.SAFETY_NUMBER_VALUE_0099,
        SafetyBoolean.FALSE),
    SCR0038_picker_basal_insulin_basal(
        HammingDistance.SAFETY_NUMBER_VALUE_0100, SafetyBoolean.FALSE),
    SCR0120_keyboard_basal(HammingDistance.SAFETY_NUMBER_VALUE_0101,
        SafetyBoolean.FALSE),
    SCR0066_info_01_confirm_delete(HammingDistance.SAFETY_NUMBER_VALUE_0102,
        SafetyBoolean.FALSE),
    RUNTIME_TEST(HammingDistance.SAFETY_NUMBER_VALUE_0253, SafetyBoolean.TRUE),
    OTHERFLOW(HammingDistance.SAFETY_NUMBER_VALUE_0254, SafetyBoolean.FALSE),
    ENDFLOW(HammingDistance.SAFETY_NUMBER_VALUE_0255, SafetyBoolean.FALSE);

    // Each step of all safety relevant activities has a unique id which shall
    // be Hamming distance of four values.
    private int mHammingId;

    // Indicate if do runtime test case in this safety sequence.
    private final SafetyBoolean mIsRunTimeEnabled;

    /**
     * The constructor of enum MONITOR
     * 
     * @param nHammingId [in] the Hamming distance id of 4 values
     *            Range: 0 - 2^31-1
     *            Unit: int
     *            Scaling: 1
     * @param isRunTimeEnabled [in] enables or disables run time test
     *            Range : valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     */
    private MONITOR(int nHammingId, SafetyBoolean isRunTimeEnabled)
    {
        mHammingId = nHammingId;
        mIsRunTimeEnabled = isRunTimeEnabled;
    }

    /**
     * Return whether run time test is enabled in the monitor sequence.
     * 
     * @return ISafetyFlowMonitorState [out] return the monitor state
     *         Range : valid object of ISafetyFlowMonitorState
     *         Unit: ISafetyFlowMonitorState
     *         Scaling: 1
     */
    public ISafetyFlowMonitorState getState(Context context)
    {
        ISafetyFlowMonitorState state = new DefaultSafetyFlowMonitorState(
                context, mHammingId);

        return state;
    }

    /**
     * Return whether enables run time test in the monitor sequence.
     * 
     * @return SafetyBoolean [out] return SafetyBoolean.TRUE if enables run time
     *         test otherwise return SafetyBoolean.FALSE.
     *         Range : valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean getIsRunTimeExecutedState()
    {
        return mIsRunTimeEnabled;
    }

    /**
     * Return the Hamming distance id of safety relevant sequences.
     * 
     * @return int [out] return the Hamming distance id
     *         sequence
     *         Range: 0 - 2^31-1
     *         Unit: int
     *         Scaling: 1
     */
    public int getHammingId()
    {
        return mHammingId;
    }

}
