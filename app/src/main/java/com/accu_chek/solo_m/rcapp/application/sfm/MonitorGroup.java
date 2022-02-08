/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: MonitorGroup
 * Brief: Record all critical path to monitor group table.
 * 
 * Create Date: 2015/08/12
 * $Revision: 25273 $
 * $Author: JamesLee $
 * $Id: MonitorGroup.java 25273 2015-12-01 12:55:41Z JamesLee $
 */
package com.accu_chek.solo_m.rcapp.application.sfm;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;

/**
 * Create a table to record all safety related activities are described using
 * menu map names and all monitor group id is Hamming distance of four values.
 */
public enum MonitorGroup
{
    POST_MONITOR(HammingDistance.SAFETY_NUMBER_VALUE_0001, new MONITOR[] {
            MONITOR.POST_START, MONITOR.POST_END }, EMWRList.EMW41501),
    BGM_MONITOR_1(HammingDistance.SAFETY_NUMBER_VALUE_0002, new MONITOR[] {
            MONITOR.SCR0086_insert_test_strip, MONITOR.SCR0088_apply_blood,
            MONITOR.SCR0378_waiting_2, MONITOR.SCR0090_bg_result,
            MONITOR.SCR0204_detailed_bg_result }, EMWRList.EMW41502),
    BGM_MONITOR_2(HammingDistance.SAFETY_NUMBER_VALUE_0003, new MONITOR[] {
            MONITOR.SCR0088_apply_blood, MONITOR.SCR0378_waiting_2,
            MONITOR.SCR0090_bg_result, MONITOR.SCR0204_detailed_bg_result },
        EMWRList.EMW41502),
    MPR_MONITOR_0(HammingDistance.SAFETY_NUMBER_VALUE_0004,
        new MONITOR[] { MONITOR.SCR0043_fitting_steps,
                MONITOR.SCR0044_picker_reservoir_amount,
                MONITOR.SCR0045_locating, }, EMWRList.EMW41503),
    MPR_MONITOR_1(HammingDistance.SAFETY_NUMBER_VALUE_0005, new MONITOR[] {
            MONITOR.SCR0053_device_authentication, MONITOR.SCR0295_scan,
            MONITOR.SCR0093_warning_15_ready_prime, MONITOR.SCR0056_MP_priming,
            MONITOR.SCR0059_attach_mp_to_i }, EMWRList.EMW41503),
    MPR_MONITOR_2(HammingDistance.SAFETY_NUMBER_VALUE_0006, new MONITOR[] {
            MONITOR.SCR0053_device_authentication, MONITOR.SCR0052_select_MP,
            MONITOR.SCR0055_pump_code, MONITOR.SCR0093_warning_15_ready_prime,
            MONITOR.SCR0056_MP_priming, MONITOR.SCR0059_attach_mp_to_i },
        EMWRList.EMW41503),
    MPR_MONITOR_3(HammingDistance.SAFETY_NUMBER_VALUE_0007, new MONITOR[] {
            MONITOR.SCR0074_replace_part_r, MONITOR.SCR0067_remove_old_parts,
            MONITOR.SCR0043_fitting_steps }, EMWRList.EMW41503),
    MPR_MONITOR_4(HammingDistance.SAFETY_NUMBER_VALUE_0008, new MONITOR[] {
            MONITOR.SCR0074_replace_part_r, MONITOR.SCR0067_remove_old_parts,
            MONITOR.SCR0043_fitting_steps,
            MONITOR.SCR0044_picker_reservoir_amount,
            MONITOR.SCR0093_warning_15_ready_prime, MONITOR.SCR0056_MP_priming,
            MONITOR.SCR0059_attach_mp_to_i }, EMWRList.EMW41503),
    MPR_MONITOR_5(HammingDistance.SAFETY_NUMBER_VALUE_0009,
        new MONITOR[] { MONITOR.SCR0074_replace_part_r,
                MONITOR.SCR0067_remove_old_parts,
                MONITOR.SCR0078_fitting_two_steps_only_rp,
                MONITOR.SCR0044_picker_reservoir_amount,
                MONITOR.SCR0045_locating }, EMWRList.EMW41503),
    MPR_MONITOR_6(HammingDistance.SAFETY_NUMBER_VALUE_0010, new MONITOR[] {
            MONITOR.SCR0074_replace_part_r, MONITOR.SCR0067_remove_old_parts,
            MONITOR.SCR0081_fitting_one_steps_only_RP,
            MONITOR.SCR0059_attach_mp_to_i }, EMWRList.EMW41503),
    MPR_MONITOR_7(HammingDistance.SAFETY_NUMBER_VALUE_0011, new MONITOR[] {
            MONITOR.SCR0074_replace_part_r, MONITOR.SCR0067_remove_old_parts,
            MONITOR.SCR0078_fitting_two_steps_only_rp,
            MONITOR.SCR0044_picker_reservoir_amount,
            MONITOR.SCR0093_warning_15_ready_prime, MONITOR.SCR0056_MP_priming,
            MONITOR.SCR0059_attach_mp_to_i }, EMWRList.EMW41503),
    HOME_START_PUMP(HammingDistance.SAFETY_NUMBER_VALUE_0012,
        new MONITOR[] { MONITOR.SCR0282_main_menu_startpump,
                MONITOR.SCR0061_start_insulin }, EMWRList.EMW41504),
    HOME_STOP_PUMP(HammingDistance.SAFETY_NUMBER_VALUE_0013, new MONITOR[] {
            MONITOR.SCR0282_main_menu_stoppump,
            MONITOR.SCR0066_info_41_stop_insulin,
            MONITOR.SCR0065_status_stopped, MONITOR.SCR0061_start_insulin },
        EMWRList.EMW41504),
    TIME_DATE_1(HammingDistance.SAFETY_NUMBER_VALUE_0014, new MONITOR[] {
            MONITOR.SCR0004_time_date, MONITOR.SCR0306_picker_clock },
        EMWRList.EMW41505),
    TIME_DATE_2(HammingDistance.SAFETY_NUMBER_VALUE_0015, new MONITOR[] {
            MONITOR.SCR0004_time_date, MONITOR.SCR0005_picker_time_24h },
        EMWRList.EMW41505),
    TIME_DATE_3(HammingDistance.SAFETY_NUMBER_VALUE_0016, new MONITOR[] {
            MONITOR.SCR0004_time_date, MONITOR.SCR0005_picker_time_12h },
        EMWRList.EMW41505),
    TIME_DATE_4(HammingDistance.SAFETY_NUMBER_VALUE_0017, new MONITOR[] {
            MONITOR.SCR0004_time_date, MONITOR.SCR0006_picker_date },
        EMWRList.EMW41505),
    WARNING_LIMIT_1(HammingDistance.SAFETY_NUMBER_VALUE_0018, new MONITOR[] {
            MONITOR.SCR0305_warning_limits, MONITOR.WarningLimitPicker },
        EMWRList.EMW41506),
    WARNING_LIMIT_2(HammingDistance.SAFETY_NUMBER_VALUE_0019, new MONITOR[] {
            MONITOR.SCR0305_warning_limits, MONITOR.SCR0356_automatic_off_1,
            MONITOR.SCR0219_picker_duration_hrs_wl,
            MONITOR.SCR0356_automatic_off_2 }, EMWRList.EMW41506),

    SETTING_MDI(HammingDistance.SAFETY_NUMBER_VALUE_0020, new MONITOR[] {
            MONITOR.SCR0250_mdi_insulin_increment_settings,
            MONITOR.SCR0011_picker_max_bolus_mdi }, EMWRList.EMW41507),

    TBR_START_1(HammingDistance.SAFETY_NUMBER_VALUE_0021, new MONITOR[] {
            MONITOR.SCR0109_basal_menu, MONITOR.SCR0110_tbr_menu,
            MONITOR.SCR0111_basic_tbr_detail,
            MONITOR.SCR0027_picker_percentage_basic_tbr }, EMWRList.EMW41508),
    TBR_START_2(HammingDistance.SAFETY_NUMBER_VALUE_0022, new MONITOR[] {
            MONITOR.SCR0109_basal_menu, MONITOR.SCR0110_tbr_menu,
            MONITOR.SCR0111_basic_tbr_detail,
            MONITOR.SCR0219_picker_duration_tbr }, EMWRList.EMW41508),
    TBR_START_3(HammingDistance.SAFETY_NUMBER_VALUE_0023,
        new MONITOR[] { MONITOR.SCR0109_basal_menu, MONITOR.SCR0110_tbr_menu,
                MONITOR.SCR0111_basic_tbr_detail,
                MONITOR.SCR0115_deliver_start_tbr }, EMWRList.EMW41508),

    TBR_START_4(HammingDistance.SAFETY_NUMBER_VALUE_0024, new MONITOR[] {
            MONITOR.SCR0109_basal_menu, MONITOR.SCR0110_tbr_menu,
            MONITOR.SCR0111_basic_tbr_detail,
            MONITOR.SCR0064_info_83_tbr_segments_max }, EMWRList.EMW41508),
    TBR_START_5(HammingDistance.SAFETY_NUMBER_VALUE_0025, new MONITOR[] {
            MONITOR.SCR0109_basal_menu, MONITOR.SCR0110_tbr_menu,
            MONITOR.SCR0111_basic_tbr_detail,
            MONITOR.SCR0066_info_67_override_tbr }, EMWRList.EMW41508),

    TBR_START_6(HammingDistance.SAFETY_NUMBER_VALUE_0026, new MONITOR[] {
            MONITOR.SCR0109_basal_menu, MONITOR.SCR0110_tbr_menu,
            MONITOR.SCR0111_basic_tbr_detail,
            MONITOR.SCR0064_info_89_tbr_less_frequent }, EMWRList.EMW41508),
    TBR_CANCEL(HammingDistance.SAFETY_NUMBER_VALUE_0027, new MONITOR[] {
            MONITOR.SCR0109_basal_menu, MONITOR.SCR0110_tbr_menu,
            MONITOR.SCR0116_info_tbr_cancel }, EMWRList.EMW41506),
    TBR_CUSTOM_1(HammingDistance.SAFETY_NUMBER_VALUE_0028, new MONITOR[] {
            MONITOR.SCR0118_custom_tbr,
            MONITOR.SCR0027_picker_percentage_basic_tbr,
            MONITOR.SCR0111_basic_tbr_detail }, EMWRList.EMW41506),
    TBR_CUSTOM_2(HammingDistance.SAFETY_NUMBER_VALUE_0029, new MONITOR[] {
            MONITOR.SCR0118_custom_tbr,
            MONITOR.SCR0038_picker_basal_insulin_basal,
            MONITOR.SCR0111_basic_tbr_detail }, EMWRList.EMW41506),
    TBR_CUSTOM_3(HammingDistance.SAFETY_NUMBER_VALUE_0030, new MONITOR[] {
            MONITOR.SCR0118_custom_tbr, MONITOR.SCR0115_deliver_start_tbr, },
        EMWRList.EMW41506),
    TBR_CUSTOM_4(HammingDistance.SAFETY_NUMBER_VALUE_0031, new MONITOR[] {
            MONITOR.SCR0118_custom_tbr, MONITOR.SCR0111_basic_tbr_detail, },
        EMWRList.EMW41506),
    TBR_CUSTOM_5(HammingDistance.SAFETY_NUMBER_VALUE_0032, new MONITOR[] {
            MONITOR.SCR0118_custom_tbr,
            MONITOR.SCR0064_info_83_tbr_segments_max
             }, EMWRList.EMW41506),
    TBR_CUSTOM_6(HammingDistance.SAFETY_NUMBER_VALUE_0033, new MONITOR[] {
            MONITOR.SCR0118_custom_tbr, MONITOR.SCR0066_info_67_override_tbr
            }, EMWRList.EMW41506),
    TBR_CUSTOM_7(HammingDistance.SAFETY_NUMBER_VALUE_0034, new MONITOR[] {
            MONITOR.SCR0118_custom_tbr,
            MONITOR.SCR0064_info_89_tbr_less_frequent
            }, EMWRList.EMW41506),
    TBR_CUSTOM_8(HammingDistance.SAFETY_NUMBER_VALUE_0035,
        new MONITOR[] { MONITOR.SCR0118_custom_tbr,
                MONITOR.SCR0066_info_01_confirm_delete }, EMWRList.EMW41506);


    // Record monitor sequences of every critical flow path.
    private MONITOR[] mMonitorId;

    // Every critical flow path id is Hamming distance of four values.
    private int mHammingId = 0;

    // Record start monitor sequence id of every critical path.
    private MONITOR mStartSequenceID;

    // The flow path are both ACTIVITY_CRITICAL_PATH and HARDWARE_CRITICAL_PATH.
    private EMWRList mEMWRList = EMWRList.EMW41501;

    /**
     * The constructor of enum MonitorGroup
     * 
     * @param nHammingId
     *            [in] the Hamming distance id of 4 values
     *            Range: 0 - 2^31-1
     *            Unit: int
     *            Scaling: 1
     * @param nMonitorId
     *            [in] put all monitor sequences.
     *            Range: 0 - 2^31-1
     *            Unit: int
     *            Scaling: 1
     * @param nflow
     *            [in] The flow path are both ACTIVITY_CRITICAL_PATH and
     *            HARDWARE_CRITICAL_PATH.
     *            Range: 0 - 2^31-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @see mHammingId[out]
     *      mMonitorId[out]
     *      mFlow[out]
     *      mStartSequenceID[out]
     * 
     */
    private MonitorGroup(int nHammingId, MONITOR[] nMonitorId, EMWRList nflow)
    {
        mHammingId = nHammingId;

        // mMonitorId = nMonitorId;
        mMonitorId = new MONITOR[nMonitorId.length];

        System.arraycopy(nMonitorId, 0, mMonitorId, 0, nMonitorId.length);
        mEMWRList = nflow;

        // To point to first column Id to get id value
        mStartSequenceID = nMonitorId[0];

    }

    /**
     * Return the ISafetyFlowMonitorState that represent the flow state.
     * 
     * @param context [in] the non-null context of application
     *            Range : valid Context object
     *            Unit: Context
     *            Scaling: 1
     * @see mStartSequenceID[in]
     * 
     * @return ISafetyFlowMonitorState [out] the ISafetyFlowMonitorState that
     *         represent the flow state.
     */
    public ISafetyFlowMonitorState getHeadMonitorId(Context context)
    {
        ISafetyFlowMonitorState flow = new DefaultSafetyFlowMonitorState(
                context, mStartSequenceID.getHammingId());

        return flow;
    }

    /**
     * Return the Hamming distance id of safety relevant activities
     * 
     * @see mHammingId[in]
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

    /**
     * Get all monitor sequences of every safety activities
     * 
     * @param monitorid [in] put all monitor sequences.
     *            Range: 0 - 2^31-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @see mMonitorId[in]
     */
    public void getMonitorId(MONITOR[] monitorid)
    {
        // do array copy
        System.arraycopy(mMonitorId, 0, monitorid, 0, mMonitorId.length);
    }

    /**
     * Get maximum sequences length of every safety relevant activities
     * 
     * @see mMonitorId[in]
     * 
     * @return int [out] return the length which assigned monitor group id
     *         Range: 0 - 2^31-1
     *         Unit: int
     *         Scaling: 1
     */
    public int getMonitorIdLength()
    {
        return mMonitorId.length;
    }

    public EMWRList getMonitorFlow()
    {
        return mEMWRList;
    }

}
