package com.accu_chek.solo_m.rcapp.application.sfm;

import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;

/**
 * The enum of monitorable sequence that list all the sequences that
 * Safety Flow Monitor will monitored.
 */
public enum FlowPath
{
    ACTIVITY_CRITICAL_PATH(HammingDistance.SAFETY_NUMBER_UINT16_0001),
    HARDWARE_CRITICAL_PATH(HammingDistance.SAFETY_NUMBER_UINT16_0002);

    /**
     * The HammingDistance value that represent this flow path id.
     */
    private final int mHammingDistanceId;

    /**
     * Status: FDD/Coding
     * 
     * The constructor 
     * 
     * @param nHammingDistanceId [in] represent this flow path id that define
     *            the flow map of the sequence
     * 
     */
    private FlowPath(int nHammingDistanceId)
    {
        mHammingDistanceId = nHammingDistanceId;

    }
    //
    // /**
    // * Return the resource id that define the
    // * flow map of the sequence.
    // *
    // * @return int [out] the resource id that define the
    // * flow map of the sequence.
    // */
    // int getFlowPathId()
    // {
    // return mHammingDistanceId;
    // }

};
