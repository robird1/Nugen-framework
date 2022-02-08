package com.accu_chek.solo_m.rcapp.application.ble;

import java.util.ArrayList;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

/**
 * The interface of request
 */
public interface IRequest
{

    /**
     * Return command code of the request or response object.
     * See design document
     * "NUGEN Software Design Document of Communication Sub-system Command Message"
     * for definition.
     * Refer to CommsConstant.CommandCode for valid values (Hamming Distance).
     */
    public SafetyNumber<Integer> getCommand();

    public void writeToByteArrayList(ArrayList<byte[]> data);

}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
