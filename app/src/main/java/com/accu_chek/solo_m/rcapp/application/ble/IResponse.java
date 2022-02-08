package com.accu_chek.solo_m.rcapp.application.ble;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

/**
 *
 */
public interface IResponse
{

    /**
     * Return command code of the request or response object.
     * See design document
     * "NUGEN Software Design Document of Communication Sub-system Command Message"
     * for definition.
     * Refer to CommsConstant.CommandCode for valid values (Hamming Distance).
     */
    public SafetyNumber<Integer> getCommand();

    public void setMessage(SafetyByteArray message);

    public byte[] getMessage();

    public void parseMessage();

}
// [BT] Fixed Klocwork issue.
