package com.accu_chek.solo_m.rcapp.application.ble;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

/**
 * The interface of Response pack 
 */
public interface IResponsePack
{
    // Sets the response
    public void setResponse(IResponse response);

    // Returns  the response
    public IResponse getResponse();

    // Sets the GroupID of response
    public void setGroupId(SafetyNumber<Integer> group);

    // Returns the GroupID of response
    public int getGroupId();
}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
