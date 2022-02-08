package com.accu_chek.solo_m.rcapp.application.ble;

import java.util.ArrayList;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

/**
 *
 */
public interface IRequestPack
{
    // Sets the GroupID of request
    public void setGroupId(int groupId);

    // Sets the request
    public void setRequest(IRequest request);

    // Sets the parameter of request into ArrayList 
    public void writeToByteArrayList(ArrayList<byte[]> data, int flags);

}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
