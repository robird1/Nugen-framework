/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.request.SystemSyncRequest
 * Brief: 
 *
 * Create Date: 2015/8/10
 * $Revision: 20560 $
 * $Author: DWYang $
 * $Id: SystemSyncRequest.java 20560 2015-10-01 14:13:14Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

public class SystemSyncRequest implements IRequest, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;

    public static final Parcelable.Creator<SystemSyncRequest> CREATOR = new Parcelable.Creator<SystemSyncRequest>()
    {
        public SystemSyncRequest createFromParcel(Parcel in)
        {
            return new SystemSyncRequest(in);
        }

        public SystemSyncRequest[] newArray(int size)
        {
            return new SystemSyncRequest[size];
        }
    };

    public SystemSyncRequest()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public SystemSyncRequest(Parcel in)
    {

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public SystemSyncRequest(int command)
    {
        this.mCommand = command;
    }


    /**
     * Return command code of the request or response object.
     * See design document
     * "NUGEN Software Design Document of Communication Sub-system Command Message"
     * for definition.
     * Refer to CommsConstant.CommandCode for valid values (Hamming Distance).
     */
    @Override
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }


    /**
     * adding attribute values to byte array list for further fram wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
    }

    /**
     * Flatten this object in to a Parcel.
     * 
     * @param dest The Parcel in which the object should be written.
     * @param flags Additional flags about how the object should be written. May
     *            be 0 or PARCELABLE_WRITE_RETURN_VALUE.
     */
    @Override
    public void writeToParcel(Parcel dest, int flags)
    {

        dest.writeInt(this.mCommand);

    }

    @Override
    public int describeContents()
    {
        // TODO Auto-generated method stub
        return 0;
    }

}// (R15209 2015-08-22 23:31:51 henrytso)
// ----------------------------------------------------------------------------
// [BT] Fixed Klocwork issue.
