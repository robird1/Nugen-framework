/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.request.FlightModeRequest
 * Brief: This class handles FlightModeRequest request
 *
 * Create Date: 2015/7/29
 * $Revision: 21919 $
 * $Author: KiddYeh $
 * $Id: FlightModeRequest.java 21919 2015-10-19 08:31:25Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

public class FlightModeRequest implements IRequest, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;


    public static final Parcelable.Creator<FlightModeRequest> CREATOR = new Parcelable.Creator<FlightModeRequest>()
    {
        public FlightModeRequest createFromParcel(Parcel in)
        {
            return new FlightModeRequest(in);
        }

        public FlightModeRequest[] newArray(int size)
        {
            return new FlightModeRequest[size];
        }
    };

    public FlightModeRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public FlightModeRequest(Parcel in)
    {
        this.mCommand = in.readInt();
    }

    public FlightModeRequest(int command)
    {
        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
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
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
