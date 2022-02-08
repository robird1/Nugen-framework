package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The BlankMessageRequest is for command only requests such as
 * "Get Comms Info", "Abort History Update", "Watchdog Challenge Request"
 * Compatible commands:
 * CommsConstant.CommandCode.COMM_INFO
 * CommsConstant.CommandCode.WATCHDOG_CHAL_REQ
 * CommsConstant.CommandCode.ABORT_HIST_UPDATE
 * CommsConstant.CommandCode.SAFETY_RESERVOIR_TEST
 * CommsConstant.CommandCode.SAFETY_RUNTIME_TEST
 * 
 * @author EDLiu
 * @version 1.0
 * @created 27-Jan-2015 10:15:01 AM
 */
public class BlankMessageRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;

    public static final Parcelable.Creator<BlankMessageRequest> CREATOR = new Parcelable.Creator<BlankMessageRequest>()
    {
        public BlankMessageRequest createFromParcel(Parcel in)
        {
            return new BlankMessageRequest(in);
        }

        public BlankMessageRequest[] newArray(int size)
        {
            return new BlankMessageRequest[size];
        }
    };

    public BlankMessageRequest()
    {
    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public BlankMessageRequest(Parcel in)
    {
        this.mCommand = in.readInt();
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public BlankMessageRequest(int command)
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

    /**
     * 
     * @param data
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand));
    }

    /**
     * 
     * @param dest
     * @param flags
     */
    public void writeToParcel(Parcel dest, int flags)
    {

        dest.writeInt(this.mCommand);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
