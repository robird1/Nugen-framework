package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * Request for command CommsConstant.CommandCode.PUMP_REC_READ.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 27-Nov-2015 6:52:08 PM
 */
public class ReadHistoryRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * The sequence number of the history record to be retrieved.
     */
    private int mSequenceNumber;

    public static final Parcelable.Creator<ReadHistoryRequest> CREATOR = new Parcelable.Creator<ReadHistoryRequest>()
    {
        public ReadHistoryRequest createFromParcel(Parcel in)
        {
            return new ReadHistoryRequest(in);
        }

        public ReadHistoryRequest[] newArray(int size)
        {
            return new ReadHistoryRequest[size];
        }
    };

    public ReadHistoryRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public ReadHistoryRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mSequenceNumber = in.readInt();
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ReadHistoryRequest(int command)
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

    public void setSequenceNumber(SafetyNumber<Integer> sequence)
    {
        this.mSequenceNumber = sequence.get().intValue();
    }


    /**
     * adding attribute values to byte array list for further frame wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(ByteConverter.getBytes(this.mSequenceNumber)); // 4 bytes
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
        dest.writeInt(this.mSequenceNumber);

    }
}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
