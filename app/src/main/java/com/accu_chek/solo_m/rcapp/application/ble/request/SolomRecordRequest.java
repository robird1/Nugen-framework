package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The request is for commands "Erase SoloM Record" and "Read SoloM Record".
 * The command codes are CommsConstant.CommandCode.REC_ERASE_SOLOM and
 * CommsConstant.CommandCode.REC_READ_SOLOM.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 03-Mar-2015 2:40:45 PM
 */
public class SolomRecordRequest implements IRequest, Parcelable
{

    /**
     * Command
     */
    private int mCommand;
    /**
     * Erase all records or number of record set
     */
    private int mCount;

    public static final Parcelable.Creator<SolomRecordRequest> CREATOR = new Parcelable.Creator<SolomRecordRequest>()
    {
        public SolomRecordRequest createFromParcel(Parcel in)
        {
            return new SolomRecordRequest(in);
        }

        public SolomRecordRequest[] newArray(int size)
        {
            return new SolomRecordRequest[size];
        }
    };

    public SolomRecordRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public SolomRecordRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mCount = in.readInt();
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public SolomRecordRequest(int command)
    {
        this.mCommand = command;
    }

    /**
     * "It is used for for some validation code -- in particular to implement Bundle.hasFileDescriptors()"
     * (Dianne Hackborn -- Android framework engineer)
     * Not implemented in SoloM_RCAPP.
     */
    @Override
    public int describeContents()
    {

        return 0;
    }

    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /**
     * 
     * @param count Read all records or number of record set
     */
    public void setCount(SafetyNumber<Integer> count)
    {
        this.mCount = count.get().intValue();
    }

    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand));
        data.add(ByteConverter.getBytes((short) this.mCount));

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
        dest.writeInt(this.mCount);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
