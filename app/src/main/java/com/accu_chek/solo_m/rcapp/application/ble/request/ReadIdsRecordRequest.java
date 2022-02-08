package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * @author EDLiu
 * @version 1.0
 * @created 09-Apr-2015 5:05:57 PM
 */
public class ReadIdsRecordRequest implements IRequest
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Type of record
     */
    private int mType;
    public static final Parcelable.Creator<ReadIdsRecordRequest> CREATOR = new Parcelable.Creator<ReadIdsRecordRequest>()
    {
        public ReadIdsRecordRequest createFromParcel(Parcel in)
        {
            return new ReadIdsRecordRequest(in);
        }

        public ReadIdsRecordRequest[] newArray(int size)
        {
            return new ReadIdsRecordRequest[size];
        }
    };

    public ReadIdsRecordRequest()
    {

    }

    public ReadIdsRecordRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mType = in.readInt();
    }

    public ReadIdsRecordRequest(int command)
    {
        this.mCommand = command;
    }

    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {
        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(ByteConverter.getBytes((byte) this.mType)); // 2 bytes

    }

    /**
     * Return command code of the request or response object.
     * See design document "NUGEN Software Design Document of Communication
     * Sub-system
     * Command Message" for definition.
     * Refer to CommsConstant.CommandCode for valid values (Hamming Distance).
     */
    @Override
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    public int getType()
    {
        return this.mType;
    }

}// [BT] Fixed Klocwork issue.
