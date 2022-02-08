package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The RecordResponse is response to commands "Read IDS/SoloM Record". If data
 * available, the command code is MORE_DATA; if not, the command code is
 * NO_DATA.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 03-Mar-2015 3:28:19 PM
 */
public class ReadRecordResponse implements IResponse, Parcelable
{

    private static final byte MAX_DATA_LEN = 96;
    /**
     * Command code
     */
    private int mCommand;
    /**
     * Max buffer of data to be read.
     * Buffer includes the record size (1 Byte) and record.
     * If the specified record is empty, the record feedback is 0
     */
    private int mDataLength;
    private byte[] mData;
    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ReadRecordResponse> CREATOR = new Parcelable.Creator<ReadRecordResponse>()
    {
        public ReadRecordResponse createFromParcel(Parcel in)
        {
            return new ReadRecordResponse(in);
        }

        public ReadRecordResponse[] newArray(int size)
        {
            return new ReadRecordResponse[size];
        }
    };

    public ReadRecordResponse()
    {

    }

    public ReadRecordResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mDataLength = in.readInt();
        if (this.mDataLength > 0)
        {
            this.mData = new byte[this.mDataLength];
            in.readByteArray(this.mData);
        }
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    public ReadRecordResponse(int command)
    {
        this.mCommand = command;
    }

    /**
     * If the command code is NO_DATA, the length of data bytes is 0.
     */
    public byte[] getData()
    {
        return this.mData;
    }

    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /**
     * 
     * @param message
     */
    @Override
    public void setMessage(SafetyByteArray message)
    {
        this.mMessage = message.getByteArray();
    }

    /**
     * Return original message bytes from Comms subsystem.
     * The interface is mainly for debugging purpose at development stage.
     */
    @Override
    public byte[] getMessage()
    {
        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

    /**
     * parse attribute information from original message bytes
     */
    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);
        this.mDataLength = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN);
        this.mData = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                this.mDataLength);

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
        dest.writeInt(this.mDataLength);
        if (this.mDataLength > 0)
        {
            dest.writeByteArray(this.mData);
        }
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
