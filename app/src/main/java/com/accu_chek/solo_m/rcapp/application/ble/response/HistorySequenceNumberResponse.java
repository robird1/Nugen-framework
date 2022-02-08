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
 * The command code is CommsConstant.CommandCode.PUMP_REC_SEQ_READ.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 27-Nov-2015 11:00:03 AM
 */
public class HistorySequenceNumberResponse implements IResponse, Parcelable
{
    /**
     * Command code
     */
    private int mCommand;
    /**
     * The sequence number of the first history record
     */
    private int mSeqNumberFirst;
    /**
     * The sequence number of the last history record
     */
    private int mSeqNumberLast;
    /**
     * The minimum value of the sequence number 
     */
    private int mSeqNumberMin;
    /**
     * The maximum value of the sequence number
     */
    private int mSeqNumberMax;
    /**
     * The number of history records.
     */
    private int mRecordCount;

    /**
     * original message bytes
     * 
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<HistorySequenceNumberResponse> CREATOR = new Parcelable.Creator<HistorySequenceNumberResponse>()
    {
        public HistorySequenceNumberResponse createFromParcel(Parcel in)
        {
            return new HistorySequenceNumberResponse(in);
        }

        public HistorySequenceNumberResponse[] newArray(int size)
        {
            return new HistorySequenceNumberResponse[size];
        }
    };

    public HistorySequenceNumberResponse()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public HistorySequenceNumberResponse(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mSeqNumberFirst = in.readInt();
        this.mSeqNumberLast = in.readInt();
        this.mSeqNumberMin = in.readInt();
        this.mSeqNumberMax = in.readInt();
        this.mRecordCount = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     */
    public HistorySequenceNumberResponse(int command)
    {

        this.mCommand = command;
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
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mSeqNumberFirst = ByteConverter.readInt(buffer, ByteOrder.LITTLE_ENDIAN); // 4 bytes
        this.mSeqNumberLast = ByteConverter.readInt(buffer, ByteOrder.LITTLE_ENDIAN); // 4 bytes
        this.mSeqNumberMin = ByteConverter.readInt(buffer, ByteOrder.LITTLE_ENDIAN); // 4 bytes
        this.mSeqNumberMax = ByteConverter.readInt(buffer, ByteOrder.LITTLE_ENDIAN); // 4 bytes
        this.mRecordCount = ByteConverter.readInt(buffer, ByteOrder.LITTLE_ENDIAN); // 4 bytes
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
        dest.writeInt(this.mSeqNumberFirst);
        dest.writeInt(this.mSeqNumberLast);
        dest.writeInt(this.mSeqNumberMin);
        dest.writeInt(this.mSeqNumberMax);
        dest.writeInt(this.mRecordCount);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);
    }

    /*
     * The function returns the value of the last sequence number.
     */
    public SafetyNumber<Integer> getLastSequenceNumber()
    {
        return new SafetyNumber<Integer>(this.mSeqNumberLast, (-this.mSeqNumberLast));
    }

    /**
     * The function returns the value of the first sequence number.
     */
    public SafetyNumber<Integer> getFirstSequenceNumber()
    {
        return new SafetyNumber<Integer>(this.mSeqNumberFirst, (-this.mSeqNumberFirst));
    }

    /**
     * The function returns the value of the minimum sequence number.
     */
    public SafetyNumber<Integer> getMinSequenceNumber()
    {
        return new SafetyNumber<Integer>(this.mSeqNumberMin, (-this.mSeqNumberMin));
    }

    /**
     * The function returns the value of the maximum sequence number.
     */
    public SafetyNumber<Integer> getMaxSequenceNumber()
    {
        return new SafetyNumber<Integer>(this.mSeqNumberMax, (-this.mSeqNumberMax));
    }

    /**
     * The function returns the number of history records.
     */
    public SafetyNumber<Integer> getRecordCount()
    {
        return new SafetyNumber<Integer>(this.mRecordCount, (-this.mRecordCount));
    }
 
}
