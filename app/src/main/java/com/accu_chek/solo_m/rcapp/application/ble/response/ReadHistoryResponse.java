package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The command code is CommsConstant.CommandCode.PUMP_REC_READ.
 * @author EDLiu
 * @version 1.0
 * @created 17-Nov-2015 9:21:38 AM
 */
public class ReadHistoryResponse implements IResponse, Parcelable
{
    
    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Event Type
     */
    private int mEventType;
    /**
     * The sequence number of the history record
     */
    private int mSequenceNumber;
    /**
     * Relative offset
     * Range:
     * Unit: seconds
     * Scaling: 1
     */
    private int mRelativeOffset;
    /**
     * Byte length of event data
     * Range: 0~10
     * Unit: N/A
     * Scaling: 1
     */
    private int mDataLength;
    /**
     * Event data array.
     */
    private byte[] mEventData;
    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ReadHistoryResponse> CREATOR = new Parcelable.Creator<ReadHistoryResponse>()
    {
        public ReadHistoryResponse createFromParcel(Parcel in)
        {
            return new ReadHistoryResponse(in);
        }

        public ReadHistoryResponse[] newArray(int size)
        {
            return new ReadHistoryResponse[size];
        }
    };

    public ReadHistoryResponse()
    {

    }

    /**
     * constructor for IPC onTransact
     * 
     * @param in
     */
    public ReadHistoryResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mEventType = in.readInt();
        this.mSequenceNumber = in.readInt();
        this.mRelativeOffset = in.readInt();
        this.mDataLength = in.readInt();
        this.mEventData = new byte[this.mDataLength];
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ReadHistoryResponse(int command)
    {
        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
    }

    /**
     * The function returns the command code of the response.
     * 
     * @return
     */
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }
    /**
     * 
     * The function returns the event type of the history record.
     *
     * @return SafetyNumber<Integer> [out]
     */
    public SafetyNumber<Integer> getEventType()
    {
        return new SafetyNumber<Integer>(this.mEventType, (-this.mEventType));
    }
    /**
     * 
     * The function returns the sequence number of the retrieved history record.
     *
     * @return SafetyNumber<Integer> [out]
     */
    public SafetyNumber<Integer> getSequenceNumber()
    {
        return new SafetyNumber<Integer>(this.mSequenceNumber, (-this.mSequenceNumber));
    }
    /**
     * The function returns the relative time offset in seconds.
     *
     * @return SafetyNumber<Integer> [out] 
     */
    public SafetyNumber<Integer> getRelativeOffset()
    {
        return new SafetyNumber<Integer>(this.mRelativeOffset, (-this.mRelativeOffset));
    }
    /**
     * 
     * The function returns the length of event data array.
     *
     * @return SafetyNumber<Integer> [out] 
     */
    public SafetyNumber<Integer> getDataLength()
    {
        return new SafetyNumber<Integer>(this.mDataLength, (-this.mDataLength));
    }
    /**
     * The function returns the event data array.
     *
     * @return
     * @return SafetyByteArray [out] eventData
     */
    public SafetyByteArray getEventData()
    {
      return new SafetyByteArray(this.mEventData,CRCTool.generateCRC16(this.mEventData));
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
        dest.writeInt(this.mEventType);
        dest.writeInt(this.mSequenceNumber);
        dest.writeInt(this.mRelativeOffset);
        dest.writeInt(this.mDataLength);
        dest.writeByteArray(this.mEventData);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);
    }

    @Override
    public void setMessage(SafetyByteArray message)
    {
        this.mMessage = message.getByteArray();
    }

    /**
     * The function returns the original data array from Communication processor.
     *
     * @return
     */
    @Override
    public byte[] getMessage()
    {
        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

    @Override
    public void parseMessage()
    {

        ByteBuffer buffer = ByteBuffer.wrap(mMessage);
        this.mCommand = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mEventType = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mSequenceNumber = ByteConverter.readInt(buffer, ByteOrder.LITTLE_ENDIAN); // 4 bytes
        this.mRelativeOffset = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mDataLength = buffer.get();
        this.mEventData = new byte[this.mDataLength];
        buffer.get(this.mEventData, 0, this.mDataLength);

    }

}// [BT] Fixed Klocwork issue.
