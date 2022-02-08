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
 * 
 * Command Code: CommsConstant.CommandCode.COMM_DATA_SENT
 * 
 * @version 1.0
 * @created 03-Feb-2015 10:35:22 AM
 */
public class CommandDataSentIndication implements IResponse, Parcelable
{
    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Token return = 1
     */
    // private int mCount;
    /**
     * Result Code
     */
    private int mResult;

    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<CommandDataSentIndication> CREATOR = new Parcelable.Creator<CommandDataSentIndication>()
    {
        public CommandDataSentIndication createFromParcel(Parcel in)
        {
            return new CommandDataSentIndication(in);
        }

        public CommandDataSentIndication[] newArray(int size)
        {
            return new CommandDataSentIndication[size];
        }
    };

    public CommandDataSentIndication()
    {

    }

    /**
     * constructor for remote IPC
     * 
     * @param in
     */
    public CommandDataSentIndication(Parcel in)
    {
        this.mCommand = in.readInt();
        // this.mCount = in.readInt();
        this.mResult = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public CommandDataSentIndication(int command)
    {
        this.mCommand = command;
    }

    @Override
    public int describeContents()
    {
        return 0;
    }

    /**
     * 
     * @param dest
     * @param flags
     */
    @Override
    public void writeToParcel(Parcel dest, int flags)
    {
        dest.writeInt(this.mCommand);
        // dest.writeInt(this.mCount);
        dest.writeInt(this.mResult);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

    @Override
    public void setMessage(SafetyByteArray message)
    {
        this.mMessage = message.getByteArray();

    }

    /**
     * The function parses message bytes into object members. The byte length of
     * value follows the message table define in comms
     */
    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);// 2 bytes
        // this.mCount = buffer.get(); // 1 byte
        this.mResult = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 1
                                                                                 // byte

    }

    @Override
    public byte[] getMessage()
    {
        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

    /**
     * 
     * @return count
     */
    // public int getCount()
    // {
    //
    // return this.mCount;
    // }
    /**
     * 
     * @return result
     */
    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mResult,(-this.mResult));
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

}// [BT] Fixed Klocwork issue.
