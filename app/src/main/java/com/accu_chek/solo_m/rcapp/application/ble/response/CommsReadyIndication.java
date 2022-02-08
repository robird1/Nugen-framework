package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

import android.os.Parcel;
import android.os.Parcelable;

/**
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 9:21:38 AM
 */
public class CommsReadyIndication implements IResponse, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Result code
     */
    private int mResult;
    /**
     * Flow control parameter ¡V initial token count (2)
     */
    private int mTokens;
    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<CommsReadyIndication> CREATOR = new Parcelable.Creator<CommsReadyIndication>()
    {
        public CommsReadyIndication createFromParcel(Parcel in)
        {
            return new CommsReadyIndication(in);
        }

        public CommsReadyIndication[] newArray(int size)
        {
            return new CommsReadyIndication[size];
        }
    };

    public CommsReadyIndication()
    {

    }

    /**
     * constructor for IPC onTransact
     * 
     * @param in
     */
    public CommsReadyIndication(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mResult = in.readInt();
        this.mTokens = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public CommsReadyIndication(int command)
    {
        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
    }

    public SafetyNumber<Integer> getTokens()
    {
        return new SafetyNumber<Integer>(this.mTokens,(-this.mTokens));
    }

    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mResult,(-this.mResult));
    }

    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
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
        dest.writeInt(this.mResult);
        dest.writeInt(this.mTokens);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

    @Override
    public void setMessage(SafetyByteArray message)
    {

        this.mMessage = message.getByteArray();

    }

    @Override
    public byte[] getMessage()
    {
        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

    @Override
    public void parseMessage()
    {

        ByteBuffer buffer = ByteBuffer.wrap(mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mResult = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2
                                                                                 // bytes
        this.mTokens = buffer.get(); // 1 byte

    }

}// [BT] Fixed Klocwork issue.
