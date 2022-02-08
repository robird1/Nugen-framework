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
 * The message is sent by Comms subsystem if the command code of a request is
 * unrecognized. The command code is CommsConstant.CommandCode.COMM_UNKNOWN.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 3:52:06 PM
 */
public class UnknownErrorIndication implements IResponse, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Requested command code
     */
    private int mOpcode;
    /**
     * Provides some additional information why this message is initiated
     */
    private int mResult;

    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<UnknownErrorIndication> CREATOR = new Parcelable.Creator<UnknownErrorIndication>()
    {
        public UnknownErrorIndication createFromParcel(Parcel in)
        {
            return new UnknownErrorIndication(in);
        }

        public UnknownErrorIndication[] newArray(int size)
        {
            return new UnknownErrorIndication[size];
        }
    };

    public UnknownErrorIndication()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public UnknownErrorIndication(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mOpcode = in.readInt();
        this.mResult = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     */
    public UnknownErrorIndication(int command)
    {
        this.mCommand = command;
    }

    public SafetyNumber<Integer> getOperationCode()
    {
        return new SafetyNumber<Integer>(this.mOpcode, (-this.mOpcode));
    }

    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mResult, (-this.mResult));
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
     * parse attribute values from message byte array
     */
    public void parseMessage()
    {

        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mOpcode = buffer.get(); // 1 byte
        this.mResult = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2
                                                                                 // bytes
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
        dest.writeInt(this.mOpcode);
        dest.writeInt(this.mResult);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
