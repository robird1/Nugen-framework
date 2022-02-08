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
 * The response of set configuration request. The command code is
 * CommsConstant.CommandCode.COMM_CONFIG.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 1:08:36 PM
 */
public class ConfigurationDataResponse implements IResponse, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Result Code
     */
    private int mResult;
    /**
     * Original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ConfigurationDataResponse> CREATOR = new Parcelable.Creator<ConfigurationDataResponse>()
    {
        public ConfigurationDataResponse createFromParcel(Parcel in)
        {
            return new ConfigurationDataResponse(in);
        }

        public ConfigurationDataResponse[] newArray(int size)
        {
            return new ConfigurationDataResponse[size];
        }
    };

    public ConfigurationDataResponse()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public ConfigurationDataResponse(Parcel in)
    {
        this.mCommand = in.readInt();
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
    public ConfigurationDataResponse(int command)
    {
        this.mCommand = command;
    }

    @Override
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
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
     * Result code
     */
    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mResult, (-this.mResult));
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
     * parse attributes from original message byte array
     */
    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
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
        dest.writeInt(this.mResult);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
