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
 * The response to run time test request and command code is
 * CommsConstant.CommandCode.SAFETY_RUNTIME_TEST
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 2:59:50 PM
 */
public class RunTimeTestResponse implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Result code
     */
    private int mResult;

    /**
     * original message byte array
     */

    private byte[] mMessage;

    public static final Parcelable.Creator<RunTimeTestResponse> CREATOR = new Parcelable.Creator<RunTimeTestResponse>()
    {
        public RunTimeTestResponse createFromParcel(Parcel in)
        {
            return new RunTimeTestResponse(in);
        }

        public RunTimeTestResponse[] newArray(int size)
        {
            return new RunTimeTestResponse[size];
        }
    };

    public RunTimeTestResponse()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */

    public RunTimeTestResponse(Parcel in)
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
    public RunTimeTestResponse(int command)
    {
        this.mCommand = command;
    }

    /**
     * Result value
     */
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
     * parse attribute values from message byte array
     */
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

    @Override
    public byte[] getMessage()
    {
        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

}// [BT] Fixed Klocwork issue.
