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
 * The response contains "cause" only for command of "Set Pairable",
 * "Flight Mode", "Scan",
 * "Erase IDS Record", "Erase SoloM Record", "Discovery Service"
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 5:41:46 PM
 */
public class CauseOnlyResponse implements IResponse, Parcelable
{
    /**
     * Command code
     */
    private int mCommand;
    /**
     * Cause
     */
    private int mCause;
    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<CauseOnlyResponse> CREATOR = new Parcelable.Creator<CauseOnlyResponse>()
    {
        public CauseOnlyResponse createFromParcel(Parcel in)
        {
            return new CauseOnlyResponse(in);
        }

        public CauseOnlyResponse[] newArray(int size)
        {
            return new CauseOnlyResponse[size];
        }
    };

    public CauseOnlyResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     */
    public CauseOnlyResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mCause = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public CauseOnlyResponse(int command)
    {
        this.mCommand = command;
    }

    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /**
     * return cause.
     * See BlueAPI TBlueAPI_Cause for definition.
     * Refer to BlueConstant.Cause for valid values.
     * 
     * @return cause
     *         Range: BlueAPI TBlueAPI_Cause
     *         Unit: N/A
     *         Scaling: NA
     */
    public SafetyNumber<Integer> getCause()
    {
        return new SafetyNumber<Integer>(this.mCause, (-this.mCause));
    }

    /**
     * set original message bytes
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
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mCause = buffer.get(); // 1 byte
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
        dest.writeInt(this.mCause);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
