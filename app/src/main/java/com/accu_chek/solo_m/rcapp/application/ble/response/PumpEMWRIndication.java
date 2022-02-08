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
 * The command code is ??
 * 
 * @author EDLiu
 * @version 1.0
 * @created 03-Mar-2015 11:38:38 AM
 */
public class PumpEMWRIndication implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * EMWR Code
     */
    private int mCode;
    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<PumpEMWRIndication> CREATOR = new Parcelable.Creator<PumpEMWRIndication>()
    {
        public PumpEMWRIndication createFromParcel(Parcel in)
        {
            return new PumpEMWRIndication(in);
        }

        public PumpEMWRIndication[] newArray(int size)
        {
            return new PumpEMWRIndication[size];
        }
    };

    public PumpEMWRIndication()
    {

    }

    public PumpEMWRIndication(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mCode = in.readInt();
    }

    public PumpEMWRIndication(int command)
    {
        this.mCommand = command;
    }

    /**
     * return EMWR code
     */
    public int getCode()
    {
        return this.mCode;
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
        dest.writeInt(this.mCode);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

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

    /**
     * parse attribute information from original message bytes
     */
    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);
        this.mCode = buffer.get();

    }

}// [BT] Fixed Klocwork issue.
