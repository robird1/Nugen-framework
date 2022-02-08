package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The command code is CommsConstant.CommandCode.BT_ATTR_WRITE.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 1:25:54 PM
 */
public class AttributeWriteResponse implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * BD Address
     */
    private byte[] mRemoteBd;
    /**
     * Indicates the result of the transaction.
     * - TBlueAPI_Cause
     */
    private int mCause;
    /**
     * More detailed result information from lower protocol layers
     */
    private int mSubCause;

    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<AttributeWriteResponse> CREATOR = new Parcelable.Creator<AttributeWriteResponse>()
    {
        public AttributeWriteResponse createFromParcel(Parcel in)
        {
            return new AttributeWriteResponse(in);
        }

        public AttributeWriteResponse[] newArray(int size)
        {
            return new AttributeWriteResponse[size];
        }
    };

    public AttributeWriteResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public AttributeWriteResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[in.readInt()];
        in.readByteArray(this.mRemoteBd);
        this.mCause = in.readInt();
        this.mSubCause = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * Constructor for factory method
     */
    public AttributeWriteResponse(int command)
    {
        this.mCommand = command;
    }

    public SafetyNumber<Integer> getCause()
    {
        return new SafetyNumber<Integer>(this.mCause, (-this.mCause));
    }

    public SafetyNumber<Integer> getSubCause()
    {
        return new SafetyNumber<Integer>(this.mSubCause, (-this.mSubCause));
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
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mRemoteBd = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mCause = buffer.get(); // 1 byte
        this.mSubCause = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes

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
        dest.writeInt(BlueConstant.BD_ADDR_LEN);
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mCause);
        dest.writeInt(this.mSubCause);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
