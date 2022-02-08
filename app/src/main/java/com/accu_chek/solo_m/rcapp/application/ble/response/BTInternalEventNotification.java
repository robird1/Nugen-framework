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
 * The reponse is for possible causes for an internalEventInfo reported by
 * stollmann BlueAPI.
 * The command code is CommsConstant.CommandCode.BT_INTERNAL_ERR.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 7:46:15 PM
 */
public class BTInternalEventNotification implements IResponse, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Indicates the reason for this indication
     */
    private int mEventType;
    /**
     * Additional information for internal reproduction and tracing purpose
     */
    private int mEventInfo;
    /**
     * Provides some additional information why this message is initiated
     */
    private int mCause;

    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<BTInternalEventNotification> CREATOR = new Parcelable.Creator<BTInternalEventNotification>()
    {
        public BTInternalEventNotification createFromParcel(Parcel in)
        {
            return new BTInternalEventNotification(in);
        }

        public BTInternalEventNotification[] newArray(int size)
        {
            return new BTInternalEventNotification[size];
        }
    };

    public BTInternalEventNotification()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public BTInternalEventNotification(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mEventType = in.readInt();
        this.mEventInfo = in.readInt();
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
    public BTInternalEventNotification(int command)
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
    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mEventType = buffer.get(); // 1 byte
        this.mEventInfo = ByteConverter
                .readInt(buffer, ByteOrder.LITTLE_ENDIAN); // 4 bytes
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
        dest.writeInt(this.mEventType);
        dest.writeInt(this.mEventInfo);
        dest.writeInt(this.mCause);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
