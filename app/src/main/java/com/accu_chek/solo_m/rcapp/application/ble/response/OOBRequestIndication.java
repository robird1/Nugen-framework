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
 * The command code is CommsConstant.CommandCode.BT_OOB_IND.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 8:26:14 PM
 */
public class OOBRequestIndication implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * BD Address
     */
//    private byte[] mRemoteBd;

    /**
     * original message byte array
     * 
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<OOBRequestIndication> CREATOR = new Parcelable.Creator<OOBRequestIndication>()
    {
        public OOBRequestIndication createFromParcel(Parcel in)
        {
            return new OOBRequestIndication(in);
        }

        public OOBRequestIndication[] newArray(int size)
        {
            return new OOBRequestIndication[size];
        }
    };

    public OOBRequestIndication()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public OOBRequestIndication(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public OOBRequestIndication(int command)
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
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
