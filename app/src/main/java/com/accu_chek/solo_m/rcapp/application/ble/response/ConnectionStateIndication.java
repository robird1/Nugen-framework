package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * Indication of connection change event.
 * Command code: CommsConstant.CommandCode.BT_CONNECTION_STATE.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 1:36:42 PM
 */
public class ConnectionStateIndication implements IResponse, Parcelable

{

    private int mCommand;
    /**
     * Remote BD Address
     */
    private byte[] mRemoteBd;
    /**
     * Connection State
     */
    private int mState;
    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ConnectionStateIndication> CREATOR = new Parcelable.Creator<ConnectionStateIndication>()
    {
        public ConnectionStateIndication createFromParcel(Parcel in)
        {
            return new ConnectionStateIndication(in);
        }

        public ConnectionStateIndication[] newArray(int size)
        {
            return new ConnectionStateIndication[size];
        }
    };

    public ConnectionStateIndication()
    {

    }

    /**
     * constructor for IPC transaction
     */
    public ConnectionStateIndication(Parcel in)
    {

        this.mCommand = in.readInt();

        this.mRemoteBd = new byte[in.readInt()];
        in.readByteArray(this.mRemoteBd);

        this.mState = in.readInt();

        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ConnectionStateIndication(int command)
    {
        this.mCommand = command;
    }

    /**
     * Command Code
     * Refer to CommsConstant.CommandCode for valid values(Hamming Distance).
     */
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /**
     * BT State
     * Refer to CommsConstant.BtState for valid values(Hamming Distance).
     */
    public SafetyNumber<Integer> getState()
    {
        return new SafetyNumber<Integer>(this.mState, (-this.mState));
    }

    /**
     * Remote BD Address
     */
    public SafetyByteArray getRemoteBD()
    {
        return new SafetyByteArray(this.mRemoteBd,
                CRCTool.generateCRC16(this.mRemoteBd));
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
        dest.writeInt(this.mRemoteBd.length);
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mState);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

    @Override
    public void setMessage(SafetyByteArray message)
    {
        this.mMessage = message.getByteArray();

    }

    /**
     * @return original message byte array
     */
    @Override
    public byte[] getMessage()
    {

        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

    /**
     * parse attributes from message byte array
     */
    @Override
    public void parseMessage()
    {

        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mRemoteBd = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mState = buffer.get(); // 1 byte

    }

}// [BT] Fixed Klocwork issue.
