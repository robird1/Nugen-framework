package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

import android.os.Parcel;
import android.os.Parcelable;

/**
 * The common response contains remote BD address and cause only for commands
 * "Disconnect", "Connection Update", " Set Security".
 * 
 * @author EDLiu
 * @version 1.0
 * @created 26-Feb-2015 4:46:40 PM
 */
public class RemoteBdCauseResponse implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Remote bluetooth device address
     */
    private byte[] mRemoteBD;

    /**
     * BlueAPI TBlueAPI_Cause
     */
    private int mCause;

    /**
     * original message bytes
     * 
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<RemoteBdCauseResponse> CREATOR = new Parcelable.Creator<RemoteBdCauseResponse>()
    {
        public RemoteBdCauseResponse createFromParcel(Parcel in)
        {
            return new RemoteBdCauseResponse(in);
        }

        public RemoteBdCauseResponse[] newArray(int size)
        {
            return new RemoteBdCauseResponse[size];
        }
    };

    public RemoteBdCauseResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public RemoteBdCauseResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBD = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBD);
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
    public RemoteBdCauseResponse(int command)
    {
        this.mCommand = command;
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


    /*
     * Return remote BD address
     */
    public SafetyByteArray getRemoteBD()
    {
        return new SafetyByteArray(this.mRemoteBD,
                CRCTool.generateCRC16(this.mRemoteBD));
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
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);
        this.mRemoteBD = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mCause = buffer.get();

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
        dest.writeByteArray(this.mRemoteBD);
        dest.writeInt(this.mCause);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
