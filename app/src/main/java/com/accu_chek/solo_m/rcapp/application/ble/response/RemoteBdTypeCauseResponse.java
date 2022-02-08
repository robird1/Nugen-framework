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
 * The common response with BD address, BD type and cause for commands
 * "Remove Bonding", "Get Link Status". The command codes are
 * CommsConstant.CommandCode.BT_RMV_BOND and
 * CommsConstant.CommandCode.COMM_LINK_STATUS
 * 
 * @author EDLiu
 * @version 1.0
 * @created 26-Feb-2015 5:17:38 PM
 */
public class RemoteBdTypeCauseResponse implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Remote BD address
     */
    private byte[] mRemoteBD;
    /**
     * Remote BD type
     */
    private int mRemoteBdType;
    /**
     * Cause
     */
    private int mCause;
    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<RemoteBdTypeCauseResponse> CREATOR = new Parcelable.Creator<RemoteBdTypeCauseResponse>()
    {
        public RemoteBdTypeCauseResponse createFromParcel(Parcel in)
        {
            return new RemoteBdTypeCauseResponse(in);
        }

        public RemoteBdTypeCauseResponse[] newArray(int size)
        {
            return new RemoteBdTypeCauseResponse[size];
        }
    };

    public RemoteBdTypeCauseResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public RemoteBdTypeCauseResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBD = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBD);
        this.mRemoteBdType = in.readInt();
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
    public RemoteBdTypeCauseResponse(int command)
    {
        this.mCommand = command;
    }

    /**
     * return command code
     */
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /**
     * return TBlueAPI_Cause
     */
    public SafetyNumber<Integer> getCause()
    {
        return new SafetyNumber<Integer>(this.mCause, (-this.mCause));
    }

    /**
     * return remote BD type. Refer to BlueAPI TBlueAPI_RemoteBDType
     */
    public SafetyNumber<Integer> getRemoteBdType()
    {
        return new SafetyNumber<Integer>(this.mRemoteBdType, 
                (-this.mRemoteBdType));
    }

    /**
     * return remote BD address
     */
    public SafetyByteArray getRemoteBD()
    {
        return new SafetyByteArray(this.mRemoteBD,
                CRCTool.generateCRC16(this.mRemoteBD));
    }

    /**
     * "It is used for for some validation code -- in particular to implement 
     * Bundle.hasFileDescriptors()"
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
        dest.writeInt(this.mRemoteBdType);
        dest.writeInt(this.mCause);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

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
        this.mRemoteBD = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mRemoteBdType = buffer.get();
        this.mCause = buffer.get();

    }

}// [BT] Fixed Klocwork issue.
