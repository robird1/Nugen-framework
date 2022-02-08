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
 * The response to watch dog challenge confirmation from Comms subsystem.
 * The command code is CommsConstant.CommandCode.WATCHDOG_CHAL_CFM.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 5:28:21 PM
 */
public class WatchDogChallengeCfmResponse implements IResponse, Parcelable
{

    /**
     * Command
     */
    private int mCommand;
    /**
     * Response byte code
     */
    private int mResponseByte;

    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<WatchDogChallengeCfmResponse> CREATOR 
    = new Parcelable.Creator<WatchDogChallengeCfmResponse>()
    {
        public WatchDogChallengeCfmResponse createFromParcel(Parcel in)
        {
            return new WatchDogChallengeCfmResponse(in);
        }

        public WatchDogChallengeCfmResponse[] newArray(int size)
        {
            return new WatchDogChallengeCfmResponse[size];
        }
    };

    public WatchDogChallengeCfmResponse()
    {

    }

    /**
     * constructor for IPC transaction
     */
    public WatchDogChallengeCfmResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mResponseByte = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public WatchDogChallengeCfmResponse(int command)
    {

        this.mCommand = command;
    }

    public int getResponseByte()
    {
        return this.mResponseByte;
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
     * parse attribute values from message bytes
     */
    @Override
    public void parseMessage()
    {

        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mResponseByte = ByteConverter.readShort(buffer,
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
        dest.writeInt(this.mResponseByte);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
