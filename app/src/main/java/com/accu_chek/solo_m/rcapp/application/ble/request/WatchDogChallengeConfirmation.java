package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The request contains the calculated results of specified challenge message
 * from
 * Command code: CommsConstant.CommandCode.WATCHDOG_CHAL_CFM.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 5:10:47 PM
 */
public class WatchDogChallengeConfirmation implements IRequest, Parcelable
{

    /**
     * 
     */
    private static final int RESPONSE_DATA_LEN = 7;
    private int mCommand;
    /**
     * The SFM status should contain two flags set by UI processor.
     * 
     * Bit 0: UI SFM waiting for activities
     * Bit 1: UI SFM error flag
     */
    private byte mUI_SFM_Status;
    /**
     * The response data contains the calculated results of the challenge
     * message. The
     * response data should equal to the pre-calculated result stored in
     * communication
     * subsystem.
     * bytes length = 7
     */
    private byte[] mResponseData;

    public static final Parcelable.Creator<WatchDogChallengeConfirmation> CREATOR = new Parcelable.Creator<WatchDogChallengeConfirmation>()
    {
        public WatchDogChallengeConfirmation createFromParcel(Parcel in)
        {
            return new WatchDogChallengeConfirmation(in);
        }

        public WatchDogChallengeConfirmation[] newArray(int size)
        {
            return new WatchDogChallengeConfirmation[size];
        }
    };

    public WatchDogChallengeConfirmation()
    {

    }

    /**
     * constructor for IPC transaction
     */
    public WatchDogChallengeConfirmation(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mUI_SFM_Status = in.readByte();
        this.mResponseData = new byte[RESPONSE_DATA_LEN];
        in.readByteArray(this.mResponseData);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public WatchDogChallengeConfirmation(int command)
    {
        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
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

    /**
     * The response data contains the calculated results of the challenge
     * message. The
     * response data should equal to the pre-calculated result stored in
     * communication
     * subsystem.
     * bytes length = 7
     * 
     * @param data
     */
    public void setResponseData(SafetyByteArray data)
    {
        this.mResponseData = new byte[RESPONSE_DATA_LEN];
        System.arraycopy(data.getByteArray(), 0, this.mResponseData, 0,
                RESPONSE_DATA_LEN);
    }

    /**
     * 
     * @param flags
     */
    public void setUI_SFM_Status(SafetyNumber<Byte> flags)
    {

        this.mUI_SFM_Status = flags.get().byteValue();
    }

    /**
     * adding attribute values to byte array list for further frame wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(ByteConverter.getBytes((byte) this.mUI_SFM_Status)); // 1 byte
        data.add(this.mResponseData); // 7 bytes

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
        dest.writeByte(this.mUI_SFM_Status);
        // dest.writeInt(RESPONSE_DATA_LEN);
        dest.writeByteArray(mResponseData);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
