package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * Request for command CommsConstant.CommandCode.BT_CCCD_CONFIG.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 5:16:20 PM
 */

public class CCCDRequest implements IRequest, Parcelable
{

    private int mCommand;
    /**
     * BD Address
     */
    private byte[] mRemoteBd;
    /**
     * UUID
     */
    private int mWUUID;
    /**
     * UUID128
     */
//    private byte[] mUUID128;
    private int mParam;

    public static final Parcelable.Creator<CCCDRequest> CREATOR = new Parcelable.Creator<CCCDRequest>()
    {
        public CCCDRequest createFromParcel(Parcel in)
        {
            return new CCCDRequest(in);
        }

        public CCCDRequest[] newArray(int size)
        {
            return new CCCDRequest[size];
        }
    };

    public CCCDRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public CCCDRequest(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mWUUID = in.readInt();
//        this.mUUID128 = new byte[BlueConstant.BD_ADDR_LEN];
//        in.readByteArray(this.mUUID128);
        this.mParam = in.readInt();
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public CCCDRequest(int command)
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
     * set parameter
     * 
     * @param param
     */
    public void setParameter(SafetyNumber<Integer> param)
    {
        this.mParam = param.get().intValue();
    }

    /**
     * set remote BD address
     * 
     * @param address
     */
    public void setRemoteBDAddress(SafetyByteArray address)
    {
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        System.arraycopy(address.getByteArray(), 0, this.mRemoteBd, 0,
                BlueConstant.BD_ADDR_LEN);
    }

    /**
     * set UUID
     * 
     * @param uuid
     */
    public void setUUID(SafetyNumber<Integer> uuid)
    {
        this.mWUUID = uuid.get().intValue();
    }

    /**
     * set UUID128
     * 
     * @param uuid
     */
//    public void setUUID128(SafetyByteArray uuid)
//    {
//        this.mUUID128 = new byte[BlueConstant.UUID128_LEN];
//        System.arraycopy(uuid.getByteArray(), 0, this.mUUID128, 0,
//                BlueConstant.UUID128_LEN);
//    }

    /**
     * adding attribute values to byte array list for further frame wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(this.mRemoteBd);
        data.add(ByteConverter.getBytes((short) this.mWUUID)); // 2 bytes
//        data.add(this.mUUID128);
        data.add(ByteConverter.getBytes((short) this.mParam)); // 2 bytes

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
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mWUUID);
//        dest.writeByteArray(this.mUUID128);
        dest.writeInt(this.mParam);

    }
}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
