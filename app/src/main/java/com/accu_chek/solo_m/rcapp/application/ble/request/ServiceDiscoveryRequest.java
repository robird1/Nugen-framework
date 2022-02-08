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
 * The command is CommsConstant.CommandCode.BT_DISCOVERY_SRV.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 01-Mar-2015 2:36:22 PM
 */
public class ServiceDiscoveryRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * BD Address
     */
    private byte[] mRemoteBd;
    private int mDiscoveryType;
    /**
     * Start of handle range to be searched for matching UUID
     */
    private int mStartHandle;
    /**
     * End of handle range to be searched for matching UUID
     */
    private int mEndHandle;
    /**
     * UUID to search for. If set to zero, UUID128 is searched for instead
     */
    private int mUUID16;
    /**
     * 128 bit UUID to search for if UUID16 is set to zero
     */
    private byte[] mUUID128;

    public static final Parcelable.Creator<ServiceDiscoveryRequest> CREATOR = new Parcelable.Creator<ServiceDiscoveryRequest>()
    {
        public ServiceDiscoveryRequest createFromParcel(Parcel in)
        {
            return new ServiceDiscoveryRequest(in);
        }

        public ServiceDiscoveryRequest[] newArray(int size)
        {
            return new ServiceDiscoveryRequest[size];
        }
    };

    public ServiceDiscoveryRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public ServiceDiscoveryRequest(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mDiscoveryType = in.readInt();
        this.mStartHandle = in.readInt();
        this.mEndHandle = in.readInt();
        this.mUUID16 = in.readInt();
        this.mUUID128 = new byte[BlueConstant.UUID128_LEN];
        in.readByteArray(this.mUUID128);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ServiceDiscoveryRequest(int command)
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
     * set discovery type. Refer to BlueAPI TBlueAPI_DiscoveryType
     * 
     * @param type
     */
    public void setDiscoveryType(SafetyNumber<Integer> type)
    {
        this.mDiscoveryType = type.get().intValue();
    }

    /**
     * set end handle
     * 
     * @param handle
     */
    public void setEndHandle(SafetyNumber<Integer> handle)
    {
        this.mEndHandle = handle.get().intValue();
    }

    /**
     * set remote BD address
     * 
     * @param address address, byte array with length of 6.
     */
    public void setRemoteBD(SafetyByteArray address)
    {
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        System.arraycopy(address.getByteArray(), 0, this.mRemoteBd, 0,
                BlueConstant.BD_ADDR_LEN);
    }

    /**
     * set start handle
     * 
     * @param handle
     */
    public void setStartHandle(SafetyNumber<Integer> handle)
    {
        this.mStartHandle = handle.get().intValue();
    }

    /**
     * 
     * @param uuid The length of UUID128 is defined by BluetoothConstant.
     *            SIZE_OF_UUID128
     */

    public void setUUID128(SafetyByteArray uuid)
    {
        this.mUUID128 = new byte[BlueConstant.UUID128_LEN];
        System.arraycopy(uuid.getByteArray(), 0, this.mUUID128, 0,
                BlueConstant.UUID128_LEN);
    }

    /**
     * set UUID16
     * 
     * @param uuid
     */
    public void setUUID16(SafetyNumber<Integer> uuid)
    {
        this.mUUID16 = uuid.get().intValue();
    }

    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand));
        data.add(this.mRemoteBd);
        data.add(ByteConverter.getBytes((byte) this.mDiscoveryType));
        data.add(ByteConverter.getBytes((short) this.mStartHandle));
        data.add(ByteConverter.getBytes((short) this.mEndHandle));
        data.add(ByteConverter.getBytes((short) this.mUUID16));
        data.add(this.mUUID128);

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
        dest.writeInt(this.mDiscoveryType);
        dest.writeInt(this.mStartHandle);
        dest.writeInt(this.mEndHandle);
        dest.writeInt(this.mUUID16);
        dest.writeByteArray(this.mUUID128);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
