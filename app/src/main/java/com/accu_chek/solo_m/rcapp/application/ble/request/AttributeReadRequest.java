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
 * Request for command code CommsConstant.CommandCode.BT_ATTR_READ
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 10:31:02 AM
 */

public class AttributeReadRequest implements IRequest, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Remote BD Address
     */
    private byte[] mRemoteBd;
    /**
     * GATT read type: basic(0x01) or UUID(0x02)
     */
    private int mReadType;
    /**
     * The first read Offset bytes of the attribute value have been skipped
     * before reading started
     */
    private int mReadOffset;
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

    public static final Parcelable.Creator<AttributeReadRequest> CREATOR = new Parcelable.Creator<AttributeReadRequest>()
    {
        public AttributeReadRequest createFromParcel(Parcel in)
        {
            return new AttributeReadRequest(in);
        }

        public AttributeReadRequest[] newArray(int size)
        {
            return new AttributeReadRequest[size];
        }
    };

    public AttributeReadRequest()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public AttributeReadRequest(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mReadType = in.readInt();
        this.mReadOffset = in.readInt();
        this.mStartHandle = in.readInt();
        this.mEndHandle = in.readInt();
        this.mUUID16 = in.readInt();
        this.mUUID128 = new byte[BlueConstant.UUID128_LEN];
        in.readByteArray(mUUID128);
    }

    /**
     * constructor for factory method
     */
    public AttributeReadRequest(int command)
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
     * set end handle
     * 
     * @param handle
     */
    public void setEndHandle(SafetyNumber<Integer> handle)
    {
        this.mEndHandle = handle.get().intValue();
    }

    /**
     * set read offset
     * 
     * @param offset
     */
    public void setReadOffset(SafetyNumber<Integer> offset)
    {
        this.mReadOffset = offset.get().intValue();
    }

    /**
     * set read type
     * 
     * @param type. See BlueAPI TBlueAPI_GATTReadType for definition, refer to
     *            BlueConstant.ReadType
     */
    public void setReadType(SafetyNumber<Integer> type)
    {
        this.mReadType = type.get().intValue();
    }

    /**
     * set bluetooth device address
     * 
     * @param address The MAC address of the remote BD.
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
     * set UUID128
     * 
     * @param uuid
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

    /**
     * The function converts attribute values to bytes and put into an array
     * list reference for further frame wrapping
     * 
     * @param data The array list reference to add in the bytes of attribute
     *            values.
     *            Range: Not NULL or NULL
     *            Unit: N/A
     *            Scaling: 1
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(this.mRemoteBd);
        data.add(ByteConverter.getBytes((byte) this.mReadType)); // 1 byte
        data.add(ByteConverter.getBytes((short) this.mReadOffset)); // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mStartHandle)); // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mEndHandle)); // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mUUID16)); // 2 bytes
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
        // dest.writeInt(BluetoothConstant.BD_ADDR_LEN);
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mReadType);
        dest.writeInt(this.mReadOffset);
        dest.writeInt(this.mStartHandle);
        dest.writeInt(this.mEndHandle);
        dest.writeInt(this.mUUID16);
        // dest.writeInt(BluetoothConstant.SIZE_OF_UUID128);
        dest.writeByteArray(this.mUUID128);

    }
}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
