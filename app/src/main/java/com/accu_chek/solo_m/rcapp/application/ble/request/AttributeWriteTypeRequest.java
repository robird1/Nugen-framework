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
 * Request for command code CommsConstant.CommandCode.BT_ATTR_WRITE
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 1:43:38 PM
 */
public class AttributeWriteTypeRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * BD Address
     */
    private byte[] mRemoteBd;
    /**
     * <b>TBlueAPI_GATTWriteType </b>
     */
    private int mWriteType;
    /**
     * The number of attribute handle.
     */
    private int mAttribHandle;

    /**
     * UUID
     */
    private int mUUID16;

    /**
     * UUID128
     */
//    private byte[] mUUID128;
    /**
     * Length of data to be written
     */
    private int mAttribLength;
    /**
     * Write offset in attribute, currently ignored
     */
    private int mWriteOffset;
    /**
     * Max buffer for data to write to attribute
     */
    private byte[] mData;

    public static final Parcelable.Creator<AttributeWriteTypeRequest> CREATOR = new Parcelable.Creator<AttributeWriteTypeRequest>()
    {
        public AttributeWriteTypeRequest createFromParcel(Parcel in)
        {
            return new AttributeWriteTypeRequest(in);
        }

        public AttributeWriteTypeRequest[] newArray(int size)
        {
            return new AttributeWriteTypeRequest[size];
        }
    };

    public AttributeWriteTypeRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     */
    public AttributeWriteTypeRequest(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);

        this.mWriteType = in.readInt();
        this.mAttribHandle = in.readInt();
        this.mUUID16 = in.readInt();
//        this.mUUID128 = new byte[BlueConstant.UUID128_LEN];
//        in.readByteArray(this.mUUID128);
        this.mAttribLength = in.readInt();
        this.mWriteOffset = in.readInt();
        this.mData = new byte[in.readInt()];
        in.readByteArray(this.mData);
    }

    /**
     * Constructor for factory method
     */
    public AttributeWriteTypeRequest(int command)
    {
        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
    }

    /**
     * set bluetooth device address.
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
     * Set handle value
     * 
     * @handle handle number
     */
    public void setAttributeHandle(SafetyNumber<Integer> handle)
    {
        this.mAttribHandle = handle.get().intValue();
    }

    /**
     * Set attribute length
     * 
     * @param length
     */
    public void setAttributeLength(SafetyNumber<Integer> length)
    {

        this.mAttribLength = length.get().intValue();
    }

    /**
     * set data
     * 
     * @param data The byte array should be wrapped in SafetyByteArray
     */
    public void setData(SafetyByteArray data)
    {
        byte[] bytes = data.getByteArray();
        this.mData = new byte[bytes.length];
        System.arraycopy(bytes, 0, this.mData, 0, this.mData.length);
    }

    /**
     * Set UUID
     * 
     * @param uuid UUID
     */
    public void setUUID16(SafetyNumber<Integer> uuid)
    {
        this.mUUID16 = uuid.get().intValue();
    }

    /**
     * Set UUID128
     * 
     * @param uuid The UUID128 bytes should be wrapped in SafetyByteArray. The
     *            byte length is 16.
     */
//    public void setUUID128(SafetyByteArray uuid)
//    {
//        this.mUUID128 = new byte[BlueConstant.UUID128_LEN];
//        System.arraycopy(uuid.getByteArray(), 0, this.mUUID128, 0,
//                BlueConstant.UUID128_LEN);
//    }

    /**
     * set write offset
     * 
     * @param offset
     */
    public void setWriteOffset(SafetyNumber<Integer> offset)
    {

        this.mWriteOffset = offset.get().intValue();
    }

    /**
     * Set GATT Write Type
     * See stollmann BlueAPI TBlueAPI_GATTWriteType for definition.
     * 
     * @param type
     */
    public void setWriteType(SafetyNumber<Integer> type)
    {
        this.mWriteType = type.get().intValue();
    }

    /**
     * adding attribute values to byte array list for further frame wrapping
     */

    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(this.mRemoteBd);
        data.add(ByteConverter.getBytes((byte) this.mWriteType)); // 1 byte
        data.add(ByteConverter.getBytes((short) this.mAttribHandle)); // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mUUID16)); // 2 bytes
//        data.add(this.mUUID128);
        data.add(ByteConverter.getBytes((short) this.mAttribLength)); // 2 bytes
        data.add(ByteConverter.getBytes((byte) this.mWriteOffset)); // 1 byte
        data.add(this.mData);

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
        dest.writeInt(this.mWriteType);
        dest.writeInt(this.mAttribHandle);
        dest.writeInt(this.mUUID16);
//        dest.writeByteArray(this.mUUID128);
        dest.writeInt(this.mAttribLength);
        dest.writeInt(this.mWriteOffset);
        dest.writeInt(this.mData.length);
        dest.writeByteArray(this.mData);

    }

}// [BT] Fixed Klocwork issue.
