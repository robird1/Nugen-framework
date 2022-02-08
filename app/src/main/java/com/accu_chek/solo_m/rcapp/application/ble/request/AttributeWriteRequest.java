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
 * Request for command CommsConstant.CommandCode.BT_ATTR_WRITE
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 1:08:50 PM
 */
public class AttributeWriteRequest implements IRequest, Parcelable
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
     * Handle of attribute
     */
    private int mAttribHandle;
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

    public static final Parcelable.Creator<AttributeWriteRequest> CREATOR = new Parcelable.Creator<AttributeWriteRequest>()
    {
        public AttributeWriteRequest createFromParcel(Parcel in)
        {
            return new AttributeWriteRequest(in);
        }

        public AttributeWriteRequest[] newArray(int size)
        {
            return new AttributeWriteRequest[size];
        }
    };

    public AttributeWriteRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public AttributeWriteRequest(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mAttribHandle = in.readInt();
        this.mAttribLength = in.readInt();
        this.mWriteOffset = in.readInt();
        this.mData = new byte[in.readInt()];
        in.readByteArray(this.mData);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public AttributeWriteRequest(int command)
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
     * set attribute handle
     * 
     * @handle handle number
     */
    public void setAttributeHandle(SafetyNumber<Integer> handle)
    {

        this.mAttribHandle = handle.get().intValue();
    }

    /**
     * set attribute length
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
     * @data data
     */
    public void setData(SafetyByteArray data)
    {
        byte[] bytes = data.getByteArray();
        this.mData = new byte[bytes.length];
        System.arraycopy(bytes, 0, this.mData, 0, this.mData.length);

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
     * set write offset
     * 
     * @param offset
     * 
     */
    public void setWriteOffset(SafetyNumber<Integer> offset)
    {
        this.mWriteOffset = offset.get().intValue();
    }

    /**
     * adding attribute values to byte array list for further frame wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(this.mRemoteBd); // 6 bytes
        data.add(ByteConverter.getBytes((short) this.mAttribHandle)); // 2 bytes
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
        dest.writeInt(BlueConstant.BD_ADDR_LEN);
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mAttribHandle);
        dest.writeInt(this.mAttribLength);
        dest.writeInt(this.mWriteOffset);
        dest.writeInt(this.mData.length);
        dest.writeByteArray(this.mData);

    }
}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
