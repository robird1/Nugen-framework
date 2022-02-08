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
 * The command code is CommsConstant.CommandCode.BT_ATTR_READ.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 11:00:03 AM
 */
public class AttributeReadResponse implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * BD address
     */
    private byte[] mRemoteBd;

    /**
     * Result
     */
    private int mResult;
    /**
     * GATT read type: basic(0x01) or UUID(0x02)
     */

    private int mReadType;
    /**
     * Indicates the result of the transaction
     * - TBlueAPI_Cause
     */
    private int mCause;
    /**
     * More detailed result information for lower protocol layers
     */
    private int mSubcause;
    /**
     * readType = Basic:
     * The first readOffset bytes of the attribute value have been skipped
     * before
     * reading started
     */
    private int mReadOffset;
    /**
     * Total number of bytes stored in the handlesData[] array
     */
    private int mTotalLength;
    /**
     * Length of a single attribute value that is stored in the handlesData
     */
    private int mAttribLength;
    /**
     * Number of handles stored in the handlesData
     */
    private int mNbrOfHandles;
    /**
     * Offset set of first handle in handlesData
     */
    private int mGap;
    /**
     * Array of attribute handles and values read
     */
    private byte[] mData;

    /**
     * original message bytes
     * 
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<AttributeReadResponse> CREATOR = new Parcelable.Creator<AttributeReadResponse>()
    {
        public AttributeReadResponse createFromParcel(Parcel in)
        {
            return new AttributeReadResponse(in);
        }

        public AttributeReadResponse[] newArray(int size)
        {
            return new AttributeReadResponse[size];
        }
    };

    public AttributeReadResponse()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public AttributeReadResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mResult = in.readInt();
        this.mReadType = in.readInt();
        this.mCause = in.readInt();
        this.mSubcause = in.readInt();
        this.mReadOffset = in.readInt();
        this.mTotalLength = in.readInt();
        this.mAttribLength = in.readInt();
        this.mNbrOfHandles = in.readInt();
        this.mGap = in.readInt();
        this.mData = new byte[this.mAttribLength];
        in.readByteArray(this.mData);
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);

    }

    /**
     * constructor for factory method
     */
    public AttributeReadResponse(int command)
    {

        this.mCommand = command;
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
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mRemoteBd = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mResult = buffer.get(); // 1 byte
        this.mReadType = buffer.get(); // 1 byte
        this.mCause = buffer.get(); // 1 byte
        this.mSubcause = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mReadOffset = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mTotalLength = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mAttribLength = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mNbrOfHandles = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mGap = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        
        if( 0!= mTotalLength &&  0!=mAttribLength) //05102015_Kidd
        {
        	ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                    this.mGap+2);
        }
        else
        {
        	ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                    this.mGap);
        }
//        ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
//                this.mGap+2);
        this.mData = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                this.mAttribLength);

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
        // dest.writeInt(BluetoothConstant.BD_ADDR_LEN);
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mResult);
        dest.writeInt(this.mReadType);
        dest.writeInt(this.mCause);
        dest.writeInt(this.mSubcause);
        dest.writeInt(this.mReadOffset);
        dest.writeInt(this.mTotalLength);
        dest.writeInt(this.mAttribLength);
        dest.writeInt(this.mNbrOfHandles);
        dest.writeInt(this.mGap);
        // dest.writeInt(BluetoothConstant.MAX_GATT_BUFFER_SIZE);
        dest.writeByteArray(mData);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

    /*
     * Return remote BD address
     */
    public SafetyByteArray getRemoteBD()
    {
        return new SafetyByteArray(this.mRemoteBd,
                CRCTool.generateCRC16(this.mRemoteBd));
    }

    /*
     * Return read type
     * Refer to TBlueAPI_GATTReadType
     */
    public SafetyNumber<Integer> getReadType()
    {
        return new SafetyNumber<Integer>(this.mReadType, (-this.mReadType));
    }

    /**
     * Return result
     * Refer to CommsConstant.Result for valid values (Hamming Distance).
     */
    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mResult, (-this.mResult));
    }

    /**
     * Return cause
     * Refer to TBlueAPI_Cause
     */
    public SafetyNumber<Integer> getCause()
    {
        return new SafetyNumber<Integer>(this.mCause, (-this.mCause));
    }

    /**
     * Return subcause
     * Refer to TBlueAPI_Cause
     */
    public SafetyNumber<Integer> getSubcause()
    {
        return new SafetyNumber<Integer>(this.mSubcause, (-this.mSubcause));
    }

    /**
     * return read offset
     */
    public SafetyNumber<Integer> getReadOffset()
    {
        return new SafetyNumber<Integer>(this.mReadOffset, (-this.mReadOffset));
    }

    /**
     * return total length of handle data bytes
     */
    public SafetyNumber<Integer> getTotalLength()
    {
        return new SafetyNumber<Integer>(this.mTotalLength,
                (-this.mTotalLength));
    }

    /**
     * return length of attribute length
     */
    public SafetyNumber<Integer> getAttributeLength()
    {
        return new SafetyNumber<Integer>(this.mAttribLength,
                (-this.mAttribLength));
    }

    /**
     * return length of attribute length
     */
    public SafetyNumber<Integer> getNumberOfHandle()
    {
        return new SafetyNumber<Integer>(this.mNbrOfHandles,
                (-this.mNbrOfHandles));
    }

    /**
     * return gap
     */
    public SafetyNumber<Integer> getGap()
    {
        return new SafetyNumber<Integer>(this.mGap, (-this.mGap));
    }

    /**
     * return data bytes
     */
    public SafetyByteArray getData()
    {
        return new SafetyByteArray(this.mData,
                CRCTool.generateCRC16(this.mData));
    }

}// [BT] Fixed Klocwork issue.
