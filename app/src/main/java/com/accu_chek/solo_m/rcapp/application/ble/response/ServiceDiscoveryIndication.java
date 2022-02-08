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
 * The command code is CommsConstant.CommandCode.BT_DISCOVERY_INFO.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 26-Feb-2015 6:42:29 PM
 */
public class ServiceDiscoveryIndication implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Discovery type - TBlueAPI_GATTDiscoveryType
     */
    private int mDiscoveryType;
    /**
     * Cause
     */
    private int mCause;
    /**
     * Sub-cause of result
     */
    private int mSubCause;
    /**
     * Total number of elements
     */
    private int mElementCount;
    /**
     * Total number of elements
     */
    private int mElementLength;
    /**
     * Offset of first element in data list
     */
    private int mGap;
    /**
     * length = elementLength* elementCount
     */
    private byte[] mData;
    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ServiceDiscoveryIndication> CREATOR = new Parcelable.Creator<ServiceDiscoveryIndication>()
    {
        public ServiceDiscoveryIndication createFromParcel(Parcel in)
        {
            return new ServiceDiscoveryIndication(in);
        }

        public ServiceDiscoveryIndication[] newArray(int size)
        {
            return new ServiceDiscoveryIndication[size];
        }
    };

    public ServiceDiscoveryIndication()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public ServiceDiscoveryIndication(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mDiscoveryType = in.readInt();
        this.mCause = in.readInt();
        this.mSubCause = in.readInt();
        this.mElementCount = in.readInt();
        this.mElementLength = in.readInt();
        this.mGap = in.readInt();
        this.mData = new byte[in.readInt()];
        in.readByteArray(this.mData);

    }

    public ServiceDiscoveryIndication(int command)
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

    /**
     * Length of data is elementCount * elementLength
     */
    public byte[] getData()
    {
        return Arrays.copyOf(this.mData, mData.length);
    }

    /**
     * Element length
     */
    public SafetyNumber<Integer> getElementLength()
    {
        return new SafetyNumber<Integer>(this.mElementLength, 
                (-this.mElementLength));
    }

    /**
     * Element count
     */
    public SafetyNumber<Integer> getElementCount()
    {
        return new SafetyNumber<Integer>(this.mElementCount, 
                (-this.mElementCount));
    }

    /**
     * 
     * @return
     */
    public SafetyNumber<Integer> getGap()
    {
        return new SafetyNumber<Integer>(this.mGap, (-this.mGap));
    }

    /**
     * 
     * @param mGap
     */
    public void setGap(int mGap)
    {
        this.mGap = mGap;
    }

    /**
     * subcause
     */
    public SafetyNumber<Integer> getSubcause()
    {
        return new SafetyNumber<Integer>(this.mSubCause, (-this.mSubCause));
    }

    /**
     * Discovery Type
     */
    public int getDiscoveryType()
    {
        return this.mDiscoveryType;
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
        int length = 0;
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);
        this.mDiscoveryType = buffer.get();
        this.mCause = buffer.get();
        this.mSubCause = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN);
        this.mElementCount = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN);
        this.mElementLength = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN);
        this.mGap = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN);

        if (0 != this.mGap)
        {
            ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                    this.mGap);
        }
        
        length = this.mElementCount * this.mElementLength;
        this.mData = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                length);

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
        dest.writeInt(this.mDiscoveryType);
        dest.writeInt(this.mCause);
        dest.writeInt(this.mSubCause);
        dest.writeInt(this.mElementCount);
        dest.writeInt(this.mElementLength);
        dest.writeInt(this.mGap);
        dest.writeInt(this.mData.length);
        dest.writeByteArray(this.mData);

    }

}// [BT] Fixed Klocwork issue.
