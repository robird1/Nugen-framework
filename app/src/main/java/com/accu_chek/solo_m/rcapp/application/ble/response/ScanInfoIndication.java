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
 * Command code: CommsConstant.CommandCode.BT_SCAN_INFO
 * 
 * @version 1.0
 * @created 26-Feb-2015 8:55:44 AM
 */
public class ScanInfoIndication implements IResponse, Parcelable
{
    private static final String TAG = "ScanInfoIndication";
    /**
     * Command code
     */
    private int mCommand;
    /**
     * Bluetooth address of remote device
     */
    private byte[] mRemoteBD;
    /**
     * Remote BD type
     */
    private int mRemoteBDType;
    /**
     * Advertising event type
     */
    private int mAdvType;
    /**
     * Remote signal strength indication
     */
    private byte mRSSI;
    /**
     * Data length indication : 0x00-0x1F
     */
    private int mDataLength;
    /**
     * Advertising data / Scan response data
     */
    private byte[] mData;
    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ScanInfoIndication> CREATOR = new Parcelable.Creator<ScanInfoIndication>()
    {
        public ScanInfoIndication createFromParcel(Parcel in)
        {
            return new ScanInfoIndication(in);
        }

        public ScanInfoIndication[] newArray(int size)
        {
            return new ScanInfoIndication[size];
        }
    };

    public ScanInfoIndication()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public ScanInfoIndication(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBD = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBD);
        this.mRemoteBDType = in.readInt();
        this.mAdvType = in.readInt();
        this.mRSSI = in.readByte();
        this.mDataLength = in.readInt();
        this.mData = new byte[this.mDataLength];
        in.readByteArray(this.mData);
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ScanInfoIndication(int command)
    {
        this.mCommand = command;
    }

    /**
     * Remote BD type. Refer to BlueAPI TBlueAPI_RemoteBDType for definition
     */
    public SafetyNumber<Integer> getRemoteBDType()
    {
        return new SafetyNumber<Integer>(this.mRemoteBDType,
                (-this.mRemoteBDType));
    }

    /**
     * Advertising type. Refer to BlueAPI TBlueAPI_LEAdvType for definition
     */
    public SafetyNumber<Integer> getAdvType()
    {
        return new SafetyNumber<Integer>(this.mAdvType, (-this.mAdvType));
    }

    /**
     * return advertising data.
     * See Bluetooth Core specification
     * or ComSpec_SoloM_Profile "Advertising Information"
     * 
     */
    public SafetyByteArray getData()
    {

        return new SafetyByteArray(this.mData,
                CRCTool.generateCRC16(this.mData));

    }

    /**
     * return data length
     */
    public SafetyNumber<Integer> getDataLength()
    {
        return new SafetyNumber<Integer>(this.mDataLength, -this.mDataLength);
    }

    /**
     * return RSSI
     * RSSI is a signed int8 in CommsSystem
     */
    public SafetyNumber<Byte> getRSSI()
    {

        return new SafetyNumber<Byte>(this.mRSSI, (byte) -this.mRSSI);
    }

    /**
     * get remote BD address
     */
    public SafetyByteArray getRemoteBD()
    {
        return new SafetyByteArray(this.mRemoteBD,
                CRCTool.generateCRC16(this.mRemoteBD));
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
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);
        this.mRemoteBD = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mRemoteBDType = buffer.get();
        this.mAdvType = buffer.get();
        this.mRSSI = buffer.get();
        this.mDataLength = buffer.get();
        this.mData = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                this.mDataLength);

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
        dest.writeByteArray(this.mRemoteBD);
        dest.writeInt(this.mRemoteBDType);
        dest.writeInt(this.mAdvType);
        dest.writeByte(this.mRSSI);
        dest.writeInt(this.mDataLength);
        dest.writeByteArray(this.mData);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}// [BT] Fixed Klocwork issue.
