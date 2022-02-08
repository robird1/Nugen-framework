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
 * Request for command CommsConstant.CommandCode.BT_CONNECT.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 5:51:57 PM
 */
public class ConnectRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Bluetooth device address
     */
    private byte[] mRemoteBd;
    /**
     * Please refer to stollmann TBlueAPI_RemoteBDType in BlueAPI specification
     */
    private int mBdType;
    /**
     * Device type 1:
     */
    private int mDeviceType;

    public static final Parcelable.Creator<ConnectRequest> CREATOR = new Parcelable.Creator<ConnectRequest>()
    {
        public ConnectRequest createFromParcel(Parcel in)
        {
            return new ConnectRequest(in);
        }

        public ConnectRequest[] newArray(int size)
        {
            return new ConnectRequest[size];
        }
    };

    public ConnectRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public ConnectRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mBdType = in.readInt();
        this.mDeviceType = in.readInt();
    }

    /**
     * Constructor for factory method
     */
    public ConnectRequest(int command)
    {
        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
    }

    @Override
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /**
     * set bluetooth device address
     * 
     * @param addr
     */
    public void setRemoteBD(SafetyByteArray address)
    {
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        System.arraycopy(address.getByteArray(), 0, this.mRemoteBd, 0,
                BlueConstant.BD_ADDR_LEN);
    }

    /**
     * Set BD type
     * 
     * @param type
     */
    public void setBdType(SafetyNumber<Integer> type)
    {
        this.mBdType = type.get().intValue();
    }

    /**
     * Set Device Type
     * 
     * @param type
     *            See BlueConstant.DeviceType for valid values.
     */
    public void setDeviceType(SafetyNumber<Integer> type)
    {
        this.mDeviceType = type.get().intValue();
    }

    /**
     * adding attribute values to byte array list for further frame wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(this.mRemoteBd);
        data.add(ByteConverter.getBytes((byte) this.mBdType)); // 1 byte
        data.add(ByteConverter.getBytes((byte) this.mDeviceType)); // 1 byte

    }

    /**
     * 
     * @param dest
     * @param flags
     */
    @Override
    public void writeToParcel(Parcel dest, int flags)
    {

        dest.writeInt(this.mCommand);
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mBdType);
        dest.writeInt(this.mDeviceType);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
