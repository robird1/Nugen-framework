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
 * The request with remote BD address and BD type for commands "Remove Bonding".
 * Request command code is CommsConstant.CommandCode.BT_RMV_BOND
 * 
 * @author EDLiu
 * @version 1.0
 * @created 26-Feb-2015 5:59:40 PM
 */
public class RemoveBondingRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand = -1;
    /**
     * BD Address
     */
    private byte[] mRemoteBd = null;
    /**
     * TBlueAPI_BDType
     */
    private int mRemoteBdType;

    public static final Parcelable.Creator<RemoveBondingRequest> CREATOR = new Parcelable.Creator<RemoveBondingRequest>()
    {
        public RemoveBondingRequest createFromParcel(Parcel in)
        {
            return new RemoveBondingRequest(in);
        }

        public RemoveBondingRequest[] newArray(int size)
        {
            return new RemoveBondingRequest[size];
        }
    };

    public RemoveBondingRequest()
    {

    }

    public RemoveBondingRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[in.readInt()];
        in.readByteArray(this.mRemoteBd);
        this.mRemoteBdType = in.readInt();

    }

    public RemoveBondingRequest(int command)
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
     * set remote BD address
     * 
     * @param address MAC address
     */
    public void setRemoteBD(SafetyByteArray address)
    {
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        System.arraycopy(address.getByteArray(), 0, this.mRemoteBd, 0,
                BlueConstant.BD_ADDR_LEN);
    }

    /**
     * set remote BD type
     * 
     * @param SafetyNumber<Integer> Remote BD type refer to TBlueAPI_BDType
     */
    public void setRemoteBdType(SafetyNumber<Integer> type)
    {
        this.mRemoteBdType = type.get().intValue();
    }

    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand));
        data.add(this.mRemoteBd);
        data.add(ByteConverter.getBytes((byte) this.mRemoteBdType));

    }

    /**
     * 
     * @param dest
     * @param flags
     */
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
        dest.writeInt(this.mRemoteBdType);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
