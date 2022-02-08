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
 * The command code is CommsConstant.CommandCode.BT_OOB_CF.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 7:59:00 PM
 */

public class OOBConfirmation implements IRequest, Parcelable
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
     * Simple Pairing Hash C
     */
    private byte[] mCbin;

    public static final Parcelable.Creator<OOBConfirmation> CREATOR = new Parcelable.Creator<OOBConfirmation>()
    {
        public OOBConfirmation createFromParcel(Parcel in)
        {
            return new OOBConfirmation(in);
        }

        public OOBConfirmation[] newArray(int size)
        {
            return new OOBConfirmation[size];
        }
    };

    public OOBConfirmation()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public OOBConfirmation(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        // TODO: the length should be fixed!
        this.mCbin = new byte[in.readInt()];
        in.readByteArray(this.mCbin);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public OOBConfirmation(int command)
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
     * 
     * @param cbin
     */
    public void setCbin(SafetyByteArray cbin)
    {
        this.mCbin = cbin.getByteArray();
    }

    /**
     * 
     * @param address
     */
    public void setRemoteBD(SafetyByteArray address)
    {
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        System.arraycopy(address.getByteArray(), 0, this.mRemoteBd, 0,
                BlueConstant.BD_ADDR_LEN);
    }

    /**
     * adding attribute values to byte array list for further frame wrapping
     */

    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand));
        data.add(this.mRemoteBd);
        data.add(this.mCbin);

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
        dest.writeByteArray(this.mRemoteBd);
        // TODO: the length should be fixed
        dest.writeInt(this.mCbin.length);
        dest.writeByteArray(this.mCbin);

    }
}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
