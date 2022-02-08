package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The command is CommsConstant.CommandCode.BT_PAIRABLE.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 8:34:25 PM
 */
public class PairableRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */

    private int mCommand;
    private int mMode;

    public static final Parcelable.Creator<PairableRequest> CREATOR = new Parcelable.Creator<PairableRequest>()
    {
        public PairableRequest createFromParcel(Parcel in)
        {
            return new PairableRequest(in);
        }

        public PairableRequest[] newArray(int size)
        {
            return new PairableRequest[size];
        }
    };

    public PairableRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public PairableRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mMode = in.readInt();
    }

    /**
     * Constuctor for factory method
     * 
     * @param command
     */
    public PairableRequest(int command)
    {
        this.mCommand = command;
    }

    @Override
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
     * To enable / disable pariable mode
     * 
     * @param mode
     *            enable mode = HammingDistance.SAFETY_BOOLEAN_TRUE;
     *            disable mode = HammingDistance.SAFETY_BOOLEAN_FALSE
     */
    /**
     * To enable / disable Pairable mode
     * 
     * @param mode
     *            enable mode = HammingDistance.SAFETY_BOOLEAN_TRUE;
     *            disable mode = HammingDistance.SAFETY_BOOLEAN_FALSE
     */
    public void setMode(SafetyBoolean mode)
    {
        if(SafetyBoolean.TRUE == mode)
        {
            this.mMode = HammingDistance.SAFETY_BOOLEAN_TRUE;
        }
        else
        {
            this.mMode = HammingDistance.SAFETY_BOOLEAN_FALSE;
        }
    }

    /**
     * adding attribute values to byte array list for further frame wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand));
        data.add(ByteConverter.getBytes((byte) this.mMode));

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
        dest.writeInt(this.mMode);

    }
}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
