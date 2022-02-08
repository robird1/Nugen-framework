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
 * The command code is CommsConstant.CommandCode.BT_SCAN
 * 
 * @author EDLiu
 * @version 1.0
 * @created 25-Feb-2015 5:27:46 PM
 */
public class ScanRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */

    private int mCommand;
    /**
     * enable = HammingDistance.SAFETY_BOOLEAN_TRUE
     * disable = HammingDistance.SAFETY_BOOLEAN_FALSE
     */
    private int mMode;

    public static final Parcelable.Creator<ScanRequest> CREATOR = new Parcelable.Creator<ScanRequest>()
    {
        public ScanRequest createFromParcel(Parcel in)
        {
            return new ScanRequest(in);
        }

        public ScanRequest[] newArray(int size)
        {
            return new ScanRequest[size];
        }
    };

    public ScanRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public ScanRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mMode = in.readInt();
    }

    /**
	 * 
	 */
    public ScanRequest(int command)
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
     * To enable or disable scan request.
     * enable = SafetyBoolean.TRUE = HammingDistance.SAFETY_BOOLEAN_TRUE
     * disable = SafetyBoolean.FALSE = HammingDistance.SAFETY_BOOLEAN_FALSE
     * 
     * @param mode enable or disable scan mode
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
