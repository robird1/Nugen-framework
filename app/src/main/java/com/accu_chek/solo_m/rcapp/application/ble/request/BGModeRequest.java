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
 * Non-discoverable mode request.
 * Request for command CommsConstant.CommandCode.COMM_BG_MODE
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 2:10:50 PM
 */

public class BGModeRequest implements IRequest, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * To enable or disable BG mode
     */
    private int mMode;

    public static final Parcelable.Creator<BGModeRequest> CREATOR = new Parcelable.Creator<BGModeRequest>()
    {
        public BGModeRequest createFromParcel(Parcel in)
        {
            return new BGModeRequest(in);
        }

        public BGModeRequest[] newArray(int size)
        {
            return new BGModeRequest[size];
        }
    };

    public BGModeRequest()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public BGModeRequest(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mMode = in.readInt();
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public BGModeRequest(int command)
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
     * enable/disable BG mode
     * 
     * @param mode enable = HammingDistance.SAFETY_BOOLEAN_TRUE; disable =
     *            HammingDistance.SAFETY_BOOLEAN_FALSE
     * 
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

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(ByteConverter.getBytes((byte) this.mMode)); // 1 bytes

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
