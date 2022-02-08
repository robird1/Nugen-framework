package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * 
 * The request is for command "Erase IDS Record" and "Read IDS Record" only
 * which
 * contain same attribute "record type". The commands are
 * CommsConstant.CommandCode.REC_ERASE_IDS
 * and CommsConstant.CommandCode.REC_READ_IDS.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 03-Mar-2015 2:58:00 PM
 */

public class IdsRecordRequest implements IRequest, Parcelable
{

    /**
     * Command
     */
    private int mCommand;
    /**
     * Record Type
     */
    private int mType;
    public static final Parcelable.Creator<IdsRecordRequest> CREATOR = new Parcelable.Creator<IdsRecordRequest>()
    {
        public IdsRecordRequest createFromParcel(Parcel in)
        {
            return new IdsRecordRequest(in);
        }

        public IdsRecordRequest[] newArray(int size)
        {
            return new IdsRecordRequest[size];
        }
    };

    public IdsRecordRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in in
     */
    public IdsRecordRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mType = in.readInt();
    }

    /**
     * constructor for factory method
     * 
     * @param command command
     */
    public IdsRecordRequest(int command)
    {
        this.mCommand = command;
    }

    @Override
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
     * Set Record Type. Defined in CommsConstant.RecordType.
     * Type: IDS, ACTIVE_BOLUS, HISTORY
     * 
     * @param type Record type(Hamming Distance) to erase.
     */
    public void setType(int type)
    {

        this.mType = type;
    }

    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand));
        data.add(ByteConverter.getBytes((byte) this.mType));

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
        dest.writeInt(this.mType);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
