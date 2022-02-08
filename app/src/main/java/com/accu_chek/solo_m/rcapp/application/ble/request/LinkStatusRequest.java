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
 * The command code is CommsConstant.GroupId.COMM_LINK_STATUS
 * 
 * @author EDLiu
 * @version 1.0
 * @created 03-Mar-2015 8:32:58 PM
 */

public class LinkStatusRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Remote BD address
     */
    private byte[] mRemoteBd;
    /**
     * Remote BD type
     */
    private int mRemoteBdType;
    /**
     * polling interval
     */
    private int mInterval;

    public static final Parcelable.Creator<LinkStatusRequest> CREATOR = new Parcelable.Creator<LinkStatusRequest>()
    {
        public LinkStatusRequest createFromParcel(Parcel in)
        {
            return new LinkStatusRequest(in);
        }

        public LinkStatusRequest[] newArray(int size)
        {
            return new LinkStatusRequest[size];
        }
    };

    public LinkStatusRequest()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public LinkStatusRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mRemoteBdType = in.readInt();
        this.mInterval = in.readInt();
    }

    public LinkStatusRequest(int command)
    {
        this.mCommand = command;
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
     * polling interval, set 0 to disable.
     * 
     * @param interval set 0 to disable.
     */
    public void setInterval(SafetyNumber<Integer> interval)
    {
        this.mInterval = interval.get().intValue();

    }

    /**
     * Set remote BD address
     * 
     * @param address Remote BD address
     */
    public void setRemoteBd(SafetyByteArray address)
    {
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        System.arraycopy(address.getByteArray(), 0, this.mRemoteBd, 0,
                BlueConstant.BD_ADDR_LEN);
    }

    /**
     * 
     * @param type
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
        data.add(ByteConverter.getBytes((short) this.mInterval));

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
        dest.writeByteArray(this.mRemoteBd);
        dest.writeInt(this.mRemoteBdType);
        dest.writeInt(this.mInterval);

    }

}// [BT] Fixed Klocwork issue.
