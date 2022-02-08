package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * This command code is CommsConstant.CommandCode.COMM_UI_UPDATE.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 3:32:24 PM
 */

public class UIStatusUpdateRequest implements IRequest, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Standby or active mode
     */
    private int mState;

    public static final Parcelable.Creator<UIStatusUpdateRequest> CREATOR = new Parcelable.Creator<UIStatusUpdateRequest>()
    {
        public UIStatusUpdateRequest createFromParcel(Parcel in)
        {
            return new UIStatusUpdateRequest(in);
        }

        public UIStatusUpdateRequest[] newArray(int size)
        {
            return new UIStatusUpdateRequest[size];
        }
    };

    public UIStatusUpdateRequest()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public UIStatusUpdateRequest(Parcel in)
    {

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public UIStatusUpdateRequest(int command)
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
     * UI State. See CommsConstant.UiState for valid values.
     * 
     * @param state ACTIVE, STANDBY
     */
    public void setState(SafetyNumber<Integer> state)
    {

        this.mState = state.get();
    }

    /**
     * adding attribute values to byte array list for further fram wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(ByteConverter.getBytes((byte) this.mState)); // 1 byte
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
        dest.writeInt(this.mState);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
