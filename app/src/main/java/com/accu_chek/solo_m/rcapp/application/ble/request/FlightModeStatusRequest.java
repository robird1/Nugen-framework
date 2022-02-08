package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

public class FlightModeStatusRequest implements IRequest, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * mode
     */
    private int mMode;

    
    public static final Parcelable.Creator<FlightModeStatusRequest> CREATOR = new Parcelable.Creator<FlightModeStatusRequest>()
            {
                public FlightModeStatusRequest createFromParcel(Parcel in)
                {
                    return new FlightModeStatusRequest(in);
                }

                public FlightModeStatusRequest[] newArray(int size)
                {
                    return new FlightModeStatusRequest[size];
                }
            };
            
            
            /**
             * Constructor for IPC transaction
             * 
             * @param in
             */
            public FlightModeStatusRequest(Parcel in)
            {
                this.mCommand = in.readInt();
                this.mMode = in.readInt();
            }        
    
    public FlightModeStatusRequest(int command)
    {
        this.mCommand = command;
    }



    @Override
    public void writeToParcel(Parcel dest, int flags)
    {
        dest.writeInt(this.mCommand);
        dest.writeInt(this.mMode);

    }

    @Override
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }
    
    /**
     * To enable / disable flight mode
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
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {
        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(ByteConverter.getBytes((byte) this.mMode));

    }

    @Override
    public int describeContents()
    {
        // TODO Auto-generated method stub
        return 0;
    }

}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
