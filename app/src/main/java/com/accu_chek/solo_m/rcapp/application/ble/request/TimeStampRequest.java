/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.request.TimeStampRequest
 * Brief: This class handles TimeStampRequest setting.
 *
 * Create Date: 2015/8/10
 * $Revision: 20560 $
 * $Author: DWYang $
 * $Id: SystemSyncRequest.java 20560 2015-10-01 14:13:14Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

public class TimeStampRequest implements IRequest, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    
    /**
     * Year
     */
    private int mYear; 
    
    /**
     * Month
     */
    private int mMonth; 
    
    /**
     * Day
     */
    private int mDay; 
    
    /**
     * Hours
     */
    private int mHours; 
    
    /**
     * Minutes
     */
    private int mMinutes; 
    
    /**
     * Seconds
     */
    private int mSeconds; 

    public static final Parcelable.Creator<TimeStampRequest> CREATOR = new Parcelable.Creator<TimeStampRequest>()
    {
        public TimeStampRequest createFromParcel(Parcel in)
        {
            return new TimeStampRequest(in);
        }

        public TimeStampRequest[] newArray(int size)
        {
            return new TimeStampRequest[size];
        }
    };

    public TimeStampRequest()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public TimeStampRequest(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mYear = in.readInt();
        this.mMonth = in.readInt();
        this.mDay = in.readInt();
        this.mHours = in.readInt();
        this.mMinutes = in.readInt();
        this.mSeconds = in.readInt();
        
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public TimeStampRequest(int command)
    {
        this.mCommand = command;
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
     * Set the parameter Year of request.
     * 
     * 
     * @param year [in] the parameter Year of request
     *            Range: a valid object of SafetyNumber<Integer>
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1 
     * 
     * 
     */
    public void setYear(SafetyNumber<Integer> year)
    {
        this.mYear = year.get().intValue();
    }

    /**
     * Set the parameter Month of request.
     * 
     * 
     * @param year [in] the parameter Year of request
     *            Range: a valid object of SafetyNumber<Integer>
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1 
     * 
     * 
     */
    public void setMonth(SafetyNumber<Integer> month)
    {
        this.mMonth = month.get().intValue();
    }
    
    /**
     * Set the parameter Day of request.
     * 
     * 
     * @param year [in] the parameter Day of request
     *            Range: a valid object of SafetyNumber<Integer>
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1 
     * 
     * 
     */
    public void setDay(SafetyNumber<Integer> day)
    {
        this.mDay = day.get().intValue();
    }
    
    /**
     * Set the parameter Hours of request.
     * 
     * 
     * @param year [in] the parameter Hours of request
     *            Range: a valid object of SafetyNumber<Integer>
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1 
     * 
     * 
     */
    public void setHours(SafetyNumber<Integer> hours)
    {
        this.mHours = hours.get().intValue();
    }
    
    
    /**
     * Set the parameter Minutes of request.
     * 
     * 
     * @param year [in] the parameter Hours of request
     *            Range: a valid object of SafetyNumber<Integer>
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1 
     * 
     * 
     */
    public void setMinutes(SafetyNumber<Integer> minutes)
    {
        this.mMinutes = minutes.get().intValue();
    }
    
    /**
     * Set the parameter Seconds of request.
     * 
     * 
     * @param year [in] the parameter Seconds of request
     *            Range: a valid object of SafetyNumber<Integer>
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1 
     * 
     * 
     */
    public void setSeconds(SafetyNumber<Integer> seconds)
    {
        this.mSeconds = seconds.get().intValue();
    }
    
    /**
     * adding attribute values to byte array list for further fram wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mYear)); // 2 bytes
        data.add(ByteConverter.getBytes((byte) this.mMonth)); // 1 byte
        data.add(ByteConverter.getBytes((byte) this.mDay)); // 1 byte
        data.add(ByteConverter.getBytes((byte) this.mHours)); // 1 byte
        data.add(ByteConverter.getBytes((byte) this.mMinutes)); // 1 byte
        data.add(ByteConverter.getBytes((byte) this.mSeconds)); // 1 byte
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
        dest.writeInt(this.mYear);
        dest.writeInt(this.mMonth);
        dest.writeInt(this.mDay);
        dest.writeInt(this.mHours);
        dest.writeInt(this.mMinutes);
        dest.writeInt(this.mSeconds);
    }

    @Override
    public int describeContents()
    {
        return 0;
    }

}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */