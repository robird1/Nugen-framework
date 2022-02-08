/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.response.ConfirmErrorLogResponse
 * Brief: 
 *
 * Create Date: 2015/8/11
 * $Revision: 23058 $
 * $Author: KiddYeh $
 * $Id: ErrorLogResponse.java 23058 2015-11-03 01:19:18Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

public class ErrorLogResponse implements IResponse, Parcelable
{
    /**
     * Command Code
     */
    private int mCommand;

    /**
     * Error count
     */
    private int mCount;
    /**
     * Error log data
     */
    private byte[] mData;

    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ErrorLogResponse> CREATOR = new Parcelable.Creator<ErrorLogResponse>()
    {
        public ErrorLogResponse createFromParcel(Parcel in)
        {
            return new ErrorLogResponse(in);
        }

        public ErrorLogResponse[] newArray(int size)
        {
            return new ErrorLogResponse[size];
        }
    };

    public ErrorLogResponse()
    {

    }

    /**
     * constructor 
     * 
     * @param in
     */
    public ErrorLogResponse(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mCount = in.readInt();
        this.mData = new byte[this.mCount*2];
        in.readByteArray(this.mData);
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ErrorLogResponse(int command)
    {
        this.mCommand = command;
    }

    @Override
    public int describeContents()
    {
        return 0;
    }

    /**
     * 
     * @param dest
     * @param flags
     */
    @Override
    public void writeToParcel(Parcel dest, int flags)
    {
        dest.writeInt(this.mCommand);
        dest.writeInt(this.mCount);
        dest.writeByteArray(this.mData);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

    @Override
    public void setMessage(SafetyByteArray message)
    {
        this.mMessage = message.getByteArray();

    }

    /**
     * The function parses message bytes into object members. The byte length of
     * value follows the message table define in comms
     */
    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);
        this.mCount = buffer.get();
        this.mData = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                this.mCount*2);
    }

    @Override
    public byte[] getMessage()
    {
        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

    /**
     * 
     * @return result
     */
    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mCount,(-this.mCount));
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
    
    public SafetyByteArray getData()
    {

        return new SafetyByteArray(this.mData,
                CRCTool.generateCRC16(this.mData));

    }

    public SafetyNumber<Integer> getCount()
    {
        return new SafetyNumber<Integer>(this.mCount, (-this.mCount));
    }
}
// (R15209 2015-08-22 23:31:51 henrytso)
// ----------------------------------------------------------------------------
// [BT] Fixed Klocwork issue.
