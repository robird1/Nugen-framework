/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.response.LinkStatusResponse
 * Brief: 
 *
 * Create Date: 2015/7/2
 * $Revision: 20560 $
 * $Author: DWYang $
 * $Id: LinkStatusResponse.java 20560 2015-10-01 14:13:14Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

public class LinkStatusResponse implements IResponse, Parcelable
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
     * Type of remote Bluetooth device (Options: TBlueAPI_RemoteBDType)
     */
    private int mRemoteBDtype;
    /**
     * Cause of result - TBlueAPI_Cause
     */
    private int mCause;

    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<LinkStatusResponse> CREATOR = new Parcelable.Creator<LinkStatusResponse>()
    {
        public LinkStatusResponse createFromParcel(Parcel in)
        {
            return new LinkStatusResponse(in);
        }

        public LinkStatusResponse[] newArray(int size)
        {
            return new LinkStatusResponse[size];
        }
    };

    public LinkStatusResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public LinkStatusResponse(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mRemoteBDtype = in.readInt();
        this.mCause = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public LinkStatusResponse(int command)
    {
        this.mCommand = command;
    }

    /**
     * @return command code
     */
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /**
     * 
     * @param message
     */
    @Override
    public void setMessage(SafetyByteArray message)
    {
        this.mMessage = message.getByteArray();
    }

    /**
     * Return original message bytes from Comms subsystem.
     * The interface is mainly for debugging purpose at development stage.
     */
    @Override
    public byte[] getMessage()
    {
        return Arrays.copyOf(this.mMessage, mMessage.length);
    }

    /**
     * parse attribute information from original message bytes
     */
    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mRemoteBd = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mRemoteBDtype = buffer.get(); // 1 byte
        this.mCause = buffer.get(); // 1 byte

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
        dest.writeInt(this.mRemoteBDtype);
        dest.writeInt(this.mCause);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }
   
}
// (R15209 2015-08-22 23:31:51 henrytso)
// ----------------------------------------------------------------------------
// [BT] Fixed Klocwork issue.
