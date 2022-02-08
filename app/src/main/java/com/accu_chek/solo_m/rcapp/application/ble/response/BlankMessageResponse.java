
/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.response.BlankMessageResponse
 * Brief: This class handles BlankMessageResponse
 *
 * Create Date: 2015/8/10
 * $Revision: 25269 $
 * $Author: KiddYeh $
 * $Id: BlankMessageResponse.java 25269 2015-12-01 10:36:33Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The response is for response or indication which contains command code only
 * such as "DateTime Sync Indication", "SoloM Configuration Indication",
 * "SoloM Record Full Alert".
 * The command codes are CommsConstant.CommandCode.TIME_SYNC_IND,
 * CommsConstant.CommandCode.SOLOM_CONFIG_IND,
 * CommsConstant.CommandCode.REC_FULL_SOLOM.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 03-Mar-2015 11:16:09 AM
 */
public class BlankMessageResponse implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<BlankMessageResponse> CREATOR = new Parcelable.Creator<BlankMessageResponse>()
    {
        public BlankMessageResponse createFromParcel(Parcel in)
        {
            return new BlankMessageResponse(in);
        }

        public BlankMessageResponse[] newArray(int size)
        {
            return new BlankMessageResponse[size];
        }
    };

    public BlankMessageResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public BlankMessageResponse(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public BlankMessageResponse(int command)
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

    @Override
    public void setMessage(SafetyByteArray message)
    {
        this.mMessage = message.getByteArray();

    }

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
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN);

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
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// [BT] Fixed Klocwork issue.
