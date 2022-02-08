/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.response.ResetConnectionResponse
 * Brief: This class handles ResetConnectionResponse response.
 *
 * Create Date: 2015/8/10
 * $Revision: 23776 $
 * $Author: KiddYeh $
 * $Id: ResetConnectionResponse.java 23776 2015-11-11 00:34:12Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The command code is CommsConstant.CommandCode.BT_RESET_CONNECTION.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 26-Feb-2015 6:23:53 PM
 */
public class ResetConnectionResponse implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * BD Address
     */
    private byte[] mRemoteBD;
    /**
     * Indicates the result of the request
     */
    private int mResult;
    /**
     * original message bytes
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<ResetConnectionResponse> CREATOR = new Parcelable.Creator<ResetConnectionResponse>()
    {
        public ResetConnectionResponse createFromParcel(Parcel in)
        {
            return new ResetConnectionResponse(in);
        }

        public ResetConnectionResponse[] newArray(int size)
        {
            return new ResetConnectionResponse[size];
        }
    };

    public ResetConnectionResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public ResetConnectionResponse(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mRemoteBD = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBD);
        this.mResult = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ResetConnectionResponse(int command)
    {
        this.mCommand = command;
    }

    /**
     * return remote BD address
     */
    /*
     * Return remote BD address
     */
    public SafetyByteArray getRemoteBD()
    {
        return new SafetyByteArray(this.mRemoteBD,
                CRCTool.generateCRC16(this.mRemoteBD));
    }

    /**
     * return result
     */
    public int getResult()
    {
        return this.mResult;
    }

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

        this.mCommand = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN);
        this.mRemoteBD = ByteConverter.readBytes(buffer,ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mResult = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN);
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
        dest.writeByteArray(this.mRemoteBD);
        dest.writeInt(this.mResult);
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
