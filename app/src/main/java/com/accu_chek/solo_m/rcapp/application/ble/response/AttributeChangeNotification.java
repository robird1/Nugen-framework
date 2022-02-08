/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification
 * Brief: This class handles AttributeChangeNotification response.
 *
 * Create Date: 2015/8/10
 * $Revision: 25269 $
 * $Author: KiddYeh $
 * $Id: AttributeChangeNotification.java 25269 2015-12-01 10:36:33Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import org.apache.http.util.ByteArrayBuffer;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * The command code is CommsConstant.CommandCode.BT_ATTR_NOTIF_IND.
 * 
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 9:42:24 AM
 */
public class AttributeChangeNotification implements IResponse, Parcelable
{
    private static final String TAG = "AttributeChangeNotification";
    /**
     * Command code
     */
    private int mCommand;
    /**
     * BD Address
     */
    private byte[] mRemoteBd;
    private int mResult;
    /**
     * Attribute handle
     */
    private int mAttribHandle;
    private int mAttribLength;
    /**
     * Offset of attribute value in data
     */
    private int mGap;
    /**
     * The data contains only ONE byte and not necessary to handle.
     */
    private byte[] mData;

    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<AttributeChangeNotification> CREATOR = new Parcelable.Creator<AttributeChangeNotification>()
    {
        public AttributeChangeNotification createFromParcel(Parcel in)
        {
            return new AttributeChangeNotification(in);
        }

        public AttributeChangeNotification[] newArray(int size)
        {
            return new AttributeChangeNotification[size];
        }
    };

    public AttributeChangeNotification()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public AttributeChangeNotification(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        in.readByteArray(this.mRemoteBd);
        this.mResult = in.readInt();
        this.mAttribHandle = in.readInt();
        this.mAttribLength = in.readInt();
        this.mGap = in.readInt();
        this.mData = new byte[in.readInt()];
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
    public AttributeChangeNotification(int command)
    {

        this.mCommand = command;

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
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);
        ByteArrayBuffer dataAndResult = new ByteArrayBuffer(0); 

        this.mCommand =  ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN);// 2 bytes
        this.mRemoteBd = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);
        this.mResult = buffer.get(); // 1 byte
        this.mAttribHandle = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mAttribLength = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mGap = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 byte
        ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
               mGap);
        this.mData = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                mAttribLength);

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
        dest.writeByteArray(mRemoteBd);
        dest.writeInt(this.mResult);
        dest.writeInt(this.mAttribHandle);
        dest.writeInt(this.mAttribLength);
        dest.writeInt(this.mGap);
        dest.writeInt(this.mData.length);
        dest.writeByteArray(this.mData);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

    
    /*
     * Return Handler
     */
    public SafetyNumber<Integer> getAttribHandle()
    {
        return new SafetyNumber<Integer>(this.mAttribHandle, (-this.mAttribHandle));
    }
    
    /*
     * Return remote BD address
     */
    public SafetyByteArray getRemoteBD()
    {
        return new SafetyByteArray(this.mRemoteBd, CRCTool.generateCRC16(this.mRemoteBd));
    }

    /**
     * Return result
     * Refer to CommsConstant.Result for valid values (Hamming Distance).
     */
    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mResult, (-this.mResult));
    }

    /**
     * return length of attribute length
     */
    public SafetyNumber<Integer> getAttributeLength()
    {
        return new SafetyNumber<Integer>(this.mAttribLength, (-this.mAttribLength));
    }

    /**
     * return gap
     */
    public SafetyNumber<Integer> getGap()
    {
        return new SafetyNumber<Integer>(this.mGap, (-this.mGap));
    }

    /**
     * return data bytes
     */
    public SafetyByteArray getData()
    {
        return new SafetyByteArray(this.mData, CRCTool.generateCRC16(this.mData));
    }

}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
