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

/**
 * CommsConstant.CommandCode.COMMS_INFO
 */
public class CommsInfoResponse implements IResponse, Parcelable
{

    private static final int VERSION_LEN = 20;
    /**
     * Command Code
     */

    private int mCommand;
    /**
     * Result Code
     */
    private int mResult;
    /**
     * Local BD Address
     */
    private byte[] mLocalBDAddress;
    /**
     * Version
     */
    private String mVersion;
    /**
     * Flow control parameter ¡V initial token count = 2
     */
    private int mTokens;
    /**
     * Cause of reset
     */
    private int mResetCause;
    /**
     * Power-on self-test tested result
     */
    private int mPostResult;
    /**
     * Runtime-test tested result
     */
    // private int mRuntimeTestResult;
    /**
     * See structure for options
     */
    private int mBtStatus;
    /**
     * original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<CommsInfoResponse> CREATOR = new Parcelable.Creator<CommsInfoResponse>()
    {
        public CommsInfoResponse createFromParcel(Parcel in)
        {
            return new CommsInfoResponse(in);
        }

        public CommsInfoResponse[] newArray(int size)
        {
            return new CommsInfoResponse[size];
        }
    };

    public CommsInfoResponse()
    {

    }

    /**
     * Constructor for IPC transaction
     * 
     * @param in
     */
    public CommsInfoResponse(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mResult = in.readInt();
        this.mLocalBDAddress = new byte[in.readInt()];
        in.readByteArray(this.mLocalBDAddress);
        this.mVersion = in.readString();
        this.mTokens = in.readInt();
        this.mResetCause = in.readInt();
        this.mPostResult = in.readInt();
        // this.mRuntimeTestResult = in.readInt();
        this.mBtStatus = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public CommsInfoResponse(int command)
    {

        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
    }

    /**
     * 
     * @param dest
     * @param flags
     */
    public void writeToParcel(Parcel dest, int flags)
    {

        dest.writeInt(this.mCommand);
        dest.writeInt(this.mResult);
        dest.writeInt(this.mLocalBDAddress.length);
        dest.writeByteArray(this.mLocalBDAddress);
        // dest.writeInt(this.mVersion.length());
        dest.writeString(this.mVersion);
        dest.writeInt(this.mTokens);
        dest.writeInt(this.mResetCause);
        dest.writeInt(this.mPostResult);
        // dest.writeInt(this.mRuntimeTestResult);
        dest.writeInt(this.mBtStatus);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

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

    @Override
    public void parseMessage()
    {
        ByteBuffer buffer = ByteBuffer.wrap(mMessage);
        byte[] version = null;

        this.mCommand = ByteConverter
                .readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2 bytes
        this.mResult = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN); // 2
                                                                                 // bytes
        this.mLocalBDAddress = ByteConverter.readBytes(buffer,
                ByteOrder.LITTLE_ENDIAN, BlueConstant.BD_ADDR_LEN);

        version = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                VERSION_LEN);
        this.mVersion = new String(version);
        this.mTokens = buffer.get(); // 1 byte
        this.mResetCause = ByteConverter.readInt(buffer,
                ByteOrder.LITTLE_ENDIAN); // 4 bytes
        this.mPostResult = ByteConverter.readShort(buffer,
                ByteOrder.LITTLE_ENDIAN); // 2 bytes
        // this.mRuntimeTestResult = buffer.getShort(); // 2 bytes
        this.mBtStatus = buffer.get(); // 1 byte

    }

    public SafetyNumber<Integer> getResult()
    {
        return new SafetyNumber<Integer>(this.mResult,(-this.mResult));
    }

    public byte[] getLocalBDAddress()
    {
        
        return Arrays.copyOf(this.mLocalBDAddress, mLocalBDAddress.length);
    }

    public String getVersion()
    {
        return this.mVersion;
    }

    public SafetyNumber<Integer> getTokens()
    {
        return new SafetyNumber<Integer>(this.mTokens,(-this.mTokens));
    }

    public SafetyNumber<Integer> getResetCause()
    {
        return new SafetyNumber<Integer>(this.mResetCause,(-this.mResetCause));
    }

    public SafetyNumber<Integer> getPostResult()
    {
        return new SafetyNumber<Integer>(this.mPostResult,(-this.mPostResult));
    }

    // public int getRuntimeTestResult()
    // {
    // return this.mRuntimeTestResult;
    //
    // }

    public SafetyNumber<Integer> getBtStatus()
    {
        return new SafetyNumber<Integer>(this.mBtStatus,(-this.mBtStatus));
    }

}// [BT] Fixed Klocwork issue.
