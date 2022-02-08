/**
 * 
 */
package com.accu_chek.solo_m.rcapp.application.ble.response;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

/**
 * @author KiddYeh
 * 
 */
public class WatchDogChallengeSuspend implements IResponse, Parcelable
{

    /**
     * Command code
     */
    private int mCommand;
    /**
     * Response Bytes
     */
    private int mResponseByte;
    /**
     * Original message byte array
     */
    private byte[] mMessage;

    public static final Parcelable.Creator<WatchDogChallengeSuspend> CREATOR = new Parcelable.Creator<WatchDogChallengeSuspend>()
    {
        public WatchDogChallengeSuspend createFromParcel(Parcel in)
        {
            return new WatchDogChallengeSuspend(in);
        }

        public WatchDogChallengeSuspend[] newArray(int size)
        {
            return new WatchDogChallengeSuspend[size];
        }
    };

    public WatchDogChallengeSuspend()
    {

    }

    public WatchDogChallengeSuspend(Parcel in)
    {
        this.mCommand = in.readInt();
        this.mResponseByte = in.readInt();
        this.mMessage = new byte[in.readInt()];
        in.readByteArray(this.mMessage);
    }

    public WatchDogChallengeSuspend(int command)
    {
        this.mCommand = command;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.accu_chek.solo_m.rcapp.application.ble.IResponse#
     * getCommand()
     */
    @Override
    public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.accu_chek.solo_m.rcapp.application.ble.IResponse#
     * setMessage
     * (com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray
     * )
     */
    @Override
    public void setMessage(SafetyByteArray message)
    {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.accu_chek.solo_m.rcapp.application.ble.IResponse#
     * getMessage()
     */
    @Override
    public byte[] getMessage()
    {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.accu_chek.solo_m.rcapp.application.ble.IResponse#
     * parseMessage()
     */
    @Override
    public void parseMessage()
    {
        // TODO Auto-generated method stub

    }

    @Override
    public int describeContents()
    {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags)
    {
        dest.writeInt(this.mCommand);
        dest.writeInt(this.mResponseByte);
        dest.writeInt(this.mMessage.length);
        dest.writeByteArray(this.mMessage);

    }

}
// [BT] Fixed Klocwork issue.
