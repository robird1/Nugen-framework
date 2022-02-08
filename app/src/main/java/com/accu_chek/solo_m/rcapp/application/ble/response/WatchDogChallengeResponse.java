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
 * The response to the request of watchdog challenge requested by UI processor.
 * The command code is CommsConstant.CommandCode.WATCHDOG_CHAL_REQ.
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 4:08:13 PM
 */
public class WatchDogChallengeResponse implements IResponse, Parcelable {

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

	
	public static final Parcelable.Creator<WatchDogChallengeResponse> CREATOR
	= new Parcelable.Creator<WatchDogChallengeResponse>() {
		public WatchDogChallengeResponse createFromParcel(Parcel in) {
			return new WatchDogChallengeResponse(in);
		}

		public WatchDogChallengeResponse[] newArray(int size) {
			return new WatchDogChallengeResponse[size];
		}
	};
	
	public WatchDogChallengeResponse(){

	}

	public WatchDogChallengeResponse(Parcel in){
		this.mCommand 		= in.readInt();
		this.mResponseByte	= in.readInt();
		this.mMessage		= new byte[in.readInt()];
		in.readByteArray(this.mMessage);
	}
	
	public WatchDogChallengeResponse(int command){
		this.mCommand = command;
	}
	

	/**
	 * The response bytes contain 2 bytes in total.
	 * - Byte 0: Flag to indicate the challenge request status.
	 * - Byte 1: Information to indicate the SFM status in communication processor.
	 * The byte should reset to "StateIdle".
	 */
	public int getResponseByte(){
		return this.mResponseByte;
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
	 * parse attribute values from original message bytes
	 */
	@Override
	public void parseMessage(){

		ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);
		
		this.mCommand 		= ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN);	// 2 bytes
		this.mResponseByte	= ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN);	// 2 bytes
	}

	/**
	 * "It is used for for some validation code -- in particular to implement Bundle.hasFileDescriptors()" (Dianne Hackborn -- Android framework engineer)
	 * Not implemented in SoloM_RCAPP.
	 */
	@Override
	public int describeContents() {
		
		return 0;
	}

	/**
	 * Flatten this object in to a Parcel.
	 * @param dest The Parcel in which the object should be written.
	 * @param flags Additional flags about how the object should be written. May be 0 or PARCELABLE_WRITE_RETURN_VALUE. 
	 */
	@Override
	public void writeToParcel(Parcel dest, int flags) {
		
		dest.writeInt(this.mCommand);
		dest.writeInt(this.mResponseByte);
		dest.writeInt(this.mMessage.length);
		dest.writeByteArray(this.mMessage);
		
		
	}

}// [BT] Fixed Klocwork issue.
