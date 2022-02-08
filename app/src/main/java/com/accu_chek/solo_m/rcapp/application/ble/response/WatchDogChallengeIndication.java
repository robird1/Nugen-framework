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

/**
 * A indication of challenge request sent by Comms subsystem for response.
 * The command code is CommsConstant.CommandCode.WATCHDOG_CHAL_IND.
 */
public class WatchDogChallengeIndication implements IResponse, Parcelable {

	private static final int CHALLENGE_DATA_LEN = 48;
	/**
	 * Command code
	 */
	private int mCommand;
	/**
	 * Challenge Data
	 */
	private byte[] mChallengeData;

	/**
	 * original message byte array
	 */
	private byte[] mMessage;
	
	public static final Parcelable.Creator<WatchDogChallengeIndication> CREATOR
	= new Parcelable.Creator<WatchDogChallengeIndication>() {
		public WatchDogChallengeIndication createFromParcel(Parcel in) {
			return new WatchDogChallengeIndication(in);
		}

		public WatchDogChallengeIndication[] newArray(int size) {
			return new WatchDogChallengeIndication[size];
		}
	};
	
	public WatchDogChallengeIndication(){

	}
	/**
	 * constructor for IPC transaction
	 */
	public WatchDogChallengeIndication(Parcel in){

		this.mCommand = in.readInt();
		this.mChallengeData = new byte[in.readInt()];
		in.readByteArray(this.mChallengeData);
		
	}
	/**
	 * constructor for factory method
	 */
	public WatchDogChallengeIndication(int command){
		this.mCommand = command;
	}


	/**
	 * he binary data of challenge message is pre-negotiated with UI processor. The
	 * challenge data contains 8 different data sets and the challenge binary should
	 * sent out in sequence.
	 */
	public SafetyByteArray getChallengeData(){
		return new SafetyByteArray(this.mChallengeData, CRCTool.generateCRC16(this.mChallengeData));
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
	public void parseMessage(){

		ByteBuffer buffer = ByteBuffer.wrap(this.mMessage);
		
		this.mCommand 		= ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN);	// 2 bytes
		this.mChallengeData = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN, CHALLENGE_DATA_LEN);

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
		dest.writeInt(CHALLENGE_DATA_LEN);
		dest.writeByteArray(this.mChallengeData);
		
	}

}// [BT] Fixed Klocwork issue.
