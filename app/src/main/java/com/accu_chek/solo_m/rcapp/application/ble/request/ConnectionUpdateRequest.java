package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * Request for command CommsConstant.CommandCode.BT_CONN_UPDATE
 * @author EDLiu
 * @version 1.0
 * @created 24-Feb-2015 6:34:17 PM
 */
public class ConnectionUpdateRequest implements IRequest, Parcelable {

	/**
	 * Command code
	 */
	private int mCommand;
	/**
	 * Remote BD address
	 */
	private byte[] mRemoteBd;
	/**
	 * Minimum connection interval
	 */
	private int mConnIntervalMin;
	/**
	 * Maximum connection interval
	 */
	private int mConnIntervalMax;
	/**
	 * Connection Latency
	 */
	private int mConnLatency;
	/**
	 * Supervision timeout
	 */
	private int mSupervisionTimeout;

	public static final Parcelable.Creator<ConnectionUpdateRequest> CREATOR
	= new Parcelable.Creator<ConnectionUpdateRequest>() {
		public ConnectionUpdateRequest createFromParcel(Parcel in) {
			return new ConnectionUpdateRequest(in);
		}

		public ConnectionUpdateRequest[] newArray(int size) {
			return new ConnectionUpdateRequest[size];
		}
	};
	
	
	public ConnectionUpdateRequest()
	{

	}
	/**
	 * Constructor for IPC transaction
	 * @param in
	 */
	public ConnectionUpdateRequest(Parcel in)
	{
		this.mCommand			= in.readInt();
		this.mRemoteBd			= new byte[BlueConstant.BD_ADDR_LEN];
		in.readByteArray(this.mRemoteBd);
		this.mConnIntervalMin	= in.readInt();
		this.mConnIntervalMax	= in.readInt();
		this.mConnLatency		= in.readInt();
		this.mSupervisionTimeout	= in.readInt();
	}
	/**
	 * constructor for factory method
	 * @param command Command Code (Hamming Distance). See CommsConstant.CommandCode for valid values.
	 */
	public ConnectionUpdateRequest(int command)
	{
		this.mCommand	 = command;
	}

	public int describeContents(){
		return 0;
	}

	/**
	 * @return mCommand
	 */
	/**
	 * Return command code of the request or response object.
	 * See design document "NUGEN Software Design Document of Communication Sub-system Command Message" for definition.
	 * Refer to CommsConstant.CommandCode for valid values (Hamming Distance).
	 */
	@Override
	public SafetyNumber<Integer> getCommand()
    {
        return new SafetyNumber<Integer>(this.mCommand, (-this.mCommand));
    }
	/**
	 * set connection latency
	 * @param latency
	 */
	public void setConnectionLatency(SafetyNumber<Integer> latency)
	{
		this.mConnLatency = latency.get().intValue();
	}

	/**
	 * set maximum connection interval
	 * @param interval
	 */
	public void setMaxConnectionInterval(SafetyNumber<Integer> interval)
	{
		this.mConnIntervalMax = interval.get().intValue();
	}
	/**
	 * set minimum connection interval
	 * @param interval
	 */
	public void setMinConnectionInterval(SafetyNumber<Integer> interval)
	{
		this.mConnIntervalMin = interval.get().intValue();
	}
	/**
	 * Set remote BD address
	 * @param address
	 */
	public void setRemoteBd(SafetyByteArray address)
	{
		        this.mRemoteBd = new byte[BlueConstant.BD_ADDR_LEN];
        System.arraycopy(address.getByteArray(), 0, this.mRemoteBd, 0,
                BlueConstant.BD_ADDR_LEN);
	}
	/**
	 * set supervision timeout
	 * @param timeout
	 */
	public void setSupervisionTimeout(SafetyNumber<Integer> timeout)
	{
		this.mSupervisionTimeout = timeout.get().intValue();
	}
	/**
	 * adding attribute values to byte array list for further frame wrapping
	 */
	@Override
	public void writeToByteArrayList(ArrayList<byte[]> data) {

		data.add(ByteConverter.getBytes((short) this.mCommand));	// 2 bytes
		data.add(this.mRemoteBd);
		data.add(ByteConverter.getBytes((short) this.mConnIntervalMin));	// 2 bytes
		data.add(ByteConverter.getBytes((short) this.mConnIntervalMax));	// 2 bytes
		data.add(ByteConverter.getBytes((short) this.mConnLatency));		// 2 bytes
		data.add(ByteConverter.getBytes((short) this.mSupervisionTimeout));	// 2 bytes
		
		
	}
	/**
	 * Flatten this object in to a Parcel.
	 * @param dest The Parcel in which the object should be written.
	 * @param flags Additional flags about how the object should be written. May be 0 or PARCELABLE_WRITE_RETURN_VALUE. 
	 */
	@Override
	public void writeToParcel(Parcel dest, int flags) {

		dest.writeInt(this.mCommand);
		dest.writeByteArray(this.mRemoteBd);
		dest.writeInt(this.mConnIntervalMin);
		dest.writeInt(this.mConnIntervalMax);
		dest.writeInt(this.mConnLatency);
		dest.writeInt(this.mSupervisionTimeout);
		
		
	}
}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
