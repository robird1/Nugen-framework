package com.accu_chek.solo_m.rcapp.application.ble.request;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * Request for command CommsConstant.CommandCode.COMM_CONFIG
 * 
 * @author EDLiu
 * @version 1.0
 * @created 17-Feb-2015 11:39:51 AM
 */
public class ConfigurationDataRequest implements IRequest, Parcelable
{

    /**
     * Command Code
     */
    private int mCommand;
    /**
     * Minimum connection interval
     */
    private int mConnIntervalMin;
    /**
     * Maximum connection interval
     */
    private int mConnIntervalMax;
    /**
     * Connection latency
     */
    private int mConnLatency;
    /**
     * Supervision timeout
     */
    private int mSupervisionTimeout;
    /**
     * Scan mode of RC in active mode
     */
    private int mScanMode;
    /**
     * Scan window of RC in active mode
     */
    private int mScanWindow;
    /**
     * Scan interval of RC in active mode
     */
    private int mScanInterval;
    /**
     * Scan state duration of RC in active mode
     */
    private int mScanStateDuration;
    /**
     * Scan state duration of RC in active mode
     */
    private int mScanPauseDuration;
    /**
     * Scan filter of RC in active mode
     */
    private int mFilterPolicyOne;
    private int mFilterPolicyTwo;
    private int mFilterPolicyThree;

    private int mStandbyTimeout;

    /**
     * Number of e2e retries
     */
    private int mE2eRetries;

    private int mProcedureTimeout;

    private int mFastReconnectionDuration;

    private int mConnLatencyIntensive;

    private int mProcedureTimeoutHistory;

    private int mProcedureTimeoutKES;

    private int mProcedureRetries;

    private int mReadHistoryMaxCount;

    private int mChallengeResponseTime;

    private int mChallengeRepeatTime;
    
    private int mPumpRecordNumberMin;
    private int mPumpRecordNumberMax;

    public static final Parcelable.Creator<ConfigurationDataRequest> CREATOR = new Parcelable.Creator<ConfigurationDataRequest>()
    {
        public ConfigurationDataRequest createFromParcel(Parcel in)
        {
            return new ConfigurationDataRequest(in);
        }

        public ConfigurationDataRequest[] newArray(int size)
        {
            return new ConfigurationDataRequest[size];
        }
    };

    public ConfigurationDataRequest()
    {

    }

    /**
     * constructor for IPC transaction
     * 
     * @param in
     */
    public ConfigurationDataRequest(Parcel in)
    {

        this.mCommand = in.readInt();
        this.mConnIntervalMin = in.readInt();
        this.mConnIntervalMax = in.readInt();
        this.mConnLatency = in.readInt();
        this.mConnLatencyIntensive = in.readInt();
        this.mSupervisionTimeout = in.readInt();

        this.mScanMode = in.readInt();
        this.mFilterPolicyOne = in.readInt();
        this.mFilterPolicyTwo = in.readInt();
        this.mFilterPolicyThree = in.readInt();

        this.mScanWindow = in.readInt();
        this.mScanInterval = in.readInt();
        this.mScanStateDuration = in.readInt();
        this.mScanPauseDuration = in.readInt();

        this.mStandbyTimeout = in.readInt();
        this.mProcedureTimeout = in.readInt();
        this.mProcedureTimeoutKES = in.readInt();
        this.mFastReconnectionDuration = in.readInt();
        this.mProcedureTimeoutHistory = in.readInt();
        this.mE2eRetries = in.readInt();
        this.mProcedureRetries = in.readInt();
        this.mReadHistoryMaxCount = in.readInt();
        this.mChallengeResponseTime = in.readInt();
        this.mChallengeRepeatTime = in.readInt();
        this.mPumpRecordNumberMin = in.readInt();
        this.mPumpRecordNumberMax = in.readInt();

    }

    /**
     * constructor for factory method
     * 
     * @param command Command Code (Hamming Distance). See
     *            CommsConstant.CommandCode for valid values.
     */
    public ConfigurationDataRequest(int command)
    {
        this.mCommand = command;
    }

    public int describeContents()
    {
        return 0;
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

    /**
     * Minimum connection interval
     * 
     * @param interval
     */
    public void setConnIntervalMin(SafetyNumber<Integer> interval)
    {
        this.mConnIntervalMin = interval.get().intValue();
    }

    /**
     * Maximum connection interval
     * 
     * @param interval
     */
    public void setConnIntervalMax(SafetyNumber<Integer> interval)
    {
        this.mConnIntervalMax = interval.get().intValue();
    }

    /**
     * Connection latency
     * 
     * @param latency
     */
    public void setConnLatency(SafetyNumber<Integer> latency)
    {
        this.mConnLatency = latency.get().intValue();
    }

    /**
     * Supervision Timeout
     * 
     * @param timeout
     */
    public void setSuperVisionTimeout(SafetyNumber<Integer> timeout)
    {
        this.mSupervisionTimeout = timeout.get().intValue();
    }

    /**
     * Scan mode of RC in active mode
     * 
     * @param mode
     */
    public void setActiveScanMode(SafetyNumber<Integer> mode)
    {
        this.mScanMode = mode.get().intValue();
    }

    /**
     * Scan window of RC in active mode
     * 
     * @param window
     */
    public void setActiveScanWindow(SafetyNumber<Integer> window)
    {
        this.mScanWindow = window.get().intValue();
    }

    /**
     * Scan interval of RC in active mode
     * 
     * @param interval
     */
    public void setActiveScanInterval(SafetyNumber<Integer> interval)
    {
        this.mScanInterval = interval.get().intValue();
    }

    /**
     * Scan filter1 of RC in standby mode
     * 
     * @param policy
     */
    public void setActiveFilterPolicyOne(SafetyNumber<Integer> policy)
    {
        this.mFilterPolicyOne = policy.get().intValue();
    }

    /**
     * Scan filter1 of RC in standby mode
     * 
     * @param policy
     */
    public void setActiveFilterPolicyTwo(SafetyNumber<Integer> policy)
    {
        this.mFilterPolicyTwo = policy.get().intValue();
    }

    /**
     * Scan filter1 of RC in standby mode
     * 
     * @param policy
     */
    public void setActiveFilterPolicyThree(SafetyNumber<Integer> policy)
    {
        this.mFilterPolicyThree = policy.get().intValue();
    }

    /**
     * Scan state duration of RC in active mode
     * 
     * @param duration
     */
    public void setActiveScanStateDuration(SafetyNumber<Integer> duration)
    {
        this.mScanStateDuration = duration.get().intValue();
    }

    /**
     * Scan Pause duration of RC in active mode
     * 
     * @param duration
     */
    public void setActiveScanPauseDuration(SafetyNumber<Integer> duration)
    {
        this.mScanPauseDuration = duration.get().intValue();
    }

    /**
     * standby Timeout
     * 
     * @param Timeout
     */
    public void setStandbyTimeout(SafetyNumber<Integer> StandbyTimeout)
    {
        this.mStandbyTimeout = StandbyTimeout.get().intValue();
    }

    /**
     * procedure Timeout
     * 
     * @param Timeout
     */
    public void setProcedureTimeout(SafetyNumber<Integer> ProcedureTimeout)
    {
        this.mProcedureTimeout = ProcedureTimeout.get().intValue();
    }

    /**
     * procedure Timeout KES
     * 
     * @param Timeout
     */
    public void setProcedureTimeoutKES(SafetyNumber<Integer> ProcedureTimeoutKES)
    {
        this.mProcedureTimeout = ProcedureTimeoutKES.get().intValue();
    }

    /**
     * Fast reconnection duration
     * 
     * @param duration
     */
    public void setFastReconnDuration(SafetyNumber<Integer> duration)
    {
        this.mFastReconnectionDuration = duration.get().intValue();
    }

    /**
     * Conn Latency Intensive
     * 
     * @param
     */
    public void setConnLatencyIntensive(
            SafetyNumber<Integer> ConnLatencyIntensive)
    {
        this.mConnLatencyIntensive = ConnLatencyIntensive.get().intValue();
    }

    /**
     * procedure Timeout History
     * 
     * @param
     */
    public void setProcedureTimeoutHistory(
            SafetyNumber<Integer> ProcedureTimeoutHistory)
    {
        this.mProcedureTimeoutHistory = ProcedureTimeoutHistory.get()
                .intValue();
    }

    /**
     * Number of transfer retries
     * 
     * @param retry
     */
    public void setE2ERetries(SafetyNumber<Integer> retry)
    {
        this.mE2eRetries = retry.get().intValue();
    }

    /**
     * Number of procedure retries
     * 
     * @param retry
     */
    public void setProcedureRetries(SafetyNumber<Integer> retry)
    {
        this.mProcedureRetries = retry.get().intValue();
    }

    /**
     * Max count of read history
     * 
     * @param count
     */
    public void setReadHistoryMaxCount(SafetyNumber<Integer> count)
    {
        this.mReadHistoryMaxCount = count.get().intValue();
    }
    
    /**
     * Response time of challenge
     * 
     * @param time
     * 
     */
    public void setChallengeResponseTime(SafetyNumber<Integer> time)
    {
        this.mChallengeResponseTime = time.get().intValue();
    }
    
    /**
     * Repeat time of challenge
     * 
     * @param 
     */
    public void setChallengeRepeatTime(SafetyNumber<Integer> time)
    {
        this.mChallengeRepeatTime = time.get().intValue();
    }
    
    /**
     * Minimum Pump Record Number
     */
    public void setPumpRecordNumberMin(SafetyNumber<Integer> recordNumMin)
    {
        this.mPumpRecordNumberMin = recordNumMin.get().intValue();
    }
    /**
     * Maximum Pump Record Number
     */
    public void setPumpRecordNumberMax(SafetyNumber<Integer> recordNumMax)
    {
        this.mPumpRecordNumberMax = recordNumMax.get().intValue();
    } 
    /**
     * adding attribute values to array list for further frame wrapping
     */
    @Override
    public void writeToByteArrayList(ArrayList<byte[]> data)
    {

        data.add(ByteConverter.getBytes((short) this.mCommand)); 			 // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mConnIntervalMin)); 	 // 2
                                                                         // bytes
        data.add(ByteConverter.getBytes((short) this.mConnIntervalMax));		 // 2
                                                                         // bytes
        data.add(ByteConverter.getBytes((short) this.mConnLatency));			 // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mConnLatencyIntensive)); // 2
                                                                              // bytes
        data.add(ByteConverter.getBytes((short) this.mSupervisionTimeout));	 // 2
                                                                            // bytes

        data.add(ByteConverter.getBytes((byte) this.mScanMode));		     // 1 byte
        data.add(ByteConverter.getBytes((byte) this.mFilterPolicyOne));      // 1
                                                                        // byte
        data.add(ByteConverter.getBytes((byte) this.mFilterPolicyTwo));      // 1
                                                                        // byte
        data.add(ByteConverter.getBytes((byte) this.mFilterPolicyThree));    // 1
                                                                          // byte

        data.add(ByteConverter.getBytes((short) this.mScanWindow));           // 2 bytes
        data.add(ByteConverter.getBytes((short) this.mScanInterval));         // 2 bytes
        data.add(ByteConverter.getBytes((int) this.mScanStateDuration));	     // 4
                                                                         // bytes
        data.add(ByteConverter.getBytes((int) this.mScanPauseDuration));      // 4
                                                                         // bytes

        data.add(ByteConverter.getBytes((int) this.mStandbyTimeout));         // 4 bytes
        data.add(ByteConverter.getBytes((int) this.mProcedureTimeout));       // 4
                                                                        // bytes
        data.add(ByteConverter.getBytes((int) this.mProcedureTimeoutKES));       // 4
                                                                           // bytes
        data.add(ByteConverter.getBytes((int) this.mFastReconnectionDuration));  // 4
                                                                                // bytes
        data.add(ByteConverter.getBytes((int) this.mProcedureTimeoutHistory));   // 4
                                                                               // bytes
        data.add(ByteConverter.getBytes((byte) this.mE2eRetries));            // 1 bytes

        data.add(ByteConverter.getBytes((int) this.mProcedureRetries));       // 4
                                                                        // bytes
        data.add(ByteConverter.getBytes((int) this.mReadHistoryMaxCount));       // 4
                                                                           // bytes
        data.add(ByteConverter.getBytes((int) this.mChallengeResponseTime));  // 4
                                                                             // bytes
        data.add(ByteConverter.getBytes((int) this.mChallengeRepeatTime));   // 4
                                                                           // bytes
        data.add(ByteConverter.getBytes((int) this.mPumpRecordNumberMin));   // 4 bytes
        data.add(ByteConverter.getBytes((int) this.mPumpRecordNumberMax));   // 4 bytes
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
        dest.writeInt(this.mConnIntervalMin);
        dest.writeInt(this.mConnIntervalMax);
        dest.writeInt(this.mConnLatency);
        dest.writeInt(this.mConnLatencyIntensive);
        dest.writeInt(this.mSupervisionTimeout);

        dest.writeInt(this.mScanMode);
        dest.writeInt(this.mFilterPolicyOne);
        dest.writeInt(this.mFilterPolicyTwo);
        dest.writeInt(this.mFilterPolicyThree);

        dest.writeInt(this.mScanWindow);
        dest.writeInt(this.mScanInterval);
        dest.writeInt(this.mScanStateDuration);
        dest.writeInt(this.mScanPauseDuration);

        dest.writeInt(this.mStandbyTimeout);
        dest.writeInt(this.mProcedureTimeout);
        dest.writeInt(this.mProcedureTimeoutKES);
        dest.writeInt(this.mFastReconnectionDuration);
        dest.writeInt(this.mProcedureTimeoutHistory);
        dest.writeInt(this.mE2eRetries);

        dest.writeInt(this.mProcedureRetries);
        dest.writeInt(this.mReadHistoryMaxCount);
        dest.writeInt(this.mChallengeResponseTime);
        dest.writeInt(this.mChallengeRepeatTime);
        dest.writeInt(this.mPumpRecordNumberMin);
        dest.writeInt(this.mPumpRecordNumberMax);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
