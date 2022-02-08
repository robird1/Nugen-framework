/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: uicommanddispatcher
 *
 * Brief: Provide interface for UI to send and receive command and response
 *
 * Create Date: 06/03/2015
 * $Revision: 24623 $
 * $Author: WillLong $
 * $Id: UICommandDispatcher.java 24623 2015-11-23 09:05:33Z WillLong $
 */

package com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;
import android.content.Intent;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommandDataSentIndication;
import com.accu_chek.solo_m.rcapp.application.common.CustUserHandle;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.uartuicp.UARTAccessWithFlowControl;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.CommsJNI;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.IUICommandDispatcher;


/**
 * The CommandDispatcher provides interfaces to send requests to Comms Subsystem
 * and to receive responses from it for further parsing and dispatching.
 * 
 */
public class UICommandDispatcher extends IUICommandDispatcher.Stub
{
    /**
     * TAG string for print trace
     */
    private final static String TAG = "UICommandDispatcher";

    /**
     * The maximum length of a frame.  
     * A frame consists of a payload and the bytes of LLF, length, sequence, CRC, group.
     *
     */
    private final static int MAX_FRM_LEN  = CommsConstant.MAX_PL_LEN 
            + CommsConstant.LEN_BYTE_LEN + CommsConstant.SEQ_BYTE_LEN 
            + CommsConstant.CRC_BYTE_LEN + CommsConstant.GROUP_BYTE_LEN 
            + CommsConstant.LLF_BYTE_LEN + CommsConstant.LLF_BYTE_LEN;
    /**
     * The maximum length of a Ack frame.  
     * A frame consists of a payload and the bytes of LLF, length, sequence, 
     * group ,ack-sequence.
     *
     */
    private final static int ACK_FRM_LEN = CommsConstant.LLF_BYTE_LEN 
            + CommsConstant.LEN_BYTE_LEN + CommsConstant.SEQ_BYTE_LEN 
            + CommsConstant.GROUP_BYTE_LEN + CommsConstant.SEQ_BYTE_LEN 
            + CommsConstant.LLF_BYTE_LEN ;
    /**
     * Maximum number of the sequence number
     * 
     * Range: 255 only Unit: N/A Scaling: 1
     */
    private final static int MAX_SEQ_NUMBER = 255;
    /**
     * The step of sequence count
     * 
     * Range: 1 only Unit: N/A Scaling: 1
     */
    private final static int SEQ_COUNT = 1;
    /**
     * The max retry times of receiving an echo response from Comms subsystem
     * 
     * Range: 3 only Unit: times Scaling: 1
     */
    private final static int MAX_RETRY_NUMBER = 3;
    /**
     * Acknowledge Count down interval of response timeout
     * 
     * Range: 1000L only Unit: millisecond Scaling: 1
     */
    private final static long ACK_TIMEOUT_COUNTDOWN_INTERVAL = 1000L;    
    /**
     * Token Count down interval of response timeout
     * 
     * Range: 1000L only Unit: millisecond Scaling: 1
     */
    private final static long TOKEN_TIMEOUT_COUNTDOWN_INTERVAL = 1000L;

    /**
     * Token MAX Countdown of response timeout
     * 
     * Range: 3 only Unit: millisecond Scaling: 1
     */
    private final static int TOKEN_TIMEOUT_MAX_COUNTER = 3;

    /**
     * The thread number in thread pool 
     * 
     * Range: 2 only
     * Unit: N/A
     * Scaling: 1
     */
    private static final int THREAD_NUMS = 2;
    
    /**
     * application's context
     * 
     * Range: valid Context Object
     * Unit: Context
     * Scaling: 1
     */
    private static Context mContext;

    /**
     * The instance of CommandDispatcher 
     * 
     * Range: valid CommandDispatcher object
     * Unit: CommandDispatcher object
     * Scaling: 1
     */
    private static UICommandDispatcher mInstance = null;

    /**
     * Request Sequence number
     * 
     * Range: 1~ MAX_SEQ_NUMBER Unit: N/A Scaling: 1
     */
    private static int mRequestSequence = 0;

    /**
     * Sequence number of UI processor
     * 
     * Range: 1~ MAX_SEQ_NUMBER Unit: N/A Scaling: 1
     */
    private static int mSequenceNumber = 0;

    /**
     * Sequence number of Comms subsystem
     * 
     * Range: 1 ~ MAX_SEQ_NUMBER Unit: N/A Scaling: 1
     */
    private static int mCommsSequence = 0;

    /**
     * To record that if this command without token.
     * 
     * Range: true/false Unit: boolean Scaling: 1
     */
    boolean mIsWithoutToken = false;

    /**
     * A token permission to send a request to Communicaiton sub-system.
     * 
     * Range: true/false Unit: boolean Scaling: 1
     */
    private static AtomicBoolean mIsGetRequestToken = new AtomicBoolean(false);
    /**
     * An acknowledgement which is confirmed from Communication sub-system.
     * 
     * Range: true/false Unit: boolean Scaling: 1
     */
    private static AtomicBoolean mIsGetAck = new AtomicBoolean(false);
    /**
     * A queue for requests which are being sent to Communication sub-system.
     * 
     * Range: Not NULL or null Unit: Object Scaling: 1
     */
    private static BlockingQueue<byte[]> mRequestQueue = null;
    
    /**
     * A queue for the received frames which are being parsed.
     * 
     * Range: Not NULL or null Unit: Object Scaling: 1
     */
    private static BlockingQueue<byte[]> mResponseQueue = null;
    /**
     * A temporary byte array list to store the remaining bytes of the received
     * frames. If remaining bytes exists, it is expected that certain length of
     * the beginning bytes of the next coming bytes combine to a complete frame
     * with the previous remaining bytes.
     * 
     * Range: null or Object Unit: Object Scaling: 1
     */
    private static ByteArrayBuffer mRemainingFrame = null;

    /**
     * Ack Timer Task
     * 
     * Range: null or object
     * Unit: object
     * Scaling: 1
     */
    private AckTimerTask mSendAckTimerTask = null;

    /**
     * The timer object for waiting Ack response from Communicaiton Sub-system.
     * 
     * Range: null or object
     * Unit: object
     * Scaling: 1
     */
    private Timer mAcktTimer = null;
    
    /**
     * Token Timer Task
     * 
     * Range: null or object
     * Unit: object
     * Scaling: 1
     */
    private TokenTimerTask mTokenTimerTask = null;

    /**
     * The timer object for waiting Token response from Communication 
     * 
     * Range: null or object
     * Unit: object
     * Scaling: 1
     */
    private Timer mTokenTimer = null;
    
    /**
     * Interface of UART and UICP
     * 
     * Range: null or Object Unit: Object Scaling: 1
     */
    private UARTAccessWithFlowControl mUARTInterface = null;

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function is used to get the UICommandDispatcher class instance.
     *
     * @param context: The client's Context
     *          Range: valid Context object
     *          Unit: Context
     *          Scaling: 1
     * @return UICommandDispatcher instance: The instance of class UICommandDispatcher
     *          Range: valid UICommandDispatcher object
     *          Unit: UICommandDispatcher object
     *          Scaling: 1
     */
    public static synchronized UICommandDispatcher getInstance(Context context)
    {
        if( null == mInstance )
        {
            mInstance = new UICommandDispatcher(context);
        }
        
        return mInstance;
    }
    
    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function is the interface for application to submit requests. The
     * application shall submit a request with correct frame data.
     * 
     * @param byteArr: A SafetyByteArray for sending to Comms Sub-system.
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray object
     *          Scaling: 1
     * @return int isOk: Is ok to submit request.
     *          Range: HammingDistance.SAFETY_BOOLEAN_TRUE 
     *                  / HammingDistance.SAFETY_BOOLEAN_FALSE
     *          Unit: int
     *          Scaling: 1 
     * @exception RemoteException
     */
    @Override
    public synchronized int submitRequest(SafetyByteArray byteArr)
            throws RemoteException
    {
        byte[] arrayReq = {};
        int ret = HammingDistance.SAFETY_BOOLEAN_FALSE;

        Debug.printI(TAG, "[submitRequest] enter");

        arrayReq = checkValidByteArray(byteArr);

        if ( arrayReq.length <= MAX_FRM_LEN )
        {
            handleRequestQueue(arrayReq);
            ret = HammingDistance.SAFETY_BOOLEAN_TRUE;
        }
        else
        {
            throw new DataIntegrityException("Outgoing frame over flow!");
        }
        return ret;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function initializes the class members and powers on Comms sub-system at
     * booting stage.
     */
    public void init()
    {
        // power on comms
        CommsJNI.powerOnComms();

        mIsGetRequestToken.set(true);
        mIsGetAck.set(true);
        mRequestQueue = new LinkedBlockingQueue<byte[]>();
        mResponseQueue = new LinkedBlockingQueue<byte[]>();
        mRemainingFrame = new ByteArrayBuffer(0);
        mRemainingFrame.clear();
        
        ExecutorService service = Executors.newFixedThreadPool(THREAD_NUMS);
        SendRequestExcutor sendRequestExcutor = new SendRequestExcutor();
        ReceiveResponseExcutor responseHandleExcutor = new ReceiveResponseExcutor();
        service.execute(sendRequestExcutor);
        service.execute(responseHandleExcutor);

    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function reset Comms and Uart interface
     *
     */
    @Override
    public void resetComms()
    {
        Debug.printI(TAG, "resetComms");

        // Reset sequence
        mSequenceNumber = 0;
        mCommsSequence = 0;

        // Restart UART
        getUARTInterface();
        try
        {
            mUARTInterface.restartUART();
        }
        catch (OperationFailException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
    }


    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function is the interface called by UART module, if response is received.
     * 
     * @param data: The data received from UART interface. 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray object
     *          Scaling: 1
     */
    public void receiveFrame(SafetyByteArray data)
    {

        byte[] frame = null;
        
        frame = data.getByteArray();
        
        boolean isValidFrame = ((frame != null) && (frame.length > 0));

        if ( true == isValidFrame )
        {
            // put to queue
            handleResponseQueue(frame);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function adds the payload, length, sequence, LLF and CRC bytes into a frame for
     * sending to Communication sub-system.
     * 
     * @param message: The message bytes which retrieved from request objects.
     *          Range: A byte array with length is less than
     *          CommsConstant.MAX_PL_LEN
     *          Unit: byte[]
     *          Scaling: 1
     * @param isWithCRC: Normal request is with CRC. Ack is without CRC.
     *          Range: true/false
     *          Unit: boolean
     *          Scaling: 1 
     * @return byte[] frame: The wrapped frame to be sent to Communication sub-system. It is a
     *         byte array wrapped with the sequence, length, crc and LLF bytes.
     *          Range: Not NULL; byte length <= (CommsConstant.MAX_PL_LEN +
     *          SEQ_BYTE_LEN + CRC_BYTE_LEN + LEN_BYTE_LEN + LLF_BYTE_LEN*2)
     *          Unit: byte[]
     *          Scaling: 1
     */
    protected synchronized static byte[] wrapFrame(byte[] message,boolean isWithCRC)
    {
        int length = 0;
        int seq = 0;
        int crc = 0;
        byte[] rtnData;

        Debug.printI(TAG, "Enter wrapPayload.");
        ArrayList<byte[]> frame = new ArrayList<byte[]>();

        // Get sequence number
        seq = commandSequenceNumber();

        // Check if with CRC
        if( true == isWithCRC )
        {
            // Count length
            length = message.length + CommsConstant.SEQ_BYTE_LEN 
                   + CommsConstant.CRC_BYTE_LEN;
            // Fill Data
            frame.add(ByteConverter.getBytes(CommsConstant.LLF));
            frame.add(ByteConverter.getBytes((byte) length));
            frame.add(ByteConverter.getBytes((byte) seq));
            frame.add(message);

            // Fill CRC
            crc = generateCrc(length, seq, message);
            frame.add(ByteConverter.getBytes((short) crc));
        }
        else
        {
            // Count length
            length = message.length + CommsConstant.SEQ_BYTE_LEN;

            // Fill Data
            frame.add(ByteConverter.getBytes(CommsConstant.LLF));
            frame.add(ByteConverter.getBytes((byte) length));
            frame.add(ByteConverter.getBytes((byte) seq));
            frame.add(message);
        }

        // Fill tail
        frame.add(ByteConverter.getBytes(CommsConstant.LLF));
        // Convert byte
        rtnData = ByteConverter.buildByteArray(frame);

        return rtnData;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * The function assigns a new sequence number for sending a new frame to
     * Communication sub-system.
     * 
     * @return int sequence: The new sequence number for the new frame. 
     *          Range:1~MAX_SEQ_NUMBER 
     *          Unit: int 
     *          Scaling: 1
     */
    protected static int commandSequenceNumber()
    {

        if (mSequenceNumber >= MAX_SEQ_NUMBER)
        {
            mSequenceNumber = SEQ_COUNT;
        }
        else
        {
            mSequenceNumber += SEQ_COUNT;
        }
        return mSequenceNumber;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * The function passes length, seq and payload to CRCTool to generate CRC.
     * 
     * @param length: The value of the length byte.
     *          Range: length <= CommsConstant.MAX_PL_LEN
     *          Unit: int
     *          Scaling: 1
     * @param seq: The value of the sequence byte. 
     *          Range: 1 ~ MAX_SEQ_NUMBER
     *          Unit: int
     *          Scaling: 1
     * @param payload: The data payload for generating CRC .
     *          Range: A byte array with length less than
     *              CommsConstant.MAX_PL_LEN
     *          Unit: byte[]
     *          Scaling: 1
     * @return int crc: The calculated CRC (2 bytes).
     *          Range:0~65535
     *          Unit: int 
     *          Scaling: 1
     */
    protected static int generateCrc(int length, int seq, byte[] payload)
    {

        int crc = 0;
        byte[] frame = null;
        ArrayList<byte[]> data = new ArrayList<byte[]>();
        
        data.add(ByteConverter.getBytes((byte) length));
        data.add(ByteConverter.getBytes((byte) seq));
        data.add(payload);
        frame = ByteConverter.buildByteArray(data);
        crc = CRCTool.generateCRC16(frame);

        return crc;
    }
    
    /**
     * Status: Coding Done
     *
     * Function Description:
     * The class constructor.
     *
     * @param context: The client's Context.
     *          Range: valid object
     *          Unit: Context
     *          Scaling: 1
     * @return UICommandDispatcher instance: The instance of class UICommandDispatcher.
     *          Range: valid object
     *          Unit: UICommandDispatcher object
     *          Scaling: 1
     */
    protected UICommandDispatcher(Context context)
    {
        mContext = context;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function puts the received frame into the response queue.
     *
     * @param frame: The response frame comes from UART.
     *          Range: A valid byte array
     *          Unit: byte[]
     *          Scaling: 1
     */
    protected void handleResponseQueue(byte[] frame)
    {
        Debug.printI(TAG, "[handleResponseQueue]: enter");
        try
        {
            mResponseQueue.put(frame);
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();

            // Show EMWR E57_E_RC_ELECTRONIC
            Debug.printD(TAG, "E57, EMW45607 Put response queue error!");
            NotifyMessage msg = new NotifyMessage(EMWRList.EMW45607);
            NotifyProxy.showEMWR( mContext, msg);
        }
        finally
        {
            // Apply to the coding standard
        }
        //notify ReceiveResponse thread to get data.
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function merges the remaining frame. If remaining bytes are available, merges 
     * them with the newly received frame. The buffer shall be clear after remaining bytes 
     * are merged.
     * 
     * @param frame: The new received frame or the frame to be merged. 
     *          Range: A byte array
     *          Unit: byte[]
     *          Scaling: 1
     * @return byte[] frameOut: The merged and complete frame for further parsing. 
     *          Range: A byte array
     *          Unit: byte[]
     *          Scaling: 1
     */
    protected byte[] mergeRemainingFrame(byte[] frame)
    {
        boolean isNonZeroSize = !mRemainingFrame.isEmpty();
        if (isNonZeroSize)
        {
            mRemainingFrame.append(frame, 0, frame.length);
            frame = mRemainingFrame.toByteArray();
            mRemainingFrame.clear();
        }
        else
        {
            // Apply to the coding standard
        }
        
        return frame;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function puts the input frame into the request queue.
     * 
     * @param frame: The request frame.
     *            Range: A valid byte array with length less than 
     *                      CommsConstant.MAX_PL_LEN
     *            Unit: byte[]
     *            Scaling: 1
     */
    protected void handleRequestQueue(byte[] frame)
    {
        Debug.printI(TAG, "[handleRequestQueue]: enter");
        try
        {
            mRequestQueue.put(frame);
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();

            // Show EMWR E57_E_RC_ELECTRONIC
            Debug.printD(TAG, "E57, EMW45605 Put request queue error!");
            NotifyMessage msg = new NotifyMessage(EMWRList.EMW45605);
            NotifyProxy.showEMWR( mContext, msg);
        }
        finally
        {
            // Apply to the coding standard
        }
        //Notify send request thread to send data
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function is to check if the SafetyByteArray is valid and return a
     * byteArray.
     * 
     * @param byteArr: A SafetyByteArray received from UI module.
     *          Rang: A valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Object Scaling: 1
     * @return byte[] byteArrOut: The byte array got from the input SafetyByteArray. 
     *          Range: A byte array
     *          Unit: byte[]
     *          Scaling: 1
     */
    protected byte[] checkValidByteArray(SafetyByteArray byteArr)
    {
        byte[] arrayReq = {};

        if (null != byteArr)
        {
            arrayReq = byteArr.getByteArray();
        }
        else
        {
            // Apply to the coding standard
        }
        return arrayReq;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function checks if the input frame is a final command.
     * 
     * @param frame: The input frame for checking command.
     *          Range: A valid byte array with length less than 
     *                  CommsConstant.MAX_FRM_LEN
     *          Unit: byte[]
     *          Scaling: 1
     * @return int isFinal: The flag shows that if it is a final command.
     *          Range: HammingDistance.SAFETY_BOOLEAN_FALSE
     *                  /HammingDistance.SAFETY_BOOLEAN_TRUE
     *          Unit: int 
     *          Scaling: 1
     */
    protected int isFinalCommand(byte[] frame)
    {
        int isFinal = HammingDistance.SAFETY_BOOLEAN_FALSE;
        ByteBuffer buffer = ByteBuffer.wrap(frame);
        int groupId;
        int commandId = 0;
        final int MASK = (int) 0xffff;
        
        buffer.mark();            
        buffer.get(); // LLF
        buffer.get(); // Length
        buffer.get(); // seq #

        // Get groupId from buffer
        groupId = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN) & MASK ;
        Debug.printD(TAG, "isfinalCommand,GroupID = 0x" + Integer.toHexString(groupId) );

        // Get Command ID
        commandId = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN) & MASK ;

        // Check if Group ID is TEST_MODE Group ID
        if ( CommsConstant.GroupId.TEST_MODE == groupId ) 
        {
            isFinal = HammingDistance.SAFETY_BOOLEAN_TRUE;

            // After test mode command, there is no activity any more
            // Stop Ack timer first
            stopAckTimer();

            // Stop token timer
            stopTokenTimer();
        }
        else
        {
            // Apply to the coding standard
        }

        // Check if this command is without token message
        if (CommsConstant.GroupId.ACK != groupId)
        {
            mIsWithoutToken = CommsConstant.CommandGroup.NO_TOKEN.contains(commandId);
            Debug.printI(TAG, "Without Token"); 
        }
        else
        {
        }

        buffer.reset();
        return isFinal;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * The function submits the input frame to UART interface.
     * 
     * @param frame: The frame to be submitted.
     *          Range: A valid byte array with length less than 
     *                  CommsConstant.MAX_FRM_LEN
     *          Unit: byte[]
     *          Scaling: 1
     */
    protected synchronized void submitFrame(byte[] frame) 
    {
        Debug.printD(TAG, "SubmitFrame = ");
        Debug.dumpPayload(TAG, frame);
        try
        {
            int isFinal;

            // Check if command is final command
            isFinal = isFinalCommand(frame);
            getUARTInterface();
            mUARTInterface.send(new SafetyByteArray(frame, CRCTool
                .generateCRC16(frame)), isFinal);
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();

            // Show EMWR E57_E_RC_ELECTRONIC
            Debug.printD(TAG, "E57,EMW45601 Send command error!");
            NotifyMessage msg = new NotifyMessage(EMWRList.EMW45601);
            NotifyProxy.showEMWR( mContext, msg);
        }
        finally
        {
            // Apply to the coding standard
        }
    }   
        
    
    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function gets and returns the single instance of UARTAccessWithFlowControl.
     */
    protected void getUARTInterface()
    {
        if (null == mUARTInterface)
        {
            mUARTInterface = UARTAccessWithFlowControl.getInstance();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function converts the received response and sends it to application.
     * 
     * @param pack: The response pack contains a response for applications.
     *            Range: Valid ResponsePack object
     *            Unit: ResponsePack object
     *            Scaling: 1
     */
    protected void obtainReceiveResponse(ResponsePack pack)
    {
        IResponse response = null;
        Intent intent = null;
        String action = "";

        response = pack.getResponse();
        intent = new Intent();
        action = response.getClass().getName();
        // String action = response.getClass().getSimpleName();

        Debug.printD(TAG, "action = " + action);
        intent.setAction(action);
        intent.putExtra(ResponseAction.EXTRA_RESPONSEPACK, pack);
        Debug.printD(TAG, "Normal sendBroadcastAsUser");
        mContext.sendBroadcastAsUser(intent, CustUserHandle.getALL());
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * The function is called to start the acknowledgement timeout count-down.
     */
    protected void startAckTimer()
    {

        // Stop Ack timer first
        stopAckTimer();

        // New timer task
        mSendAckTimerTask = new AckTimerTask();
        // Reset counter
        mSendAckTimerTask.resetCounter();

        // New timer class
        mAcktTimer = new Timer();       
        // Schedule timer
        mAcktTimer.schedule(mSendAckTimerTask, ACK_TIMEOUT_COUNTDOWN_INTERVAL
                , ACK_TIMEOUT_COUNTDOWN_INTERVAL);

        Debug.printI(TAG, "[startAckTimer]");
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * The function is called to stop the acknowledgement timeout count-down.
     */
    protected void stopAckTimer()
    {
        // Check if timer class is valid.
        if( null != mAcktTimer )
        {
            // Cancel timer
            mAcktTimer.cancel();
            mAcktTimer = null;
            Debug.printI(TAG, "[stopAckTimer]");
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * The function is called to start the Token timeout count-down.
     */
    protected void startTokenTimer()
    {

        // Stop timer first
        stopTokenTimer();

        // New timer task
        mTokenTimerTask = new TokenTimerTask();
        // Reset counter
        mTokenTimerTask.resetCounter();

        // New timer class
        mTokenTimer = new Timer();

        // Schedule timer
        mTokenTimer.schedule(mTokenTimerTask, TOKEN_TIMEOUT_COUNTDOWN_INTERVAL,
                TOKEN_TIMEOUT_COUNTDOWN_INTERVAL);

        Debug.printI(TAG, "[startTokenTimer]");
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * The function is called to stop the Token timeout count-down.
     */
    protected void stopTokenTimer()
    {
        if( null != mTokenTimer )
        {
            mTokenTimer.cancel();
            mTokenTimer = null;
            Debug.printI(TAG, "[stopTokenTimer]");
        }
        else
        {
            // Apply to the coding standard
        }
    }


    /**
     * Status: Coding Done
     *
     * The timer counts down until the acknowledge from Communication is received. 
     * It would re-send the current per second. The max retry times is 3.
     */
    class AckTimerTask extends TimerTask
    {
        private AtomicInteger mAckTickCounter = new AtomicInteger(0);
        
        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function resets the Ack timer counter to zero.
         */
        public void resetCounter()
        {
            mAckTickCounter.set(0);
        }
        
        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function is the Ack timer task run function. It will be called when Ack timer tick happens.
         * This timer will be stopped when the counter reaches MAX_RETRY_NUMBER.
         */
        @Override
        public void run()
        { 
            // Get and increase tick counter
            int count = mAckTickCounter.incrementAndGet();
            Debug.printI(TAG, "SendAckTimerTask@run = " + count );

            // Check if count equal or grater than MAX_RETRY_NUMBER
            if( count < MAX_RETRY_NUMBER )
            {
                // Apply to the coding standard
            }
            else
            {
                // Stop timer and reset frame
                stopAckTimer();

                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57, EMW45609 Ack timeout!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45609);
                NotifyProxy.showEMWR( mContext, msg);
            }
            
        }
        
    }

    /**
     * Status: Coding Done
     *
     * The timer counts down until the CommandDataSentIndication received. An
     * error should be handled if it timeouts.
     */
    class TokenTimerTask extends TimerTask
    {
        private AtomicInteger mTokenTickCounter = new AtomicInteger(0);
        
        /**
         * Status: Coding Done
         *
         * Function Description:
         * The function resets the Token timer counter to zero.
         */
        public void resetCounter()
        {
            mTokenTickCounter.set(0);
        }
        
        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function is the Token timer task run function. It will be called when Token timer tick happens.
         * This timer will be stopped when the counter reaches TOKEN_TIMEOUT_MAX_COUNTER.
         */
        @Override
        public void run()
        {            
            // Get and increase tick counter
            int count = mTokenTickCounter.incrementAndGet();

            Debug.printI(TAG, "TokenTimerTask@run = " + count );
           
            // Check if counter equal or grater than TOKEN_TIMEOUT_MAX_COUNTER
            if( count < TOKEN_TIMEOUT_MAX_COUNTER)
            {
                // Apply to the coding standard
            }
            else
            {
                // Stop timer and reset frame
                stopTokenTimer();

                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57, EMW45610 Token timeout!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45610);
                NotifyProxy.showEMWR( mContext, msg);

                mTokenTickCounter.set(0);
            }
            
        }
        
    }
    

    /**
     * Status: Coding Done
     *
     * This class would handle the sending request to Communication sub-system.
    */
    protected class SendRequestExcutor implements Runnable 
    {
        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function is the run function of the SendRequestExcutor thread. It checks the Token
         * and Ack repeatedly. It then submits the command to UART interface when available.
         */
        @Override
        public void run()
        {
            byte[] payload = {};
            byte[] frame = {};

            try
            {
                while(true)
                {
    //                Debug.printI(TAG, "SendRequestExcutor run");
                    
                    if ( mIsGetRequestToken.compareAndSet(true, false) )
                    {
                        Debug.printI(TAG, "Token get!!"); 
                        if ( mIsGetAck.compareAndSet(true, false) )
                        {
                                Debug.printI(TAG, "Ack get!!"); 

                                // Get payload from request queue
                                payload = mRequestQueue.take();
                                
                                Debug.printI(TAG, "[SubmitRequest]"); 

                                // Wrap paload to frame
                                frame = wrapFrame(payload, true);

                                // Store request sequence number
                                mRequestSequence = mSequenceNumber;

                                // Set Token flag to false and restart Token timer
                                mIsGetRequestToken.set(false);
                                startTokenTimer();

                                // Set Ack flag to false and restart Ack timer
                                mIsGetAck.set(false);
                                startAckTimer();

                                // Submit Frame to UART interface
                                submitFrame(frame);
                        }
//                        else
//                        {
//                            // Apply to the coding standard
//                        }
                    }
                    else
                    {
                        // Apply to the coding standard
                    }
                }
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();

                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57, EMW45606 Take request queue error!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45606);
                NotifyProxy.showEMWR( mContext, msg);
            }
            finally
            {
                // Apply to the coding standard
            }
        }
    }

    /**
     * Status: Coding Done
     *
     * This class would handle the parsing response from Communication sub-system.
    */
    protected class ReceiveResponseExcutor implements Runnable
    {
        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function is the run function of the ReceiveResponseExcutor thread. It merges incoming frame
         * and remaining frame. Then it parse the frame to application.
         */
        @Override
        public void run()
        {            
            byte[] bytes;
            try
            {
                while(true)
                {
                    //Wait response data
                    Debug.printI(TAG, "Wait response data");
                        
                    // Get response frame from response queue
                    bytes = mResponseQueue.take();

                    // Merge to remaining frame                    
                    bytes = mergeRemainingFrame(bytes);

                    // Parse frame                    
                    FrameParser parser = new FrameParser();
                    parser.parse(bytes);  
                }
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();

                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57, EMW45608 Take response queue error!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45608);
                NotifyProxy.showEMWR( mContext, msg);
            }
            finally
            {
                // Apply to the coding standard
            }
        }
    }


    /**
     * Status: Coding Done
     *
     * The inner class FrameParse is dedicated to parse a received frame which
     * is poll from the response queue.
     * 
     */
    protected class FrameParser
    {

        /**
         * Status: Coding Done
         *
         * Function Description:
         * The FrameParser classpParses payload bytes from the received frame which might contain more
         * than one frame even with an incomplete one. A normal frame is combined with a LENGTH byte,
         * a SEQUENCE byte, payload bytes and  wrapped with two LLF bytes. See SWDD of Communication 
         * sub-system -Command and Message for frame format.
         * 
         * @param frame: The frame to be parsed.
         *          Range: A valid byte array
         *          Unit: byte[]
         *          Scaling: 1
         */
        public void parse(byte[] frame)
        {
            int receivedLength = 0;
            int parsedLength = 0;
            int hasLLFHead = HammingDistance.SAFETY_BOOLEAN_FALSE;
            ByteBuffer buffer = ByteBuffer.wrap(frame);

            receivedLength = frame.length;
            
            Debug.printD(TAG, "total length of the ReceivedFrame = "
                    + receivedLength);
            Debug.dumpPayload(TAG, frame);
            do
            {
                // Check heading LLF exist
                hasLLFHead = isLLFHead(buffer);
                if( HammingDistance.SAFETY_BOOLEAN_TRUE == hasLLFHead ) 
                {
                    // Check received frame
                    checkReceivedFrame(buffer);

                    // Get parsed length
                    parsedLength = buffer.position();
                    Debug.printD(TAG, "ParsedLength = " + parsedLength);
                }
                else
                {
                    // Frame head error
                    parsedLength = receivedLength;

                    // Show EMWR E57_E_RC_ELECTRONIC
                    Debug.printD(TAG, "E57,EMW45602 Receive data header error!");
                    NotifyMessage msg = new NotifyMessage(EMWRList.EMW45602);
                    NotifyProxy.showEMWR( mContext, msg);
                }
            } while (receivedLength > parsedLength);

        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function is used to check the received frame with Ack frame / sequence number /
         * CRC.
         * 
         * @param buffer: The byte buffer of the received frame.
         *          Range: Valid ByteBuffer object
         *          Unit: ByteBuffer 
         *          Scaling: 1
         */
        protected void checkReceivedFrame(ByteBuffer buffer)
        {
            int frameLength;
            int payloadLength;
            int remaining;
            int isFrameCompleted = HammingDistance.SAFETY_BOOLEAN_FALSE;
            int isAck = HammingDistance.SAFETY_BOOLEAN_FALSE;
            byte[] payload = null;

            // check if complete Frame
            remaining = buffer.remaining();
            isFrameCompleted = isCompleteFrame(buffer);
            if( HammingDistance.SAFETY_BOOLEAN_TRUE == isFrameCompleted )
            {
                // Check if it is Ack Response
                isAck = isAckResponse(buffer);
                if( HammingDistance.SAFETY_BOOLEAN_FALSE == isAck )
                {
                    // Check CRC
                    verifyCrc(buffer);

                    // Verify Frame End
                    buffer.get(); //LLF
                    frameLength = buffer.get();
                    buffer.get();//seq #
                    payloadLength = frameLength
                          - (CommsConstant.SEQ_BYTE_LEN + CommsConstant.CRC_BYTE_LEN);
                    payload = new byte[payloadLength];
                    buffer.get(payload, 0, payloadLength);
                    buffer.getShort();// crc bytes
                    validateFrameEnd(buffer);

                    // Convert response and send to client
                    convertIntoResponse(payload);
                }
                else
                {
                    // Apply to the coding standard
                }
            }
            else
            {
                storeRemainingFrame(remaining, buffer);
            }
        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function checks if the input frame is an ACK response frame.
         * 
         * @param buffer: The byte buffer of the received frame.
         *          Range: Valid ByteBuffer object
         *          Unit: ByteBuffer 
         *          Scaling: 1
         * @return int isAck: The flag shows that if input is Ack frame.
         *          Range: HammingDistance.SAFETY_BOOLEAN_TRUE 
         *                  / HammingDistance.SAFETY_BOOLEAN_FALSE
         *          Unit: int 
         *          Scaling: 1
         */
        protected int isAckResponse(ByteBuffer buffer)
        {
            // Set return value to HammingDistance.SAFETY_BOOLEAN_FALSE
            int ret = HammingDistance.SAFETY_BOOLEAN_FALSE;
            int sequenceNumber = 0;
            int groupId;
            final int MASK = (int) 0xffff;
            final int MASK_L = (int) 0xff;
            
            buffer.mark();            
            buffer.get(); // LLF
            buffer.get(); // Length
            buffer.get(); // seq #

            // Get groupId
            groupId = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN) & MASK ;
            Debug.printD(TAG, "GroupID = 0x" + Integer.toHexString(groupId) );

            // Check if Group ID is Ack Group ID
            if ( CommsConstant.GroupId.ACK == groupId ) 
            {
                // Check if sequence Number is request sequence number
                sequenceNumber = buffer.get() & MASK_L;
                buffer.reset();
                if ( mRequestSequence == sequenceNumber )
                {
                    // Ack received, set Ack flag to true and stop Ack timer
                    mIsGetAck.set(true);
                    stopAckTimer();
                    Debug.printD(TAG, "Ack received. Seq = " + sequenceNumber);

                    // Check if no need to wait token
                    if(true == mIsWithoutToken)
                    {
                        Debug.printI(TAG, "Set for without Token"); 
                        // Reset flag
                        mIsWithoutToken = false;
                        // Set mIsGetRequestToken flag to true
                        mIsGetRequestToken.set(true);
                        // Stop token timer
                        stopTokenTimer();
                    }
                }
                else
                {
                    Debug.printD(TAG, "Received Seq Error = " + sequenceNumber + " not " + mRequestSequence );
                    // Apply to the coding standard
                }
                // Read out all Ack frame buffer
                ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN, 
                        ACK_FRM_LEN );
                // Set return value to HammingDistance.SAFETY_BOOLEAN_TRUE
                ret = HammingDistance.SAFETY_BOOLEAN_TRUE;
            }
            else
            {
                // Reset frame buffer point
                buffer.reset();
            }
            return ret;
        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function checks that the receiving frame includes at least one 
         * complete response frame. It's also checks the sequence number 
         * of the received frame.
         *
         * @param buffer: The byte buffer of the received frame.
         *          Range: Valid ByteBuffer object
         *          Unit: ByteBuffer 
         *          Scaling: 1
         * @return int isComplete: The flag shows that if the input frame is complete or not
         *          Range: HammingDistance.SAFETY_BOOLEAN_TRUE 
         *                  / HammingDistance.SAFETY_BOOLEAN_FALSE
         *          Unit: int 
         *          Scaling: 1
         */
        protected int isCompleteFrame(ByteBuffer buffer)
        {
            // Set return value to HammingDistance.SAFETY_BOOLEAN_FALSE
            int ret = HammingDistance.SAFETY_BOOLEAN_FALSE;
            int frameLength = 0;
            int sequence = 0;
            int remaining = -1;
            int FrameLenWithHead = 0;
            final int MASK = 0xff;

            // Check if input frame has sufficient header length
            remaining = buffer.remaining();
            if(remaining > (CommsConstant.LLF_BYTE_LEN + CommsConstant.LEN_BYTE_LEN 
                    + CommsConstant.SEQ_BYTE_LEN + CommsConstant.GROUP_BYTE_LEN ) )
            {
                // Get sequence and frame length
                buffer.mark(); 
                buffer.get(); //Head LLF
                frameLength = buffer.get();
                sequence = buffer.get() & MASK;
                buffer.reset();

                // Check Sequence
                verifyCommsSequence(sequence);

                // Check if input frame has sufficient frame length
                remaining = buffer.remaining();
                FrameLenWithHead = CommsConstant.LLF_BYTE_LEN 
                      + CommsConstant.LEN_BYTE_LEN + frameLength 
                      + CommsConstant.LLF_BYTE_LEN;
                if(remaining >= FrameLenWithHead)    
                {
                    ret = HammingDistance.SAFETY_BOOLEAN_TRUE;
                }
                else
                {
                    Debug.printD(TAG, "Not a complete frame ..... 2");
                    // Apply to the coding standard
                }
            }
            else
            {
                  Debug.printD(TAG, "Not a complete frame ..... 1");
                  // Apply to the coding standard
            }
            
            return ret;
        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function retrieves the heading LLF byte until it is found. If sometimes unknown frames
         * are received, this function helps to parse out unexpected bytes.
         * 
         * @param buffer: The byte buffer of the received frame.
         *          Range: Valid ByteBuffer object
         *          Unit: ByteBuffer 
         *          Scaling: 1
         * @return int isLLF: The flag shows that if the input buffer has LLF head.
         *          Range: HammingDistance.SAFETY_BOOLEAN_TRUE 
         *                  / HammingDistance.SAFETY_BOOLEAN_FALSE
         *          Unit: int 
         *          Scaling: 1
         */
        protected int isLLFHead(ByteBuffer buffer)
        {
            byte llf = 0;
            int bufLen = buffer.limit();
            int ret = HammingDistance.SAFETY_BOOLEAN_FALSE;
            
            for(int i = buffer.position(); i < bufLen; i++)
            {
                // Check if return value is HammingDistance.SAFETY_BOOLEAN_FALSE
                if( HammingDistance.SAFETY_BOOLEAN_FALSE == ret )
                {
                    // Get next byte
                    buffer.mark();
                    llf = buffer.get();
                    // Check if it is LLF byte
                    if( CommsConstant.LLF == llf )
                    {
                        // Reset buffer to this LLF byte
                        buffer.reset();
                        ret = HammingDistance.SAFETY_BOOLEAN_TRUE;
                    }
                    else
                    {
                        // Reset buffer for next cycle
                        buffer.reset();
                        buffer.get();
                    }
                }
                else
                {
                    // Apply to the coding standard
                }
            }            
            
            return ret;

        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function checks if the CRC in the frame buffer is the same as the re-calculated CRC.
         *
         * Be careful not to mutate the buffer by calling buffer.mark() and buffer.reset() else where!
         * 
         * @param buffer: The byte buffer of the received frame.
         *          Range: Valid ByteBuffer object
         *          Unit: ByteBuffer 
         *          Scaling: 1
         */

        protected void verifyCrc(ByteBuffer buffer)
        {
            
            boolean isCRCEqual = false;
            int frameLength = 0;
            byte[] crc = new byte[CommsConstant.CRC_BYTE_LEN];
            byte[] check = new byte[CommsConstant.CRC_BYTE_LEN];
            byte[] bytes = {};// bytes without length and crc bytes (sequence
                              // byte included)
            byte[] payload = {};
            ArrayList<byte[]> data = new ArrayList<byte[]>();
            
            // Marks current position
            buffer.mark();
            buffer.get();
            frameLength = buffer.get();
            bytes = new byte[frameLength - CommsConstant.CRC_BYTE_LEN];
            buffer.get(bytes, 0, (frameLength - CommsConstant.CRC_BYTE_LEN));

            // Build payload for count CRC
            data.add(ByteConverter.getBytes((byte) frameLength));
            data.add(bytes);
            payload = ByteConverter.buildByteArray(data);

            // Get CRC from frame
            crc = ByteConverter.readBytes(buffer, ByteOrder.LITTLE_ENDIAN,
                    CommsConstant.CRC_BYTE_LEN);

            // Count CRC
            check = ByteConverter.getBytes((short) CRCTool.generateCRC16(payload));

            // Resets the position of this buffer to marked position.
            buffer.reset();

            // Check if CRC the same
            isCRCEqual = Arrays.equals(crc, check);
            if ( false == isCRCEqual )
            {
                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57,EMW45603 Receive data crc error!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45603);
                NotifyProxy.showEMWR( mContext, msg);
            }
            else
            {
                // Apply to the coding standard
            }

        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function checks if the received frame sequence is correct.
         * 
         * @param sequence: The sequence number of a newly received frame.
         *          Range: 1 ~ MAX_SEQ_NUMBER 
         *          Unit: int 
         *          Scaling: 1
         */
        protected void verifyCommsSequence(int sequence)
        {
            // Get correct sequence number
            int correctSequence = getNextSequence(mCommsSequence);

            // Check if input sequence is correct
            Debug.printD(TAG, "VerifySeqNumber : " + sequence);
            if( sequence != correctSequence )
            {
                Debug.printD(TAG, "SequenceError! previous = " + mCommsSequence);

                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57,EMW45604 Receive data sequence error!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45604);
                NotifyProxy.showEMWR( mContext, msg);
            }
            else
            {
                // Apply to the coding standard
            }

            // Store new sequence number            
            mCommsSequence = sequence;

        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function is used to get the next sequence number.
         *
         * @param sequence: The current sequence number.
         *            Range: 1 ~ MAX_SEQ_NUMBER 
         *            Unit: int 
         *            Scaling: 1
         * @return int nextSequence: The next sequence number.
         *            Range: 1 ~ MAX_SEQ_NUMBER 
         *            Unit: int 
         *            Scaling: 1
         */
        protected int getNextSequence(int sequence)
        {
            int nextSequence;
            // Check if the sequence equal MAX_SEQ_NUMBER
            if( MAX_SEQ_NUMBER == sequence )
            {
                // Set nextSequence to 1
                nextSequence = 1;
            }
            else
            {
                // Add nextSequence by 1
                nextSequence = sequence + 1;
            }
            return nextSequence;
        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function stores the input frame bytes into mRemainingFrame buffer.
         * 
         * @param length: The length of the input byte buffer.
         *          Range: 1 ~ CommsConstant.MAX_PL_LEN
         *          Unit: int
         *          Scaling: 1
         * @param buffer: The byte buffer to be append.
         *          Range: Valid ByteBuffer object
         *          Unit: ByteBuffer 
         *          Scaling: 1
         */
        protected void storeRemainingFrame(int length, ByteBuffer buffer)
        {
            // Store input buffer to new frame buffer
            byte[] frame = new byte[length];
            buffer.get(frame, 0, length);

            // Append frame buffer to mRemainingFrame
            mRemainingFrame.append(frame, 0, length);
        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function verifies the end of a frame. The byte after CRC bytes must be LLF flag,
         * If not, it throws a DataIntegrityException for error handling.
         * 
         * @param buffer: The byte buffer to be check.
         *          Range: Valid ByteBuffer object
         *          Unit: ByteBuffer 
         *          Scaling: 1
         * @throws DataIntegrityException
         */
        protected void validateFrameEnd(ByteBuffer buffer)
        {
            boolean isTailLLF = ( CommsConstant.LLF != buffer.get() );
            
            if( true == isTailLLF )    
            {
                // Throw DataIntegrityException
                Debug.printD(TAG, "Tail LLF byte missed!");
                throw new DataIntegrityException("Tail LLF byte missed!");
            }
            else
            {
                // Apply to the coding standard
            }
        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function is used to send an acknowledgement back to Communication Sub-system
         * for response / indication / notification messages.
         * 
         * @param sequence: The sequence number from Comms sub-system.
         *            Range: 1 ~ MAX_SEQ_NUMBER 
         *            Unit: int 
         *            Scaling: 1
         */
        protected void acknowledgeResponse(int sequence)
        {
            ArrayList<byte[]> bytes = new ArrayList<byte[]>();
            byte[] frame = {};

            // Build acknowledge response frame
            bytes.add(ByteConverter.getBytes((short) CommsConstant.GroupId.ACK));
            bytes.add(ByteConverter.getBytes((byte) sequence));

            Debug.printD(TAG, "AckToComm: Seq = " + sequence);

            // Wrap Frame to byte[] to be send
            frame = wrapFrame(ByteConverter.buildByteArray(bytes),false);

            // Submit frame
            submitFrame(frame);
        }

        /**
         * Status: Coding Done
         *
         * Function Description:
         * This function retrieves the message bytes from the payload. The first two bytes are
         * group id and the rest of the payload bytes are message bytes.
         * 
         * @param payload: The payload bytes parsed out from a frame (without the
         *                  length, sequence, LLF and crc bytes). 
         *          Range: Not NULL; byte length 1 ~ CommsConstant.MAX_PL_LEN
         *          Unit: byte[] 
         *          Scaling: 1
         */
        protected void convertIntoResponse(byte[] payload)
        {
            int group = 0;
            byte[] message = null;
            final int MASK = 0xffff; 
            int command = 0;
            boolean isToken = false;
            
            try
            {
                // Wrap to ByteBuffer
                ByteBuffer buffer = ByteBuffer.wrap(payload);
                
                //Get groupId
                group = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN) & MASK;
                buffer.mark();
                message = new byte[(buffer.capacity() - CommsConstant.GROUP_BYTE_LEN)];
                buffer.get(message, 0, (buffer.capacity() - CommsConstant.GROUP_BYTE_LEN));
                buffer.reset();

                // Get Command ID
                command = ByteConverter.readShort(buffer, ByteOrder.LITTLE_ENDIAN);
                Debug.printD(TAG, "ResponseGroup = 0x" + Integer.toHexString(group & MASK) + 
                                " command = 0x" + Integer.toHexString(command & MASK));
    //            Debug.dumpPayload(TAG, message);

                // Get response interface
                IResponse response = ResponsePayloadFactory
                    .getResponsePayload(command);

                // Check if response interface is valid            
                if(null != response)
                {
                    // Set message into response
                    response.setMessage(new SafetyByteArray(message, CRCTool
                            .generateCRC16(message)));
                    response.parseMessage();

                    //Send acknowledges response to Comms sub-system
                    if ((CommsConstant.CommandCode.COMM_PRE_POWER_OFF == command)
                        || (CommsConstant.CommandCode.COMM_SET_FLIGHT_MODE == command))
                    {
                            mUARTInterface.enterSleepMode();
                    }
                    else
                    {
                        acknowledgeResponse(mCommsSequence);
                    }

                    Debug.printD(TAG, "response isinstanceof ["
                                        + response.getClass().getSimpleName() + "]");

                    // Check if it is a token message
                    isToken = (response instanceof CommandDataSentIndication);
                    if (isToken)
                    {
                        Debug.printI(TAG, "Set Token, for response"); 
                        // Set mIsGetRequestToken flag to true
                        mIsGetRequestToken.set(true);
                        // Stop token timer
                        stopTokenTimer();
                    }
                    else
                    {
                        // Apply to the coding standard
                    }

                    // Pack response
                    ResponsePack pack = new ResponsePack();
                    pack.setGroupId(group);
                    pack.setResponse(response);

                    // Send response to client
                    obtainReceiveResponse(pack);
                        
                }
                else
                {
                    // Apply to the coding standard
                }

            }
            catch (OperationFailException e)
            {
                e.printStackTrace();

                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57, EMW45611 Uart enter sleep error!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45611);
                NotifyProxy.showEMWR( mContext, msg);
            }
            finally
            {
                // Apply to the coding standard
            }
       }

    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */

