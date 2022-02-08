/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.bgmcontrol.bgmcommunication
 * 
 * Brief:
 *
 * Create Date: 6/17/2015
 * $Revision: 25175 $
 * $Author: VictorChen $
 * $Id: BgmCommunication.java 25175 2015-11-30 11:25:57Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.communication.bgmcommunication;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmControl;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmUtils;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBgmConstant;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.ICommandType;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmCommunicationTimeOutException;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmException;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.RDYDessertException;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager.UARTPort;

public class BgmCommunication
{

    /**
     * BgmCommunication object.
     */
    private static BgmCommunication mInstance = null;
    /**
     * A flag to record the current command whether is Strip insert action.
     */
    private static boolean mIsStripInsertAction = false;
    /**
     * A flag to record the current command whether is wait blood action.
     */
    private static boolean mIsWaitBloodAction = false;

    /**
     * A flag to record the current command whether is communication finish.
     */
    private static boolean mIsReadDone = false;

    /**
     * UartManager Object
     * Range: not null
     * Unit: UARTManager
     * Scaling: 1
     */
    private static UARTManager mUartData = null;

    private static final String TAG = "BgmCommunication";
    /**
     * Timer object
     */
    private Timer mTimer = null;
    /**
     * ACK counter.
     * Range: 2~3
     * Unit: integer
     * Scaling: 1
     */
    private int mEndACKValue = -1;
    /**
     * BGMWorkRun object.
     * Range: not null
     * Unit: BGMWorkRun
     * Scaling: 1
     */
    private BgmCommunicationThead mBgmWorkThread = null;

    /**
     * Send to Bgm submodule byte data
     * Range: must larger than 0x00
     * Unit: byte
     * Scaling: 1
     */
    private byte mSendToBgm = IBgmConstant.BYTEZERO;
    /**
     * Receive byte data form Bgm submodule
     * Range: must larger than 0x00
     * Unit: byte
     * Scaling: 1
     */
    private byte mBgmData = IBgmConstant.BYTEZERO;
    /**
     * ArrayList to record Bgm submodule byte data.
     *
     */
    private ArrayList<Byte> mReceiveBgmData = new ArrayList<Byte>();

    /**
     * Map specific command to action.
     */
    private HashMap<Byte, String> mBgCommandMap = new HashMap<Byte, String>();

    {
        mBgCommandMap.put(IBgmConstant.EOT, IBgmConstant.ACTION_BG_EOT);

        mBgCommandMap.put(IBgmConstant.NAK, IBgmConstant.ACTION_BG_NAK);

        mBgCommandMap.put(IBgmConstant.ACK, IBgmConstant.ACTION_BG_ACK);

        mBgCommandMap.put(IBgmConstant.ON_WAITSTRIP,
                IBgmConstant.ACTION_WAIT_STRIP);

        mBgCommandMap.put(IBgmConstant.ON_STRIPINSER,
                IBgmConstant.ACTION_STRIP_INSERT);

        mBgCommandMap.put(IBgmConstant.ON_WAITBLOOD,
                IBgmConstant.ACTION_WAIT_BLOOD);

        mBgCommandMap.put(IBgmConstant.ON_BG_PROCESSING,
                IBgmConstant.ACTION_bG_PROCESSING);

    }

    /**
     * 
     * Set mBgmData value
     * 
     * see mBgmData[out]
     * 
     * @param data,[in] bgm data from UART
     *            Range: lager than 0x00
     *            Unit: byte
     *            Scaling: 1
     */
    protected final void setBgmData(byte data)
    {
        if (IBgmConstant.BYTEZERO > data)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        mBgmData = data;
    }

    /**
     * 
     * Get mBgmData value
     * see mBgmData[in]
     * 
     * return byte [out] see mBgmData.
     */
    protected final byte getBgmData()
    {
        if (IBgmConstant.BYTEZERO == mBgmData)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        return mBgmData;
    }

    /**
     * 
     * Start to read bgm data.
     * Use a thread to read bgm data in uart periodically.
     * 
     * see mTimer[in]
     * see mBgmWorkThread[in]
     * 
     * return void [out] None.
     */
    public final void startReadBgmData()
    {
        if (mTimer == null)
        {
            Debug.printI(TAG, "startReadBgmData");
            mTimer = new Timer();
            mBgmWorkThread = new BgmCommunicationThead(this);
            mTimer.schedule(mBgmWorkThread, 0L, 1L);
        }
    }

    /**
     * 
     * Stop read bgm data thread.
     * 
     * see mTimer[in]
     * see mBgmWorkThread[in]
     * 
     * return void [out] None.
     */
    public void stopReadBgmData()
    {
        Debug.printI(TAG, "stopReadBgmData");
        if (mTimer != null)
        {
            mTimer.cancel();
            mTimer = null;
            mBgmWorkThread = null;
        }
    }

    /**
     * Before write command, reset all flags.
     *
     * see StatusType.READ_NAK[out]
     * see StatusType.CR_AND_ACK[out]
     * see StatusType.START_RECIEVE[out]
     * see StatusType.START_WRITE[out]
     * see mIsStripInsertAction[out]
     * see mIsWaitBloodAction[out]
     *
     * 
     * return void [out] None.
     */
    private void initialCondition()
    {
        BgmUtils.StatusType.READ_NAK.setState(false);
        BgmUtils.StatusType.CR_AND_ACK.setState(false);
        BgmUtils.StatusType.START_RECIEVE.setState(false);
        BgmUtils.StatusType.START_WRITE.setState(false);
        mReceiveBgmData.clear();
        mIsStripInsertAction = false;
        mIsWaitBloodAction = false;
        BgmUtils.setACKNum(0);
    }

    /**
     * Write command to Bgm submodule.
     * 
     * 
     * see mBgmWorkThread[in]
     * see StatusType.READ_NAK[in]
     * see mUartData[in]
     * see mSendToBgm[out] byte data, write to bgm
     * 
     * return void [out] None.
     * 
     * @param command[in] command byte value
     *            Range: array length:1~10
     *            Unit: byte
     *            Scaling: 1
     * @param bgmData[in] byte data that should write to BGM subsystem.
     *            Range: null, 8~14
     *            Unit: byte
     *            Scaling: 1
     * @throws RDYDessertException
     * @throws BgmCommunicationTimeOutException
     *
     */

    public void writeBgmCommand(byte[] command, byte[] bgmData)
            throws BgmException, BgmCommunicationTimeOutException
    {
        Debug.printI(TAG, "[writeBGMCommand]: enter");
        CommonUtils.objectCheck(command);

        int length = command.length;
        int minLength = 1;
        int maxLength = 10;
        boolean status = false;
        boolean nakState = false;
        byte[] commandArray = new byte[IBgmConstant.ONE];
        SafetyByteArray data = null;
        boolean isEqualMinLength = length < minLength;
        boolean isEqualMaxLength = length > maxLength;
        boolean isBgMeasurementTimeOut = false;
        boolean isbGTestCommand = BgmUtils.getIsbGTestCommand();
        boolean isUpdateCodeKeyCommand = BgmUtils.getCommandKey().equals(
                ICommandType.UpdateCodeKeyCommand.getCommandKey());

        // check command length
        if (isEqualMinLength || isEqualMaxLength)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }

        // initial readBGMData parameter
        initialCondition();

        // start read timer
        startReadBgmData();

        // check is read to bgm or write to bgm
        checkReadWriteData(bgmData);

        // start read bgm data

        mIsReadDone = false;
        Debug.printI(TAG, "writeBgmCommand mIsEnable " + mIsReadDone);
        // start write byte command to bgm
        for (int i = 0; i < length; i++)
        {
            mSendToBgm = command[i];
            commandArray[IBgmConstant.ZERO] = command[i];
            data = new SafetyByteArray(commandArray,
                    CRCTool.generateCRC16(commandArray));

            Debug.printI(TAG, "write: " + toHex(mSendToBgm));
            // write data command to bgm by UART
            try
            {
                mUartData.send(UARTPort.BGMUART, data);
                Debug.dumpPayload("W", mSendToBgm);
            }
            catch (OperationFailException e)
            {

                e.printStackTrace();
            }
            finally
            {
                // Empty for static code analysis
            }
            // wait until read success
            status = BgmUtils
                    .readConditionBlockState(IBgmConstant.BGM_WAIT_TIMEOUT);

            // time out or read NAK, stop write next byte
            nakState = BgmUtils.StatusType.READ_NAK.getState();
            Debug.printI(TAG, "writeBGMCommand status: " + status);

            // if state is NAK, throw exception
            // else continue to write next byte command
            if (true == nakState)
            {
                throw new BgmException();
            }
            else if (status == false)
            {
                throw new BgmCommunicationTimeOutException();
            }
        }
        // check whether is codekey update command.
        if (isUpdateCodeKeyCommand == true)
        {
            writeCodeKeyData(bgmData);
        }
        // other command, set data to bgm
        else if (null != bgmData)
        {
            writeBgmData(bgmData);
        }
        else
        {
            // static code for analysis.
        }
        isbGTestCommand = BgmUtils.getIsbGTestCommand();
        Debug.printI(TAG, "isbGTestFlow status: " + isbGTestCommand);
        // if bg test command, should wait until bg test finish
        if (isbGTestCommand)
        {
            // wait for bg test finish.
            isBgMeasurementTimeOut = BgmUtils
                    .blockBgTestState(IBgmConstant.BGM_WAIT_BLOOD_TIME);
        }
        else
        {
            isBgMeasurementTimeOut = true;
        }

        if (isBgMeasurementTimeOut == false)
        {
            // bgm test time out.
            throw new BgmCommunicationTimeOutException();
        }
        else
        {
            // static code for analysis
        }
        // wait read command thread done.
        // isReadFinish = getCommunicationDoneFlag();
        Debug.printI(TAG, "isReadNotFinish: " + isbGTestCommand);

        Debug.printI(TAG, "isReadNotFinish mIsEnable: " + mIsReadDone);
        if (mIsReadDone == false)
        {
            waitReadDone();
        }
        else
        {
            // static code for analysis
        }
        // read NAK, error Handling.
        nakState = BgmUtils.StatusType.READ_NAK.getState();
        if (nakState)
        {
            throw new BgmException();
        }
        else
        {
            // Empty for static code analysis
        }
        Debug.printI(TAG, "[writeBGMCommand]: exit");
    }

    /**
     * 
     * Write code key data to bgm.
     * return void [out] None.
     * 
     * @param bgmData
     *            Range: valid object of byte array
     *            Unit: byte array
     *            Scaling: 1
     * @throws RDYDessertException
     */
    private void writeCodeKeyData(byte[] bgmData) throws BgmException,
            BgmCommunicationTimeOutException
    {
        CommonUtils.objectCheck(bgmData);

        final int codekeySize = 1200;
        int codekeyInputSize = bgmData.length;
        int codeKeyPacketSize = 21;
        int codekeyPacketLength = 56;
        int codeKeyLastLenth = 24;
        byte[][] codekeydata = new byte[codeKeyPacketSize][codekeyPacketLength];// 21*56
        byte[] codekeydataLast = new byte[codeKeyLastLenth];// 24
        int index = 0;
        SafetyByteArray data = null;
        boolean status = false;
        boolean nakState = false;
        Debug.printI("test", "codekeyInputSize " + codekeyInputSize);
        if (codekeySize != codekeyInputSize)
        {
            throw new DataIntegrityException();
        }
        // build code key data command
        for (int i = 0; i < codeKeyPacketSize; i++)
        {
            for (int j = 0; j < codekeyPacketLength; j++)
            {
                codekeydata[i][j] = bgmData[index];
                index++;
            }
            Debug.printI("test", "codekeyInputSize "
                    + toHex(codekeydata[i][53]) + " "
                    + toHex(codekeydata[i][54]));
        }
        for (int i = 0; i < codeKeyLastLenth; i++)
        {
            codekeydataLast[i] = bgmData[index];
            index++;
        }
        // start send codekey packets to bgm
        // packet 1~21
        for (int i = 0; i < codeKeyPacketSize; i++)
        {
            data = new SafetyByteArray(codekeydata[i],
                    CRCTool.generateCRC16(codekeydata[i]));
            try
            {
                Debug.printI("test", "write++");
                mUartData.send(UARTPort.BGMUART, data);
                Debug.printI("test", "write--");
                Debug.printI(TAG, "No." + i);

                status = BgmUtils
                        .readConditionBlockState(IBgmConstant.BGM_WAIT_TIMEOUT);
                nakState = BgmUtils.StatusType.READ_NAK.getState();
                Debug.printI("test", "writeBGMCommand status: " + status);
                Debug.printI("test", "No." + i);
                // if state is NAK, throw exception
                // else continue to write next byte command
                if (true == nakState)
                {
                    throw new BgmException();
                }
                else if (status == false)
                {
                    throw new BgmCommunicationTimeOutException();
                }
            }
            catch (OperationFailException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // do nothing
            }
            CommonUtils.sleep(10L);
        }
        // last data packet
        data = new SafetyByteArray(codekeydataLast,
                CRCTool.generateCRC16(codekeydataLast));
        try
        {
            mUartData.send(UARTPort.BGMUART, data);
            Debug.printI(TAG, "No.22");
            Debug.printI("test", "No.22");
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // do nothing
        }
    }

    /**
     * 
     * Write data to bgm.
     * return void [out] None.
     * 
     * @param bgmData
     *            Range: valid object of byte array
     *            Unit: byte array
     *            Scaling: 1
     */
    private void writeBgmData(byte[] bgmData) throws BgmException
    {
        SafetyByteArray data = new SafetyByteArray(bgmData,
                CRCTool.generateCRC16(bgmData));

        Debug.printI(TAG, "[writeBGMCommand]: write data");
        try
        {
            mUartData.send(UARTPort.BGMUART, data);
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // do nothing
        }
    }

    /**
     * 
     * Wait read bgm data finish.
     * 
     * return void [out] None.
     * 
     * @throws RDYDessertException
     */
    private void waitReadDone() throws BgmCommunicationTimeOutException
    {
        boolean isReadDoneTimeout = BgmUtils
                .blockBgmCommandState(IBgmConstant.BGM_WAIT_TIMEOUT);
        boolean isReadFinish = false;
        Debug.printI(TAG, "waitReadDone " + isReadDoneTimeout);

        Debug.printI(TAG, "waitReadDone mIsEnable" + mIsReadDone);

        isReadFinish = mIsReadDone;
        if (isReadDoneTimeout == false && isReadFinish == false)
        {
            throw new BgmCommunicationTimeOutException();
        }
        else
        {
            // static code for analysis
        }
    }

    /**
     * Check the command is write data to BGM or read data form BGM
     * If bgmData null, is read data.
     * 
     * return void [out] None.
     * 
     * @param bgmData[in] the data would write to BGM
     *            Range: null,
     *            Unit: byteArray
     *            Scaling: 1
     * 
     */
    private void checkReadWriteData(byte[] bgmData)
    {
        boolean isUpdateCodeKeyCommand = BgmUtils.getCommandKey().equals(
                ICommandType.UpdateCodeKeyCommand.getCommandKey());
        int numUpdateCodeKeyAck = 24;
        if (null == bgmData)
        {
            // read data command
            BgmUtils.StatusType.START_RECIEVE.setState(true);
            mEndACKValue = IBgmConstant.READACK_VALUE;
        }
        else if (isUpdateCodeKeyCommand)
        {
            // upadate code key command
            BgmUtils.StatusType.START_WRITE.setState(true);
            mEndACKValue = numUpdateCodeKeyAck;
        }
        else
        {
            // write data command
            BgmUtils.StatusType.START_WRITE.setState(true);
            mEndACKValue = IBgmConstant.WRITEACK_VALUE;
        }
    }

    /**
     * Read data from BGM subsystem through UART module .
     * After success get one byte data, call parssingCommandProcess();
     *
     * see mUartData[in]
     *
     * return void [out] None.
     */

    protected void readBgmData()
    {
        SafetyByteArray data = null;
        try
        {
            data = mUartData.receive(UARTPort.BGMUART,
                    IBgmConstant.mExpectedByteLength);
            byte[] uartData = data.getByteArray();
            boolean isgetBgmData = uartData.length > IBgmConstant.ZERO
                    && uartData[0] != IBgmConstant.BYTEZERO;

            if (isgetBgmData)
            {
                Debug.printI(TAG, "readBgmData: " + toHex(uartData[0]));
                Debug.printI("test", "readBgmData: " + toHex(uartData[0]));
                setBgmData(uartData[0]);
                Debug.dumpPayload("R", uartData[0]);
                parssingCommandProcess();
            }
            else
            {
                // Empty for static code analysis
            }
        }
        catch (OperationFailException e1)
        {
            e1.printStackTrace();
        }
        catch (ArgumentErrorException e1)
        {
            e1.printStackTrace();
        }
        finally
        {
            // Empty for static code analysis
        }
    }

    /**
     * Check the read data, In case the data is ACK, EOT or NAK,
     * call BgmSpecificCommandProcess() to do specific command process.
     * On the contrary, call checkBgTestUIAction().
     * Then, call checkCommandComplete() to check the command has finished.
     * 
     * see mBgmData[in]
     * see mSendToBgm[in]
     * 
     * return void [out] None.
     *
     */

    protected void parssingCommandProcess()
    {
        String bgCommand = mBgCommandMap.get(getBgmData());
        Debug.printI(TAG, "read: " + toHex(getBgmData()));
        Debug.printI(TAG, "[doBgProcess]:Bgcommand " + bgCommand);
        Debug.printI(TAG, "[doBgProcess]:mSendtobgm " + toHex(mSendToBgm));
        // ACK, EOT, NAK
        // ACK: add ACK times
        // EOT: mSendtobgm equals 0x06
        // NAK: status change
        // Get specific byte, these bytes have special purpose
        // ACK, EOT, NAK
        if (IBgmConstant.BYTEZERO == mSendToBgm)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        BgmSpecificCommandProcess bgmCommandProcess = BgmSecificCommandFactory
                .create(bgCommand);
        if (null != bgmCommandProcess)
        {
            Debug.printI(TAG, "bgmCommdProcess enter");
            Debug.printI("test", "bgmCommdProcess enter");
            bgmCommandProcess.doProcess(mSendToBgm);
        }
        else
        {
            checkBgTestUIAction();
        }
        checkCommandComplete();
        Debug.printI(TAG, "parssingCommandProcess exit");

    }

    /**
     * Check the command has finished.
     * 
     * see mEndACKValue[in]
     * see StatusType.READ_NAK[in]
     * 
     * return void [out] None.
     */

    protected void checkCommandComplete()
    {
        try
        {
            if (BgmUtils.StatusType.READ_NAK.getState())
            {
                throw new BgmException();
            }
            else
            {
                // Empty for static code analysis
            }
            int ackNumValue = BgmUtils.getACKNum();
            Debug.printI(TAG, "[doBgProcess]:ackNumValue " + ackNumValue
                    + "ENDACK_VALUE " + mEndACKValue);
            if (mEndACKValue == ackNumValue)
            {
                finishCommunication();
            }
            else
            {
                // Empty for static code analysis
            }
        }
        catch (BgmException ex)
        {
            Debug.printI(TAG, "checkCommandComplete catch exception");
            Debug.printI(TAG, "exception set  mIsEnable " + mIsReadDone);
            mIsReadDone = true;
            // setCommunicationDoneFlag(true);
            BgmUtils.readConditionOpenState();
            BgmUtils.openBgTestState();
            BgmUtils.openBgmCommandState();
        }
        finally
        {
            // Empty for static code analysis
        }
        Debug.printI("test", "checkCommandComplete--" + toHex(mSendToBgm));
    }

    /**
     * Check byte data is for change bg measurement UI screen.
     * In case the receive data is key that defined in mBgCommandMap and in bgm
     * test flow.
     * Call setBgTestControlFlow().
     * On the contrary, call doBgmCommandProcess().
     * 
     * see StatusType.CR_AND_ACK[in]
     * see mBgmData[in]
     * see mBgCommandMap[in]
     * 
     * return void [out] None.
     */

    protected void checkBgTestUIAction()
    {
        CommonUtils.objectCheck(mBgCommandMap);
        boolean isBgmActoin = false;
        boolean cr_and_ack_state = BgmUtils.StatusType.CR_AND_ACK.getState();
        boolean isBgTestState = (BgmUtils.getIsbGTestCommand() && cr_and_ack_state);
        String bgmCommmand = mBgCommandMap.get(getBgmData());
        boolean isBgUIAction = mBgCommandMap.containsKey(getBgmData());

        isBgmActoin = (isBgUIAction && isBgTestState);
        // is for change UI screen byte
        if (isBgmActoin)
        {
            setBgTestControlFlow(bgmCommmand);
        }
        else
        {
            doBgmCommandProcess();
        }
    }

    /**
     * Check state is in command state or data state
     * Check mBgmData and mSendTobgm whether is the same
     * 
     * see mBgmData[in]
     * see mSendtobgm[in]
     * 
     * return void [out] None.
     */
    protected void doBgmCommandProcess()
    {

        boolean isCommand = false;
        if (IBgmConstant.BYTEZERO == mSendToBgm)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        isCommand = getBgmData() == mSendToBgm;
        // handshaking

        if (isCommand)
        {
            // send and receive same, is command process,
            // write next.
            Debug.printI(TAG, "[doBgmCommandProcess]:isCommand");
            BgmUtils.readConditionOpenState();
        }
        else
        {
            // is read data,
            Debug.printI(TAG, "[doBgmCommandProcess]:isData");
            doBgmDataProcess();
        }
    }

    /**
     * Check is read BGM data write BGM data.
     * In case the receive data is CR and StatusType is write,
     * then start write data to bgm.
     * On the contrary, start read data from bgm
     * 
     * StatusType.CR_AND_ACK[in]
     * StatusType.START_WRITE[in]
     * StatusType.START_RECIEVE[in]
     * 
     * return void [out] None.
     */
    protected void doBgmDataProcess()
    {
        // start read data
        BgmUtils.StatusType CRandAck = BgmUtils.StatusType.CR_AND_ACK;
        BgmUtils.StatusType startWrite = BgmUtils.StatusType.START_WRITE;
        BgmUtils.StatusType startRecieve = BgmUtils.StatusType.START_RECIEVE;

        boolean isWrite = startWrite.getState() && CRandAck.getState();
        boolean isRead = startRecieve.getState() && CRandAck.getState();

        if (isWrite && !isRead)
        {
            // write data to bgm
            Debug.printI(TAG, "[compareBGMData]:Startwrite");
            BgmUtils.readConditionOpenState();
        }
        else if (isRead && !isWrite)
        {
            // read data from bgm
            Debug.printI(TAG, "[compareBGMData]: startreceive");
            collectBgmData();
        }
        else
        {
            // static code
        }
    }

    /**
     * Collect byte data that should be saved and parsed
     * 
     * see mBgmData[in]
     * see mReceiveBgmData[in]
     * 
     * return void [out] None.
     */
    protected final void collectBgmData()
    {
        Debug.printI(TAG, "[collectBGMData]: startreceive");
        mReceiveBgmData.add(getBgmData());

    }

    /**
     * Finish current command communication.
     * Open condition variable that block writeBgmCommand();
     * 
     * return void [out] None.
     */
    protected static void finishCommunication()
    {

        Debug.printI(TAG, "[readBGMDataEnd]: enter " + BgmUtils.getCommandKey());

        BgmCommunication.getInstance().stopReadBgmData();
        mIsReadDone = true;
        Debug.printI(TAG, "finishCommunication mIsEnable " + mIsReadDone);
        BgmUtils.readConditionOpenState();
        BgmUtils.openBgTestState();
        BgmUtils.openBgmCommandState();
        Debug.printI(TAG, "[readBGMDataEnd]: exit ");

    }

    /**
     * Control bG test UI screen according input action.
     * There is a UI listener call back function to control UI screen
     * If misjudgment or flow incorrect, collect mBgmData to mReceiveBGMData
     * 
     * see mBgmData[in]
     * see mReceiveBGMData[in]
     * see mReceiveBGMData[out]
     *
     * return void [out] None.
     * 
     * @param action[in] UI action
     *            Range: not null
     *            Unit: String
     *            Scaling: 1
     */

    protected void setBgTestControlFlow(String action)
    {
        Debug.printI(TAG, "[bGTestControlFlow]: action: " + action);
        // range check
        CommonUtils.objectCheck(action);
        boolean isWaitStrip = action.equals(IBgmConstant.ACTION_WAIT_STRIP);
        boolean isStripInsert = action.equals(IBgmConstant.ACTION_STRIP_INSERT);
        boolean isWaitblood = action.equals(IBgmConstant.ACTION_WAIT_BLOOD);
        boolean isProcessing = action.equals(IBgmConstant.ACTION_bG_PROCESSING);

        if (isWaitStrip)
        {
            Debug.printI(TAG, "[bGTestControlFlow]: isWaitStrip enter ");
            try
            {
                BgmControl.getInstance(null).getOnlistener().onWaitStrip();
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Empty for static code analysis
            }
        }

        // is strip insert action
        else if (isStripInsert)
        {
            Debug.printI(TAG, "[bGTestControlFlow]: isStripinset enter ");
            try
            {
                BgmControl.getInstance(null).getOnlistener().onStripInsert();
                mIsStripInsertAction = true;
            }
            catch (RemoteException e)
            {
                e.printStackTrace();

            }
            finally
            {
                // Empty for static code analysis
            }
        }
        // is wait blood action and strip has inserted
        else if (isWaitblood && mIsStripInsertAction)
        {
            Debug.printI(TAG, "[bGTestControlFlow]: ACTION_WAIT_BLOOD enter ");
            try
            {
                BgmControl.getInstance(null).getOnlistener().onBlood();
                mIsWaitBloodAction = true;
            }
            catch (RemoteException e)
            {
                e.printStackTrace();

            }
            finally
            {
                // Empty for static code analysis
            }

        }
        // is waiting action and blood has dropped
        else if (isProcessing && mIsWaitBloodAction)
        {
            Debug.printI(TAG,
                    "[bGTestControlFlow]: ACTION_bG_PROCESSING enter ");
            try
            {
                BgmControl.getInstance(null).getOnlistener().onProcessing();
                BgmUtils.setIsProcessingAction(true);
                BgmUtils.setIsbGTestCommand(false);
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
                // call EMWR
            }
            finally
            {
                // Empty for static code analysis
            }

        }
        // mistake, is data, save the data.
        else
        {
            collectBgmData();
        }
    }

    // just for log out byte data
    public static String toHex(byte b)
    {
        int shift = 4;
        byte data = 0xf;
        String tohex = "" + "0123456789ABCDEF".charAt(data & b >> shift)
                + "0123456789ABCDEF".charAt(b & data);
        return tohex;
    }

    /**
     * 
     * BgmCommunication constructor.
     *
     * return BgmCommunication [out] BgmCommunication object.
     * Range: valid object of BgmCommunication
     * Unit: BgmCommunication
     * Scaling: 1
     * 
     */
    public static synchronized BgmCommunication getInstance()
    {

        if (null == mInstance)
        {
            mUartData = (UARTManager) ICustomizedHWManager
                    .getSystemService(ICustomizedHWManager.UART_SERVICE);
            CommonUtils.objectCheck(mUartData);
            try
            {
                mUartData.open(UARTPort.BGMUART);
            }
            catch (OperationFailException ex)
            {
                Debug.printI(TAG, ex.getMessage());
            }
            finally
            {
                // Empty for static code analysis
            }
            mInstance = new BgmCommunication();
        }
        else
        {
            // Empty for static code analysis
        }
        return mInstance;
    }

    /**
     * 
     * Return receive data in BgmCommunication
     * see mReceiveData[in]
     * 
     * return ArrayList<Byte> [out] mReceiveData
     * Range: valid object of ArrayList
     * Unit: ArrayList
     * Scaling: 1
     */
    public ArrayList<Byte> getReceiveData()
    {
        ArrayList<Byte> ReceiveBgmData = new ArrayList<Byte>(mReceiveBgmData);

        return ReceiveBgmData;
    }
}

class BgmCommunicationThead extends TimerTask
{

    /**
     * Bgm Communication object
     */
    private BgmCommunication mBgmCommunication = null;

    /**
     * BgmCommunicationThead constructor
     * 
     * see mBgmCommunication[out]
     * 
     * return void [out] None.
     * 
     * @param bgmCommunication [in], BgmCommunication object.
     *            Range: valid object.
     *            Unit: BgmCommunication.
     *            Scaling: 1.
     */
    public BgmCommunicationThead(BgmCommunication bgmCommunication)
    {
        CommonUtils.objectCheck(bgmCommunication);
        mBgmCommunication = bgmCommunication;
    }

    /**
     * Override function, Thread of readBGMData()
     * 
     * return void [out] None.
     */
    @Override
    public void run()
    {
        mBgmCommunication.readBgmData();
    }

}
// (R20570 2015-10-01 10:43:57 DWYang)
// ----------------------------------------------------------------------------
// add updateCodeKey function.
// (R22746 2015-10-28 06:11:38 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
// (R23130 2015-11-03 08:03:45 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
// (R23259 2015-11-04 06:20:40 VictorChen)
// ----------------------------------------------------------------------------
// Fix NSIQ 60
// (R23259 2015-11-04 06:20:40 VictorChen)
// ----------------------------------------------------------------------------
// Fix NSIQ 60 issue.
// (R23452 2015-11-05 07:45:04 VictorChen)
// ----------------------------------------------------------------------------
// Stop read thread when communication finish.
// (R23961 2015-11-12 03:44:53 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
// (R24488 2015-11-20 01:53:37 VictorChen)
// ----------------------------------------------------------------------------
// Fix finishCommunication release lock sequence.
// (R24496 2015-11-20 02:24:34 VictorChen)
// ----------------------------------------------------------------------------
// Fix communication thread timing issue.
// (R24946 2015-11-26 06:46:21 VictorChen)
// ----------------------------------------------------------------------------
// Add update code key command call back.
