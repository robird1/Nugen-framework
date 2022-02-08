/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmSecificCommandFactory
 * Brief:
 *
 * Create Date: 6/17/2015
 * $Revision: 23130 $
 * $Author: VictorChen $
 * $Id: BgmSpecificCommandProcess.java 23130 2015-11-03 12:03:45Z VictorChen $
 */
package com.accu_chek.solo_m.rcapp.communication.bgmcommunication;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmUtils;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBgmConstant;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.ICommandType;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager.UARTPort;

public abstract class BgmSpecificCommandProcess
{

    protected static final String TAG = "BgmSpecificCommandProcess";

    /**
     * do process of specific byte data.
     * 
     * @param sendtobgm the data byte that are written to BGM subsystem
     *            Range: larger than 0x00
     *            Unit: byte
     *            Scaling: 1
     */
    abstract void doProcess(byte sendtobgm);

}

class EOT extends BgmSpecificCommandProcess
{
    /**
     * Receive EOT, send ACK to BGM subsystem
     *
     * see StatusType.START_RECIEVE[out]
     * 
     * @param sendtobgm the data byte that are written to BGM subsystem
     *            Range: larger than 0x00
     *            Unit: byte
     *            Scaling: 1
     */
    @Override
    public void doProcess(byte sendtobgm)
    {
        SafetyByteArray data = null;
        Debug.printI(TAG, "[doProcess]:EOT enter");
        UARTManager uartdata;
        byte[] sendcommand = new byte[1];
        sendcommand[0] = IBgmConstant.ACK;
        data = new SafetyByteArray(sendcommand,
                CRCTool.generateCRC16(sendcommand));
        try
        {
            uartdata = (UARTManager) ICustomizedHWManager
                    .getSystemService(ICustomizedHWManager.UART_SERVICE);
            CommonUtils.objectCheck(uartdata);
            uartdata.send(UARTPort.BGMUART, data);
            Debug.dumpPayload("W", (byte) IBgmConstant.ACK);
            BgmUtils.StatusType.START_RECIEVE.setState(false);
        }
        catch (OperationFailException e)
        {
            // TODO Auto-generated catch block
            // call EMWR
            e.printStackTrace();
        }
        finally
        {
            // Empty for static code analysis
        }

    }
}

class NAK extends BgmSpecificCommandProcess
{

    /**
     * Receive NAK, call BGM control do error handling
     * see StatusType.START_RECIEVE[out]
     * 
     * @param sendtobgm the data byte that are written to BGM subsystem
     *            Range: larger than 0x00
     *            Unit: byte
     *            Scaling: 1
     */
    @Override
    public void doProcess(byte sendtobgm)
    {

        Debug.printI(TAG, "[doProcess]: NAK enter");
        String commandKey = BgmUtils.getCommandKey();
        final int readEnd = 2;
        Debug.printI(TAG, "[doProcess]:  NAK enter" + commandKey);
        boolean isConnectCommand = commandKey
                .equals(ICommandType.ConnectCommand.getCommandKey());
        boolean isInterruptCommand = commandKey
                .equals(ICommandType.InterruptCommand.getCommandKey());
        Debug.printI(TAG, "[doProcess]: isConnectCommand: " + isConnectCommand);
        Debug.printI(TAG, "[doProcess]: isInterruptCommand: "
                + isInterruptCommand);

        if (true == isConnectCommand)
        {
            /**
             * is connect command, it return NAK.
             * disguise the ack times is 2 to exit bgmcommunication
             */
            BgmUtils.setACKNum(readEnd);

        }
        else if (isInterruptCommand)
        {
            /**
             * is interrupt command, it return NAK.
             * disguise the ack times is 2 to exit bgmcommunication
             */

            BgmUtils.StatusType.READ_NAK.setState(true);

        }
        else
        {
            Debug.printI(TAG, "[doProcess]: set NAK enter");
            BgmUtils.StatusType.READ_NAK.setState(true);
            BgmUtils.readConditionOpenState();
        }

        Debug.printI(TAG, "[doProcess]: NAK exit");

    }

}

class ACK extends BgmSpecificCommandProcess
{
    /**
     * Receive ACK, increase ACK times
     *
     * see StatusType.CR_AND_ACK[in]
     * 
     * @param sendtobgm the data byte that are written to BGM subsystem
     *            Range: larger than 0x00
     *            Unit: byte
     *            Scaling: 1
     * 
     * 
     */
    @Override
    public void doProcess(byte sendtobgm)
    {
        Debug.printI(TAG, "[doProcess]: ACK enter sendtobgm: " + sendtobgm);
        if (IBgmConstant.BYTEZERO >= sendtobgm)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        Debug.printI(TAG, "[doProcess]: ACK enter");
        BgmUtils.incACKNum();
        boolean isinputCR = false;

        isinputCR = sendtobgm == IBgmConstant.CR;
        if (isinputCR)
        {
            BgmUtils.StatusType.CR_AND_ACK.setState(true);
            BgmUtils.readConditionOpenState();
            Debug.printI(TAG, "[doProcess]:" + BgmUtils.getACKNum());
        }
        else
        {
            // Empty for static code analysis
        }
    }
}

class BgmSecificCommandFactory
{
    private static final String TAG = "BgmCommandFactory";

    /**
     * Get full class name of specific byte data.
     * class EOT, class ACK, class NAK.
     * exception Exception, the input classname is not specific data.
     * 
     * @param classname specific byte data
     *            Range: valid object
     *            Unit: String
     *            Scaling:1
     * @return full class path
     *         Range: null, ACK class, EOT class, NAK class.
     *         Unit: BgmSpecificCommandProcess
     *         Scaling:1
     * 
     */

    public static BgmSpecificCommandProcess create(String classname)
    {
        BgmSpecificCommandProcess bgmInstance = null;

        try
        {
            if (null != classname)
            {
                String fullClassName = BgmSpecificCommandProcess.class
                        .getPackage().getName() + "." + classname;
                Debug.printI(TAG, "[create]: enter");
                Class<?> c = Class.forName(fullClassName);
                bgmInstance = (BgmSpecificCommandProcess) c.newInstance();
            }
        }
        catch (Exception e)
        {
            bgmInstance = null;
        }
        finally
        {
            // do nothing
        }
        return bgmInstance;
    }
}
// add updateCodeKey function.
// (R22746 2015-10-28 06:11:38 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
