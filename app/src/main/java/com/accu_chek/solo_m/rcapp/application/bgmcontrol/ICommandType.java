/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ICommandType
 * Brief:
 *
 * Create Date: 2015/2/5
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: ICommandType.java 24486 2015-11-20 05:48:56Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import java.util.Arrays;

public enum ICommandType
{

    /**
     * Clear bmg status.
     */
    ClearStatusCommand("ClearStatusCommand",
        new byte[] { 0x0b, IBgmConstant.CR }, null),
    /**
     * Read error code
     */
    ReadErrorStatusCommand("ReadErrorStatusCommand", new byte[] { 0x0b,
            IBgmConstant.CR }, null),
    /**
     * Do bg measurement
     */
    BgTestCommand("BgTestCommand", new byte[] { 'G', IBgmConstant.TAB, '1',
            IBgmConstant.CR }, null),
    /**
     * Power down bgm submodule
     */
    PowerDownCommand("PowerDownCommand", new byte[] { 0x1d, IBgmConstant.CR },
        null),
    /**
     * Bgm password command
     */
    AccessCommand("AccessCommand", new byte[] { 'X', IBgmConstant.TAB, 'A',
            '5', 'A', '5', IBgmConstant.CR }, null),
    /**
     * Bgm is ready for a serial command
     */
    ConnectCommand("ConnectCommand", new byte[] { 0x18 }, null),
    /**
     * Cancel command
     */
    InterruptCommand("InterruptCommand", new byte[] { 0x18 }, null),
    /**
     * Set bgm date
     */
    SetDateCommand("SetDateCommand", new byte[] { 0x0c, IBgmConstant.TAB, '1',
            IBgmConstant.CR }, null),
    /**
     * Set bgm time
     */
    SetTimeCommand("SetTimeCommand", new byte[] { 0x0c, IBgmConstant.TAB, '2',
            IBgmConstant.CR }, null),
    /**
     * Read bgm status
     */
    ReadMeStatusCommand("ReadMeStatusCommand", new byte[] { 0x65,
            IBgmConstant.CR }, null),
    /**
     * De-assert Event INT pin
     */
    ReadEventINTCommand("ReadEventINTCommand", new byte[] { 0x65,
            IBgmConstant.CR }, null),
    /**
     * Read bgm date
     */
    ReadDateCommand("ReadDateCommand", new byte[] { 'S', IBgmConstant.TAB, '1',
            IBgmConstant.CR }, null),
    /**
     * Read bgm time
     */
    ReadTimeCommand("ReadTimeCommand", new byte[] { 'S', IBgmConstant.TAB, '2',
            IBgmConstant.CR }, null),
    /**
     * Read software version
     */
    ReadSoftwareVersionCommand("ReadSoftwareVersionCommand", new byte[] { 'C',
            IBgmConstant.TAB, '1', IBgmConstant.CR }, null),
    /**
     * Read model name
     */
    ReadModelNameCommand("ReadModelNameCommand", new byte[] { 'X',
            IBgmConstant.TAB, 'A', '5', 'A', '5', IBgmConstant.CR }, null),
    /**
     * Read instrument name
     */
    ReadInstrumentNameCommand("ReadInstrumentNameCommand", new byte[] { 'I',
            IBgmConstant.CR }, null),
    /**
     * Read strip counter
     */
    GetStripCounterCommand("GetStripCounterCommand", new byte[] { 0x23,
            IBgmConstant.CR }, null),
    /**
     * down load bgm codekey
     */
    UpdateCodeKeyCommand("UpdateCodeKeyCommand", new byte[] { 'W',
            IBgmConstant.TAB, 'K', IBgmConstant.TAB, '0', IBgmConstant.TAB,
            '2', '0', '0', IBgmConstant.CR }, null),
    /**
     * Check bgm codekey value
     */
    CheckCodeKeyCommand("CheckCodeKeyCommand", new byte[] { 'x',
            IBgmConstant.TAB, '1', IBgmConstant.CR }, null),
    /**
     * Read strip connector type
     */
    ReadStripConnectorCommand("ReadStripConnectorCommand", new byte[] { 'N',
            IBgmConstant.TAB, 'C', IBgmConstant.CR }, null),
    /**
     * Check control solution 1 range
     */
    CheckCgL1RangeCommand("CheckcGL1RangeCommand", new byte[] { 'x',
            IBgmConstant.TAB, '3', IBgmConstant.TAB, '0', '0', '0',
            IBgmConstant.TAB, '1', IBgmConstant.CR }, null)
    /**
     * Check control solution 2 range
     */
    ,
    CheckCgL2RangeCommand("CheckcGL2RangeCommand", new byte[] { 'x',
            IBgmConstant.TAB, '3', IBgmConstant.TAB, '0', '0', '0',
            IBgmConstant.TAB, '2', IBgmConstant.CR }, null)

    ;
    /**
     * command name
     */
    private String mKey = null;
    /**
     * command value
     */
    private byte[] mCommand = null;
    /**
     * command data
     */
    private byte[] mData = null;

    /**
     * ICommandType constructor
     * see mKey[out]
     * see mCommand[out]
     * see mData[out]
     * return void [out] None.
     * 
     * @param key [in] command name
     *            Range: not null
     *            Unit: String
     *            Scaling: 1
     * @param command [in] command byte value
     *            Range: not null
     *            Unit: byte array
     *            Scaling: 1
     * @param data [in] data would write to BGM
     *            Range: not null
     *            Unit: byte array
     *            Scaling: 1
     */

    private ICommandType(String key, byte[] command, byte[] data)
    {
        mKey = key;
        mCommand = Arrays.copyOf(command, command.length);
        if (data != null)
        {
            mData = Arrays.copyOf(data, data.length);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Get command key
     * 
     * 
     * return String [out] command name
     *         Range: not null
     *         Unit: String
     *         Scaling: 1
     */
    public String getCommandKey()
    {
        return mKey;
    }

    /**
     * 
     * Set command contents.
     * 
     * 
     * @param command
     *            Range: not null
     *            Unit: byte array
     *            Scaling: 1
     * 
     * 
     */
    public void setCommand(byte[] command)
    {
        mCommand = Arrays.copyOf(command, command.length);
    }

    /**
     * Get command contents.
     * 
     * 
     * return byte[] [out] command prototype
     *         Range: not null
     *         Unit: byte array
     *         Scaling: 1
     */
    public byte[] getCommand()
    {

        byte[] command = Arrays.copyOf(mCommand, mCommand.length);
        return command;
    }

    /**
     * Set write data of Bgm command
     * return void [out] None.
     * 
     * @param data [in] set mData that would write to BGM
     *            Range: not null
     *            Unit: byte array
     *            Scaling: 1
     */
    public void setByteData(byte[] data)
    {
        if (data != null)
        {
            mData = Arrays.copyOf(data, data.length);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Get write data of BGM command
     * 
     * return byte[] [out] get byte data to write to BGM
     *         Range: not null
     *         Unit: byte array
     *         Scaling: 1
     */
    public byte[] getByteData()
    {
        byte[] data = null;
        if (mData != null)
        {
            data = Arrays.copyOf(mData, mData.length);
        }
        else
        {
            // Apply to the coding standard
        }
        return data;
    }
}
// add header footer.
// add updateCodeKey function.
// Refine code and comment.
// (R23923 2015-11-12 01:29:51 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
