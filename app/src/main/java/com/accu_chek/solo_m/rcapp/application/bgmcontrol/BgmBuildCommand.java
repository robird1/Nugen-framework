/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bgmcontrol.
 * BgmBuildCommand
 * Brief:
 *
 * Create Date: 2015¦~8¤ë7¤é
 * $Revision: 24899 $
 * $Author: VictorChen $
 * $Id: BgmBuildCommand.java 24899 2015-11-26 05:40:03Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import java.util.ArrayList;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmBuildCommand.ParseResult;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmCommunicationTimeOutException;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmException;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.RDYDessertException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public abstract class BgmBuildCommand
{
    BgmControl bgmControl = BgmControl.getInstance(null);
    ArrayList<ICommandType> mCommandList = new ArrayList<ICommandType>();
    final String TAG = "BgmBuildCommand";

    abstract protected void buildCommand() throws BgmException,
            RDYDessertException, BgmCommunicationTimeOutException;

    public interface ParseResult
    {
        <T> void setResult(T input);
    }
}

class CheckBgmStatus extends BgmBuildCommand implements ParseResult
{
    /**
     * string insert flag
     */
    private static boolean mIsStripInsert = false;

    /**
     * Build command list for check bgm status and call sendCommand();
     * In case in power on state, send command to get software version, strip
     * connector, set current date and time.
     * 
     * see mCommandList[out]
     *
     * return void [out] None.
     * 
     * @throws BgmCommunicationTimeOutException
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        Debug.printI(TAG, "BuildCommand" + BgmUtils.getIsPowerOnInterrupt());
        boolean isPowerOn = BgmUtils.getIsPowerOnInterrupt();
        mCommandList.clear();
        mCommandList.add(ICommandType.ConnectCommand);
        bgmControl.sendCommand(mCommandList, null);
        mCommandList.clear();
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.ReadMeStatusCommand);
        if (isPowerOn)
        {
            bgmControl.setTimeData();
            bgmControl.setDateData();
            mCommandList.add(ICommandType.AccessCommand);
            mCommandList.add(ICommandType.ReadSoftwareVersionCommand);
            mCommandList.add(ICommandType.ReadInstrumentNameCommand);
            mCommandList.add(ICommandType.ReadStripConnectorCommand);
            mCommandList.add(ICommandType.SetDateCommand);
            mCommandList.add(ICommandType.SetTimeCommand);
        }
        else
        {
            // static code for analysis
        }
        bgmControl.sendCommand(mCommandList, new CheckBgmStatus());
    }

    /**
     * 
     * Call back when ReadMeStatusCommand parse finish.
     * Detects whether a strip is present.
     * 
     * @param input[in], strip insert flag.
     *            Range:true, strip insert
     *            false, strip not insert
     *            Unit: <T>
     *            Scaling: 1
     */

    @Override
    public <T> void setResult(T input)
    {
        boolean result = (Boolean) input;
        setIsStripInsert(result);
        Debug.printI(TAG, "CheckBgmStatus " + mIsStripInsert);

    }

    /**
     * 
     * Set strip insert or remove flag.
     * see mIsStripInsert[in]
     * 
     * @param result[in], strip insert flag.
     *            Range:true or false
     *            Unit: boolean
     *            Scaling: 1
     */
    private void setIsStripInsert(boolean result)
    {
        mIsStripInsert = result;
    }

    /**
     * 
     * Get strip insert or remove flag.
     * see mIsStripInsert[out]
     * 
     * return boolean [out],strip insert flag.
     * Range: true, strip insert
     * false, strip not insert
     * Unit: boolean
     * Scaling: 1
     */
    public static boolean getIsStripInsert()
    {
        return mIsStripInsert;
    }

}

class PowerDownBgm extends BgmBuildCommand
{

    /**
     * Build command list for power down bgm and call sendCommand();
     * see mCommandList[out]
     *
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        Debug.printI(TAG, "PowerDownBgm");
        mCommandList.clear();
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.PowerDownCommand);
        bgmControl.sendCommand(mCommandList, null);
    }

}

class BgMeasurement extends BgmBuildCommand implements ParseResult
{
    /**
     * Flag for Bg result or Cg result
     */
    private static boolean mIsCgResult = false;
    /**
     * Control solution test value
     */
    private static SafetyChannel<Integer> mSfCgValue = null;

    // private static int mCgResult = 0;

    /**
     * Build command list, set bgm date time and bg test;
     * see mCommandList[out]
     * 
     *
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        mCommandList.clear();
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.AccessCommand);
        mCommandList.add(ICommandType.SetDateCommand);
        mCommandList.add(ICommandType.SetTimeCommand);
        mCommandList.add(ICommandType.BgTestCommand);
        bgmControl.sendCommand(mCommandList, new BgMeasurement());
    }

    /**
     * Set call back result
     * 
     * return void [out] None.
     * 
     * @param input[in],object list contain flag whether is cG and cG value
     *            Range: valid object ArrayList.
     *            Unit: T
     *            Scaling: 1
     */
    @SuppressWarnings("unchecked")
    @Override
    public <T> void setResult(T input)
    {
        int CgFlagIndex = 0;
        int CgValueIndex = 1;
        ArrayList<Object> list = (ArrayList<Object>) input;
        CommonUtils.objectCheck(list);
        boolean result = (Boolean) list.get(CgFlagIndex);

        setIsCgResult(result);
        if (result)
        {
            mSfCgValue = (SafetyChannel<Integer>) list.get(CgValueIndex);
            setCgValue(mSfCgValue);
            int cgch1 = ((SafetyChannel<Integer>) mSfCgValue).getValueCH1();
            int cgch2 = ((SafetyChannel<Integer>) mSfCgValue).getValueCH2();
            ((SafetyChannel<Integer>) mSfCgValue).set(cgch1, cgch2);
            CommonUtils.getOriginValue(cgch1, cgch2);

        }

    }

    /**
     * 
     * Set test result whether is cg value.
     * see mIsCgResult[out]
     * return void [out] None.
     * 
     * @param result[in], cg result flag.
     *            Range: true, cg result
     *            false, not cg result.
     *            Unit: boolean
     *            Scaling: 1
     */
    private void setIsCgResult(boolean result)
    {
        mIsCgResult = result;
    }

    /**
     * 
     * Get test result whether is cg value.
     * see mIsCgResult[in]
     * return boolean [out] cg result flag.
     * Range: true, cg result
     * false, not cg result.
     * Unit: boolean
     * Scaling: 1
     * 
     */
    public static boolean getIsCgResult()
    {
        return mIsCgResult;
    }

    /**
     * 
     * Set cg value.
     * see mSfCgValue[out]
     * return void [out] None.
     * 
     * @param CgValue[in] cg test value
     *            Range: valid object of SafetyChannel
     *            Unit: SafetyChannel
     *            Scaling: 1
     */

    private static void setCgValue(SafetyChannel<Integer> CgValue)
    {
        mSfCgValue = CgValue;
    }

    /**
     * 
     * Get cg value.
     * see mSfCgValue[in]
     * return void [out] None.
     * 
     * return CgValue[in] cg test value
     * Range: valid object of SafetyChannel
     * Unit: SafetyChannel
     * Scaling: 1
     */
    public static SafetyChannel<Integer> getCgValue()
    {
        return mSfCgValue;
    }

}

class CheckCodeKey extends BgmBuildCommand
{
    /**
     * 
     * Build command list, set check code key data;
     * see mCommandlist[out]
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        mCommandList.clear();
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.ReadEventINTCommand);
        mCommandList.add(ICommandType.CheckCodeKeyCommand);
        mCommandList.add(ICommandType.PowerDownCommand);
        bgmControl.sendCommand(mCommandList, null);
    }
}

class GetStripCounter extends BgmBuildCommand
{
    /**
     * 
     * Build command list, get strip counter.
     * see mCommandlist[out]
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        mCommandList.clear();
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.ReadEventINTCommand);
        mCommandList.add(ICommandType.GetStripCounterCommand);
        mCommandList.add(ICommandType.PowerDownCommand);
        bgmControl.sendCommand(mCommandList, null);
    }
}

class SetBgmDateTime extends BgmBuildCommand
{
    /**
     * 
     * Build command list, set bgm date and time.
     * see mCommandlist[out]
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        Debug.printI(TAG, "SetBgmDateTime");
        bgmControl.setTimeData();
        bgmControl.setDateData();
        mCommandList.clear();
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.ReadEventINTCommand);
        mCommandList.add(ICommandType.AccessCommand);
        mCommandList.add(ICommandType.SetDateCommand);
        mCommandList.add(ICommandType.SetTimeCommand);
        mCommandList.add(ICommandType.PowerDownCommand);

        bgmControl.sendCommand(mCommandList, null);
    }

}

class ComposeCgCommand extends BgmBuildCommand implements ParseResult
{
    /**
     * Flag for control solution in the range or out of range.
     */
    private static boolean mIsLevel1InRange = false;

    /**
     * 
     * Use test result to compose Check_cG_L1_Range_Command and
     * Check_cG_L2_Range_Command and call sendCommnad().
     *
     * see mCommandlist[out]
     * 
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        Debug.printI(TAG, "composeCgCommand++");
        SafetyChannel<Integer> safeCgvalue = BgMeasurement.getCgValue();
        byte[] cG_L1_Command = ICommandType.CheckCgL1RangeCommand.getCommand();
        byte[] cG_L2_Command = ICommandType.CheckCgL2RangeCommand.getCommand();
        int cgch1 = safeCgvalue.getValueCH1();
        int cgch2 = safeCgvalue.getValueCH2();
        safeCgvalue.set(cgch1, cgch2);
        // get cg value
        int cgValue = CommonUtils.getOriginValue(cgch1, cgch2);
        byte[] byte_cG_value = String.valueOf(cgValue).getBytes();
        int i = 0;
        int commandStartPostition = 4;
        int length = byte_cG_value.length;

        for (i = 0; i < length; i++)
        {
            cG_L1_Command[commandStartPostition] = byte_cG_value[i];
            cG_L2_Command[commandStartPostition] = byte_cG_value[i];
            commandStartPostition++;
        }

        ICommandType.CheckCgL1RangeCommand.setCommand(cG_L1_Command);
        ICommandType.CheckCgL2RangeCommand.setCommand(cG_L2_Command);
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.CheckCgL1RangeCommand);
        mCommandList.add(ICommandType.CheckCgL2RangeCommand);
        bgmControl.sendCommand(mCommandList, new ComposeCgCommand());
    }

    /**
     * Get control solution1 result flag.
     * see mIsLevel1InRange [out]
     * 
     * return boolean [out] Get control solution 1 result
     * Range: true, in the range
     * false, not in the range
     * Unit: boolean
     * Scaling: 1
     */
    public static boolean getLevel1Resut()
    {
        return mIsLevel1InRange;
    }

    /**
     * 
     * Set control solution1 result flag.
     * see mIsLevel1InRange [in]
     * return void [out] None.
     * 
     * @param result [in] Get control solution 1 result
     *            Range: true, in the range
     *            false, not in the range
     *            Unit: boolean
     *            Scaling: 1
     * 
     */
    public static void setLevel1Resut(boolean result)
    {
        mIsLevel1InRange = result;
    }

    /**
     * Get control solution1 result that whether in the range.
     * 
     * see mIsLevel1InRange [in]
     * return void [out] None.
     * 
     * @param input[in] Get control solution 1 result
     *            Range: valid object
     *            Unit: T
     *            Scaling: 1
     */
    @Override
    public <T> void setResult(T input)
    {
        setLevel1Resut((Boolean) input);
        Debug.printI(TAG, "isLevel1InRange " + mIsLevel1InRange);
    }

}

class HandleError extends BgmBuildCommand
{
    /**
     * 
     * Build command list,get error code.
     * see mCommandlist[out]
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        Debug.printI(TAG, "HandleError");
        mCommandList.clear();
        mCommandList.clear();
        mCommandList.add(ICommandType.ReadErrorStatusCommand);
        mCommandList.add(ICommandType.ReadEventINTCommand);
        mCommandList.add(ICommandType.PowerDownCommand);
        bgmControl.sendCommand(mCommandList, null);
    }

}

class GetBgmDateTime extends BgmBuildCommand
{
    /**
     * 
     * Build command list,get bgm date and time.
     * see mCommandlist[out]
     * return void [out] None.
     */
    @Override
    protected void buildCommand() throws BgmException, RDYDessertException,
            BgmCommunicationTimeOutException
    {
        Debug.printI(TAG, "GetBgmDateTime");
        mCommandList.clear();
        mCommandList.add(ICommandType.ClearStatusCommand);
        mCommandList.add(ICommandType.ReadEventINTCommand);
        mCommandList.add(ICommandType.AccessCommand);
        mCommandList.add(ICommandType.ReadDateCommand);
        mCommandList.add(ICommandType.ReadTimeCommand);
        mCommandList.add(ICommandType.PowerDownCommand);
        bgmControl.sendCommand(mCommandList, null);
    }
}

class BuildCommandFactory
{
    /**
     * 
     * Get command list class path.
     *
     * return BgmBuildCommand [out] command list class path
     * Range: null or valid object of BgmBuildCommand.
     * Unit: BgmBuildCommand
     * Scaling: 1
     * 
     * @param name[in] command list class name
     *            Range: valid object of string.
     *            Unit: String
     *            Scaling: 1
     */
    public static BgmBuildCommand create(String name)
    {
        BgmBuildCommand buildpath = null;
        String className = BuildCommandFactory.class.getPackage().getName()
                + "." + name;
        try
        {
            Class<?> c = Class.forName(className);
            buildpath = (BgmBuildCommand) c.newInstance();
        }
        catch (ClassNotFoundException e)
        {

            e.printStackTrace();
        }
        catch (InstantiationException e)
        {

            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }
        return buildpath;

    }
}// (R20520 2015-10-01 07:04:11 DWYang)
// ----------------------------------------------------------------------------
// add set date time at power on.
// (R21502 2015-10-14 02:36:18 VictorChen)
// ----------------------------------------------------------------------------
// add instrument name and strip Connector for RPC.
// (R22675 2015-10-27 22:25:56 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
// (R23259 2015-11-04 06:20:40 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
// (R24486 2015-11-20 01:48:56 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
