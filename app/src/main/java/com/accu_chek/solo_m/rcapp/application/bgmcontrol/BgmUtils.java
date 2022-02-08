/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.BgmUtils
 * Brief:
 *
 * Create Date: 2015/2/5
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: BgmUtils.java 24486 2015-11-20 05:48:56Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;

import org.json.JSONException;
import org.json.JSONObject;

import android.content.Context;
import android.os.BatteryManager;
import android.os.ConditionVariable;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmCRCDataErrorException;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.BGMConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.usbconnection.UsbConnectReceiver;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.bgmcommunication.BgmCommunication;

public class BgmUtils
{
    private static String TAG = "BgmUtils";

    /**
     * ConditionVariable of sendCommand()
     * Range:not null
     * Unit: ConditionVariable
     * Scaling: 1
     */
    private static ConditionVariable mBgmTestCoditionVariable = null;
    /**
     * ConditionVariable of writeBgmCommand()
     * Range:not null
     * Unit: ConditionVariable
     * Scaling: 1
     */
    private static ConditionVariable mReadConditionVariable = null;
    /**
     * ConditionVariable of writeBgmCommand() for bg measurement
     * Range:not null
     * Unit: ConditionVariable
     * Scaling: 1
     */
    private static ConditionVariable mBgTestConditionVariable = null;

    /**
     * Record current command in Bgm communication
     * Range:not null
     * Unit: String
     * Scaling: 1
     */
    private static String mCommandKey = null;
    /**
     * Other UI flow flag.
     */
    private static boolean mOtherWorkFlow = false;
    /**
     * Power on bgm interrupt flag
     * Range: true or false
     * Unit: boolean
     * Scaling: 1
     */
    private static boolean mIsPowerOnInterrupt = false;
    /**
     * Wake up bgm interrupt interrupt
     * Range: true or false
     * Unit: boolean
     * Scaling: 1
     */
    private static boolean mIsWakeUpInterrupt = false;
    /**
     * Bg test in porcessing flag
     * Range: true or false
     * Unit: boolean
     * Scaling: 1
     */
    private static boolean misProcessingAction = false;

    /**
     * Bgm Control context
     * Range: not null
     * Unit: Context
     * Scaling: 1
     */
    private static Context mContext = null;
    /**
     * MDI mode enalbe flag
     */
    private static boolean mIsEnableMDIMode = false;
    /**
     * Bg test flow flag
     */
    private static boolean mIsBgFlow = false;
    /**
     * Bg test command flag
     */
    private static boolean mIsBgCommand = false;

    /**
     * ACK times.
     * Range: 0~3
     * Unit: int
     * Scaling: 1
     */
    private static int mACKNum = 0;

    static
    {
        mBgmTestCoditionVariable = new ConditionVariable();
        mReadConditionVariable = new ConditionVariable();
        mBgTestConditionVariable = new ConditionVariable();

    }

    /**
     * BGMUtils constructor
     *
     * return void [out] None.
     */

    private BgmUtils()
    {
        // can not instance.
    }

    /**
     * ConditionVariable block of writeBgmCommand()
     * 
     * return boolean [out] is ConditionVarialbe open.
     * Range: true, ConditionVariable open
     * false, ConditionVariable not open
     * Unit: boolean
     * Scaling: 1
     * 
     * see mReadConditionVariable[in]
     * 
     * return void [out] None.
     * 
     * @param time[in] delay time of mReadConditionVariable,
     *            Range:0~2^63-1
     *            Unit: long
     *            Scaling: 1
     */

    public static boolean readConditionBlockState(long time)
    {
        boolean isOpen = false;
        if (time < 0)
        {
            throw new DataIntegrityException();
        }
        else
        {
            mReadConditionVariable.close();
            isOpen = mReadConditionVariable.block(time);
        }
        return isOpen;
    }

    /**
     * ConditionVariable open of writeBgmCommand()
     * see mReadConditionVariable[in]
     * 
     * return void [out] None.
     * 
     */
    public static void readConditionOpenState()
    {
        mReadConditionVariable.open();
    }

    /**
     * 
     * ConditionVariable block of sendCommand()
     * 
     * see mBgmTestCoditionVariable[in]
     * 
     * return boolean [out] is ConditionVarialbe open.
     * Range: true, ConditionVariable open
     * false, ConditionVariable not open
     * Unit: boolean
     * Scaling: 1
     * 
     * @param time, delay time of mBgmTestCoditionVariable,
     *            Range:0~2^63-1
     *            Unit: int
     *            Scaling: 1
     * 
     */
    public static boolean blockBgmCommandState(long time)
    {
        boolean isOpen = true;
        if (time < 0)
        {
            throw new DataIntegrityException();
        }
        else
        {
            mBgmTestCoditionVariable.close();
            isOpen = mBgmTestCoditionVariable.block(time);
        }
        return isOpen;
    }

    /**
     * ConditionVariable open of writeBgmCommand()
     * see mBgmTestCoditionVariable[in]
     * return void [out] None.
     */
    public static void openBgmCommandState()
    {
        mBgmTestCoditionVariable.open();
    }

    /**
     * 
     * ConditionVariable block of writeBgmCommand() for bg test.
     * 
     * return boolean [out] is ConditionVarialbe open.
     * Range: true, ConditionVariable open
     * false, ConditionVariable not open
     * Unit: boolean
     * Scaling: 1
     * 
     * @param time [in] delay time of mBgTestConditionVariable,
     *            Range:0~2^63-1
     *            Unit: int
     *            Scaling: 1
     */
    public static boolean blockBgTestState(long time)
    {
        boolean isTimeOut = true;
        if (time < 0)
        {
            throw new DataIntegrityException();
        }
        else
        {
            mBgTestConditionVariable.close();
            isTimeOut = mBgTestConditionVariable.block(time);
        }
        return isTimeOut;
    }

    /**
     * ConditionVariable open of send command to writeBGMCommand
     * see mBgTestConditionVariable[in]
     * return void [out] None.
     */
    public static void openBgTestState()
    {
        mBgTestConditionVariable.open();
    }

    /**
     * Set current command key that sent to BGM subsystem
     * see mCommandKey[out]
     * return void [out] None.
     * 
     * @param key [in] command name
     *            Range: not null
     *            Unit: String
     *            Scaling: 1
     */
    public static void setCommandKey(String key)
    {
        CommonUtils.objectCheck(key);
        mCommandKey = key;

    }

    /**
     * Get current command key that sent to BGM subsystem
     * 
     * see mCommandKey[in]
     * return [out]command name
     * Range: not null
     * Unit: String
     * Scaling: 1
     */
    public static String getCommandKey()
    {
        CommonUtils.objectCheck(mCommandKey);
        return mCommandKey;
    }

    /**
     * 
     * Set other work flow abort confirm flag.
     * see mOtherWorkFlow[out]
     * return void [out] None.
     * 
     * @param flag[in] other work flow confirm flag.
     *            Range: true, in other work UI flow
     *            flag, not in bg test UI flow
     *            Unit: String
     *            Scaling: 1
     * 
     */
    public static void setWorkFlowConfirm(boolean flag)
    {
        mOtherWorkFlow = flag;

    }

    /**
     * 
     * Get other work flow abort confirm flag.
     * 
     * see mOtherWorkFlow[in]
     * 
     * 
     */
    public static boolean getWorkFlowConfirm()
    {
        return mOtherWorkFlow;
    }

    /**
     * Increase communication ACK times
     * see mACKNum[out]
     *
     */
    public static void incACKNum()
    {
        mACKNum++;
    }

    /**
     * 
     * Set communication ACK times
     * see mACKNum[out]
     * return void [out] None.
     * 
     * @param times[in] communication ACK counter.
     *            Range: larger than 0
     *            Unit: int
     *            Scaling: 1
     * 
     */
    public static void setACKNum(int times)
    {
        if (times < 0)
        {
            throw new DataIntegrityException();
        }
        else
        {
            mACKNum = times;
        }
    }

    /**
     * 
     * Get communication ACK times
     *
     * see mACKNum[in]
     */
    public static int getACKNum()
    {
        return mACKNum;
    }

    /**
     * 
     * Check is in bg test flow command flag.
     *
     * see mIsBgFlow [in]
     */
    public static boolean getIsbGTestFlow()
    {
        return mIsBgFlow;
    }

    /**
     * 
     * Set bg test command flow flag
     * see mIsBgFlow [out]
     * return void [out] None.
     * 
     * @param IsbGTestFlow [in] bg test command flow flag
     *            Range:true, bg test command flow flag
     *            false, not bg test command flow flag
     *            Unit: boolean
     *            Scaling: 1
     */
    public static void setIsbGTestFlow(boolean IsbGTestFlow)
    {
        mIsBgFlow = IsbGTestFlow;
    }

    /**
     * 
     * Check is in bg test command
     *
     * See mIsBgCommand[in]
     */
    public static boolean getIsbGTestCommand()
    {
        return mIsBgCommand;
    }

    /**
     * 
     * Set is in bg test command
     * see mIsBgCommand[out]
     * return void [out] None.
     * 
     * @param IsbGTestCommand [in] bg test command flag
     *            Range:true or false
     *            Unit: boolean
     *            Scaling: 1
     */
    public static void setIsbGTestCommand(boolean IsbGTestCommand)
    {
        mIsBgCommand = IsbGTestCommand;
    }

    /**
     * 
     * Get bgm power on flag.
     *
     * see mIsPowerOnInterrupt[in]
     */
    public static boolean getIsPowerOnInterrupt()
    {
        return mIsPowerOnInterrupt;
    }

    /**
     * 
     * Set bgm power on flag.
     * see mIsPowerOnInterrupt[out]
     * return void [out] None.
     * 
     * @param isPoweron [in] power on flag
     *            Range:true or false
     *            Unit: boolean
     *            Scaling: 1
     */
    public static void setIsPowerOnInterrupt(boolean isPoweron)
    {
        mIsPowerOnInterrupt = isPoweron;
    }

    /**
     * 
     * Get bgm wake up flag.
     * see mIsWakeUpInterrupt[in]
     */
    public static boolean getIsWakeUpInterrupt()
    {
        return mIsWakeUpInterrupt;
    }

    /**
     * 
     * Set bgm wake up flag.
     * 
     * see mIsWakeUpInterrupt[out]
     * return void [out] None.
     * 
     * @param isWakeMethod [in] wake up flag
     *            Range:true or false
     *            Unit: boolean
     *            Scaling: 1
     */
    public static void setIsWakeUpInterrupt(boolean isWakeMethod)
    {
        mIsWakeUpInterrupt = isWakeMethod;
    }

    /**
     * Get bg test flow whether is in processing action.
     * See misProcessingAction[in]
     */
    public static boolean getIsProcessingAction()
    {
        return misProcessingAction;
    }

    /**
     * Set processing action flag in bg test flow .
     * see misProcessingAction[out]
     * return void [out] None.
     * 
     */
    public static void setIsProcessingAction(boolean isProcessingAction)
    {
        misProcessingAction = isProcessingAction;
    }

    /**
     * Get the meter mdi mode flag .
     * see mIsEnableMDIMode[in]
     */
    public static boolean getIsMDIMode()
    {
        return mIsEnableMDIMode;
    }

    /**
     * Set the meter mdi mode flag .
     * see mIsEnableMDIMode[out]
     * return void [out] None.
     */
    public static void setIsMDIMode(boolean enable)
    {
        mIsEnableMDIMode = enable;
    }

    /**
     * Get usb plug in status.
     * When USB state is plug in and connect and meter in normal mode, set flag
     * true.
     *
     * return isPlugIn [out] USB plug in flag.
     * Range: true, USB plug in
     * false, USB not plug in
     * Unit: boolean
     * Scaling: 1
     */
    public static boolean getIsUsbPlugIn()

    {
        UsbConnectReceiver usbState = new UsbConnectReceiver();
        boolean configState = usbState.getUsbConfigureState(BgmUtils
                .getContext()) == SafetyBoolean.TRUE;
        boolean connectState = usbState.getUsbConnectState(BgmUtils
                .getContext()) == SafetyBoolean.TRUE;
        boolean isInProductionMode = NugenSettingModel.getSafetyBoolean(
                BgmUtils.getContext(), ProductionConstants.KEY_INPRODUCTION) == SafetyBoolean.TRUE;
        // USB connect and config and meter not in production mode.
        boolean isPlugIn = configState && connectState
                && (false == isInProductionMode);
        return isPlugIn;
    }

    /**
     * Get Context.
     * 
     * See mContext[in]
     */
    public static Context getContext()
    {
        CommonUtils.objectCheck(mContext);
        return mContext;
    }

    /**
     * 
     * Set Context.
     * see mContext[out]
     * return void [out] None.
     * 
     * @param context [in] Android context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     */
    public static void setContext(Context context)
    {
        CommonUtils.objectCheck(context);
        mContext = context;
    }

    /**
     * 
     * Get current flow from share preference.
     * 
     * return isInOtherWorkFlow [out] in other UI flow flag.
     * Range: true, in other UI flow
     * false, in bg test UI flow
     * Unit: boolean
     * Scaling: 1
     * 
     * @param context[in] Android context.
     *            Range:valid object
     *            Unit: context
     *            Scaling: 1
     */
    public static boolean getCurrentWorkFlow(Context context)
    {
        CommonUtils.objectCheck(context);
        // get the flow from share preference
        SafetyBoolean safetyInOtherWorkFlow = NugenGeneralModel
                .getSafetyBoolean(context,
                        BGMConstants.KEY_BG_IS_WORKFLOW_INTERRUPTION);
        boolean isInOtherWorkFlow = safetyInOtherWorkFlow.getByte() == SafetyBoolean.TRUE
                .getByte();
        Debug.printI(TAG, "getCurrentWorkFlow: " + isInOtherWorkFlow);
        return isInOtherWorkFlow;

    }

    /**
     * 
     * Change hex data to integer data type for change to safetychannel
     * 
     * return int [out] integer data type data
     * Range: larger than 0
     * Unit: int
     * Scaling: 1
     * 
     * @param startIndex, start index of mReceiveBgmData
     *            Range: larger than 0
     *            Unit: int
     *            Scaling: 1
     * @param length, data length
     *            Range: larger than 0
     *            Unit: int
     *            Scaling: 1
     * 
     */

    public static int getResultData(int startIndex, int length)
    {
        int power = length;
        int result = 0;
        int dataLength = startIndex + length;
        BgmCommunication bgmCommunication = BgmCommunication.getInstance();
      
        if ((length <= 0) || (startIndex < 0))
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }

        // get result value, integer type.
        // e,g,. 236=2*10^2 + 3*10^1 + 6*10^0
        for (int i = startIndex; i < dataLength; i++)
        {
            Debug.printI(TAG, "getResultData result raw: "
                    + bgmCommunication.getReceiveData().get(i));
            power--;
            result = (int) (result + Character.getNumericValue(bgmCommunication
                    .getReceiveData().get(i))
                    * Math.pow(IBgmConstant.TENS, power));
            Debug.printI(TAG, "getResultData result1: " + result);
        }
        return result;
    }

    /**
     * 
     * Get flag boolean value.
     * 
     * return SafetyBoolean [out] input flag boolean value.
     * Range: SafetyT
     * Unit: SafetyBoolean
     * Scaling: 1
     * 
     * @param safeBgResultFlag [in] flag raw data.
     *            Range: valid object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * @param flagPostion[in] flag value position
     *            Range: 0~16
     *            Unit: int
     *            Scaling: 1
     */
    public static SafetyBoolean getFlagContentValue(
            SafetyChannel<Integer> safeBgResultFlag, int flagPostion)
    {

        Debug.printD(TAG, "getResultFlag ++");
        String dataLength = "%016d";
        int flagLength = 16;
        int i = 0;
        // array index from 0~15
        int index = flagLength - 1;
        if (flagPostion > flagLength)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        String flagTrue = "1";
        String flagSubstr = "";
        Boolean[] flagBooleanArray = new Boolean[flagLength];
        CommonUtils.objectCheck(safeBgResultFlag);
        SafetyBoolean Flag = SafetyBoolean.FALSE;

        // check two channel
        SafetyChannel<Integer> safeChannelFlag = new SafetyChannel<Integer>();
        int ch1 = safeBgResultFlag.getValueCH1();
        int ch2 = safeBgResultFlag.getValueCH2();
        Debug.printI(TAG, "ch1Flag:" + ch1);
        Debug.printI(TAG, "ch2Flag:" + ch2);
        safeChannelFlag.set(ch1, ch2);
        int flag = CommonUtils.getOriginValue(ch1, ch2);

        // get result flag raw data. from 0000 to 0000000000000000
        int rawHexFlag = Integer.parseInt(String.valueOf(flag), flagLength);
        Debug.printI(TAG, "rawHexFlag:" + rawHexFlag);
        String rawBinFlag = Integer.toBinaryString(rawHexFlag);
        Debug.printI(TAG, "rawBinFlag:" + rawBinFlag);
        String resultstr = String
                .format(dataLength, Long.parseLong(rawBinFlag));
        Debug.printI(TAG, "resultstr:" + resultstr);
        // change to boolean array
        // the left bit is high byte, it is different with string's position.
        for (i = 0; i < flagLength; i++)
        {
            flagSubstr = resultstr.substring(i, i + 1);
            if (flagSubstr.equals(flagTrue))
            {
                flagBooleanArray[index] = true;
            }
            else
                flagBooleanArray[index] = false;
            index--;
        }

        for (i = 0; i < flagLength; i++)
        {
            Debug.printD(TAG, "Flag: " + i + " " + flagBooleanArray[i]);
        }

        if (true == flagBooleanArray[flagPostion])
        {
            Flag = SafetyBoolean.TRUE;
        }
        else
        {
            Flag = SafetyBoolean.FALSE;
        }
        return Flag;
    }

    /**
     * Check return data CRC value whether is the same.
     * Find out the data content index and call getDataContent() to get CRC
     * value.
     * If CRC fail, throw exception and show EMWR.
     * 
     * throw BgmCRCDataErrorException
     * return void [out] None.
     */
    public static void checkDataCRC() throws BgmCRCDataErrorException
    {
        Debug.printI(TAG, "checkDataCRC");
        BgmCommunication bgmCommunication = BgmCommunication.getInstance();
        CommonUtils.objectCheck(bgmCommunication);
        // 0x02 0x00 0x00 0x09 ...
        // startIndex is 0x09
        int startIndex = 3;
        int endIndex = 0;

        int startPosition = 0;
        int lengthPosition1 = 1;
        int lengthPosition2 = 2;

        int tens = 0;
        int ones = 0;
        int hex = 16;
        byte[] currentCRC = null;
        byte[] dataCRC = new byte[lengthPosition2];
        boolean isCRCsame = false;

        for (byte receiveData : bgmCommunication.getReceiveData())
        {
            // get STX next 2 bytes is data length
            // And call getDataContent to get content
            if (receiveData == IBgmConstant.STX)
            {
                startIndex = startIndex + startPosition;
                // get the length form return data
                tens = Character.getNumericValue(bgmCommunication
                        .getReceiveData().get(startPosition + lengthPosition1));
                ones = Character.getNumericValue(bgmCommunication
                        .getReceiveData().get(startPosition + lengthPosition2));

                Debug.printI(TAG, "tens:" + tens);
                Debug.printI(TAG, "ones:" + ones);
                // calculate data length, change hex to Decimals

                endIndex = startIndex + tens * hex + ones;
                Debug.printI(TAG, "start_index " + startIndex + "end_index "
                        + endIndex);
                // get crc value form calculation funciton.
                currentCRC = getDataContent(startIndex, endIndex);
                dataCRC[0] = bgmCommunication.getReceiveData().get(endIndex);
                dataCRC[1] = bgmCommunication.getReceiveData()
                        .get(endIndex + 1);

                Debug.printI(TAG, "dataCRC  " + dataCRC[0] + " " + dataCRC[1]);
                Debug.printI(TAG, "currentCRC  " + currentCRC[0] + " "
                        + currentCRC[1]);

                // check crc value
                isCRCsame = Arrays.equals(currentCRC, dataCRC);
                Debug.printI(TAG, "isCRCsame " + isCRCsame);
                if (isCRCsame)
                {
                    isCRCsame = false;
                }
                else
                {

                    throw new BgmCRCDataErrorException();
                }
            }
            else
            {
                // Empty for static code analysis
            }

            startPosition++;
        }

    }

    /**
     * 
     * Get data contents from mReceiveBGMData. Call caculateCRC() to get CRC
     * value
     * 
     * see mReceiveBGMData[in]
     * 
     * return byte[] [out] CRC value.
     * Range: length 2 of byte array
     * Unit: byteArray
     * Scaling: 1
     * 
     * @param start start index of bgmcommunication receive data
     *            Range: must larger than 0
     *            Unit: int
     *            Scaling: 1
     * @param end end index of bgmcommunication receive data
     *            Range: must larger start index
     *            Unit: int
     *            Scaling:1
     *            return
     * 
     * 
     */
    private static byte[] getDataContent(int start, int end)
    {
        Debug.printI(TAG, "getDataContent");
        boolean isEndSmallerStart = end < start;
        // range check
        if (true == isEndSmallerStart)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }

        byte[] data = new byte[end - start];
        byte[] crc = null;
        int i = 0;
        int j = 0;
        // get mReceiveBGMData data
        for (i = start; i < end; i++)
        {
            data[j] = BgmCommunication.getInstance().getReceiveData().get(i);
            j++;
        }
        // call caculateCRC method
        crc = calculateCRC(data);
        return crc;
    }

    /**
     * Calculate input data CRC
     * return byte array of CRC
     * Range: length 2 of byte array
     * Unit: byteArray
     * Scaling: 1
     * 
     * @param inputData calculate the CRC value of inputData
     *            Range: 6~25
     *            Unit: byteArray
     *            Scaling: 1
     * 
     * 
     */

    public static byte[] calculateCRC(byte[] inputData)
    {
        int i = 1;
        // CRC initial value
        int initialValue = 110;
        int minLength = 1;
        int dataLength = inputData.length;
        CommonUtils.objectCheck(inputData);
        if (dataLength < minLength)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        byte[] CRC = null;

        for (i = 0; i < dataLength; i++)
        {
            initialValue = initialValue ^ (int) inputData[i];
        }

        Debug.printI(TAG, "caculateCRC rowdata" + initialValue);
        CRC = Integer.toHexString(initialValue).toUpperCase().getBytes();
        return CRC;
    }

    /**
     * 
     * Compose date time data which would write to bgm.
     * return byte[] [out] date or time data use bgm format with crc
     * value.
     * return
     * Range: length of 14
     * Unit: byteArray
     * Scaling: 1
     * 
     * @param format according format to get system date or time.
     *            Range: must be hhmmss or yyMMdd
     *            Unit: String
     *            Scaling: 1
     */
    protected static byte[] setDateTimeFormat(String format)
    {
        Debug.printI(TAG, "setDateTimeFormat++ ");
        // start position in data/time command
        int commandPosition = 4;
        int i = 0;
        int k = 0;
        int datetimeLength = 6;
        int commandLength = 8;
        CommonUtils.objectCheck(format);
        DateFormat dateFormat = new SimpleDateFormat(format);
        // get current date/time
        String Date = dateFormat.format(Calendar.getInstance().getTime());
        Debug.printI("test", "Date " + Date);
        byte[] byteDate = Date.getBytes();
        byte[] commandFormateDatetime = new byte[commandLength];
        // build the command format for calculate CRC
        // e.g., TAB X X X TAB
        commandFormateDatetime[0] = IBgmConstant.TAB;
        commandFormateDatetime[commandLength - 1] = IBgmConstant.TAB;

        for (i = 1; i <= datetimeLength; i++)
        {
            commandFormateDatetime[i] = byteDate[k];
            k++;
        }

        // set command data template
        byte[] dateCommand = { IBgmConstant.STX, '0', '8', IBgmConstant.TAB,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, IBgmConstant.TAB, 0x00,
                0x00, IBgmConstant.EOT };

        // call CRC function to calculate CRC
        byte[] byteCRC = BgmUtils.calculateCRC(commandFormateDatetime);
        // set data into dateCommand,
        for (i = 0; i < byteDate.length; i++)
        {
            dateCommand[commandPosition] = byteDate[i];
            commandPosition++;
        }
        commandPosition++;// for TAB
        // set CRC into dateCommand,
        for (i = 0; i < byteCRC.length; i++)
        {
            dateCommand[commandPosition] = byteCRC[i];
            commandPosition++;
        }
        return dateCommand;
    }

    public static boolean getBatteryPower()
    {
        // Read battery info
        final SafetyString safeBatteryInfo = NugenSettingModel.getString(
                mContext,
                NugenFrameworkConstants.PowerManagerConstants.BATTERY_INFO);
        final String batteryInfo = safeBatteryInfo.getString();
        JSONObject json;
        final SafetyString sKey2 = new SafetyString(
                ConfigParameter.EMPTY_BATTERY_THRESHOLD,
                CRCTool.generateCRC16(ConfigParameter.EMPTY_BATTERY_THRESHOLD
                        .getBytes()));
        final SafetyNumber<Integer> safetyEmptyValue = ReadConfig
                .getIntegerDataByKey(sKey2);
        final int minLevel = safetyEmptyValue.get();
        boolean isBatteryEnough = false;
        try
        {
            json = new JSONObject(batteryInfo);
            int batteryLevel = (Integer) json.get(BatteryManager.EXTRA_LEVEL);
            if (batteryLevel > minLevel)
            {
                isBatteryEnough = true;
            }
            else
            {
                isBatteryEnough = false;
            }

        }
        catch (JSONException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis.
        }
        return isBatteryEnough;
    }

    public enum StatusType
    {
        /**
         * Start receive data flag.
         * Range: true or false
         * Unit:enum
         * Scaling: 1
         */
        START_RECIEVE(false), // start receive BGM data

        /**
         * Start write data flag.
         * Range: true or false
         * Unit:enum
         * Scaling: 1
         */
        START_WRITE(false), // start write BGM data

        /**
         * Receive NAK flag
         * Range: true or false
         * Unit:enum
         * Scaling: 1
         */
        READ_NAK(false), // read nak

        /**
         * When BGM communication sends ¡§CR¡¨ and receives ¡§ACK¡¨, it indicates
         * command state finish; BGM communication could read or write data.
         * Range: true or false
         * Unit:enum
         * Scaling: 1
         */
        CR_AND_ACK(false); // send cr and return ack

        /**
         * State of StatusType
         * Range: true or false
         * Unit:boolean
         * Scaling: 1
         */
        private boolean mIstrue = false;
        private static String TAG = "StatusType";

        /**
         * Constructor of StatusType
         * mIstrue[out]
         * 
         * @param istrue Constructor,
         *            set mIstrue boolean value
         */
        private StatusType(boolean istrue)
        {
            mIstrue = istrue;
        }

        /**
         * Set state of StatusType
         * mIstrue[out]
         * 
         * @param istrue
         *            set mIstrue boolean value
         */
        public void setState(boolean istrue)
        {

            mIstrue = istrue;
        }

        /**
         * Get state of StatusType
         * mIstrue[in]
         * 
         * @return
         */
        public boolean getState()
        {
            return mIstrue;
        }

    }

}
// add header footer.
// add updateCodeKey function.
// Refine code and comment.
// Refine bgtestflow constant naming.
// add broadcast receiver for ble SystemSyn command.
// Add flag to check in Bg test flow.
// (R24379 2015-11-18 23:07:09 henrytso)
// ----------------------------------------------------------------------------
// Refine code comment.
