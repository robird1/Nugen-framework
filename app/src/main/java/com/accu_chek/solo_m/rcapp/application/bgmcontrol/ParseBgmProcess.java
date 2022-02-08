/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ParseBgmProcess
 * Brief:
 *
 * Create Date: 2015/2/5
 * $Revision: 25188 $
 * $Author: VictorChen $
 * $Id: ParseBgmProcess.java 25188 2015-12-01 02:30:28Z VictorChen $
 */
package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmBuildCommand.ParseResult;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmCRCDataErrorException;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager.LEDTYPE;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.bgmcommunication.BgmCommunication;
import com.accu_chek.solo_m.rcapp.data.nugendata.BGTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.CGTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public abstract class ParseBgmProcess
{

    static final String TAG = "ParseBgmProcess";
    BgmControl bgmControl = BgmControl.getInstance(null);

    void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: enter");
    }

}

class PowerDownCommand extends ParseBgmProcess
{
    /**
     * Bgm power down command.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        final long SLEEP_TIME = 30L;

        // for 3PMM spec
        CommonUtils.sleep(SLEEP_TIME);
        // set bgm POST flag true.
        NugenGeneralModel.setSafetyBoolean(BgmUtils.getContext(),
                NugenFrameworkConstants.BGMConstants.KEY_BG_POST,
                SafetyBoolean.TRUE);
    }
}

class SetDateCommand extends ParseBgmProcess
{
    /**
     * Set bgm date command.
     * In case the command is trigger by other module, call onSucess() to the
     * module.
     *
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        IMeInformationListener callback = BgmControl
                .getIMeInformationListener();
        String className = SetDateCommand.class.getSimpleName();

        if (null != callback)
        {
            try
            {
                callback.onSuccess(
                        new SafetyString(className, CRCTool
                                .generateCRC16(className.getBytes())), null);
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // static code for analysis.
            }
        }

    }
}

class SetTimeCommand extends ParseBgmProcess
{
    /**
     * Set bgm time command.
     * In case the command is trigger by other module, call onSucess() to the
     * module.
     *
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {

        IMeInformationListener callback = BgmControl
                .getIMeInformationListener();
        String className = SetTimeCommand.class.getSimpleName();

        if (null != callback)
        {
            try
            {
                callback.onSuccess(
                        new SafetyString(className, CRCTool
                                .generateCRC16(className.getBytes())), null);
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // static code for analysis.
            }
        }

    }
}

class ReadErrorStatusCommand extends ParseBgmProcess
{

    /**
     * Read error status command to get error code and show EMWR.
     * In case the command is trigger by other module, call onError() to the
     * module.
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */

    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: Read_Error_Status_Command");
        // command data start index
        int index = 6;
        // error code length
        int errorCodeLength = 2;
        int length = index + errorCodeLength;
        int resultArrayIndex = 0;
        IMeInformationListener callBackObject = null;
        byte[] resultArray = new byte[errorCodeLength];
        String errorNumber = "";
        String errorType = "";
        boolean isErrorType = false;
        final String StripRemoveErrorCode = "01";
        // get error code, from byte hex data to ASCII
        // the error code length is 2
        for (int i = index; i < length; i++)
        {
            resultArray[resultArrayIndex] = BgmCommunication.getInstance()
                    .getReceiveData().get(i);
            resultArrayIndex++;
        }

        try
        {
            errorNumber = new String(resultArray);
            BgmUtils.checkDataCRC();
            boolean isStripRemove = errorNumber.equals(StripRemoveErrorCode);
            // not remove error, show error
            if (true == isStripRemove)
            {
                removeStripHandle();
            }
            else
            {
                // get error type
                errorType = BgmErrorCodeTable.getErrorType(errorNumber);
                // get emwr id
                isErrorType = BgmErrorCodeTable.checkEMWRtype(errorType);

                if (isErrorType)
                {
                    EMWRList value = BgmErrorCodeTable.getEmwrId(errorType);
                    NotifyMessage msg = new NotifyMessage(value);
                    try
                    {
                        LEDManager mLED = (LEDManager) ICustomizedHWManager
                                .getSystemService(ICustomizedHWManager.LED_SERVICE);
                        mLED.closeLED(LEDTYPE.STRIP);
                    }
                    catch (OperationFailException e)
                    {
                        e.printStackTrace();
                    }
                    finally
                    {
                        // static code for analysis
                    }

                    NotifyProxy.showEMWR(BgmUtils.getContext(), msg);

                }
            }

            // call the trigger object, the command fail.
            callBackObject = BgmControl.getIMeInformationListener();
            if (null != callBackObject)
            {
                try
                {
                    callBackObject.onError();
                }
                catch (RemoteException e)
                {
                    e.printStackTrace();
                }
                finally
                {
                    // static code for analysis
                }
            }
        }
        catch (BgmCRCDataErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }
    }

    /**
     * 
     * When strip remove, check the UI screen status.
     * In case the action is wait blood, show insert strip.
     * On the contrary the action is processing, show remove early dialog.
     *
     * 
     * see mListener[in]
     * 
     * return void [out] None.
     *
     */
    protected void removeStripHandle()
    {
        Debug.printI(TAG,
                "is_ACTION_PROCESSING" + BgmUtils.getIsProcessingAction());
        boolean isProcessing = BgmUtils.getIsProcessingAction();
        try
        {
            if (true == isProcessing)
            {
                Debug.printI(TAG, "is_ACTION_PROCESSING enter ");
                bgmControl.getOnlistener().onRemoveEarly();
            }
            else
            {
                Debug.printI(TAG, "onWaitStrip enter ");
                bgmControl.getOnlistener().onWaitStrip();

            }
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
}

class ReadMeStatusCommand extends ParseBgmProcess
{

    /**
     * 
     * parse Read_ME_Status_Command to get the bgm status flag.
     * 
     * see mReceiveBgmData[in]
     * see mIsStripInsert[out]
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     *
     */

    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: Read_ME_Status");

        // the flag bit from 3PMM
        final int stripPresentIndex = 8;
        final int warringTempIndex = 9;
        final int errorTempIndex = 10;
        final int warrringBatteryIndex = 11;
        final int errorBatteryIndex = 12;
        final int clockErrorIndex = 13;
        final int safemodeStrip = 14;
        final int statusIndex = 4;
        final int statusLength = 4;
        // get status result
        final int status = BgmUtils.getResultData(statusIndex, statusLength);
        final int dataCh1 = CommonUtils.encodeCH1Value(status);
        final int dataCh2 = CommonUtils.encodeCH2Value(status);
        boolean isSafeModeStrip = false;
        boolean isStripInsert = true;
        boolean isErrorTemperatue = false;
        boolean isErrorBattery = false;
        boolean isWarringBattery = false;
        boolean isClockError = false;
        boolean isWarringTemperatue = false;
        NotifyMessage msg = null;
        SafetyChannel<Integer> safeStatus = new SafetyChannel<Integer>(dataCh1,
                dataCh2);
        // get result status flag.

        isSafeModeStrip = BgmUtils.getFlagContentValue(safeStatus,
                safemodeStrip) == SafetyBoolean.TRUE;
        if (isSafeModeStrip)
        {
            Debug.printI(TAG, "safemode insert");
            // set safe mode flag true
            isStripInsert = false;
            NugenSettingModel
                    .setSafetyBoolean(
                            BgmUtils.getContext(),
                            NugenFrameworkConstants.PowerManagerConstants.SAFE_MODE_STRIP,
                            SafetyBoolean.TRUE);
        }
        else
        {
            // set safe mode flag true
            NugenSettingModel
                    .setSafetyBoolean(
                            BgmUtils.getContext(),
                            NugenFrameworkConstants.PowerManagerConstants.SAFE_MODE_STRIP,
                            SafetyBoolean.FALSE);
        }
        // temperature warring
        isWarringTemperatue = BgmUtils.getFlagContentValue(safeStatus,
                warringTempIndex) == SafetyBoolean.TRUE;
        if (isWarringTemperatue)
        {
            Debug.printI(TAG, "warring temp");
            msg = new NotifyMessage(EMWRList.EMW46002);
            NotifyProxy.showEMWR(BgmUtils.getContext(), msg);

        }
        else
        {
            // static code for analysis
        }
        // temperature error
        isErrorTemperatue = BgmUtils.getFlagContentValue(safeStatus,
                errorTempIndex) == SafetyBoolean.TRUE;
        if (isErrorTemperatue)
        {
            // show error dialog.
            Debug.printI(TAG, "error temp");
            isStripInsert = false;
            msg = new NotifyMessage(EMWRList.EMW46001);
            NotifyProxy.showEMWR(BgmUtils.getContext(), msg);
        }
        else
        {
            // static code for analysis
        }

        // battery warring
        isWarringBattery = BgmUtils.getFlagContentValue(safeStatus,
                warrringBatteryIndex) == SafetyBoolean.TRUE;
        if (isWarringBattery)
        {
            Debug.printI(TAG, "warring battery");
        }
        else
        {
            // static code for analysis
        }

        // battery error
        isErrorBattery = BgmUtils.getFlagContentValue(safeStatus,
                errorBatteryIndex) == SafetyBoolean.TRUE;
        if (isErrorBattery)
        {
            Debug.printI(TAG, "error battery");

        }
        else
        {
            // static code for analysis
        }

        // clock error
        isClockError = BgmUtils
                .getFlagContentValue(safeStatus, clockErrorIndex) == SafetyBoolean.TRUE;
        if (isClockError)
        {
            Debug.printI(TAG, "clockErrorIndex");
            isStripInsert = false;
            // notify date time or call EMWR
        }
        else
        {
            // static code for analysis
        }

        // check strip insert
        if (isStripInsert)
        {
            isStripInsert = BgmUtils.getFlagContentValue(safeStatus,
                    stripPresentIndex) == SafetyBoolean.TRUE;
        }
        else
        {
            // static code for analysis
        }

        try
        {
            // check CRC value
            BgmUtils.checkDataCRC();
            // set result call back
            CommonUtils.objectCheck(parseresult);
            parseresult.setResult(isStripInsert);

        }
        catch (BgmCRCDataErrorException e)
        {

            e.printStackTrace();
        }
        finally
        {
            // static code for analysis.
        }

    }

}

class CheckCodeKeyCommand extends ParseBgmProcess
{
    /**
     * Parse code key data. Return key number , data, status to caller.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */

    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: Check_Code_Key_Command");
        final int KetIndex = 4;
        final int keyLength = 3;
        final int keyDateIndex = 8;
        final int keyDateLength = 6;
        final int keyStatusIndex = 15;
        final int keyStatusLength = 2;
        final int keyFieldIndex = 18;
        final int keyFieldLength = 2;
        int dataPosition = 0;
        int i = 0;
        int arrayIndex = 0;
        final byte[] number = new byte[keyLength];
        final byte[] date = new byte[keyDateLength];
        final byte[] status = new byte[keyStatusLength];
        final byte[] field = new byte[keyFieldLength];
        dataPosition = KetIndex + keyLength;
        for (i = KetIndex; i < dataPosition; i++)
        {
            number[arrayIndex] = BgmCommunication.getInstance()
                    .getReceiveData().get(i);
            arrayIndex++;
        }

        arrayIndex = 0;

        dataPosition = keyDateIndex + keyDateLength;

        for (i = keyDateIndex; i < dataPosition; i++)
        {
            date[arrayIndex] = BgmCommunication.getInstance().getReceiveData()
                    .get(i);
            arrayIndex++;
        }
        arrayIndex = 0;
        dataPosition = keyStatusIndex + keyStatusLength;

        for (i = keyStatusIndex; i < dataPosition; i++)
        {
            status[arrayIndex] = BgmCommunication.getInstance()
                    .getReceiveData().get(i);
            arrayIndex++;
        }
        arrayIndex = 0;
        dataPosition = keyFieldIndex;
        for (i = keyFieldIndex; i < dataPosition; i++)
        {
            field[arrayIndex] = BgmCommunication.getInstance().getReceiveData()
                    .get(i);
            arrayIndex++;
        }
        SafetyByteArray sfnumber = new SafetyByteArray(number,
                CRCTool.generateCRC16(number));
        SafetyByteArray sfdate = new SafetyByteArray(date,
                CRCTool.generateCRC16(date));
        SafetyByteArray sfstatus = new SafetyByteArray(status,
                CRCTool.generateCRC16(status));
        SafetyByteArray sffield = new SafetyByteArray(field,
                CRCTool.generateCRC16(field));
        try
        {
            BgmUtils.checkDataCRC();
            BgmControl.getIMeInformationListener().onCodeKeyInformation(
                    sfnumber, sfdate, sfstatus, sffield);

        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        catch (BgmCRCDataErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis.
        }

    }

}

class BgTestCommand extends ParseBgmProcess
{
    /**
     * Parse BgTestCommand data. Get strip key number, bg value, test date and
     * time, test flag.
     * Then save result to database the display result to user.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: valid object
     *            Unit: ParseResult
     *            Scaling: 1
     * 
     * 
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: bG_Test_Command");
        final int FATAviva = 999888;
        final int FATPerforma = 999777;
        final int resetDataOnFATInsert = 0x9F6;
        // strip key number index
        int stripKey = 4;
        int stripKeyLength = 6;
        // bg result index
        int bgIndex = 19;
        int bGValueLength = 3;
        // time index
        int timeIndex = 23;
        int timeLength = 4;
        // date index
        int dateIndex = 28;
        int dateLength = 6;
        // result flag index
        int flagIndex = 39;
        int flagLength = 4;
        // get key number
        int stripKeyNum = BgmUtils.getResultData(stripKey, stripKeyLength);

        // result flag bit position from 3PMM
        int indexcG1Flag = 5;
        int indexcG2Flag = 4;
        int indexcGFlag = 15;

        // cg result two channgel
        int cgCh1 = 0;
        int cgCh2 = 0;

        int bgValue = 0;

        boolean isPlugIn = BgmUtils.getIsUsbPlugIn();

        ArrayList<Object> resultList = new ArrayList<Object>();

        SafetyChannel<Integer> cgvalue;

        // should read config matrix "resetDataOnFATInsert"

        // SafetyNumber<Integer> FATresult = ReadConfig
        // .getIntegerDataByKey(new SafetyString(
        // ConfigParameter.KEY_RESET_DATA_ON_FAT_INSERT,
        // CRCTool.generateCRC16(ConfigParameter.KEY_RESET_DATA_ON_FAT_INSERT
        // .getBytes())));

        boolean isClearData = NugenProductionModel
                .getInt(new SafetyString(
                        ProductionConstants.KEY_RESETDATAONFATINSERT,
                        CRCTool.generateCRC16(ProductionConstants.KEY_RESETDATAONFATINSERT
                                .getBytes()))).get() == resetDataOnFATInsert;

        boolean isCgResult = false;

        Debug.printI(TAG, "stripKeyNum " + stripKeyNum);
        Debug.printI("result", "isClearData " + isClearData);

        // check is FAT strip
        boolean isFAT = (FATAviva == stripKeyNum)
                || (FATPerforma == stripKeyNum);
        if (isFAT && isClearData)
        {
            try
            {
                bgmControl.getOnlistener().onFATStrip();
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // static code for analysis
            }
        }
        else if (isPlugIn == false)
        {

            // get bg value
            bgValue = BgmUtils.getResultData(bgIndex, bGValueLength);
            SafetyString safeBgValue = DatabaseUtil
                    .toSafeInsertionString(bgValue);
            Debug.printI(TAG,
                    "bgvalue " + BgmUtils.getResultData(bgIndex, bGValueLength));

            // get timestamp
            int time = BgmUtils.getResultData(timeIndex, timeLength);
            int date = BgmUtils.getResultData(dateIndex, dateLength);
            Long timeStamp = getTestTimeStamp(time, date);
            SafetyString safeTimeStamp = DatabaseUtil
                    .toSafeInsertionString(timeStamp);
            Debug.printI(TAG, "timeStamp " + timeStamp);

            // get result flag
            SafetyString safeFlag = DatabaseUtil.toSafeInsertionString(BgmUtils
                    .getResultData(flagIndex, flagLength));

             int ch1Flag = CommonUtils.encodeCH1Value(BgmUtils.getResultData(
             flagIndex, flagLength));
             int ch2Flag = CommonUtils.encodeCH2Value(BgmUtils.getResultData(
             flagIndex, flagLength));

//            int ch1Flag = -8001;
//            int ch2Flag = 16001;

            Debug.printI(TAG, "ch1Flag:" + ch1Flag);
            Debug.printI(TAG, "ch2Flag:" + ch2Flag);
            SafetyChannel<Integer> safeFlagChannel = new SafetyChannel<Integer>();
            safeFlagChannel.set(ch1Flag, ch2Flag);
            Debug.printI(TAG, "safeFlagChannel ");

            // check temperaure flag
            checkTemperaureFlag(safeFlagChannel);

            // check cg flag
            isCgResult = BgmUtils.getFlagContentValue(safeFlagChannel,
                    indexcG1Flag) == SafetyBoolean.TRUE
                    || BgmUtils.getFlagContentValue(safeFlagChannel,
                            indexcG2Flag) == SafetyBoolean.TRUE
                    || BgmUtils.getFlagContentValue(safeFlagChannel,
                            indexcGFlag) == SafetyBoolean.TRUE;
            // set result flag to determine cg or bg
            resultList.add(isCgResult);
            try
            {
                if (isCgResult)
                {
                    // cg result

                    Debug.printI(TAG, "cg result");
                    // build two channel
                    cgCh1 = CommonUtils.encodeCH1Value(BgmUtils.getResultData(
                            bgIndex, bGValueLength));
                    cgCh2 = CommonUtils.encodeCH2Value(BgmUtils.getResultData(
                            bgIndex, bGValueLength));

                    cgvalue = new SafetyChannel<Integer>();
                    cgvalue.set(cgCh1, cgCh2);

                    // check crc value
                    BgmUtils.checkDataCRC();
                    // set result to build command
                    resultList.add(cgvalue);
                    // parseresult.setResult(isCgResult, cgvalue);
                    CommonUtils.objectCheck(parseresult);
                    parseresult.setResult(resultList);
                    // save data
                    saveCgResult(safeBgValue, safeTimeStamp, safeFlag);

                }
                else
                {
                    // bg result
                    // check crc value
                    BgmUtils.checkDataCRC();
                    parseresult.setResult(resultList);
                    // save data
                    saveBgResult(safeBgValue, safeTimeStamp, safeFlag);
                    bgmControl.getOnlistener().onbgResult();
                }
            }
            catch (BgmCRCDataErrorException e)
            {
                e.printStackTrace();
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // static code for analysis
            }
        }
        else
        {
            try
            {
                bgmControl.getOnlistener().onUsbConnect();
            }
            catch (RemoteException e)
            {

                e.printStackTrace();
            }
            finally
            {
                // static code for analysis.
            }
        }
    }

    /**
     * 
     * Parse test date and time to time stamp object.
     * 
     * return long [out], time stamp
     * Range: greater than 0
     * Unit: long
     * Scaling: 1
     * 
     * @param time[in], test time, hours and minutes.
     *            Range: 1~2359
     *            Unit: int
     *            Scaling: 1
     * @param date[in], test time,
     *            Range: 030101 - 311231
     *            Unit: int
     *            Scaling: 1
     * 
     */
    protected long getTestTimeStamp(int time, int date)
    {
        boolean isNotInRange = (time < 0) || (date < 0);
        if (isNotInRange)
        {
            throw new DataIntegrityException();
        }
        else
        {
            // Empty for static code analysis
        }
        String dateFromate = "%06d";
        String timeFromate = "%04d";

        long testTimeStamp = 0;
        final int two = 2;
        final int four = 4;
        final int six = 6;
        final String yearHead = "20";

        String sTime = String.format(timeFromate, time);
        // hh:mm
        sTime = sTime.substring(0, two) + ":" + sTime.substring(two, four);

        String sDate = String.format(dateFromate, date);
        // 20yy-MM-DD
        sDate = yearHead + sDate.substring(0, two) + "-"
                + sDate.substring(two, four) + "-" + sDate.substring(four, six);
        try
        {
            // timestamp format
            final SimpleDateFormat dateTimeFormat = new SimpleDateFormat(
                    "yyyy-MM-dd hh:mm");

            final java.util.Date parsedDate = dateTimeFormat.parse(sDate + " "
                    + sTime);
            final Timestamp timestamp = new java.sql.Timestamp(
                    parsedDate.getTime());
            // get long type timestamp
            testTimeStamp = timestamp.getTime();
        }
        catch (ParseException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Empty for static code analysis
        }
        return testTimeStamp;
    }

    /**
     * 
     * Check temperature warring flag
     * return void [out] None.
     * 
     * @param flagArray[in] test result flag.
     *            Range: valid object of SafetyChannel
     *            Unit: Boolean[]
     *            Scaling: 1
     */
    protected void checkTemperaureFlag(SafetyChannel<Integer> flagArray)
    {
        final int indexWarringTempFlag = 6;
        CommonUtils.objectCheck(flagArray);
        boolean isWarringTemperatue = BgmUtils.getFlagContentValue(flagArray,
                indexWarringTempFlag) == SafetyBoolean.TRUE;
        if (isWarringTemperatue)
        {
            Debug.printI(TAG, "temperature warring");
            NotifyMessage msg = new NotifyMessage(EMWRList.EMW46002);
            NotifyProxy.showEMWR(BgmUtils.getContext(), msg);
        }
        else
        {
            // static code for analysis
        }
    }

    /**
     * 
     * Save bg result to database
     * 
     * return void [out] None.
     * 
     * @param safebgvalue[in], bg value
     *            Range: valid object of SafetyString
     *            Unit: SafetyString
     *            Scaling: 1
     * @param safetime[in], bg test time stamp
     *            Range: valid object of SafetyString
     *            Unit: SafetyString
     *            Scaling: 1
     * @param safebgresultFlag[in], bg test result flag
     *            Range: valid object of SafetyString
     *            Unit: SafetyString
     *            Scaling: 1
     * 
     */
    private void saveBgResult(SafetyString safebgvalue, SafetyString safetime,
            SafetyString safebgresultFlag)
    {
        Debug.printI(TAG, "saveBgmData enter");
        CommonUtils.objectCheck(safebgvalue, safetime, safebgresultFlag);
        DatabaseModel model = new DatabaseModel(UrlType.bgUri);
        HashMap<String, Object> values = new HashMap<String, Object>();
        // int bgvalue = Integer.parseInt(bg_test_vaule);
        int SegID = 0;
        int PairPump = 0;
        int Temp = 0;
        int uid = 0;

        SafetyString safeSegID = DatabaseUtil.toSafeInsertionString(SegID);
        SafetyString safePairPump = DatabaseUtil
                .toSafeInsertionString(PairPump);
        SafetyString safeTemp = DatabaseUtil.toSafeInsertionString(Temp);
        // SafetyString safebgresult = DatabaseUtil
        // .toSafeInsertionString(bgresult);
        SafetyString safeuid = DatabaseUtil.toSafeInsertionString(uid);

        values.put(BGTable.COLUMN_BG_VALUE, safebgvalue);
        values.put(BGTable.COLUMN_IS_PAIRED_TO_PUMP, safePairPump);
        values.put(BGTable.COLUMN_TIMESTAMP, safetime);
        values.put(BGTable.COLUMN_SEGMENT_ID, safeSegID);
        values.put(BGTable.COLUMN_TEMP_RESULT, safeTemp);

        values.put(BGTable.COLUMN_BG_RESULT, safebgresultFlag);
        values.put(BGTable.COLUMN_USER_SETTING_ID, safeuid);

        Debug.printI(TAG, "call insert");
        model.insertData(BgmUtils.getContext(), values);

        Debug.printI(TAG, "saveBgmData exit");
    }

    /**
     * 
     * Save cg result to database
     * 
     * return void [out] None.
     * 
     * @param cgresult[in], cg value
     *            Range: valid object of SafetyString
     *            Unit: SafetyString
     *            Scaling: 1
     * @param timesatamp[in], cg test time stamp
     *            Range: valid object of SafetyString
     *            Unit: SafetyString
     *            Scaling: 1
     * @param flag[in], cg test result flag
     *            Range: valid object of SafetyString
     *            Unit: SafetyString
     *            Scaling: 1
     * 
     */
    public void saveCgResult(SafetyString cgresult, SafetyString timesatamp,
            SafetyString flag) throws RemoteException
    {

        Debug.printI(TAG, "savecg enter");
        DatabaseModel model = new DatabaseModel(UrlType.cgUri);
        HashMap<String, Object> values = new HashMap<String, Object>();

        // SafetyString safeSegID = DatabaseUtil.toSafeInsertionString(SegID);

        values.put(CGTable.COLUMN_CG_RESULT, cgresult);
        values.put(CGTable.COLUMN_TIMESTAMP, timesatamp);
        values.put(CGTable.COLUMN_SEGMENT_ID, flag);

        model.insertData(BgmUtils.getContext(), values);

        Debug.printI(TAG, "saveBgmData exit");
    }
}

class ReadDateCommand extends ParseBgmProcess
{
    /**
     * Read date command.
     * call onSucess() to the caller.
     *
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: ReadDateCommand");
        int startIndex = 4;
        int dataLength = 6;
        String formatStr = "%06d";
        String time = null;
        int result = BgmUtils.getResultData(startIndex, dataLength);
        time = String.format(formatStr, result);
        Debug.printI("BgmControl", "time " + time);
        String key = "ReadDateCommand";
        try
        {
            BgmControl.getIMeInformationListener()
                    .onSuccess(
                            new SafetyString(key, CRCTool.generateCRC16(key
                                    .getBytes())),
                            new SafetyString(time, CRCTool.generateCRC16(time
                                    .getBytes())));
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }

    }

}

class ReadTimeCommand extends ParseBgmProcess
{

    /**
     * Read time command.
     * call onSucess() to the caller.
     *
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: ReadTimeCommand");
        int startIndex = 4;
        int dataLength = 6;
        String formatStr = "%06d";
        String time = null;
        int result = BgmUtils.getResultData(startIndex, dataLength);
        time = String.format(formatStr, result);

        String key = "ReadTimeCommand";
        try
        {
            BgmControl.getIMeInformationListener()
                    .onSuccess(
                            new SafetyString(key, CRCTool.generateCRC16(key
                                    .getBytes())),
                            new SafetyString(time, CRCTool.generateCRC16(time
                                    .getBytes())));
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }
    }
}

class ReadSoftwareVersionCommand extends ParseBgmProcess
{
    /**
     * Read software version command.
     * call onSucess() to the caller.
     *
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: Read_Software_Version_Command");
        String version = null;
        BgmCommunication bgmCommunication = BgmCommunication.getInstance();
        int dataLennth = bgmCommunication.getReceiveData().size();
        int commandStartIndex = 4;
        final byte[] versionArray = new byte[commandStartIndex];
        int versionArrayIndex = 0;
        byte data;
        for (int i = commandStartIndex; i < dataLennth; i++)
        {
            Debug.printI(TAG, "data "
                    + bgmCommunication.getReceiveData().get(i) + " i " + i);
            data = bgmCommunication.getReceiveData().get(i);
            // check is last byte
            if (data != IBgmConstant.TAB)
            {
                versionArray[versionArrayIndex] = bgmCommunication
                        .getReceiveData().get(i);
                versionArrayIndex++;

            }
            else
            {
                i = bgmCommunication.getReceiveData().size();
            }
        }
        try
        {
            version = new String(versionArray);
            BgmUtils.checkDataCRC();
            NugenGeneralModel.setString(
                    BgmUtils.getContext(),
                    NugenFrameworkConstants.BGMConstants.KEY_BG_VERSION,
                    new SafetyString(version, CRCTool.generateCRC16(version
                            .getBytes())));
        }
        catch (BgmCRCDataErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }
    }

}

class ReadModelNameCommand extends ParseBgmProcess
{
    /**
     * Read ME Model name,
     * return model name to caller
     */
    @Override
    public void parse(ParseResult parseresult)
    {

        Debug.printI(TAG, "[Process]: Read_Model_Name");

    }

}

class ReadInstrumentNameCommand extends ParseBgmProcess
{

    /**
     * Parse Read InstrumentName command.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */

    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: Read_Instrument_Name");
        final int index = 4;
        final int length = 3;
        byte[] resultArray = new byte[length];
        int resultArrayIndex = 0;
        int dataLength = index + length;
        for (int i = index; i < dataLength; i++)
        {
            resultArray[resultArrayIndex] = BgmCommunication.getInstance()
                    .getReceiveData().get(i);
            resultArrayIndex++;
        }
        try
        {
            BgmUtils.checkDataCRC();
            String instrument = new String(resultArray);
            NugenGeneralModel.setString(
                    BgmUtils.getContext(),
                    NugenFrameworkConstants.BGMConstants.KEY_BG_INSTRUMENT,
                    new SafetyString(instrument, CRCTool
                            .generateCRC16(instrument.getBytes())));
        }
        catch (BgmCRCDataErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }
    }

}

class GetStripCounterCommand extends ParseBgmProcess
{

    /**
     * Parse StripCounter command.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: GetStripCounterCommand");
        int keyIndex = 4;
        int keyLength = 4;
        int dataLenght = keyIndex + keyLength;
        int i = 0;
        int arrayIndex = 0;
        byte[] counter = new byte[keyLength];

        for (i = keyIndex; i < dataLenght; i++)
        {
            counter[arrayIndex] = BgmCommunication.getInstance()
                    .getReceiveData().get(i);
            arrayIndex++;
        }
        SafetyByteArray sfCounter = new SafetyByteArray(counter,
                CRCTool.generateCRC16(counter));

        try
        {
            BgmUtils.checkDataCRC();
            BgmControl.getIMeInformationListener().onStripCounter(sfCounter);
        }
        catch (RemoteException e)
        {
            Debug.printI(TAG, "[Process]: RemoteException");
            e.printStackTrace();
        }
        catch (BgmCRCDataErrorException e)
        {

            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }
    }

}
//FIXME
class UpdateCodeKeyCommand extends ParseBgmProcess
{
    @Override
    public void parse(ParseResult parseresult)
    {
        IMeInformationListener callback = BgmControl
                .getIMeInformationListener();
        String className = SetDateCommand.class.getSimpleName();

        if (null != callback)
        {
            try
            {
                callback.onSuccess(
                        new SafetyString(className, CRCTool
                                .generateCRC16(className.getBytes())), null);
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // static code for analysis.
            }
        }

    }
}

class ReadStripConnectorCommand extends ParseBgmProcess
{
    /**
     * Parse StripConnector command.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {
        Debug.printI(TAG, "[Process]: ReadStripConnectorCommand");
        final int index = 4;
        final int length = 4;
        int dataLength = 8;
        byte[] resultArray = new byte[length];
        int resultArrayIndex = 0;
        String stripConnector = "";
        for (int i = index; i < dataLength; i++)
        {
            resultArray[resultArrayIndex] = BgmCommunication.getInstance()
                    .getReceiveData().get(i);
            resultArrayIndex++;
        }
        try
        {
            stripConnector = new String(resultArray);
            BgmUtils.checkDataCRC();
            NugenGeneralModel
                    .setString(
                            BgmUtils.getContext(),
                            NugenFrameworkConstants.BGMConstants.KEY_BG_STRIP_CONNECTOR,
                            new SafetyString(stripConnector, CRCTool
                                    .generateCRC16(stripConnector.getBytes())));
        }
        catch (BgmCRCDataErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }
    }

}

class CheckcGL1RangeCommand extends ParseBgmProcess
{
    /**
     * Parse check control solution 1 command and check result whether in the
     * range.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */

    @Override
    public void parse(ParseResult parseresult)
    {

        Debug.printI(TAG, "[Process]: Check_cG_L1_Range_Command");

        // cg result flag index
        int index = 4;
        Debug.printI(TAG, "cg flag"
                + BgmCommunication.getInstance().getReceiveData().get(index));
        boolean isInRange = BgmCommunication.getInstance().getReceiveData()
                .get(index) == IBgmConstant.cG_INRANGE;
        try
        {
            BgmUtils.checkDataCRC();
            Debug.printI(TAG, "cg result" + isInRange);
            CommonUtils.objectCheck(parseresult);
            parseresult.setResult(isInRange);
        }
        catch (BgmCRCDataErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis
        }

    }

}

class CheckcGL2RangeCommand extends ParseBgmProcess
{

    /**
     * Parse check control solution 2 command and check result whether in the
     * range.
     * 
     * return void [out] None.
     * 
     * @param parseresult
     *            Range: null or valid object of ParseResult.
     *            Unit: ParseResult
     *            Scaling: 1
     */
    @Override
    public void parse(ParseResult parseresult)
    {

        Debug.printI(TAG, "[Process]: Check_cG_L2_Range_Command");
        int index = 4;
        Debug.printI(TAG, "cg flag"
                + BgmCommunication.getInstance().getReceiveData().get(index));
        boolean isInRange = BgmCommunication.getInstance().getReceiveData()
                .get(index) == IBgmConstant.cG_INRANGE;

        // Debug.printI(TAG, "cG L1: " + BgmUtils.getIsCgL1Range() + "cG L2: "
        // + BgmUtils.getIsCgL1Range());
        Debug.printI(TAG, "cG L1: " + ComposeCgCommand.getLevel1Resut()
                + "cG L2: " + isInRange);
        try
        {
            BgmUtils.checkDataCRC();
            bgmControl.getOnlistener().oncgResult(
                    ComposeCgCommand.getLevel1Resut(), isInRange);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        catch (BgmCRCDataErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Empty for static code analysis
        }
    }

}

class ParseBgmFactory
{

    private static final String TAG = "ParseBgmFactory";

    /**
     * Get class name of command
     * 
     * return class path
     * Range: null or class path
     * Unit: ParseBgmProcess
     * Scaling: 1
     * 
     * @param name[in], command name
     *            Range: not null
     *            Unit: String
     *            Scaling: 1
     * 
     * 
     */
    public static ParseBgmProcess createBgmparse(String name)
    {
        ParseBgmProcess bgmCommand = null;
        try
        {
            String className = "com.accu_chek.solo_m.rcapp.application.bgmcontrol."
                    + name;
            Debug.printI(TAG, "className = " + className);
            Class<?> c = Class.forName(className);
            bgmCommand = (ParseBgmProcess) c.newInstance();
        }
        catch (Exception e)
        {
            bgmCommand = null;
        }
        finally
        {
            // static code for analysis
        }
        return bgmCommand;
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// add header footer.
// add updateCodeKey function.
// add instrument name and strip Connector for RPC.
// Refine code and comment.
// Refine code and comment.
// Fix NSIQ 60
// Fix NSIQ 60 issue.
// add broadcast receiver for ble SystemSyn command.
// disable get FAT flag.
// Refine code comment.
// Add flag to check in Bg test flow.
// (R23923 2015-11-12 01:29:51 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
// (R24486 2015-11-20 01:48:56 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
// (R24899 2015-11-26 01:40:03 VictorChen)
// ----------------------------------------------------------------------------
// Add update code key command call back.
// (R25175 2015-11-30 07:25:57 VictorChen)
// ----------------------------------------------------------------------------
// Remove control solution result.
