/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.PatientRecord
 * Brief: 
 *
 * Create Date: 2015/10/27
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import android.database.Cursor;
import android.net.Uri;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.DataTransferHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.LogBookView;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTimeBlockTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.UserSettingTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

public class PatientRecord extends AbstractContinuaSegment
{
    
    /**
     * The patient record structure of database.
     */
    private LogBookView mData = new LogBookView();
    
    /**
     * The list contains patient time block record of database.
     */
    private static List<IDBData> mTimeblock = null;
    
    /**
     * The list contains user settings record of database.
     */
    private static UserSettingTable mUserSettings = null;
    
    /**
     * The index of this data in the database.
     */
    private int mIndex = -1;
    
    /**
     * Return the time stamp of this data.
     *
     * see mData [in]
     *
     * return SafetyNumber<Long> [out]: The time stamp of this data.
     *         Range: Valid object of SafetyNumber.
     *         Unit: Milliseconds.
     *         Scaling: 1.
     */
    @Override
    public SafetyNumber<Long> getTimeStamp()
    {
        SafetyNumber<Long> result = new SafetyNumber<Long>();
        
        long time = CommonUtils.getOriginValue(mData.getTimestamp().getValueCH1(), 
                mData.getTimestamp().getValueCH2());
        
        result.set(time, -time);
        
        return result;
    }
    
    /**
     * Convert the patient record data to byte array according to the defined structure.
     *
     * see mData [in]
     * see mIndex [in]
     *
     * return SafetyByteArray [out]: The byte array of this structure.
     *         Range: Valid object of SafetyByteArray.
     *         Unit: SafetyByteArray.
     *         Scaling: 1.
     */
    @Override
    public SafetyByteArray generateBytes()
    {        
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
        byte[] segmentId = ParseUtils.parseInt16(GlucoseSegmentId.PATIENT_RECORD);
        byte[] index = ParseUtils.parseInt16(mIndex);
        
        SafetyChannel<Long> absTimeChannel = mData.getAbsoluteTime();
        byte[] absTime = ParseUtils.convertToRPCAbsTimeFormat(CommonUtils.getOriginValue(
                absTimeChannel.getValueCH1(), absTimeChannel.getValueCH2()));
        
        SafetyChannel<Long> relativeTimeChannel = mData.getRelativeTime();
        long relativeTime = CommonUtils.getOriginValue(
                relativeTimeChannel.getValueCH1(), relativeTimeChannel.getValueCH2());
        byte[] relative = ParseUtils.parseAbsoluteTime(relativeTime);
        
        PatientRecordTimeBlockTable timeblock = getTimeblock(relativeTime);
        
        int adviceFlag = retrieveInteger(mData.getAdviceFlag().getString());
        
        int testFlag = retrieveInteger(mData.getTestFlag().getString());
        
        byte[] recordContent = CRCTool.getBytes(
                retrieveInteger(mData.getRecordContent().getString()));
        
        byte[] bg = CRCTool.getBytes(getValueFromChannel(mData.getBgValue())
                .shortValue());
        
        byte[] carbAmount = CRCTool.getBytes(getValueFromChannel(
                mData.getCarbAmount()).shortValue());
        
        byte[] healthPercentage = CRCTool.getBytes(getValueFromChannel(
                mData.getHealthPercentage()).shortValue());
        
        byte[] correction = CRCTool.getBytes(getValueFromChannel(
                mData.getUserCorrectionBolus()));
        
        byte[] mealBolus = CRCTool.getBytes(getValueFromChannel(
                mData.getUserSelectMealBolus()));
        
        byte[] totalBolus = CRCTool.getBytes(getValueFromChannel(
                mData.getUserSelectMealBolus()));
        
        int bolusType = getValueFromChannel(mData.getBolusDeliverType());
        
        byte[] lagTime = CRCTool.getBytes(getValueFromChannel(
                mData.getLagTime()).shortValue());
        
        int activationType = getValueFromChannel(mData.getActivationType());
        
        byte[] confirmedCorrection = CRCTool.getBytes(getValueFromChannel(
                mData.getConfirmCorrectionBolus()));
        
        byte[] confirmedMeal = CRCTool.getBytes(getValueFromChannel(
                mData.getConfirmMealBolus()));
        
        byte[] confirmedTotal = CRCTool.getBytes(getValueFromChannel(
                mData.getConfirmTotalBolus()));
        
        byte[] ratioInsulin = CRCTool.getBytes(getValueFromChannel(
                timeblock.getCarbRatioInsulin()));
        
        byte[] ratioCarb = CRCTool.getBytes(getValueFromChannel(
                timeblock.getCarbRatioCarbs()).shortValue());
        
        byte[] sensitiveInsulin = CRCTool.getBytes(getValueFromChannel(
                timeblock.getSensitivityInsulin()));
        
        byte[] sensitiveBG = CRCTool.getBytes(getValueFromChannel(
                timeblock.getSensitivityBg()).shortValue());
        
        int snack = getValueFromChannel(mUserSettings.getSnackSize());
        
        byte[] mealRaise = CRCTool.getBytes(getValueFromChannel(
                mUserSettings.getMealRise()).shortValue());
        
        byte[] actingTime = CRCTool.getBytes(getValueFromChannel(
                mUserSettings.getActingtime()).shortValue());

        byte[] offsetTime = CRCTool.getBytes(getValueFromChannel(
                mUserSettings.getOffsetTime()).shortValue());

        byte[] lowerTarget = CRCTool.getBytes(getValueFromChannel(
                timeblock.getBgLowerTarget()).shortValue());
        
        byte[] upperTarget = CRCTool.getBytes(getValueFromChannel(
                timeblock.getBgUpperTarget()).shortValue());
        
        byte[] recommendCorrection = CRCTool.getBytes(getValueFromChannel(
                mData.getRecommendCorrectionBolus()));
        
        byte[] recommendMeal = CRCTool.getBytes(getValueFromChannel(
                mData.getRecommendMealBolus()));
        
        byte[] recommendTotal = CRCTool.getBytes(getValueFromChannel(
                mData.getRecommendTotalBolus()));
        
        byte[] carbSuggestion = CRCTool.getBytes(getValueFromChannel(
                mData.getCarbSuggestion()));
        
        byte[] currentTarget = CRCTool.getBytes(getValueFromChannel(
                mData.getCurrentTarget()));
        
        byte[] correctionMealIncrease = CRCTool.getBytes(getValueFromChannel(
                mData.getCorrectionMealIncrease()));
        
        byte[] correctionDeltaBG = CRCTool.getBytes(getValueFromChannel(
                mData.getCorrectionDeltaBg()));
        
        byte[] currentAllowedBG = CRCTool.getBytes(getValueFromChannel(
                mData.getCurrentAllowedBg()).shortValue());
        
        byte[] currentDeltaBG = CRCTool.getBytes(getValueFromChannel(
                mData.getCurrentDeltaBg()).shortValue());
        
        byte[] activeInsulin = CRCTool.getBytes(getValueFromChannel(
                mData.getActiveInsulin()).shortValue());
        
        byte[] healthEvent = CRCTool.getBytes((byte) 0);
        
        byte[] mealTime = CRCTool.getBytes((byte) 0);
        
        byte[] basalMDI = CRCTool.getBytes(getValueFromChannel(
                mData.getBasalInsulinMDI()));
        
        byte[] immediateInsulin = CRCTool.getBytes(getValueFromChannel(
                mData.getImmediateInsulin()));
        
        byte[] delayInsulin = CRCTool.getBytes(getValueFromChannel(
                mData.getDelayedInsulin()));
        
        byte[] delayDuration = CRCTool.getBytes(getValueFromChannel(
                mData.getDelayedDuration()).shortValue());
        
        byte[] notes = CRCTool.getBytes((short) 0);
        
        byte[] noteRecordId = CRCTool.getBytes(getValueFromChannel(
                mData.getRecordId()));
        
        byte[] bolusId = CRCTool.getBytes(getValueFromChannel(
                mData.getBolusId()));
                
        
        byte[] data = null;
        byte[] length = null;
        byte[] crc = null;
        
        buffer.append(bolusId, 0, bolusId.length);
        buffer.append(absTime, 0, absTime.length);
        buffer.append(relative, 0, relative.length);
        buffer.append(adviceFlag);
        buffer.append(testFlag);
        buffer.append(recordContent, 0, recordContent.length);
        buffer.append(bg, 0, bg.length);
        buffer.append(carbAmount, 0, carbAmount.length);
        buffer.append(healthPercentage, 0, healthPercentage.length);
        buffer.append(correction, 0, correction.length);
        buffer.append(mealBolus, 0, mealBolus.length);
        buffer.append(totalBolus, 0, totalBolus.length);
        buffer.append(bolusType);
        buffer.append(lagTime, 0, lagTime.length);
        buffer.append(activationType);
        buffer.append(confirmedCorrection, 0, confirmedCorrection.length);
        buffer.append(confirmedMeal, 0, confirmedMeal.length);
        buffer.append(confirmedTotal, 0, confirmedTotal.length);
        buffer.append(ratioInsulin, 0, ratioInsulin.length);
        buffer.append(ratioCarb, 0, ratioCarb.length);
        buffer.append(sensitiveInsulin, 0, sensitiveInsulin.length);
        buffer.append(sensitiveBG, 0, sensitiveBG.length);
        buffer.append(snack);
        buffer.append(mealRaise, 0, mealRaise.length);
        buffer.append(actingTime, 0, actingTime.length);
        buffer.append(offsetTime, 0, offsetTime.length);
        buffer.append(lowerTarget, 0, lowerTarget.length);
        buffer.append(upperTarget, 0, upperTarget.length);
        buffer.append(recommendCorrection, 0, recommendCorrection.length);
        buffer.append(recommendMeal, 0, recommendMeal.length);
        buffer.append(recommendTotal, 0, recommendTotal.length);
        buffer.append(carbSuggestion, 0, carbSuggestion.length);
        buffer.append(currentTarget, 0, currentTarget.length);
        buffer.append(correctionMealIncrease, 0, correctionMealIncrease.length);
        buffer.append(correctionDeltaBG, 0, correctionDeltaBG.length);
        buffer.append(currentAllowedBG, 0, currentAllowedBG.length);
        buffer.append(currentDeltaBG, 0, currentDeltaBG.length);
        buffer.append(activeInsulin, 0, activeInsulin.length);
        buffer.append(healthEvent, 0, healthEvent.length);
        buffer.append(mealTime, 0, mealTime.length);
        buffer.append(basalMDI, 0, basalMDI.length);
        buffer.append(immediateInsulin, 0, immediateInsulin.length);
        buffer.append(delayInsulin, 0, delayInsulin.length);
        buffer.append(delayDuration, 0, delayDuration.length);
        buffer.append(notes, 0, notes.length);
        buffer.append(noteRecordId, 0, noteRecordId.length);
        buffer.append(bolusId, 0, bolusId.length);
        
        crc = CRCTool.getBytes(CRCTool.generateCRC16(buffer.toByteArray()));
        buffer.append(crc, 0, crc.length);
        data = buffer.toByteArray();
        length = ParseUtils.parseInt16(data.length);
        
        buffer.clear();
        
        buffer.append(segmentId, 0, segmentId.length);
        buffer.append(index, 0, index.length);
        buffer.append(length, 0, length.length);
        buffer.append(data, 0, data.length);
        
        return ParseUtils.appendCRC(buffer.toByteArray());
    }
    
    /**
     * Convert the value from SafetyChannel to integer.
     * Return 0, if the input value is null.
     *
     * @param value : The integer value in SafetyChannel type.
     *        Range: A valid object of SafetyChannel<Integer>.
     *        Unit: SafetyChannel<Integer>.
     *        Scaling: 1.
     * 
     * return Integer [out]: The converted value from input SafetyChannel.
     * Return 0 if the input value is null.
     *        Range : -2^31 to (2^31)-1.
     *        Unit : Integer.
     *        Scaling : 1.
     */
    protected Integer getValueFromChannel(SafetyChannel<Integer> value)
    {
        Integer result = 0;
        
        if (null != value)
        {        
            result = CommonUtils.getOriginValue(value.getValueCH1(), value.getValueCH2());
        }
        else
        {
            // Apply to coding standard.
        }
        
        return result;
    }
    
    /**
     * Convert the value from string to integer.
     * Return 0, if the string can't be converted to integer.
     *
     * @param value : The integer value in string type.
     *        Range: A valid object of String.
     *        Unit: String.
     *        Scaling: 1.
     * 
     * return Integer [out]: The converted value from input string.
     * Return 0 if the string can't be converted to integer.
     *        Range : -2^31 to (2^31)-1.
     *        Unit : Integer.
     *        Scaling : 1.
     */
    protected Integer retrieveInteger(String value)
    {
        int result = 0;
        
        if (!LogBookView.EMPTY_COLUMN_VALUE.equalsIgnoreCase(value))
        {
            result = Integer.parseInt(value);
        }
        else
        {
            // Apply to coding standard.
        }
        
        return result;
    }
    
    /**
     * Retrieve the patient record time block data from database according to the id of this table.
     *
     * @param id : The id of target record.
     *        Range : -2^31 to (2^31)-1
     *        Unit : int
     *        Scaling : 1 
     * 
     * return PatientRecordTimeBlockTable [out]: The data structure contains all patient record time block data.
     * Null if the record can't be found.
     *        Range: Valid object of UserSettingTable.
     *        Unit: UserSettingTable.
     *        Scaling: 1.
     */
    protected PatientRecordTimeBlockTable getTimeblock(long time)
    {
        PatientRecordTimeBlockTable result = new PatientRecordTimeBlockTable();
        
        for (IDBData each : mTimeblock)
        {
            SafetyChannel<Long> start = ((PatientRecordTimeBlockTable) each).getStartTime();
            SafetyChannel<Long> end = ((PatientRecordTimeBlockTable) each).getEndTime();
            long startTime = CommonUtils.getOriginValue(start.getValueCH1(), start.getValueCH2());
            long endTime = CommonUtils.getOriginValue(end.getValueCH1(), end.getValueCH2());
            
            if ((startTime < time) && (endTime > time))
            {
                result = (PatientRecordTimeBlockTable) each;
            }
            else
            {
                // Apply to coding standard.
            }
        }
        
        return result;
    }

    /**
     * Transfer the input data to Agent via 10417 callback function.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param data : The required segment data.
     *        Range: Valid object of List.
     *        Unit: List.
     *        Scaling: 1.
     *        
     * return int [out]: The transferred count of segment data.
     *        Range: 0 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */    
    @Override
    public int transferDataToAgent(ContinuaCommandSet commandSet,
            List<? extends AbstractContinuaSegment> data)
    {
        Iterator<? extends AbstractContinuaSegment> iterator = data.iterator();
        int count = 0;
        int transferredSize = 0;
        boolean isFinished = !iterator.hasNext();
        
        List<IDBData> userSettings = new DatabaseModel(UrlType.userSettingUri)
                .queryData(commandSet.getController().getContext(), null);
        
        if (null == userSettings)
        {
            mUserSettings = new UserSettingTable();
        }
        else
        {
            mUserSettings = (UserSettingTable) userSettings.get(0);
        }
        
        mTimeblock = new DatabaseModel(UrlType.patientRecordTimeBlockUri).queryData(
                commandSet.getController().getContext(), null);
        
        if (null == mTimeblock)
        {
            mTimeblock = new LinkedList<IDBData>();
        }
        else
        {
            // Apply to coding standard.
        }
        
        while (!isFinished)
        {
            AbstractContinuaSegment segment = iterator.next();
            SafetyByteArray result = segment.generateBytes();
            
            isFinished = !iterator.hasNext();
            transferredSize += result.getByteArray().length;
            
            Log.i("PatientRecord", "[transferDataToAgent] transferredSize: " + transferredSize);
            
            if (DataTransferHandler.APDU_TX_SIZE > transferredSize)
            {
                commandSet.getController().setSegmentDataOf10417(
                        ContinuaCommand.PM_SEGMENT_ENTRY, result);
                
                count++;
            }
            else
            {
                isFinished = true;
            }
        } 
        
        return count;
    }

    /**
     * Transfer the input segment count data to Agent via 10417 callback function.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param countData : The required segment count data.
     *        Range: Valid object of SafetyByteArray.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     *        
     * return [out]: None.
     */    
    @Override
    public void transferCountToAgent(ContinuaCommandSet commandSet,
            SafetyByteArray countData)
    {
        commandSet.getController().setSegmentDataOf10417(
                ContinuaCommand.PM_SEGMENT_ENTRY_COUNT, countData);
    }

    /**
     * Put the data from database cursor to this object.
     *
     * @param cursor : The cursor contains the query data. Cursor is provided by Android.
     *        Range: Valid object of Cursor.
     *        Unit: Cursor.
     *        Scaling: 1.
     *        
     * see mIndex [in]
     * see mData [in]        
     *        
     * return IDBData [out]: This object with data from cursor.
     *         Range: Valid object of IDBData.
     *         Unit: IDBData.
     *         Scaling: 1.
     */    
    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {   
        PatientRecord data = new PatientRecord();
        
        data.mIndex = cursor.getPosition();
        data.mData = (LogBookView) mData.onQueryDataFromCursor(cursor);
        
        return data;
    }

    /**
     * Return the CRC of this data for checking the data integrity.
     * 
     * see mData [in]
     * 
     * return int [out]: The stored CRC of this data.
     *         Range: CRC 16 value.
     *         Unit: Integer.
     *         Scaling: 1.
     */    
    @Override
    public int getCRC()
    {
        return mData.generateCRC();
    }

    /**
     * Return the CRC which is calculated by all necessary value of this data.
     * 
     * see mData [in]
     * 
     * return int [out]: The calculated CRC of this data
     *         Range: CRC 16 value.
     *         Unit: Integer.
     *         Scaling: 1.
     */    
    @Override
    public int generateCRC()
    {        
        return mData.generateCRC();
    }

    /**
     * Return the select type which queries the record shall with absolute time stamp.
     *
     * return IQuerySelectType [out]: The select type which describes what data shall be queried.
     *         Range: Valid object of IQuerySelectType.
     *         Unit: IQuerySelectType.
     *         Scaling: 1.
     */    
    @Override
    public IQuerySelectType getSelectType()
    {
        return new IQuerySelectType()
        {
            @Override
            public String onSelection()
            {
                return LogBookView.COLUMN_ABSOLUTE_TIME_STAMP + " NOTNULL";
            }

            @Override
            public String[] onSelectionArgs()
            {
                return null;
            }

            @Override
            public String onOrderBy()
            {
                return null;
            }            
        };
    }

    /**
     * Return the URI of patient record table.
     *
     * return Uri [out]: The URI of patient record table.
     *         Range: Refer to UrlType.
     *         Unit: Uri.
     *         Scaling: 1.
     */    
    @Override
    public Uri onUri()
    {
        return UrlType.logBookViewUri;
    }
    
}
