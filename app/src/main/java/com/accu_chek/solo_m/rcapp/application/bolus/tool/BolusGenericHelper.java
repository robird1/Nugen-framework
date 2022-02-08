/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.presentation.bolus.tool.BolusGenericHelper
 * Brief: FIXME
 *
 * Create Date: 077/30/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */
package com.accu_chek.solo_m.rcapp.application.bolus.tool;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;

public class BolusGenericHelper
{    
    private final int VERIFY_TWO = 10;
    private final float FLOAT_FACTOR = 100.0f;
    private final int HOUR_MINUTES = 60;
    
    private static BolusGenericHelper mInstance = null;

//    private int mPatientRecordIdDelivery = 0;
    
    private BolusGenericHelper()
    {
        
    }
    
    public static BolusGenericHelper getInstance()
    {
        if(null == mInstance)
        {
            mInstance = new BolusGenericHelper();
        }
        
        return mInstance;        
    }
    
//    public void savePatientRecordIdAfterDelivery(int id)
//    {
//        mPatientRecordIdDelivery = id;
//    }
//    
//    public void saveBolusIdToPatientRecordAfterPumpAck(Context context, int bolusIdAck)
//    {
//        SafetyNumber<Integer> patientRecordId = this.convertIntegerToSafety(mPatientRecordIdDelivery);
//        SafetyNumber<Integer> bolusId = this.convertIntegerToSafety(bolusIdAck);
//        BolusPatientRecordDatabaseModel.getInstance().updatePatientRecordBolusId(context, patientRecordId, bolusId);
//    }
    
    public SafetyString convertStringToSafety(final String inputString)
    {
        int nCRC16 = CRCTool.generateCRC16(inputString.getBytes());
        SafetyString safetyData = new SafetyString(inputString, nCRC16);
        
        return safetyData;
    }
    
    public SafetyNumber<Integer> convertIntegerToSafety(final Integer inputNumber)
    {
        SafetyNumber<Integer> safetyData = new SafetyNumber<Integer>(inputNumber, -inputNumber);
        
        return safetyData;
    }
    
    public SafetyNumber<Long> convertLongToSafety(final Long inputNumber)
    {
        SafetyNumber<Long> safetyData = new SafetyNumber<Long>(inputNumber, -inputNumber);
        
        return safetyData;
    }
    
    public SafetyChannel<Long> convertLongToSafetyChannel(final Long inputNumber)
    {
        SafetyChannel<Long> result = new SafetyChannel<Long>(CommonUtils.encodeCH1Value(inputNumber), CommonUtils.encodeCH2Value(inputNumber));
        
        return result;
    }    
    
    public SafetyFloat convertFloatToSafety(final Float inputNumber)
    {
        String totalCH2 = null;        
        int verifyTwo = Math.round(inputNumber.floatValue() * FLOAT_FACTOR);
        SafetyFloat finalAmount = null;
        
        verifyTwo = verifyTwo % VERIFY_TWO;
        if (verifyTwo != 0)
        {
            totalCH2 = String.format(Locale.getDefault(), "%.2f", inputNumber.floatValue());
        }
        else
        {
            totalCH2 = String.format(Locale.getDefault(), "%.1f", inputNumber.floatValue());
        }

        finalAmount = new SafetyFloat(Float.valueOf(totalCH2), totalCH2);
        
        return finalAmount;
    }
    
    public SafetyString convertSafetyTimeValueToSafetyTimeString(final SafetyChannel<Long> timeValue, final SafetyString timeFormat)
    {      
        CommonUtils.objectCheck(timeValue, timeFormat);
        
        SimpleDateFormat dateFormat = null;
        Date date = null;
        
        try
        {
            dateFormat = new SimpleDateFormat(timeFormat.getString(), Locale.getDefault());        
            date = new Date(CommonUtils.decodeCH1Value(timeValue.getValueCH1()));            
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace();
        }
        finally
        {

        }
        
        return convertStringToSafety(dateFormat.format(date));
    }
    
    public SafetyString convertSafetyTimeStringToSafetyInsertionString(final SafetyString timeString, final SafetyString timeFormat)
    {      
        CommonUtils.objectCheck(timeString, timeFormat);
        SimpleDateFormat dateFormat = null;
        Date date = null;
        
        try
        {
            dateFormat = new SimpleDateFormat(timeFormat.getString(), Locale.getDefault());        
            date = dateFormat.parse(timeString.getString());
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace();
        }
        catch (ParseException e)
        {
            e.printStackTrace();
        }
        finally
        {

        }
        
        return DatabaseUtil.toSafeInsertionString(date.getTime());
    }
    
    public SafetyNumber<Long> convertSafetyTimeStringToSafetyLong(final SafetyString timeString, final SafetyString timeFormat)
    {      
        CommonUtils.objectCheck(timeString, timeFormat);
        SimpleDateFormat dateFormat = null;
        Date date = null;
        
        try
        {
            dateFormat = new SimpleDateFormat(timeFormat.getString(), Locale.getDefault());        
            date = dateFormat.parse(timeString.getString());
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace();
        }
        catch (ParseException e)
        {
            e.printStackTrace();
        }
        finally
        {

        }
        
        return convertLongToSafety(date.getTime());
    }
    
    public SafetyString convertSafetyIntegerToSafetyInsertionString(final SafetyNumber<Integer> input)
    {      
        CommonUtils.objectCheck(input);
        return DatabaseUtil.toSafeInsertionString(input.get().intValue());
    }
    
    public SafetyString convertSafetyFloatToSafetyInsertionString(final SafetyFloat input)
    {      
        CommonUtils.objectCheck(input);
        int floatToInt = Math.round(input.get().floatValue() * FLOAT_FACTOR);        
        return DatabaseUtil.toSafeInsertionString(floatToInt);
    }
    
    
    
     
    
    public SafetyNumber<Integer> convertSafetyChannelToSafetyNumberInt(final SafetyChannel<Integer> inputNumber)
    {
        Integer i = CommonUtils.decodeCH1Value(inputNumber.getValueCH1());
        SafetyNumber<Integer> safetyData = new SafetyNumber<Integer>(i, -i);
        
        return safetyData;
    }
    
    public SafetyNumber<Long> convertSafetyChannelToSafetyNumberLong(final SafetyChannel<Long> inputNumber)
    {
        Long i = CommonUtils.decodeCH1Value(inputNumber.getValueCH1());
        SafetyNumber<Long> safetyData = new SafetyNumber<Long>(i, -i);
        
        return safetyData;
    }
    
      
    
    
    
    public int convertTimeToMinutes(final int hours, final int minutes)
    {
        int m = (hours * HOUR_MINUTES) + minutes;
        
        return m;
    }  
    
    public int convertMinutesToHours(final int minutes)
    {
        int h = (minutes / HOUR_MINUTES);
        
        return h;
    }
    
    public int convertMinutesWithoutHours(final int minutes)
    {
        int m = (minutes % HOUR_MINUTES);
        
        return m;
    }
}
/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// Update UI screens for Bolus Menu: Custom Bolus
// Update UI screens for Bolus Menu: Custom Bolus
// Update UI screens for Bolus Menu: Custom Bolus
// Merge from branch: Update Bolus Menu and others
// Merge from branch: Update Bolus Menu and others
// 1. Add comments
// 2. Fixed Klockwork issues
// 3. Add default and preference controllers
// 1. Add comments
// 2. Fixed Klockwork issues
// 3. Add default and preference controllers
// 1. Add comments
// 2. Fixed Klockwork issues
// 3. Add default and preference controllers
// 1. Update Bolus default and preference controllers
// 2. Apply Bolus default and preference controllers to Bolus Startup and Settings
// 1. Add Bolus Settings -- timeblocks
// 2. Update all package path for Bolus
// 3. Update Bolus design
// 1. Add Bolus Settings -- timeblocks
// 2. Update all package path for Bolus
// 3. Update Bolus design
// Add file header and footer comment block.
// 1. Move Bolus Controllers to Application package
// 2. Display Bolus SOUP results on UI screens
// 1. Update Bolus Controllers(Grouping)
// 2. Update the relative UI screens with Bolus Controllers
// [NSM-2889]
// 1. Add tools
// 2. fix bug and Kclokwork
// 3. Round the insulin values from Calculator
// [NSM-2889]
// 1. Add accessing time-block database table APIs
// 2. Remove useless files
// [NSM-2889]
// 1. Add accessing time-block database table APIs
// 2. Remove useless files
// [NSM-2889] Rearrange packages
// [NSM-2889] Update Bolus Database APIs
// [NSM-2889] Apply Patient Record storing to UI
// [NSM-2889] Apply Patient Record storing to UI
