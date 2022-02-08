/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.util.CommonUtils
 * Brief:
 *
 * Create Date: 2015/5/5
 * $Revision: 23049 $
 * $Author: VictorChen $
 * $Id: CommonUtils.java 23049 2015-11-02 12:40:34Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.util;

import java.io.File;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.NoSuchElementException;

import android.content.Context;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.os.SystemClock;
import android.text.format.DateFormat;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.SettingConstants;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrix;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.VibrationManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.VibrationManager.VibrationType;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.DataTypeMismatchException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.FixPointFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.generalsetting.Sound;
import com.accu_chek.solo_m.rcapp.application.setting.generalsetting.TouchFeedback;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class CommonUtils
{

    /**
     * 
     */
    private static final String TAG = CommonUtils.class.getSimpleName();
    private static final int MAX_VOLUME = 100;

    // This map stores the keys and values defined by configuration matrix
    private static HashMap<String, Object> mMap = new HashMap<String, Object>();

    /* Unit mgdL */
    private static String mMGDL = null;
    /* Unit mmol */
    private static String mMMOL = null;
    /* Min mgdL value */
    private static final int MGDL_MIN = 0;
    /* Max mgdL value (This threshold must be multiplied by 100 first) */
    private static final int MGDL_MAX = 99900;
    /* Min mmol value */
    private static final int MMOL_MIN = 0;
    /* Max mmol value */
    private static final int MMOL_MAX = 3330;
    /* Convert factor to convert from mgdL to mmol */
    private static final float BG_CONVERT_FACTOR = 18.02f;
    /*
     * 
     */
    private static final int FIX_POINT_BG_CONVERT_FACTOR = FixPointFloat.FIX_POINT_FACTOR
            .multiply(new BigDecimal(BG_CONVERT_FACTOR)).intValue();
    /* Integer 2 */
    private static final int HALF = 2;
    /* Integer 16 */
    private static final int RADIX = 16;
    /* Integer 4 */
    private static final int SHIFT_BIT = 4;
    /* AND logic */
    private static final int AND_LOGIC_VALUE = 0x0f;
    /* Integer 2 */
    private static final int MULTIPLE_CONST = 2;

    private static final int MMOL = 0x0F;
    private static final int MGDL = 0x33;
    
    /**
     * Media player
     * Range: Valid MediaPlayer
     * Unit: MediaPlayer
     * Scaling: 1
     */
    private static MediaPlayer mPlayer = null;

    /**
     * Call this function to perform sleep time.
     * 
     * param nTime [in] The unit of time is milliseconds.
     * Range: > 0
     * Unit: ms
     * Scaling: 1
     * return void [out] None
     */
    public static void sleep(long nTime)
    {
        // check parameter range
        if (0 > nTime)
        {
            throw new DataIntegrityException();
        }
            
        SystemClock.sleep(nTime);
    }

    /**
     * Given the context to check the current bG unit is equals to MMOL or not
     * 
     * @param context [in] Current context object
     * Range: Valid object
     * Unit: Context
     * Scaling: 1
     * 
     * @return SafeBolean [out] Return SafeTrue if the current bG unit is mmol/L
     * Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     * Unit: SafetyBoolean
     * Scaling: 1
     */
    public static SafetyBoolean isMMOL(Context context)
    {
        SafetyBoolean ret = SafetyBoolean.TRUE;
        if (context != null)
        {
            MeterParameterMatrix mpm = MeterParameterMatrix
                    .getMeterParameterMatrixInstance();
            String bGUnit = "bGMeasurementDisplayUnits";
            SafetyString safetybGUnit = new SafetyString(bGUnit,
                    CRCTool.generateCRC16(bGUnit.getBytes()));
            SafetyNumber<Integer> returnbGValue = null;
            try
            {
                returnbGValue = mpm.getParameterInteger(safetybGUnit);
            }
            catch (DataIntegrityException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (NoSuchElementException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (DataTypeMismatchException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            int returnValue = returnbGValue.get();

            if (returnValue == MMOL)
            {
                ret = SafetyBoolean.TRUE;
            }
            else if (returnValue == MGDL)
            {
                ret = SafetyBoolean.FALSE;
            }
            else
            {
                // Empty for static analysis
            }
        }
        else
        {
            ret = SafetyBoolean.FALSE;
        }
        return ret;
    }

    /**
     * Return the current date with the given time string (24 hour format)
     * 
     * param time [in] the non-null time string in 24 hour format with
     * colon(HH:mm)
     * Range: Valid object
     * Unit: String
     * Unit: 1
     * 
     * return Date [out] Return the current date with given time string
     */
    public static Date getCurrentDateWithTime(String time)
    {
        Calendar calendar = Calendar.getInstance();
        String[] temp = time.split(":");

        calendar.set(Calendar.HOUR_OF_DAY, Integer.parseInt(temp[0]));
        calendar.set(Calendar.MINUTE, Integer.parseInt(temp[1]));
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        return calendar.getTime();
    }

    /**
     * Convert the given time string (24 hour format) to the system time format
     * string.
     * 
     * param context [in] The non-null current running app context
     * Range: Valid object
     * Unit: Context
     * Scaling: 1
     * 
     * param time [in] the non-null time string in 24 hour format with colon
     * (HH:mm)
     * Range: Valid object
     * Unit: String
     * Scaling: 1
     * 
     * return String [out] Return the given time string to the system time
     * format string.
     * Range: Valid object
     * Unit: String
     * Scaling: 1
     */
    public static String parseSystemTimeFormat(Context context, String time)
    {
        String result = null;

        // Parameter range check
        objectCheck(context);

        result = parseSystemTimeFormat(context, time, 0);

        return result;
    }

    /**
     * Convert the given time string (24 hour format) to the system time format
     * string with adjust minutes.
     * 
     * param context [in] The non-null current context object
     * Range: Valid object
     * Unit: Context
     * Scaling: 1
     * 
     * param time [in] the non-null time string in 24 hour format with
     * colon(HH:mm)
     * Range: Valid object
     * Unit: String
     * Scaling: 1
     * 
     * param nAdjust [in] adjust the given time for minutes
     * Range:
     * Unit: integer
     * Scaling: 1
     * 
     * return String [out] Return the given time string to the system time
     * format string with adjust minutes.
     * Range: Valid object
     * Unit: String
     * Scaling: 1
     */
    public static String parseSystemTimeFormat(Context context, String time,
            int nAdjust)
    {
        String sResult = null;
        Boolean timeContain = time.contains(":");

        // Parameter range check
        objectCheck(context);

        if (timeContain == false)
        {
            sResult = time;
        }
        else
        {
            String[] temp = time.split(":");
            Calendar calendar = Calendar.getInstance();
            SimpleDateFormat formatter = null;
            boolean is24HourFormat = DateFormat.is24HourFormat(context);

            calendar.set(Calendar.HOUR_OF_DAY, Integer.parseInt(temp[0]));
            calendar.set(Calendar.MINUTE, Integer.parseInt(temp[1]) + nAdjust);

            if (is24HourFormat)
            {
                formatter = new SimpleDateFormat("HH:mm");
            }
            else
            {
                formatter = new SimpleDateFormat("hh:mm a", Locale.US);
            }
            sResult = formatter.format(calendar.getTime()).toLowerCase();
        }
        return sResult;
    }

    /**
     * Call this function to get the dd MMM YYYY date format.
     * 
     * @param context [in]Current context object
     * Range: Valid object
     * Unit: Context
     * Scaling: 1
     * 
     * @param nMillisecond [in]Date with millisecond format.
     * Range: > 0
     * Unit: ms
     * Scaling: 1
     * 
     * @return String [out]"dd MMM YYYY" format with specified MONTH string.
     * Range: Valid object
     * Unit: String
     * Scaling: 1
     */
    public static String getFormatDate(Context context, long nMillisecond)
    {
        SimpleDateFormat dateFormatter = null;
        Calendar calender = Calendar.getInstance();
        Date date = null;
        String month = null;
        int nDay = 0;
        int nYear = 0;
        String sDate = "";
        String[] monthList = null;

        monthList = context.getResources().getStringArray(R.array.month_array);
        calender.setTimeInMillis(nMillisecond);
        date = calender.getTime();
        if (null != monthList)
        {
            month = monthList[calender.get(Calendar.MONTH)];
            nDay = calender.get(Calendar.DAY_OF_MONTH);
            nYear = calender.get(Calendar.YEAR);
            sDate = String.format("%02d %s %04d", nDay, month, nYear);
        }
        else
        {
            dateFormatter = new SimpleDateFormat("dd MMM yyyy", Locale.ENGLISH);
            sDate = dateFormatter.format(date);
        }
        return sDate;
    }
    
    /**
     * Call this for getting the clock format code (Hamming value)
     *
     * @param context [in] Current context object
     * Range: Valid Context object
     * Unit: Context
     * Scaling: 1
     * @return SafetyNumber<Integer> [out] Current clock format
     * Range: TIME_24H = 0x33, TIME_12H = 0x0F
     * Unit: int
     * Scaling: 1
     */
    public static SafetyNumber<Integer> getClockFormat(Context context)
    {
        SafetyString sKey = new SafetyString(ConfigParameter.KEY_TIME_FORMAT, 
                CRCTool.generateCRC16(ConfigParameter.KEY_TIME_FORMAT.getBytes()));
        SafetyNumber<Integer> nClockFormatId = NugenSettingModel.getInt(context,
                SettingConstants.KEY_CLOCK_FORMAT_ID, 
                ReadConfig.getIntegerDataByKey(sKey));
        
        return nClockFormatId;
    }

    /**
     * Call this for getting the clock format display string
     *
     * @param context [in] Current context object
     * Range: Valid Context object
     * Unit: Context
     * Scaling: 1
     * @return String [out] String of current clock format
     * Range: Valid String object.
     * Unit: String
     * Scaling: 1
     */
    public static String getClockDisplayFormat(Context context)
    {
        SafetyNumber<Integer> nClockFormatId = getClockFormat(context);
        String sFormat = "";
            
        if (nClockFormatId.get() == CommonConstants.TIME_12H)
        {
            sFormat = context.getResources().getString(R.string.txt_12ampm);
        }
        else
        {    
            sFormat = context.getResources().getString(R.string.txt_24h);
        }
        
        return sFormat;
    }
    
    /**
     * Call this function to get the time string that is related to the clock format
     *
     * @param context [in]Current context object
     * Range: Valid object
     * Unit: Context
     * Scaling: 1
     * @param nMillisecond [in]Date with millisecond format.
     * Range: > 0
     * Unit: ms
     * Scaling: 1
     * 
     * @return SafetyString [out] String of the converted time
     * Range: Valid object
     * Unit: String
     * Scaling: 1
     */
    public static SafetyString getFormatTime(Context context, long nMillisecond)
    {
        return getFormatTime(context, nMillisecond, getClockFormat(context));
    }
    
    /**
     * Call this function to get the formatted time to display on UI screen.
     * 
     * @param context : The activity context.
     *            Range: Valid object
     *            Unit: Context
     *            Scaling: 1
     * @param nMillisecond : Date with millisecond format.
     *            Range: -2^63 to (2^63)-1
     *            Unit: long
     *            Scaling: 1
     * @param nClockFormatId : Safety clock format. 
     *            Range: valid object
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1
     *            
     * @return Safety string of the converted time.
     *         Range: Valid object
     *         Unit: SafetyString
     *         Scaling: 1
     */
    public static SafetyString getFormatTime(Context context, long nMillisecond, SafetyNumber<Integer> nClockFormatId)
    {
        SimpleDateFormat formatter = null;
        Calendar calendar = Calendar.getInstance();
        String sTime = "";
        
        calendar.setTimeInMillis(nMillisecond);
        if (nClockFormatId.get() == CommonConstants.TIME_12H)
        {
            formatter = new SimpleDateFormat("hh:mm a", Locale.US);
        }
        else
        {
            formatter = new SimpleDateFormat("HH:mm");
        }
        sTime = formatter.format(calendar.getTime()).toLowerCase();
        return new SafetyString(sTime, CRCTool.generateCRC16(sTime.getBytes()));
    }


    /**
     * Given the Year, Month and day value for converting to 
     * display format of the Date.
     *
     * @param context [in]Current context object
     * Range: Valid context object
     * Unit: Context
     * Scaling: 1
     * @param nYear
     * 
     * @param nMonth
     * Range: 0 ~ 11
     * Unit: Integer
     * Scaling: 1
     * @param nDay
     * Range: 1 ~ 31
     * Unit: Integer
     * Scaling: 1
     * @return SafetyString [out] SafetyString object of the display date
     * Range: Valid SafetyString object
     * Unit: SafetyString
     * Scaling: 1
     */
    public static SafetyString getFormatDate(Context context,
            SafetyNumber<Integer> nYear, SafetyNumber<Integer> nMonth,
            SafetyNumber<Integer> nDay)
    {
        SafetyString sResult = null;
        String[] monthList = context.getResources().getStringArray(
                R.array.month_array);
        String month = null;
        String sDate = "";
        
        month = monthList[nMonth.get()];
        sDate = String.format("%02d %s %04d", nDay.get(), month, nYear.get());
        sResult = new SafetyString(sDate, CRCTool.generateCRC16(sDate
                .getBytes()));
        return sResult;
    }
    
    /**
     * Convert bG value from mg/dL to mmol/L unit.
     * This function will do the conversion twice in diverse way, and return the
     * result by SafetyNumber if the conversion results are same. Otherwise it
     * will throw the DataIntegrityException.
     * 
     * param value [in] the mg/dL value that need to be converted to mmol/L
     * Range: low bG limit to high bG limit
     * Unit: mg/dL
     * Scaling: 1
     * 
     * return SafetyNumber<Double, Double> [out] the bG value with mmol/L unit.
     * Range: low bG limit to high bG limit
     * Unit: mmol
     * Scaling: 1
     */
    public static FixPointFloat convertMgdlToMmol(FixPointFloat value)
    {
        FixPointFloat result = null;
        int nOriginalResult = 0;
        int nDiverseResult = 0;

        // Parameter range check
        objectCheck(value);

        // range check
        int nOriginalValue = value.get();

        if ((nOriginalValue < MGDL_MIN) || (nOriginalValue > MGDL_MAX))
        {
            throw new IllegalArgumentException(
                    "The passed in value must between " + MGDL_MIN + " and "
                            + MGDL_MAX);
        }
        else
        {
            // Empty for static analysis
        }

        // convert twice and compare the result
        nOriginalResult = convertMgdlToMmol1(value.getOriginal());
        nDiverseResult = convertMgdlToMmol2(value.getDiverse());

        result = new FixPointFloat(nOriginalResult, nDiverseResult);
        return result;
    }

    /**
     * Convert bG value from mg/dL to mmol/L by divide bg value by convert
     * factor.
     * 
     * param nMGDLValue [in] Given the bG value with mg/dL for the conversion.
     * Range: low bG limit to high bG limit
     * Unit: mg/dL
     * Scaling: 1
     * 
     * return double [out] The bG value with mmol/L unit
     * Range: low bG limit to high bG limit
     * Unit: mmol
     * Scaling: 1
     */
    private static int convertMgdlToMmol1(int nMGDLValue)
    {
        int nResult = 0;
        BigDecimal factor = null;
        BigDecimal mmolValue = null;

        checkValue(nMGDLValue);

        factor = new BigDecimal(FIX_POINT_BG_CONVERT_FACTOR);
        mmolValue = new BigDecimal(nMGDLValue).multiply(
                FixPointFloat.FIX_POINT_FACTOR).divide(factor, 0,
                BigDecimal.ROUND_HALF_UP);

        nResult = mmolValue.intValue();

        return nResult;
    }

    /**
     * Convert bG value from mg/dL to mmol/L by multiplying the bG value by 2.
     * Then divided by the convert factor and then divided by 2.
     * 
     * param nMGDLValue [in] Given the bG value with mg/dL for the conversion.
     * Range: low bG limit to high bG limit
     * Unit: mg/dL
     * Scaling: 1
     * 
     * return double [out] The bG value with mmol/L unit.
     * Range: low bG limit to high bG limit
     * Unit: mmol
     * Scaling: 1
     */
    private static int convertMgdlToMmol2(int nMGDLValue)
    {
        int nResult = 0;
        BigDecimal difference = new BigDecimal(2);
        BigDecimal factor = null;
        BigDecimal mmolValue = null;

        checkDiverseValue(nMGDLValue);

        factor = new BigDecimal(FIX_POINT_BG_CONVERT_FACTOR);
        mmolValue = new BigDecimal(nMGDLValue).multiply(difference)
                .multiply(FixPointFloat.FIX_POINT_FACTOR).divide(difference)
                .divide(factor, 0, BigDecimal.ROUND_HALF_UP);

        nResult = mmolValue.intValue();

        return nResult;
    }

    /**
     * Convert bG value from mmol/L to mg/dL unit.
     * This function will do the conversion twice in diverse way, and return the
     * result by SafetyNumber if the conversion results are the same. Otherwise
     * it
     * will throw the DataIntegrityException.
     * 
     * param value [in] the mmol/L value that need to be converted to mg/dL
     * Range: Low bG limit to high bG limit
     * Unit: mmol
     * Scaling: 1
     * 
     * return SafetyNumber<Integer, Integer> [out] the bG value with mg/dL
     * unit.
     * Range: Low bG limit to high bG limit
     * Unit: mg/dL
     * Scaling: 1
     */
    public static FixPointFloat convertMmolToMgdl(FixPointFloat value)
    {
        FixPointFloat result = null;
        int nOriginalResult = 0;
        int nDiverseResult = 0;

        // Parameter range check
        objectCheck(value);

        // range check
        int nOriginalValue = value.get();

        if ((nOriginalValue < MMOL_MIN) || (nOriginalValue > MMOL_MAX))
        {
            throw new IllegalArgumentException(
                    "The passed in value must between " + MMOL_MIN + " and "
                            + MMOL_MAX);
        }
        else
        {
            // Empty for static analysis
        }

        // convert twice and compare the result
        nOriginalResult = convertMmolToMgdl1(value.getOriginal());
        nDiverseResult = convertMmolToMgdl2(value.getDiverse());

        result = new FixPointFloat(nOriginalResult, nDiverseResult);

        return result;
    }

    /**
     * Convert bG value from mmol/L to mg/dL by multiplying the bG value by the
     * convert factor.
     * 
     * param nValue [in] the mmol/L value that need to be converted to mg/dL
     * Range: Low bG limit to high bG limit
     * Unit: mmol
     * Scaling: 1
     * return int [out] the bG value with mg/dL unit.
     * Range: Low bG limit to high bG limit
     * Unit: mg/dL
     * Scaling: 1
     */
    private static int convertMmolToMgdl1(int nValue)
    {
        int nResult = 0;

        // Parameter range check
        checkValue(nValue);

        nResult = new BigDecimal(nValue)
                .multiply(new BigDecimal(BG_CONVERT_FACTOR))
                .setScale(0, BigDecimal.ROUND_HALF_UP).intValue();

        return nResult;
    }

    /**
     * Convert bG value from mmol/L to mg/dL by multiplying the bG value by 2.
     * Then multiplied by the convert factor and the divided by 2.
     * 
     * param nValue [in] the mmol/L value that need to be converted to mg/dL
     * Range: Low bG limit to high bG limit
     * Unit: mmol
     * Scaling: 1
     * 
     * return int [out] the bG value with mg/dL unit.
     * Range: Low bG limit to high bG limit
     * Unit: mg/dL
     * Scaling: 1
     */
    private static int convertMmolToMgdl2(int nValue)
    {
        final int BIG_DECIMAL_VALUE = 2;
        BigDecimal difference = null;
        BigDecimal factor = null;

        checkDiverseValue(nValue);

        difference = new BigDecimal(BIG_DECIMAL_VALUE);
        factor = new BigDecimal(BG_CONVERT_FACTOR);

        int nResult = new BigDecimal(nValue).multiply(difference)
                .multiply(factor)
                .divide(difference, 0, BigDecimal.ROUND_HALF_UP).intValue();

        return nResult;
    }

    /**
     * To check the input value.
     * 
     * param value [in] the input value
     * Range: >= 0
     * Unit: integer
     * Scaling: 1
     */
    private static void checkValue(int value)
    {
        // Check parameter range
        if (0 > value)
        {
            throw new RuntimeException();
        }
        // End check
    }
    
    /**
     * To check the input diverse value.
     *
     * param value [in] the input diverse value
     * Range: < 0
     * Unit: integer
     * Scaling: 1
     * return void [out] 
     */
    private static void checkDiverseValue(int value)
    {
        // Check parameter range
        if (0 < value)
        {
            throw new RuntimeException();
        }
        // End check
    }

    /**
     * Given the current time calendar object to compare with given session
     * start time. If the same date then return TRUE, otherwise return FALSE
     * 
     * param sessionStart [in]Calendar object of the session start time.
     * param currentTime [in]Calendar object of the current time.
     * 
     * return SafetyBoolean [out]Return TRUE when the current time is same as
     * session start date and return FALSE when is not the same.
     */
    private static SafetyBoolean isEqualToSameDate(Calendar dateA,
            Calendar dateB)
    {
        SafetyBoolean isResultOK = SafetyBoolean.TRUE;
        boolean isYearEqual = dateB.get(Calendar.YEAR) == dateA
                .get(Calendar.YEAR);
        boolean isMonthEqual = dateB.get(Calendar.MONTH) == dateA
                .get(Calendar.MONTH);
        boolean isDayEqual = dateB.get(Calendar.DAY_OF_MONTH) == dateA
                .get(Calendar.DAY_OF_MONTH);
        boolean isEqual = (isYearEqual && isMonthEqual && isDayEqual);

        if (isEqual)
        {
            isResultOK = SafetyBoolean.TRUE;
        }
        else
        {
            isResultOK = SafetyBoolean.FALSE;
        }
        return isResultOK;
    }

    /**
     * Check whether the input arguments are valid or not.
     * 
     * param args: input arguments.
     * Range:
     * Unit: object
     * Scaling: 1
     */
    public static void objectCheck(Object... args)
    {
        for (Object arg : args)
        {
            if (arg == null)
            {
                throw new NullPointerException(
                        "One of the input arguments is null.");
            }
        }
    }

    /**
     * 
     * param originValue
     * return
     */
    public static long encodeCH1Value(long originValue)
    {
        return originValue * (-1) - 1;
    }

    /**
     * 
     * param originValue
     * return
     */
    public static long encodeCH2Value(long originValue)
    {
        return originValue * 2 + 1;
    }

    /**
     * 
     * param channelValue
     * return
     */
    public static long decodeCH1Value(long channelValue)
    {
        return (channelValue + 1) / (-1);
    }
    /**
     * 
     * param channelValue
     * return
     */
    public static long decodeCH2Value(long channelValue)
    {
        return (channelValue - 1) / (2);
    }

    /**
     * 
     * param originValue
     * return
     */
    public static int encodeCH1Value(int originValue)
    {
        long value = encodeCH1Value((long) originValue);

        int channelValue = new BigDecimal(value).intValueExact();

        return channelValue;
    }

    /**
     * 
     * param originValue
     * return
     */
    public static int encodeCH2Value(int originValue)
    {
        long value = encodeCH2Value((long) originValue);

        int channelValue = new BigDecimal(value).intValueExact();

        return channelValue;
    }

    /**
     * 
     * param channelValue
     * return
     */
    public static int decodeCH1Value(int channelValue)
    {
        long value = decodeCH1Value((long) channelValue);

        int originValue = new BigDecimal(value).intValueExact();

        return originValue;
    }
    /**
     * 
     * Function Description
     *
     * @param channelValue
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public static int decodeCH2Value(int channelValue)
    {
        long value = decodeCH2Value((long) channelValue);

        int originValue = new BigDecimal(value).intValueExact();

        return originValue;
    }
    /**
     * Return true if the safety comparison of the two channel values is no
     * problem.
     * 
     * param value1: channel 1 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * param value2: channel 2 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return SafetyBoolean: return true if the 2 channel comparison is OK.
     * Range: SafetyBoolean.TRUE / SafetyBoolean.FALSE
     * Unit: SafetyBoolean
     * Scaling: 1
     */
    public static SafetyBoolean compareTwoChannel(long value1, long value2)
    {
        SafetyBoolean result = SafetyBoolean.FALSE;
        final long CHANNEL_SUM = -1L;

        long sum = value1 * 2 + value2;

        if (sum == CHANNEL_SUM)
        {
            result = SafetyBoolean.TRUE;
        }
        else
        {
            result = SafetyBoolean.FALSE;
        }

        return result;
    }

    /**
     * Compare the 2 channel values and return the original value if the
     * comparison is OK. Throw DataIntegrityException if the comparison is
     * failed.
     * 
     * param value_CH1 : channel 1 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * param value_CH2 : channel 2 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return long : the original value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * @throw DataIntegrityException: throw this exception if the 2 channel
     *        comparison is failed.
     * 
     */
    public static long getOriginValue(long value_CH1, long value_CH2)
    {
        long value = 0;
        SafetyBoolean isResultOk = CommonUtils.compareTwoChannel(value_CH1,
                value_CH2);

        if (isResultOk.getByte() == SafetyBoolean.TRUE.getByte())
        {
            value = CommonUtils.decodeCH1Value(value_CH1);
        }
        else
        {
            throw new DataIntegrityException();
        }

        return value;
    }

    /**
     * Compare the 2 channel values and return the original value if the
     * comparison is OK. Throw DataIntegrityException if the comparison is
     * failed.
     * 
     * param value_CH1 : channel 1 value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * param value_CH2 : channel 2 value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * return int : the original value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     */
    public static int getOriginValue(int value_CH1, int value_CH2)
    {
        long value = getOriginValue((long) value_CH1, (long) value_CH2);

        int oriValue = new BigDecimal(value).intValueExact();

        return oriValue;
    }

    /**
     * 
     * param oriValue
     * return
     */
    public static SafetyChannel<Long> getSafetyChannel(long originValue)
    {
        long value1 = CommonUtils.encodeCH1Value(originValue);
        long value2 = CommonUtils.encodeCH2Value(originValue);
        SafetyChannel<Long> channel = new SafetyChannel<Long>(value1, value2);

        return channel;
    }

    /**
     * 
     * param oriValue
     * return
     */
    public static SafetyChannel<Integer> getSafetyChannel(int originValue)
    {
        int value1 = CommonUtils.encodeCH1Value(originValue);
        int value2 = CommonUtils.encodeCH2Value(originValue);
        SafetyChannel<Integer> channel = new SafetyChannel<Integer>(value1,
                value2);

        return channel;
    }

    /**
     * Convert a float value to an integer by a scaling factor.
     * 
     * param value : the converted value.
     * Range: the range which can be represented by a float value
     * Unit: float
     * Scaling: 1
     * 
     * return int : the scaling value.
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 10^2
     * 
     */
    public static int scaleFloatToInt(float value)
    {
        FixPointFloat fixFloat = new FixPointFloat(value, String.valueOf(value));
        int scalingValue = fixFloat.getOriginal();

        return scalingValue;
    }

    /**
     * Restore a scaling integer to original float value.
     * 
     * param value : the scaling integer value.
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 10^2
     * 
     * return float : the original float value.
     * Range: the range which can be represented by a float value
     * Unit: float
     * Scaling: 1
     * 
     */
    public static float resoreIntToFloat(int value)
    {
        FixPointFloat fixFloat = new FixPointFloat(value, -value);
        float oriValue = fixFloat.getOriginFloat();

        return oriValue;
    }
    
    /**
     * 
     * Play a sound
     * 
     * return None
     * 
     * @param soundPath: path of the sound file
     *            Range: Valid String
     *            Unit: String
     *            Scaling: 1
     * 
     * @param context: the Context to use
     *            Range: Valid Context
     *            Unit: Context
     *            Scaling: 1
     */
    public static void playSound(String soundPath, Context context)
    {
        Debug.printI(TAG, "[playSound] enter");
        OnCompletionListener listener = null;
        
        // Null object check
        CommonUtils.objectCheck(soundPath);

        CommonUtils.objectCheck(context);
        
        // Check whether the sound file is existed or not
        checkSoundDirectory(soundPath);

        mPlayer = MediaPlayer.create(context, Uri.parse(soundPath));

        if (mPlayer != null)
        {

            float volume = getPlayerVolume(context);
            
            listener = new PlayerComplete();

            mPlayer.setVolume(volume, volume);
            
            mPlayer.setOnCompletionListener(listener);

            mPlayer.start();
        }
        else
        {
            // Sound is not playable
            errorHandler();
        }  
    }
    
    /**
     * Get the volume for setting MediaPlayer.
     * 
     * @param context: The activity context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return The MediaPlayer volume.
     *            Range: 0.0 ~ 1.0
     *            Unit: float
     *            Scaling: 1
     */
    public static float getPlayerVolume(final Context context)
    {
        int value = -1;
        final int soundMode = getInt(context, ConfigParameter.KEY_SIGNALIZATION_MODE);
        
        if (soundMode == Sound.Loud.getCode())
        {
            value = getInt(context, ConfigParameter.KEY_VOLUME_LOUD);
        }
        else if (soundMode == Sound.Normal.getCode())
        {
            value = getInt(context, ConfigParameter.KEY_VOLUME_NORMAL);
        }
        else if (soundMode == Sound.Quiet.getCode())
        {
            value = getInt(context, ConfigParameter.KEY_VOLUME_QUIET);
        }
        else if (soundMode == Sound.Vibrate.getCode())
        {
            value = getInt(context, ConfigParameter.KEY_VOLUME_VIBRATE);
        }
        else 
        {
            value = getInt(context, ConfigParameter.KEY_VOLUME_NORMAL);
        }
        
        return toVolumeScalar(value);
    }
    
    /**
     * Create a MediaPlayer instance for playing sound.
     * 
     * @param context: The activity context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param soundPath: The path from which to get the datasource.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     *            
     * @return None
     * 
     * @see mPlayer
     */
    public static void createPlayer(final Context context, final String soundPath)
    {
        Debug.printI(TAG, "[createPlayer] enter");
        
        CommonUtils.objectCheck(context, soundPath);
        
        // Check whether the sound file is existed or not
        checkSoundDirectory(soundPath);

        mPlayer = MediaPlayer.create(context, Uri.parse(soundPath));
    }
    
    /**
     * Invoke MediaPlayer instance to play sound.
     * 
     * @param context: The activity context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param isRepeat: Set the player to be looping or non-looping.
     *            Range: true, false
     *            Unit: boolean
     *            Scaling: 1
     *            
     * @return None
     */
    public static void playSound(final Context context, final boolean isRepeat)
    {
        playSound(getPlayerVolume(context), isRepeat);
    }
    
    /**
     * Invoke MediaPlayer instance to play sound.
     * 
     * @param volume: The volume of this player.
     *            Range: 0.0 ~ 1.0
     *            Unit: float
     *            Scaling: 1
     * @param isRepeat: Set the player to be looping or non-looping.
     *            Range: true, false
     *            Unit: boolean
     *            Scaling: 1
     *            
     * @return None
     * 
     * @see mPlayer           
     */
    public static void playSound(final float volume, final boolean isRepeat)
    {
        Debug.printI(TAG, "[playSound] enter");
        Debug.printI(TAG, "mPlayer = " + mPlayer);
        if (mPlayer != null)
        {
            mPlayer.setLooping(isRepeat);
            mPlayer.setVolume(volume, volume);
            mPlayer.start();
        }
    }

    /**
     * Pauses playback. Call start() to resume.
     *            
     * @return None
     * 
     * @see mPlayer           
     */
    public static void pausePlaySound()
    {
        Debug.printI(TAG, "[pausePlaySound] enter");
        Debug.printI(TAG, "mPlayer = " + mPlayer);
        if (mPlayer != null)
        {
            mPlayer.pause();
        }
    }
    
    /**
     * Stops playback after playback has been stopped or paused and releases
     * resources associated with this MediaPlayer object.
     * 
     * @return None
     * 
     * @see mPlayer
     */
    public static void releasePlayer()
    {
        Debug.printI(TAG, "[releasePlayer] enter");
        Debug.printI(TAG, "mPlayer = " + mPlayer);
        if (mPlayer != null)
        {
            mPlayer.stop();
            mPlayer.release();
            mPlayer = null;
        }
    }
    
    /**
     * Vibrate meter based on a given vibration type.
     * 
     * @param vibrateType: The vibration type.
     *            Range: Defined in the interface VibrationType.
     *            Unit: int
     *            Scaling: 1
     *            
     * @return None
     */
    public static void vibrate(final int vibrateType)
    {
        final VibrationManager manager = (VibrationManager) ICustomizedHWManager.getSystemService(ICustomizedHWManager.VIBRATION_SERVICE);
        
        manager.play(vibrateType);
    }
    
    /**
     * Vibrate meter when user selects "Vibration" or "Tone & Vibration" as the screen feedback.
     * 
     * @param context: The activity context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return None
     */
    public static void vibrateTouch(final Context context)
    {
        final SafetyNumber<Integer> defaultValue = new SafetyNumber<Integer>(TouchFeedback.NONE.getCode(), -TouchFeedback.NONE.getCode());
        final int touchFeedBack = NugenSettingModel.getInt(context, ConfigParameter.KEY_TOUCHSCREEN, defaultValue).get();
        final int vibration = TouchFeedback.VIBRATION.getCode();
        final int toneAndVibration = TouchFeedback.TONE_VIBRATE.getCode();
        
        if (touchFeedBack == vibration || touchFeedBack == toneAndVibration)
        {
            vibrate(VibrationType.TOUCH_FEEDBACK);
        }
    }
    
    /**
     * Get the integer value from the configuration matrix by the given key.
     * 
     * @param key : The key to access the configuration matrix.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     *            
     * @return The value defined by the configuration matrix.
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     */
    public static int getCMInt(final String key)
    {
        final SafetyString cmKey = new SafetyString(key, CRCTool.generateCRC16(key
                .getBytes()));

        return ReadConfig.getIntegerDataByKey(cmKey).get();
    }

    /**
     * Get the string value from the configuration matrix by the given key.
     * 
     * @param key : The key to access the configuration matrix.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     *            
     * @return The value defined by the configuration matrix.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     */
    public static String getCMString(final String key)
    {
        final SafetyString cmKey = new SafetyString(key, CRCTool.generateCRC16(key
                .getBytes()));

        return ReadConfig.getStringDataByKey(cmKey).getString();
    }

    /**
     * Get the integer value from the share preference by the given key. If
     * there is no value stored then return the default value defined by the
     * configuration matrix.
     * 
     * @param context : Current activity context. 
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param key : The key to access the share preference.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * 
     * @return The stored integer value.
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     *         
     * @see mMap        
     */
    public static int getInt(final Context context, final String key)
    {
        final int defaultValue;
        final boolean isValueExist = mMap.containsKey(key);
        
        if (!isValueExist)
        {
            mMap.put(key, getCMInt(key));
        }
        
        defaultValue = (Integer) mMap.get(key);
        
        return NugenSettingModel.getInt(context, key,
                new SafetyNumber<Integer>(defaultValue, -defaultValue)).get();
    }

    /**
     * Get the string value from the share preference by the given key. If
     * there is no value stored then return the default value defined by the
     * configuration matrix.
     * 
     * @param context : Current activity context. 
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param key : The key to access the share preference.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * 
     * @return The stored string value.
     *         Range: valid object
     *         Unit: String
     *         Scaling: 1
     *         
     * @see mMap        
     */
    public static String getString(final Context context, final String key)
    {
        final SafetyString defaultValue;
        final boolean isValueExist = mMap.containsKey(key);
        
        if (!isValueExist)
        {
            mMap.put(key, getCMString(key));
        }

        defaultValue = new SafetyString((String) mMap.get(key),
                CRCTool.generateCRC16(((String) mMap.get(key)).getBytes()));

        return NugenSettingModel.getString(context, key, defaultValue).getString();
    }

    /**
     * 
     * Check whether the sound file exist or not.
     * If the file exists then return the path, else call the error-handler.
     * 
     * return String [out]: path of a sound file
     * Range: Valid String
     * Unit: String
     * Scaling: 1
     * 
     * @param soundPath :path of the sound file
     *            Range: Valid String
     *            Unit: String
     *            Scaling: 1
     */
    protected static String checkSoundDirectory(String soundPath)
    {
        Debug.printI(TAG, "[loadSound] enter");
        String returnedPath = null;
        File fileDirectory = null;
        boolean isExist = false;

        CommonUtils.objectCheck(soundPath);
        fileDirectory = new File(soundPath);
        isExist = fileDirectory.exists();

        if (isExist == true)
        {
            // Sound file is existed
            returnedPath = soundPath;
        }
        else
        {
            // Sound file is not existed
            errorHandler();
        }

        return returnedPath;
    }
    
    /**
     * 
     * Error Handler for CommonUtils
     * 
     * return None
     */
    protected static void errorHandler()
    {
        // Show EMWR: EMW47801

        NotifyMessage message = new NotifyMessage(EMWRList.EMW47801);

        NotifyProxy.showEMWR(message);
    }
    
    /**
     * 
     * Convert the sound volume got from share preference to volume scalar to set the
     * volume of media player.
     * 
     * @param value : Sound volume got from share preference.
     *            Range: 0 ~ 100
     *            Unit: int
     *            Scaling: 1
     *            
     * @return Volume scalar to set the volume of media player. 
     *            Range: 0.0 ~ 1.0
     *            Unit: float
     *            Scaling: 1
     */
    private static float toVolumeScalar(int value)
    {
        return value / (float) MAX_VOLUME;
    }

    
    private static final class PlayerComplete implements
            MediaPlayer.OnCompletionListener
    {
        /**
         * Called when the end of a media source is reached during playback.
         * 
         * return None
         * 
         * @param mp: Not used in this override method
         *            Range: null, valid object
         *            Unit: MediaPlayer
         *            Scaling: 1
         * 
         * @see mPlayer
         */
        @Override
        public void onCompletion(MediaPlayer mp)
        {
            CommonUtils.mPlayer.stop();
            CommonUtils.mPlayer.release();
        }
    }

}
// Update for ReadConfig.java change
// (R15017 2015-08-19 03:21:28 JacksonHuang)
// ----------------------------------------------------------------------------
// 1. Move loadSound() & playSound() to CommonUtils.java
// 2. Add errorHandler() to CommonUtils.java
// (R15555 2015-08-27 00:05:52 IvanHuang)
// ----------------------------------------------------------------------------
// [Setting] add functionality for SCR0362_picker_sound
// (R18089 2015-09-16 05:46:50 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] fix - vibrate when clicking next button, back button, home button
// (R18355 2015-09-18 03:12:35 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] fix the issue of set volume
// (R18383 2015-09-18 05:52:22 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update media player code
// (R18977 2015-09-21 01:52:11 SteveSu)
// ----------------------------------------------------------------------------
// Update APIs of CommonUtils for playing sound
// (R19046 2015-09-21 05:30:37 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] add and update function comment
// (R19284 2015-09-22 03:33:47 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update code to follow coding rules
// (R20555 2015-10-01 09:50:22 DWYang)
// ----------------------------------------------------------------------------
// [Setting] RCSWSPUI33.5, RCSWSPUI33.8
