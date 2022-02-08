/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: StringHelper
 * Brief: Provide the interface function of the StringHelper UI component
 *
 * Create Date: 10/16/2015
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: StringHelper.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.content.Context;

public class StringHelper
{
    
    private static final double INSULINUNIT_CONDITION = 10.0;
    private static final double FACTOR = 18.02;

    /**
     * 
     * Function Description
     *
     * @param value
     * @param unitId
     * @param ctx
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatValue(int value, int unitId, Context ctx)
    {
        return String.format(Locale.getDefault(), "%d %s", value,
                ctx.getString(unitId));
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @param ctx
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatMgDl(int value, Context ctx)
    {
        return String.format(Locale.getDefault(), "%d %s", value,
                ctx.getString(R.string.txt_mgdl));
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @param ctx
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatMmoll(int value, Context ctx)
    {
        return String.format(Locale.getDefault(), "%.1f %s",
                ((double) value) / FACTOR, ctx.getString(R.string.txt_mmoll));
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @param ctx
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatCarbsGram(double value, Context ctx)
    {
        String ret = "";

        ret = String.format(Locale.getDefault(), "%.0f %s", value,
                ctx.getString(R.string.txt_carbgrams));

        return ret;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @param ctx
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatInsulinUnit(double value, Context ctx)
    {
        String ret = "";
        if (value >= INSULINUNIT_CONDITION)
        {
            ret = String.format(Locale.getDefault(), "%.1f %s", value,
                    ctx.getString(R.string.txt_insulinunit));
        }
        else
        {
            ret = String.format(Locale.getDefault(), "%.2f %s", value,
                    ctx.getString(R.string.txt_insulinunit));
        }
        return ret;
    }

    /**
     * 
     * Function Description
     *
     * @param hour
     * @param min
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatTime(int hour, int min)
    {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.HOUR_OF_DAY, hour);
        cal.set(Calendar.MINUTE, min);
        Date tmpDate = cal.getTime();
        return new SimpleDateFormat("HH:mm a", Locale.getDefault())
                .format(tmpDate);
    }

    /**
     * 
     * Function Description
     *
     * @param hour
     * @param min
     * @param ctx
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatDuration(int hour, int min, Context ctx)
    {
        DecimalFormat df = new DecimalFormat("00");
        String hours = df.format(hour) + " " + ctx.getString(R.string.txt_hr);
        String minutes = df.format(min) + " "
                + ctx.getString(R.string.txt_minute);
        return hours + "" + minutes;
    }

    /**
     * 
     * Function Description
     *
     * @param day
     * @param month
     * @param year
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatDate(int day, int month, int year)
    {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.YEAR, year);
        cal.set(Calendar.MONTH, month - 1); // Index of month starts with 0
        cal.set(Calendar.DAY_OF_MONTH, day);
        Date tmpDate = cal.getTime();
        String ret = null;

        // return SimpleDateFormat.getDateInstance(SimpleDateFormat.DATE_FIELD,
        // Locale.getDefault()).format(tmpDate);

        String language = Locale.getDefault().getISO3Language();
        boolean equalsARA = language.equals("ara");
        boolean equalsKOR = language.equals("kor");
        boolean equalsJPN = language.equals("jpn");
        boolean equalsCHI = language.equals("chi");
        boolean equalsZHO = language.equals("zho");
        
        if (equalsARA)
        {
            ret = new SimpleDateFormat("yyyy/MM/dd", Locale.getDefault())
                    .format(tmpDate);
        }
        else if (equalsKOR || equalsJPN
                || equalsCHI || equalsZHO)
        {
            // return new SimpleDateFormat("YYYY MMM dd",
            // Locale.getDefault()).format(tmpDate);
            String date2 = "";
            date2 = SimpleDateFormat.getDateInstance(SimpleDateFormat.LONG,
                    Locale.getDefault()).format(tmpDate);
            ret = date2;
        }
        else
        {

            ret =  new SimpleDateFormat("dd MMM yyyy", Locale.getDefault())
                    .format(tmpDate);
        }
        
        return ret;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [GUI] update GUI framework to ClickThrough v0.34
