/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.application.settings.CGMGeneralModel
 * Brief: This class is handle the access of general data that saved in shared
 * preference.
 * 
 * Create Date: 2013/12/25
 * $Revision: 20551 $
 * $Author: DWYang $
 * $Id: NugenGeneralModel.java 20551 2015-10-01 13:43:53Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.setting;

import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

/**
 * @author HenryTso
 * 
 */
public class NugenGeneralModel
{

    private static final Uri GENERAL_URI = Uri.withAppendedPath(
            NugenFrameworkConstants.SP_URI, NugenFrameworkConstants.GENERAL_DESTINATION);

    private static final SharedPreferenceModel mModel = 
            new SharedPreferenceModel(GENERAL_URI);

    public static SafetyBoolean delete(Context context, String key)
    {
        SafetyBoolean isResult = mModel.delete(context, key);
        
        return isResult;
    }

    public static SafetyBoolean deleteAll(Context context)
    {
        SafetyBoolean isResult = mModel.deleteAll(context);
        
        return isResult;
    }

    public static void setString(Context context, String key, SafetyString value)
    {
        mModel.setString(context, key, value);
    }

    public static void setSafetyBoolean(Context context, String key,
            SafetyBoolean isOKValue)
    {
        mModel.setSafetyBoolean(context, key, isOKValue);
    }

    public static void setLong(Context context, String key,
            SafetyNumber<Long> value)
    {
        mModel.setLong(context, key, value);
    }

    public static void setInt(Context context, String key,
            SafetyNumber<Integer> value)
    {
        mModel.setInt(context, key, value);
    }

    public static SafetyString getString(Context context, String key)
    {
        SafetyString result = mModel.getString(context, key, null);
        return result;
    }

    public static SafetyString getString(Context context, String key,
            SafetyString defaultValue)
    {
        SafetyString result = mModel.getString(context, key, defaultValue);
        return result;
    }

    public static SafetyBoolean getSafetyBoolean(Context context, String key)
    {
        SafetyBoolean isResultOK = mModel.getSafetyBoolean(context, key,
                SafetyBoolean.FALSE);
        
        return isResultOK;
    }

    public static SafetyBoolean getSafetyBoolean(Context context, String key,
            SafetyBoolean isResultOKDefault)
    {
        SafetyBoolean isResultOK = mModel.getSafetyBoolean(context, key,
                isResultOKDefault);
        
        return isResultOK;
    }

    public static SafetyNumber<Long> getLong(Context context, String key)
    {
        SafetyNumber<Long> result = mModel.getLong(context, key, null);
        return result;
    }

    public static SafetyNumber<Long> getLong(Context context, String key,
            SafetyNumber<Long> defaultValue)
    {
        SafetyNumber<Long> result = mModel.getLong(context, key, defaultValue);
        return result;
    }

    public static SafetyNumber<Integer> getInt(Context context, String key)
    {
        SafetyNumber<Integer> result = mModel.getInt(context, key, null);
        return result;
    }

    public static SafetyNumber<Integer> getInt(Context context, String key,
            SafetyNumber<Integer> defaultValue)
    {
        SafetyNumber<Integer> result = mModel
                .getInt(context, key, defaultValue);
        return result;
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Restore the sources to revision 3682.
// [JIRA-ID]: N/A
// [Comment]: Add file header and footer.
// (4914 2014-02-20 05:13:11Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: update header footer comments
// (10744 2014-06-06 06:44:52Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-555
// [Comment]: Implements the Fix Point method to replace the floating point
// calculation and Safety for the bG, Insulin, Carbs data processing.
// (10890 2014-06-10 12:41:03Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix Klocwork issues (Remove or comment out the never used
// functions)
// (13995 2014-07-29 01:22:33Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-272
// [Comment]: Fix eclipse compile warning
// (14456 2014-07-31 03:06:19Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18586 2014-09-10 06:54:10Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18673 2014-09-11 07:16:50Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18870 2014-09-12 08:04:52Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (18870 2014-09-12 08:04:52Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Preform KEYWORD function of SVN for each source files. No Source
// code content will be changed
// (21021 2014-10-03 02:34:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (21467 2014-10-07 14:22:36Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline - Fix rule R17, R88, R100, R102, etc.
