/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.application.settings.SharedPreferenceModel
 * Brief: Model class of SharedPreference for set and get the preference data.
 * 
 * Create Date: 2013/09/01
 * $Revision: 22322 $
 * $Author: WilliyChiang $
 * $Id: SharedPreferenceModel.java 22322 2015-10-22 08:10:30Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.setting;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.text.TextUtils;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.FixPointFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;


public class SharedPreferenceModel
{
    private final Uri mUri;

    public SharedPreferenceModel(Uri uri)
    {
        mUri = uri;
    }

    SafetyBoolean delete(Context context, String key)
    {
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;

        if (null != context)
        {
            int nDeleteResult = context.getContentResolver().delete(
                    Uri.withAppendedPath(mUri, key), null, null);

            if (nDeleteResult == 1)
            {
                isResultOK = SafetyBoolean.TRUE;
            }
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }

        return isResultOK;
    }

    SafetyBoolean deleteAll(Context context)
    {
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;

        if (null != context)
        {
            int nDeleteResult = context.getContentResolver().delete(mUri, null,
                    null);

            if (nDeleteResult == 1)
            {
                isResultOK = SafetyBoolean.TRUE;
            }
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }
        return isResultOK;
    }

    public void setString(Context context, String key, SafetyString value)
    {
        if (null != context)
        {
            if (value != null)
            {
                set(context, key, value);
            }
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }
    }

    public void setSafetyBoolean(Context context, String key,
            SafetyBoolean isValueTrue)
    {
        if (null != context)
        {
            final int BIT_MASK_FF = 0xFF;
            if ((isValueTrue != null)
                    && ((isValueTrue.getByte() == SafetyBoolean.TRUE.getByte()) 
                            || (isValueTrue.getByte() == SafetyBoolean.FALSE.getByte())))
            {
                String originalValue = String.valueOf(Integer.valueOf(isValueTrue
                        .getByte() & BIT_MASK_FF));
                String diverseValue = String.valueOf(Integer.valueOf(isValueTrue
                        .getByte() & BIT_MASK_FF));

                int nCRCValue = CRCTool.generateCRC16(diverseValue.getBytes());

                set(context, key, new SafetyString(originalValue, nCRCValue));
            }
            else
            {
                throw new DataIntegrityException();
            }
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }

    }

    void setLong(Context context, String key, SafetyNumber<Long> value)
    {
        if (null != context)
        {
            String originalValue = String.valueOf(value.getOriginal());
            String diverseValue = String.valueOf(-value.getDiverse());

            int nCRCValue = CRCTool.generateCRC16(diverseValue.getBytes());

            set(context, key, new SafetyString(originalValue, nCRCValue));
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }

    }

    public void setInt(Context context, String key, SafetyNumber<Integer> value)
    {
        if (null != context)
        {
            String originalValue = String.valueOf(value.getOriginal());
            String diverseValue = String.valueOf(-value.getDiverse());

            int nCRCValue = CRCTool.generateCRC16(diverseValue.getBytes());

            set(context, key, new SafetyString(originalValue, nCRCValue));
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }

    }

    void setFloat(Context context, String key, SafetyFloat value)
    {
        if (null != context)
        {
            String originalValue = String.valueOf(value.getOriginal());
            String diverseValue = value.getDiverse();

            int nCRCValue = CRCTool.generateCRC16(diverseValue.getBytes());

            set(context, key, new SafetyString(originalValue, nCRCValue));
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }

    }

    public SafetyString getString(Context context, String key,
            SafetyString defaultValue)
    {
        SafetyString result = null;

        if (null != context)
        {
            result = get(context, key);

            if (result == null)
            {
                result = defaultValue;
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }

        return result;
    }

    public SafetyBoolean getSafetyBoolean(Context context, String key,
            SafetyBoolean isResultOKDefault)
    {
        SafetyBoolean isResultOK = isResultOKDefault;

        if (null != context)
        {
            SafetyString strResult = get(context, key);
            byte nResult = -1;
            String safetyString = "";
            boolean isEmpty = false;

            if (strResult == null)// || (true == isEmpty))
            {
                isResultOK = isResultOKDefault;
            }
            else
            {
                safetyString = strResult.getString();
                isEmpty = TextUtils.isEmpty(safetyString);
                
                if (true == isEmpty)
                {
                    isResultOK = isResultOKDefault;
                }
                else
                {
                    nResult = (byte) Integer.parseInt(strResult.getString());
                }
            }

            if (nResult != -1)
            {
                isResultOK = SafetyBoolean.valueOf(nResult);

                if (null == isResultOK)
                {
                    throw new DataIntegrityException(
                            "no such enum item of SafetyBoolean with byte value ["
                                    + nResult + "]");
                }
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }

        return isResultOK;
    }

    SafetyNumber<Long> getLong(Context context, String key,
            SafetyNumber<Long> defaultValue)
    {
        SafetyNumber<Long> result = defaultValue;

        if (null != context)
        {
            SafetyString strResult = get(context, key);

            if ((strResult == null)
                    || (TextUtils.isEmpty(strResult.getString())))
            {
                result = defaultValue;
            }
            else
            {
                long nValue = Long.valueOf(strResult.getString());

                result = new SafetyNumber<Long>(nValue, -nValue);

                // data integrity check
                {
                    int nOriginalCRC = strResult.getCRC();
                    int nComparedCRC = CRCTool.generateCRC16(String.valueOf(
                            result.get()).getBytes());

                    if (nOriginalCRC != nComparedCRC)
                    {
                        throw new DataIntegrityException();
                    }
                }
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }
        return result;
    }

    public SafetyNumber<Integer> getInt(Context context, String key,
            SafetyNumber<Integer> defaultValue)
    {
        SafetyNumber<Integer> result = defaultValue;

        if (null != context)
        {
            SafetyString strResult = get(context, key);

            if ((strResult == null)
                    || (TextUtils.isEmpty(strResult.getString())))
            {
                result = defaultValue;
            }
            else
            {
                int nValue = Integer.valueOf(strResult.getString());

                result = new SafetyNumber<Integer>(nValue, -nValue);

                // data integrity check
                {
                    int nOriginalCRC = strResult.getCRC();
                    int nComparedCRC = CRCTool.generateCRC16(String.valueOf(
                            result.get()).getBytes());

                    if (nOriginalCRC != nComparedCRC)
                    {
                        throw new DataIntegrityException();
                    }
                }
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }

        return result;
    }

    public SafetyFloat getFloat(Context context, String key, SafetyFloat defaultValue)
    {
        SafetyFloat result = defaultValue;

        if (null != context)
        {
            SafetyString strResult = get(context, key);

            if ((strResult == null)
                    || (TextUtils.isEmpty(strResult.getString())))
            {
                result = defaultValue;
            }
            else
            {
                float floatValue = Float.valueOf(strResult.getString());

                result = new SafetyFloat(floatValue, String.valueOf(floatValue));
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }
        return result;
    }

    public FixPointFloat getFixPoint(Context context, String key,
            FixPointFloat defaultValue)
    {
        FixPointFloat result = defaultValue;

        if (null != context)
        {
            SafetyString strResult = get(context, key);

            if ((strResult == null)
                    || (TextUtils.isEmpty(strResult.getString())))
            {
                result = defaultValue;
            }
            else
            {
                int nValue = Integer.valueOf(strResult.getString());

                result = new FixPointFloat(nValue, -nValue);

                // data integrity check
                {
                    int nOriginalCRC = strResult.getCRC();
                    int nComparedCRC = CRCTool.generateCRC16(String.valueOf(
                            result.get()).getBytes());

                    if (nOriginalCRC != nComparedCRC)
                    {
                        throw new DataIntegrityException();
                    }
                }
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }

        return result;
    }
    
    private void set(Context context, String key, SafetyString value)
    {
        if (null != context)
        {
            Uri uri = Uri.withAppendedPath(mUri, key);

            String crcKey = key.concat(NugenFrameworkConstants.SP_KEY_CHECK_POSTFIX);

            ContentValues cv = new ContentValues();
            cv.put(key, value.getString());
            cv.put(crcKey, value.getCRC());

            int updateCount = context.getContentResolver().update(uri, cv, null, null);

            
            Log.i("SCR0004_time_date", "context.getContentResolver().update()");
            Log.i("SCR0004_time_date", "updateCount: "+ updateCount);

            // read after write
//            get(context, key);
        }
        else
        {
            throw new IllegalArgumentException("context is null");
        }
    }
    
    private SafetyString get(Context context, String key)
    {
        SafetyString result = null;

        if (null != context)
        {
            Uri uri = Uri.withAppendedPath(mUri, key);
            Cursor cursor = null;
            try
            {
                cursor = context.getContentResolver().query(uri, null, null,
                        null, null);
                
                if (cursor != null && cursor.moveToFirst())
                {
                    // 0 means the first column; 1 means the second column
                    String value = cursor.getString(0);
                    int nCRC = cursor.getInt(1);

                    result = new SafetyString(value, nCRC);
                    
                }
                else
                {
                    result = getDefault(context, key);
                    
                }
            }
            catch (Exception exception)
            {
                exception.printStackTrace();
                
                throw new DataIntegrityException();
            }
            finally
            {
                if (cursor != null)
                {
                    cursor.close();
                }
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }

        return result;
    }

    private SafetyString getDefault(Context context, String key)
    {
        SafetyString result = null;

        if (null != context)
        {
            int nResourceId = DefaultSettings.getResourceId(context, key);
            if (nResourceId != -1)
            {
                String strValue = context.getResources().getString(nResourceId);
                int nCRCValue = CRCTool.generateCRC16(strValue.getBytes());

                result = new SafetyString(strValue, nCRCValue);
            }
        }
        else
        {
            throw new IllegalArgumentException();
        }

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
// Fix compile warnings.
// (4147 2014-02-07 05:54:23Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: fix cursor may not closed problem
// [JIRA-ID]: N/A
// [Comment]: update header footer comments
// (5225 2014-02-26 02:42:07Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-281
// [Comment]: Log File for Spec 4.0
// (6398 2014-03-21 12:55:37Z ChristinaJiang)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-300
// [Comment]:implement the set SharedPreference with read after write
// (6448 2014-03-24 07:34:18Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:atos-360
// [Comment]:[update]Only records Settings change in logfile
// (7831 2014-04-14 09:08:37Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:n/a
// [Comment]:[update]remove unused code
// (10821 2014-06-09 11:54:01Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-555
// [Comment]: Implements the Fix Point method to replace the floating point
// calculation and Safety for the bG, Insulin, Carbs data processing.
// (12708 2014-07-08 03:33:54Z TerryHsieh)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix klocwork issue ( Always use brackets in logical expressions,
// even if the operator priority is unambiguous. )
// (15941 2014-08-19 10:24:45Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-276
// [Comment]:klocwork
// (17472 2014-08-29 01:36:25Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: fix klocwork issues
// (17495 2014-08-29 03:20:20Z StanleyWu)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-276
// [Comment]:klocwork
// (17666 2014-08-29 10:40:27Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: fix klocwork issues -- add BIT_MASK_FF = 0xFF
// (17783 2014-09-01 08:01:10Z StanleyWu)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18595 2014-09-10 07:56:18Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18673 2014-09-11 07:16:50Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:NA
// [Comment]:[fix]when CRC check error, not throw new DataIntegrityException
// (18748 2014-09-11 11:01:50Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18902 2014-09-12 10:11:36Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (18902 2014-09-12 10:11:36Z PinwenChen)
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
// (21594 2014-10-13 00:03:06Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix Klocwork issues.
// (21596 2014-10-13 00:48:34Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix Klocwork issues.
