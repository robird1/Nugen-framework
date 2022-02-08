/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.data.SharedPreferenceProvider
 * Brief: Setting and DB operate step read and write in SharedPreference via
 * content provider to execute CRUD.
 * 
 * Create Date: 2013/12/27
 * $Revision: 20551 $
 * $Author: DWYang $
 * $Id: SharedPreferenceProvider.java 20551 2015-10-01 13:43:53Z DWYang $
 */
package com.accu_chek.solo_m.rcapp.application.setting;

import java.util.List;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.UriMatcher;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;

public class SharedPreferenceProvider extends ContentProvider
{

    private static final String AUTHORITY = NugenFrameworkConstants.SP_AUTHORITY;

    private static final String SETTINGS_SP_NAME = NugenFrameworkConstants.SETTING_DESTINATION;
    private static final String GENERAL_SP_NAME = NugenFrameworkConstants.GENERAL_DESTINATION;
    private static final String PRODICTION_SP_NAME = NugenFrameworkConstants.PRODUCTION_DESTINATION;
    private static final String BASAL_SP_NAME = NugenFrameworkConstants.BASAL_DESTINATION;
    
    private static final String SLASH = "/";
    private static final int MODE = 0;

    private static final int I_SETTINGS = 0;
    private static final int I_SETTING_KEY = 1;

//    private static final int I_CARB_UNIT = 1;
//    private static final int I_BG_UNIT = 2;
//    private static final int I_WAKE_TIME = 3;
//    private static final int I_SLEEP_TIME = 4;
//    private static final int I_WAKE_THRESHOLD_LOW = 5;
//    private static final int I_WAKE_THRESHOLD_HIGH = 6;
//    private static final int I_SLEEP_THRESHOLD_LOW = 7;
//    private static final int I_SLEEP_THRESHOLD_HIGH = 8;
//    private static final int I_SHOW_INSULIN = 9;
//    private static final int I_SHOW_CARBOHYDRATES = 10;
//    private static final int I_SHOW_HEALTH_EVENT = 11;
//    private static final int I_SHOW_HIGH_THRESHOLD = 12;
//    private static final int I_SHOW_LOW_THRESHOLD = 13;
//    private static final int I_TARGET_RANGE_UPPER_VALUE = 14;
//    private static final int I_TARGET_RANGE_LOWER_VALUE = 15;
//    private static final int I_SLEEP_ENABLE = 16;
//    private static final int I_REPEAT_THRESHOLD_WARNING_ENABLED = 17;
//    private static final int I_REPEAT_THRESHOLD_WARNING_TIMED = 18;
//    private static final int I_CALIBRATION_REMINDER_ENABLED = 19;
//    private static final int I_CALIBRATION_REMINDER_TIME = 20;
//    private static final int I_CALIBRATION_REMINDER_TONE = 21;
//    private static final int I_CALIBRATION_RECOMMENDED_ENABLED = 22;
//    private static final int I_CALIBRATION_RECOMMENDED_TONE = 23;
//    private static final int I_SESSION_ENDING_REMINDER_ENABLED = 24;
//    private static final int I_SESSION_ENDING_REMINDER_TIME = 25;
//    private static final int I_SESSION_ENDING_REMINDER_TONE = 26;
//    private static final int I_SETUP_WIZARD_STEP = 27;
//    private static final int I_TARGET_RANGE_UNIT = 28;
//
    private static final int I_GENERAL = 1000;
    private static final int I_GENERAL_KEY = 1001;

    private static final int I_BASAL = 2000;
    private static final int I_BASAL_KEY = 2001;    
    
    private static final int I_PRODUCTION = 3000;
    private static final int I_PRODUCTION_KEY = 3001;
//
//    private static final String CARB_UNIT = SETTINGS_SP_NAME.concat(SLASH)
//            .concat(CGMSettings.CARB_UNIT);
//    private static final String BG_UNIT = SETTINGS_SP_NAME.concat(SLASH)
//            .concat(CGMSettings.BG_UNIT);
//    private static final String TARGET_RANGE_UNIT = SETTINGS_SP_NAME.concat(
//            SLASH).concat(CGMSettings.TARGET_RANGE_UNIT);
//
//    private static final String THRESHOLD_WARNING_SLEEP_START = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.THRESHOLD_WARNING_SLEEP_START);
//    private static final String THRESHOLD_WARNING_SLEEP_END = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.THRESHOLD_WARNING_SLEEP_END);
//    private static final String THRESHOLD_WARNING_WAKE_LOW_VALUE = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.THRESHOLD_WARNING_WAKE_LOW_VALUE);
//    private static final String THRESHOLD_WARNING_WAKE_HIGH_VALUE = SETTINGS_SP_NAME
//            .concat(SLASH)
//            .concat(CGMSettings.THRESHOLD_WARNING_WAKE_HIGH_VALUE);
//    private static final String THRESHOLD_WARNING_SLEEP_LOW_VALUE = SETTINGS_SP_NAME
//            .concat(SLASH)
//            .concat(CGMSettings.THRESHOLD_WARNING_SLEEP_LOW_VALUE);
//    private static final String THRESHOLD_WARNING_SLEEP_HIGH_VALUE = SETTINGS_SP_NAME
//            .concat(SLASH).concat(
//                    CGMSettings.THRESHOLD_WARNING_SLEEP_HIGH_VALUE);
//
//    private static final String GRAPHIC_SHOW_INSULIN = SETTINGS_SP_NAME.concat(
//            SLASH).concat(CGMSettings.GRAPHIC_SHOW_INSULIN);
//    private static final String GRAPHIC_SHOW_CARB = SETTINGS_SP_NAME.concat(
//            SLASH).concat(CGMSettings.GRAPHIC_SHOW_CARB);
//    private static final String GRAPHIC_SHOW_HEALTH_EVENT = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.GRAPHIC_SHOW_HEALTH_EVENT);
//    private static final String GRAPHIC_SHOW_HIGH_THRESHOLD = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.GRAPHIC_SHOW_HIGH_THRESHOLD);
//    private static final String GRAPHIC_SHOW_LOW_THRESHOLD = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.GRAPHIC_SHOW_LOW_THRESHOLD);
//
//    private static final String TARGET_RANGE_UPPER_VALUE = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.TARGET_RANGE_UPPER_VALUE);
//    private static final String TARGET_RANGE_LOWER_VALUE = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.TARGET_RANGE_LOWER_VALUE);
//
//    private static final String THRESHOLD_WARNING_ENABLED = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.THRESHOLD_WARNING_ENABLED);
//
//    private static final String REPEAT_THRESHOLD_WARNING_ENABLED = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.REPEAT_THRESHOLD_WARNING_ENABLED);
//    private static final String REPEAT_THRESHOLD_WARNING_TIME = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.REPEAT_THRESHOLD_WARNING_TIME);
//
//    private static final String CALIBRATION_REMINDER_ENABLED = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.CALIBRATION_REMINDER_ENABLED);
//    private static final String CALIBRATION_REMINDER_TIME = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.CALIBRATION_REMINDER_TIME);
//    private static final String CALIBRATION_REMINDER_TONE = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.CALIBRATION_REMINDER_TONE);
//
//    private static final String CALIBRATION_RECOMMENDED_ENABLED = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.CALIBRATION_RECOMMENDED_ENABLED);
//    private static final String CALIBRATION_RECOMMENDED_TONE = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.CALIBRATION_RECOMMENDED_TONE);
//
//    private static final String SESSION_ENDING_REMINDER_ENABLED = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.SESSION_ENDING_REMINDER_ENABLED);
//    private static final String SESSION_ENDING_REMINDER_TIME = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.SESSION_ENDING_REMINDER_TIME);
//    private static final String SESSION_ENDING_REMINDER_TONE = SETTINGS_SP_NAME
//            .concat(SLASH).concat(CGMSettings.SESSION_ENDING_REMINDER_TONE);
//
//    private static final String SETUP_WIZARD_STEP = SETTINGS_SP_NAME.concat(
//            SLASH).concat(CGMSettings.SETUP_WIZARD_STEP);
//
    private static final String SETTING_KEY = SETTINGS_SP_NAME.concat(SLASH)
            .concat("*");
    private static final String GENERAL_KEY = GENERAL_SP_NAME.concat(SLASH)
            .concat("*");

    private static final String PRODUCTION_KEY = PRODICTION_SP_NAME.concat(SLASH)
            .concat("*");

    private static final String BASAL_KEY = BASAL_SP_NAME.concat(SLASH)
            .concat("*");
    private static final UriMatcher sURIMatcher = new UriMatcher(
            UriMatcher.NO_MATCH);
    static
    {
        sURIMatcher.addURI(AUTHORITY, SETTINGS_SP_NAME, I_SETTINGS);
        sURIMatcher.addURI(AUTHORITY, SETTING_KEY, I_SETTING_KEY);
//        sURIMatcher.addURI(AUTHORITY, CARB_UNIT, I_CARB_UNIT);
//        sURIMatcher.addURI(AUTHORITY, BG_UNIT, I_BG_UNIT);
//        sURIMatcher.addURI(AUTHORITY, TARGET_RANGE_UNIT, I_TARGET_RANGE_UNIT);
//        sURIMatcher.addURI(AUTHORITY, THRESHOLD_WARNING_SLEEP_START,
//                I_SLEEP_TIME);
//        sURIMatcher.addURI(AUTHORITY, THRESHOLD_WARNING_SLEEP_END, I_WAKE_TIME);
//        sURIMatcher.addURI(AUTHORITY, THRESHOLD_WARNING_WAKE_LOW_VALUE,
//                I_WAKE_THRESHOLD_LOW);
//        sURIMatcher.addURI(AUTHORITY, THRESHOLD_WARNING_WAKE_HIGH_VALUE,
//                I_WAKE_THRESHOLD_HIGH);
//        sURIMatcher.addURI(AUTHORITY, THRESHOLD_WARNING_SLEEP_LOW_VALUE,
//                I_SLEEP_THRESHOLD_LOW);
//        sURIMatcher.addURI(AUTHORITY, THRESHOLD_WARNING_SLEEP_HIGH_VALUE,
//                I_SLEEP_THRESHOLD_HIGH);
//        sURIMatcher.addURI(AUTHORITY, GRAPHIC_SHOW_INSULIN, I_SHOW_INSULIN);
//        sURIMatcher.addURI(AUTHORITY, GRAPHIC_SHOW_CARB, I_SHOW_CARBOHYDRATES);
//        sURIMatcher.addURI(AUTHORITY, GRAPHIC_SHOW_HEALTH_EVENT,
//                I_SHOW_HEALTH_EVENT);
//        sURIMatcher.addURI(AUTHORITY, GRAPHIC_SHOW_HIGH_THRESHOLD,
//                I_SHOW_HIGH_THRESHOLD);
//        sURIMatcher.addURI(AUTHORITY, GRAPHIC_SHOW_LOW_THRESHOLD,
//                I_SHOW_LOW_THRESHOLD);
//        sURIMatcher.addURI(AUTHORITY, TARGET_RANGE_UPPER_VALUE,
//                I_TARGET_RANGE_UPPER_VALUE);
//        sURIMatcher.addURI(AUTHORITY, TARGET_RANGE_LOWER_VALUE,
//                I_TARGET_RANGE_LOWER_VALUE);
//        sURIMatcher
//                .addURI(AUTHORITY, THRESHOLD_WARNING_ENABLED, I_SLEEP_ENABLE);
//        sURIMatcher.addURI(AUTHORITY, REPEAT_THRESHOLD_WARNING_ENABLED,
//                I_REPEAT_THRESHOLD_WARNING_ENABLED);
//        sURIMatcher.addURI(AUTHORITY, REPEAT_THRESHOLD_WARNING_TIME,
//                I_REPEAT_THRESHOLD_WARNING_TIMED);
//        sURIMatcher.addURI(AUTHORITY, CALIBRATION_REMINDER_ENABLED,
//                I_CALIBRATION_REMINDER_ENABLED);
//        sURIMatcher.addURI(AUTHORITY, CALIBRATION_REMINDER_TIME,
//                I_CALIBRATION_REMINDER_TIME);
//        sURIMatcher.addURI(AUTHORITY, CALIBRATION_REMINDER_TONE,
//                I_CALIBRATION_REMINDER_TONE);
//        sURIMatcher.addURI(AUTHORITY, CALIBRATION_RECOMMENDED_ENABLED,
//                I_CALIBRATION_RECOMMENDED_ENABLED);
//        sURIMatcher.addURI(AUTHORITY, CALIBRATION_RECOMMENDED_TONE,
//                I_CALIBRATION_RECOMMENDED_TONE);
//        sURIMatcher.addURI(AUTHORITY, SESSION_ENDING_REMINDER_ENABLED,
//                I_SESSION_ENDING_REMINDER_ENABLED);
//        sURIMatcher.addURI(AUTHORITY, SESSION_ENDING_REMINDER_TIME,
//                I_SESSION_ENDING_REMINDER_TIME);
//        sURIMatcher.addURI(AUTHORITY, SESSION_ENDING_REMINDER_TONE,
//                I_SESSION_ENDING_REMINDER_TONE);
//        sURIMatcher.addURI(AUTHORITY, SETUP_WIZARD_STEP, I_SETUP_WIZARD_STEP);

        sURIMatcher.addURI(AUTHORITY, GENERAL_SP_NAME, I_GENERAL);
        sURIMatcher.addURI(AUTHORITY, GENERAL_KEY, I_GENERAL_KEY);
        
        sURIMatcher.addURI(AUTHORITY, PRODICTION_SP_NAME, I_PRODUCTION);
        sURIMatcher.addURI(AUTHORITY, PRODUCTION_KEY, I_PRODUCTION_KEY);

        sURIMatcher.addURI(AUTHORITY, BASAL_SP_NAME, I_BASAL);
        sURIMatcher.addURI(AUTHORITY, BASAL_KEY, I_BASAL_KEY);
        
    }

    private final Cursor EMPTY_CURSOR = new MatrixCursor(new String[] {});

    private SharedPreferences.Editor getSPEditor(String name)
    {
        return getSP(name).edit();
    }

    private SharedPreferences getSP(String name)
    {
        SharedPreferences result = getContext()
                .getSharedPreferences(name, MODE);
        return result;
    }

    /**
     * delete
     * delete the DB
     * 
     * @param uri
     * @param selection
     * @param selectionArgs
     * 
     */
    @Override
    public int delete(Uri uri, String selection, String[] selectionArgs)
    {
        int nMatch = sURIMatcher.match(uri);
        int nGetName = 0;
        int nGetKey = 1;
        int nResult = -1;
        List<String> pathSegments = uri.getPathSegments();

        if (null == pathSegments)
        {
            nResult = 0;
        }
        else
        {
            if ((-1 == nMatch) || (0 == pathSegments.size()))
            {
                nResult = 0;
            }
            else
            {
                String name;
                String nameBack;
                String key;
                Editor editor = null;
                Editor editorBack = null;
                switch (nMatch)
                {
                case I_SETTINGS :
                    // falls through
                case I_GENERAL :
                    name = pathSegments.get(nGetName);
//                    nameBack = name.concat(NugenFrameworkConstants.SP_NAME_BACK_POSTFIX);

                    editor = getSPEditor(name).clear();
//                    editorBack = getSPEditor(nameBack).clear();
                    break;
                default :
                    name = pathSegments.get(nGetName);
//                    nameBack = name.concat(NugenFrameworkConstants.SP_NAME_BACK_POSTFIX);

                    key = pathSegments.get(nGetKey);

                    editor = getSPEditor(name).remove(key);
//                    editorBack = getSPEditor(nameBack).remove(key);
                    break;
                }

//                if (editor.commit() && editorBack.commit())
                if (editor.commit())

                {
                    nResult = 1;
                }
            }

        }
        return nResult;
    }

    /**
     * getType
     * 
     * @param uri
     * 
     */
    @Override
    public String getType(Uri uri)
    {
        return null;
    }

    /**
     * insert function
     * 
     * @param uri
     * @param values
     * 
     */
    @Override
    public Uri insert(Uri uri, ContentValues values)
    {
        Uri reUri = null;
        int nUpdateResult = update(uri, values, null, null);
        
        if (1 == nUpdateResult)
        {
            reUri = uri;
        }
        return reUri;
    }

    /**
     * onCreate
     * Create the DB
     * 
     */
    @Override
    public boolean onCreate()
    {
        return true;
    }

    /**
     * query function
     * 
     * @param uri
     * @param projection
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * 
     */
    @Override
    public Cursor query(Uri uri, String[] projection, String selection,
            String[] selectionArgs, String sortOrder)
    {
        int nUriNotFound = -1;
        int nPathSegmentSize = 2;
        int nGetName = 0;
        int nGetKey = 1;
        MatrixCursor cursor = null;
        List<String> pathSegments = uri.getPathSegments();
        Cursor resultCursor = EMPTY_CURSOR;
        int nIsMatch = sURIMatcher.match(uri);

        if ((nIsMatch == nUriNotFound) || (pathSegments == null)
                || (pathSegments.size() != nPathSegmentSize))
        {
            resultCursor = EMPTY_CURSOR;
        }
        else
        {
            String name = pathSegments.get(nGetName);
            String valueKey = pathSegments.get(nGetKey);
            String crcKey = valueKey.concat(NugenFrameworkConstants.SP_KEY_CHECK_POSTFIX);

//            String nameBack = name.concat(NugenFrameworkConstants.SP_NAME_BACK_POSTFIX);
            SharedPreferences sp = getSP(name);
//            SharedPreferences backupsp = getSP(nameBack);
            
            if (sp != null)
            {
                String result = sp.getString(valueKey, null);
                int nCRC = sp.getInt(crcKey, -1);
                int nGenerateCRCValue = -1;

//                backupsp.getString(valueKey, null);
//                backupsp.getInt(crcKey, -1);

                if (result == null)
                {
                    resultCursor = EMPTY_CURSOR;
                }
                else
                {
                    nGenerateCRCValue = CRCTool.generateCRC16(result.getBytes());
                    if (nGenerateCRCValue != nCRC)
                    {
//                        SafetyBoolean isRollbackResultOK = rollback(name, valueKey,
//                                crcKey);
//
//                        if (isRollbackResultOK.getByte() == SafetyBoolean.TRUE
//                                .getByte())
//                        {
//                            result = sp.getString(valueKey, null);
//                            nCRC = sp.getInt(crcKey, -1);
//                        }
//                        else
//                        {
                            throw new DataIntegrityException();
//                        }

                    }
                    cursor = new MatrixCursor(new String[] { valueKey, crcKey });
                    cursor.addRow(new Object[] { result, nCRC });

                    resultCursor = cursor;
                }
            }
        }
        return resultCursor;
    }

    /**
     * update function
     * 
     * @param uri
     * @param values
     * @param selection
     * @param selectionArgs
     * 
     */
    @Override
    public int update(Uri uri, ContentValues values, String selection,
            String[] selectionArgs)
    {
        int nUpdateCount = -1;
        int nUriNotFound = -1;
        int nPathSegmentSize = 2;
        int nGetName = 0;
        int nGetKey = 1;
        int nIsMatch = 0;

        List<String> pathSegments = uri.getPathSegments();
        String name = "";
        String valueKey = "";
        String crcKey = "";

//        String nameBack = "";

        String value = "";
        int nCRC = -1;
        int nGeneratedCRC = -1;

        
        nIsMatch = sURIMatcher.match(uri);
        
        if (null == pathSegments)
        {
            nUpdateCount = 0;
        }
        else
        {
            if (null == values)
            {
                nUpdateCount = 0;
            }
            else
            {
                if ((nIsMatch == nUriNotFound)
                        || (pathSegments.size() != nPathSegmentSize))
                {
                    nUpdateCount = 0;
                }
                else
                {
                    name = pathSegments.get(nGetName);
                    valueKey = pathSegments.get(nGetKey);
                    crcKey = valueKey.concat(NugenFrameworkConstants.SP_KEY_CHECK_POSTFIX);

//                    nameBack = name.concat(NugenFrameworkConstants.SP_NAME_BACK_POSTFIX);

                    value = values.getAsString(valueKey);
                    nCRC = values.getAsInteger(crcKey);
                    nGeneratedCRC = CRCTool.generateCRC16(value.getBytes());
                    if (nGeneratedCRC != nCRC)
                    {
                        throw new DataIntegrityException();
                    }

                    if (getSPEditor(name).putString(valueKey, value)
                            .putInt(crcKey, nCRC).commit())
                    {
//                        if (getSPEditor(nameBack).putString(valueKey, value)
//                                .putInt(crcKey, nCRC).commit())
//                        {
//                            getContext().getContentResolver().notifyChange(uri,
//                                    null);
//
//                            nUpdateCount = 1;
//                            
//                        }
//                        else
//                        {
//                            SafetyBoolean isRollbackResultOK = rollback(name,
//                                    valueKey, crcKey);
//                            
//                            if (SafetyBoolean.FALSE.getByte() == isRollbackResultOK
//                                    .getByte())
//                            {
//                                throw new DataIntegrityException();
//                            }
//                        }
                        
                        getContext().getContentResolver().notifyChange(uri,
                                null);

                        nUpdateCount = 1;
                        
                        Log.i("SCR0004_time_date", "update OK !!");
                    }
                }
            }
        }

        Log.i("SCR0004_time_date", "nUpdateCount: "+ nUpdateCount);

        return nUpdateCount;
    }

}
/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// update setup wizard according to UI 3.0
// update Setup Wizard according to UI 3.0// [UPDATE]new add header and shared
// preference backup function
// (3411 2014-01-08 06:24:27Z PinwenChen)
// ----------------------------------------------------------------------------
// Restore the sources to revision 3682.
// (3700 2014-01-22 08:38:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]:n/a
// [Comment]:[update]add code comment
// (5164 2014-02-25 06:07:31Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:n/a
// [Comment]:[update]add Brief
// (5254 2014-02-26 07:11:30Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:atos-267
// [Comment]:[update]coding guideline
// (9711 2014-05-16 09:28:39Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix klocwork issue ( Always use brackets in logical expressions,
// even if the operator priority is unambiguous. )
// (14782 2014-08-05 12:00:05Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix klocwork issue.
// (15978 2014-08-19 11:58:15Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-276
// [Comment]:klocwork
// (17425 2014-08-28 11:55:21Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-276
// [Comment]:klocwork
// (17765 2014-09-01 06:06:11Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-276
// [Comment]:klocwork
// (17809 2014-09-01 09:14:23Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18596 2014-09-10 07:56:45Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18615 2014-09-10 09:10:35Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:NA
// [Comment]:[fix]when CRC check error, not throw new DataIntegrityException
// (18749 2014-09-11 11:02:05Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:[update]coding guideline
// (18868 2014-09-12 08:04:12Z PinwenChen)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (18868 2014-09-12 08:04:12Z PinwenChen)
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
// (R21594 2014-10-12 20:03:06 HenryTso)
// ----------------------------------------------------------------------------
// Add Basal TBR sharedpreference operation
