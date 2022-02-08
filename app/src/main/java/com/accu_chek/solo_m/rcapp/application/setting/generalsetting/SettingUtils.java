/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SettingUtils
 * Brief: This class provides util functions for accessing setting values.
 * 
 * Create Date: 09/17/2015
 * $Revision: 24198 $
 * $Author: SteveSu $
 * $Id: SettingUtils.java 19544 2015-09-23 09:17:13Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.setting.generalsetting;

import java.util.HashMap;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools.Mode;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

final public class SettingUtils
{
    
    private static final String TAG = SettingUtils.class.getSimpleName();
    
    // This map stores the keys and values defined by configuration matrix
    private static HashMap<String, Object> mMap = new HashMap<String, Object>();
    
    /**
     * This class constructor shall prevent other classes from creating an instance
     * of SettingUtils.
     * 
     * @return None
     */
    private SettingUtils() 
    { 
        // Do nothing. Prevent other classes to create an instance of SettingUtils.
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
     * Convert the input value into the safety type value (SafetyNumber).
     * 
     * @param value : The input value which will be converted into safety type. 
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @return The converted safety value.
     *         Range: valid object
     *         Unit: SafetyNumber<Integer>
     *         Scaling: 1
     */
    public static SafetyNumber<Integer> convertSafeInt(final int value)
    {
        return new SafetyNumber<Integer>(value, -value);
    }
    
    /**
     * Convert the input string into the safety type string (SafetyString).
     * 
     * @param value : The input string which will be converted into safety type. 
     *            Range: valid string object
     *            Unit: String
     *            Scaling: 1
     * 
     * @return The converted safety string.
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     */
    public static SafetyString convertSafeString(final String value)
    {
        return new SafetyString(value, CRCTool.generateCRC16(value.getBytes()));
    }
    
    /**
     * Check whether current system mode is micro pump.
     * 
     * @param context : The activity context. 
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return True if current system mode is micro pump.
     *         Range: true, false
     *         Unit: boolean
     *         Scaling: 1
     */
    public static boolean isPumpMode(Context context)
    {
        String mode = getCurrentMode(context);
        
        Debug.printI(TAG, "current mode: "+ mode);
        Debug.printI(TAG, "isPumpMode: "+ mode.equals(Mode.MicroPump.toString()));

        return mode.equals(Mode.MicroPump.toString());
    }   
        
    /**
     * Check whether current system mode is MDI.
     * 
     * @param context : The activity context. 
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return True if current system mode is MDI.
     *         Range: true, false
     *         Unit: boolean
     *         Scaling: 1
     */
    public static boolean isMDIMode(Context context)
    {
        String mode = getCurrentMode(context);
        
        return mode.equals(Mode.MDI.toString());
    }   
    
    /**
     * Call this API to get the current mode.
     * 
     * @param context : The activity context. 
     *            Range: valid Context object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return Mode string.
     *         Range: valid String object
     *         Unit: String
     *         Scaling: 1
     */
    public static String getCurrentMode(Context context)
    {
        SafetyString defaultMode = new SafetyString(Mode.MicroPump.toString(), CRCTool.generateCRC16(Mode.MicroPump.toString().getBytes()));
        String currentMode = NugenSettingModel.getString(context, CommonConstants.KEY_SYSTEM_MODE, defaultMode).getString();
        return currentMode;
    }

    /**
     * For mapping the values between SafetyBoolean and boolean.
     */
    public enum SafetyBooleanMap
    {
        // enum constant
        True(SafetyBoolean.TRUE, true),
        // enum constant
        False(SafetyBoolean.FALSE, false);        
        
        // SafetyBoolean value of a certain enum constant 
        private SafetyBoolean mSafetyValue = null;
        // boolean value of a certain enum constant
        private boolean mBooleanValue = false;
        
        /**
         * This enum constructor shall initialize the instance variables.
         *          
         * @param safetyValue : The SafetyBoolean value of the enum object.
         *            Range: SafetyBoolean.TRUE / SafetyBoolean.FALSE
         *            Unit: SafetyBoolean
         *            Scaling: 1
         * @param booleanValue : The boolean value of the enum object.
         *            Range: true / false
         *            Unit: boolean
         *            Scaling: 1
         *            
         * @return None
         * 
         * @see mSafetyValue
         * @see mBooleanValue
         */
        SafetyBooleanMap(final SafetyBoolean safetyValue, final boolean booleanValue)
        {
            mSafetyValue = safetyValue;
            mBooleanValue = booleanValue;
        }
        
        /**
         * Get SafetyBoolean value of the current enum object.
         *          
         * @return SafetyBoolean value
         *            Range: SafetyBoolean.TRUE / SafetyBoolean.FALSE
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @see mSafetyValue           
         */
        public SafetyBoolean getSafetyValue()
        {
            return mSafetyValue;
        }
        
        /**
         * Get boolean value of the current enum object.
         *          
         * @return boolean value
         *            Range: true / false
         *            Unit: boolean
         *            Scaling: 1
         *            
         * @see mBooleanValue           
         */
        public boolean getBooleanValue()
        {
            return mBooleanValue;
        }
        
        /**
         * A util function for obtaining a certain enum object from a given SafetyBoolean value.
         *          
         * @param value : The SafetyBoolean value of an enum object.
         *            Range: SafetyBoolean.TRUE / SafetyBoolean.FALSE
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return An enum object.
         *            Range: Valid enum object.
         *            Unit: SafetyBooleanMap
         *            Scaling: 1
         */
        public static SafetyBooleanMap fromSafetyValue(final SafetyBoolean value)
        {
            SafetyBooleanMap map = SafetyBooleanMap.False;
            SafetyBoolean safetyValue = null;
            
            for (SafetyBooleanMap s : SafetyBooleanMap.values())
            {
                safetyValue = s.getSafetyValue();
                
                if (safetyValue == value)
                {
                    map = s;
                }
            }
            
            return map;
        }
        
        /**
         * A util function for obtaining a certain enum object from a given boolean value.
         *          
         * @param value : The boolean value of an enum object.
         *            Range: true / false
         *            Unit: boolean
         *            Scaling: 1
         *            
         * @return An enum object.
         *            Range: Valid enum object.
         *            Unit: SafetyBooleanMap
         *            Scaling: 1
         */
        public static SafetyBooleanMap fromBooleanValue(final boolean value)
        {
            SafetyBooleanMap map = SafetyBooleanMap.False;
            boolean booleanValue = false;
            
            for (SafetyBooleanMap s : SafetyBooleanMap.values())
            {
                booleanValue = s.getBooleanValue();
                
                if (booleanValue == value)
                {
                    map = s;
                }
            }
            
            return map;
        }
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// ----------------------------------------------------------------------------
// update header info
// (R15479 2015-08-26 03:03:05 SteveSu)
// ----------------------------------------------------------------------------
// update footer info
// (R15487 2015-08-26 03:10:56 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] add function comment
// (R16933 2015-09-10 02:23:42 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update code to follow coding rules
// (R19324 2015-09-22 05:47:55 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update code to follow coding rules
// (R19327 2015-09-22 05:59:25 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update code to follow coding rules from Checkstyle
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] 1. add comment for class attributes
// 2. update function comment
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] remove duplicated footer information
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] Add and update comment
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [Settings] update code based on code review findings
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update code to improve performance
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [fixed NSIQ-176]
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [Warning Limits] add screens
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// add function comment
// (R19544 2015-09-23 05:17:13 SteveSu)
// ----------------------------------------------------------------------------
// [Settings] move SettingUtils.class to FrameworkLibrary package
