/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.application.settings.DefaultSettings
 * Brief: Access the default value that stored in Configuration Matrix
 * 
 * Create Date: 2014/1/6
 * $Revision: 20551 $
 * $Author: DWYang $
 * $Id: DefaultSettings.java 20551 2015-10-01 13:43:53Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.setting;

import java.util.HashMap;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;


public class DefaultSettings
{
    private static HashMap<String, Integer> mResourceMap = 
            new HashMap<String, Integer>();

    static int getResourceId(Context context, String key)
    {
        int nId = -1;
        if (mResourceMap.containsKey(key))
        {
            nId = mResourceMap.get(key);
        }
        return nId;
    }
    
    /**
     * Call this API for getting the default minimum percentage
     * value of the Health Events.
     *
     * @return SafetyNumber<Integer> [out] The default minimum percentage of 
     * Health Events.
     * Range: Valid SafetyNumber<Integer> object (Refer to Config Matrix)
     * Unit: SafetyNumber<Integer>
     * Scaling: 1
     */
    public static SafetyNumber<Integer> getHealthEventsPercentageMin()
    {
        SafetyString sKey = new SafetyString(
                ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_MIN,
                CRCTool.generateCRC16(
                        ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_MIN.getBytes()));
        SafetyNumber<Integer> nResult = ReadConfig.getIntegerDataByKey(sKey);
        
        return nResult;
    }
    
    /**
     * Call this API for getting the default maximum percentage
     * value of the Health Events.
     *
     * @return SafetyNumber<Integer> [out] The default maximum percentage of 
     * Health Events.
     * Range: Valid SafetyNumber<Integer> object (Refer to Config Matrix)
     * Unit: SafetyNumber<Integer>
     * Scaling: 1
     */
    public static SafetyNumber<Integer> getHealthEventsPercentageMax()
    {
        SafetyString sKey = new SafetyString(
                ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_MAX,
                CRCTool.generateCRC16(
                        ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_MAX.getBytes()));
        SafetyNumber<Integer> nResult = ReadConfig.getIntegerDataByKey(sKey);
        
        return nResult;
    }
    
    /**
     * Call this API for getting the default percentage
     * value of the Health Events.
     *
     * @return SafetyNumber<Integer> [out] The default percentage of the Health
     * events.
     * Range: Valid SafetyNumber<Integer> object (Refer to Config Matrix)
     * Unit: SafetyNumber<Integer>
     * Scaling: 1
     */
    public static SafetyNumber<Integer> getHealthEventsPercentageDefault()
    {
        SafetyString sKey = new SafetyString(
                ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_DEFAULT,
                CRCTool.generateCRC16(
                        ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_DEFAULT.getBytes()));
        SafetyNumber<Integer> nResult = ReadConfig.getIntegerDataByKey(sKey);
        
        return nResult;
    }
    
    /**
     * Call this API for getting the default resolution
     * value of the Health Events percentage.
     *
     * @return SafetyNumber<Integer> [out] The default percentage resolution of
     * the Health events.
     * Range: Valid SafetyNumber<Integer> object (Refer to Config Matrix)
     * Unit: SafetyNumber<Integer>
     * Scaling: 1
     */
    public static SafetyNumber<Integer> getHealthEventsPercentageResolution()
    {
        SafetyString sKey = new SafetyString(
                ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_RESOLUTION,
                CRCTool.generateCRC16(
                        ConfigParameter.KEY_HEALTHEVENTS_PERCENTAGE_RESOLUTION.getBytes()));
        SafetyNumber<Integer> nResult = ReadConfig.getIntegerDataByKey(sKey);
        
        return nResult;
    }
    
    /**
     * Call this API for getting the default selected item of 
     * the Health Events.
     *
     * @return SafetyNumber<Integer> [out] The Health Event Id
     * Range: Valid SafetyNumber<Integer> object
     * HEALTH_EVENT_NO_VALUE = 0x000F
     * HEALTH_EVENT_EXERCISE1 = 0x0033
     * HEALTH_EVENT_EXECRISE2 = 0x003C
     * HEALTH_EVENT_STRESS = 0x0055
     * HEALTH_EVENT_ILLNESS = 0x005A
     * HEALTH_EVENT_PMS = 0x0066
     * HEALTH_EVETN_CUSTOM1 = 0x0069
     * HEALTH_EVENT_CUSTOM2 = 0x0096
     * HEALTH_EVENT_CUSTOM3 = 0x0099
     * Unit: SafetyNumber<Integer>
     * Scaling: 1
     */
    public static SafetyNumber<Integer> getHealthEventsDefault()
    {
        SafetyString sKey = new SafetyString(
                ConfigParameter.KEY_HEALTHEVENTS_DEFAULT_ITEM,
                CRCTool.generateCRC16(
                        ConfigParameter.KEY_HEALTHEVENTS_DEFAULT_ITEM.getBytes()));
        SafetyNumber<Integer> nResult = ReadConfig.getIntegerDataByKey(sKey);
        
        return nResult;
    }
}
/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// (R21594 2014-10-12 20:03:06 HenryTso)
// ----------------------------------------------------------------------------
// Update for ReadConfig.java change
