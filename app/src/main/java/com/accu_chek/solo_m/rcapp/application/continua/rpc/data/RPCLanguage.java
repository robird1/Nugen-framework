/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCLanguage
 * Brief: 
 *
 * Create Date: 2015/7/20
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: RPCLanguage.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.data;

import java.util.LinkedList;
import java.util.List;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

public enum RPCLanguage
{
    US_ENGLISH(0x0002, ProductionConstants.KEY_SELECTABLE_LANGUAGE_01, HammingDistance.SAFETY_NUMBER_VALUE_0100),
    GERMAN(0x0001, ProductionConstants.KEY_SELECTABLE_LANGUAGE_02, HammingDistance.SAFETY_NUMBER_VALUE_0101),
    GREEK(0x000B, ProductionConstants.KEY_SELECTABLE_LANGUAGE_03, HammingDistance.SAFETY_NUMBER_VALUE_0102),
    ITALIAN(0x0005, ProductionConstants.KEY_SELECTABLE_LANGUAGE_04, HammingDistance.SAFETY_NUMBER_VALUE_0103),
    SIMPLIFIED_CHINESE(0x0015, ProductionConstants.KEY_SELECTABLE_LANGUAGE_05, HammingDistance.SAFETY_NUMBER_VALUE_0104),
    TRADITIONAL_CHINESE(0x0019, ProductionConstants.KEY_SELECTABLE_LANGUAGE_06, HammingDistance.SAFETY_NUMBER_VALUE_0105),
    FRENCH(0x0003, ProductionConstants.KEY_SELECTABLE_LANGUAGE_07, HammingDistance.SAFETY_NUMBER_VALUE_0106),
    POLISH(0x0016, ProductionConstants.KEY_SELECTABLE_LANGUAGE_08, HammingDistance.SAFETY_NUMBER_VALUE_0107),
    ENGLISH(0x0020, ProductionConstants.KEY_SELECTABLE_LANGUAGE_09, HammingDistance.SAFETY_NUMBER_VALUE_0108),
    SPANISH(0x0004, ProductionConstants.KEY_SELECTABLE_LANGUAGE_10, HammingDistance.SAFETY_NUMBER_VALUE_0109),
    BRAZIL_PORTUGUESE(0x0018, ProductionConstants.KEY_SELECTABLE_LANGUAGE_11, HammingDistance.SAFETY_NUMBER_VALUE_0110),
    PORTUGAL_PORTUGUESE(0x000A, ProductionConstants.KEY_SELECTABLE_LANGUAGE_12, HammingDistance.SAFETY_NUMBER_VALUE_0111),
    SWEDISH(0x0007, ProductionConstants.KEY_SELECTABLE_LANGUAGE_13, HammingDistance.SAFETY_NUMBER_VALUE_0112),
    DANISH(0x000F, ProductionConstants.KEY_SELECTABLE_LANGUAGE_14, HammingDistance.SAFETY_NUMBER_VALUE_0113),
    HEBREW(0x0021, ProductionConstants.KEY_SELECTABLE_LANGUAGE_15, HammingDistance.SAFETY_NUMBER_VALUE_0114),
    NORWEGIAN(0x000C, ProductionConstants.KEY_SELECTABLE_LANGUAGE_16, HammingDistance.SAFETY_NUMBER_VALUE_0115),
    SLOVAKIAN(0x0017, ProductionConstants.KEY_SELECTABLE_LANGUAGE_17, HammingDistance.SAFETY_NUMBER_VALUE_0116),
    FINNISH(0x000D, ProductionConstants.KEY_SELECTABLE_LANGUAGE_18, HammingDistance.SAFETY_NUMBER_VALUE_0117),
    SLOVENIAN(0x0012, ProductionConstants.KEY_SELECTABLE_LANGUAGE_19, HammingDistance.SAFETY_NUMBER_VALUE_0118),
    CZECH(0x0010, ProductionConstants.KEY_SELECTABLE_LANGUAGE_20, HammingDistance.SAFETY_NUMBER_VALUE_0119),
    HUNGARIAN(0x0011, ProductionConstants.KEY_SELECTABLE_LANGUAGE_21, HammingDistance.SAFETY_NUMBER_VALUE_0120),
    DUTCH(0x0006, ProductionConstants.KEY_SELECTABLE_LANGUAGE_22, HammingDistance.SAFETY_NUMBER_VALUE_0121),
    RUSSIAN(0x0013, ProductionConstants.KEY_SELECTABLE_LANGUAGE_23, HammingDistance.SAFETY_NUMBER_VALUE_0122),
    JAPANESE_HIRAGANA(0x0022, ProductionConstants.KEY_SELECTABLE_LANGUAGE_24, HammingDistance.SAFETY_NUMBER_VALUE_0123),
    JAPANESE_KANJI(0x0008, ProductionConstants.KEY_SELECTABLE_LANGUAGE_25, HammingDistance.SAFETY_NUMBER_VALUE_0124),
    ARABIC(0x001F, ProductionConstants.KEY_SELECTABLE_LANGUAGE_26, HammingDistance.SAFETY_NUMBER_VALUE_0125),
    KOREAN(0x0014, ProductionConstants.KEY_SELECTABLE_LANGUAGE_27, HammingDistance.SAFETY_NUMBER_VALUE_0126),
    FRENCH_CANADA(0x0023, ProductionConstants.KEY_SELECTABLE_LANGUAGE_28, HammingDistance.SAFETY_NUMBER_VALUE_0127);
    
    /**
     * The unique code of language.
     */
    public final int CODE;
    
    /**
     * The key of language in production model.
     */
    public final String KEY;
    
    /**
     * The key of language in system.
     */
    public final int VALUE;
    
    /**
     * Put the parameter into the enumeration.
     * 
     * @param code : The value of Continua defined.
     *        Range: Refer to the definition of RPCLanguage.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param key : The key value of production model defined.
     *        Range: Valid object of String.
     *        Unit: String.
     *        Scaling: 1.
     * @param value : The code value of device defined.
     *        Range: Valid objecr of String.
     *        Unit: String.
     *        Scaling: 1.
     *        
     * see CODE [in]
     * see KEY [in]
     * see CODE_IN_SYSTEM [in]        
     */
    private RPCLanguage(int code, String key, int value)
    {
        CODE = code;
        KEY = key;
        VALUE = value;
    }
    
    /**
     * Get all supported languages from production model.
     *
     * @param context : The application context.
     *        Range: Valid object of Context 
     *        Unit: Context.
     *        Scaling: 1.
     * 
     * return List<RPCLanguage> [out]: The list contains all supported languages.
     *        Range: Valid object of List<RPCLanguage>.
     *        Unit: List<RPCLanguage>.
     *        Scaling: 1.
     */
    public static List<RPCLanguage> getSupportedLanguages(Context context)
    {
        List<RPCLanguage> result = new LinkedList<RPCLanguage>();
        
        for (RPCLanguage item : RPCLanguage.values())
        {
            SafetyString key = new SafetyString(item.KEY, CRCTool.generateCRC16(item.KEY.getBytes()));
            
            SafetyNumber<Integer> isSupported = NugenProductionModel.getInt(key);
            
            if (HammingDistance.SAFETY_NUMBER_VALUE_0080 == isSupported.get())
            {
                result.add(item);
            }
            else
            {
                // Apply to coding standard.
            }
        }
        
        return result;
    }
    
    /**
     * Return RPCLanguage according to the input id.
     *
     * @param id : The input id of language.
     *        Range: Refer to the definition of RPCLanguage.
     *        Unit: Integer.
     *        Scaling: 1.
     *  
     * return RPCLanguage [out]: The corresponding RPCLanguage.
     *        Range: Valid object of RPCLanguage.
     *        Unit: RPCLanguage.
     *        Scaling: 1.
     *        
     * throws ArgumentErrorException if the input id is not supported.
     */
    public static RPCLanguage getLanguageById(int id) throws ArgumentErrorException
    {
        RPCLanguage result = null;
        
        for (RPCLanguage item : RPCLanguage.values())
        {
            if (item.CODE == id)
            {
                result = item;
            }
        }
        
        if (null == result)
        {
            throw new ArgumentErrorException("This language [" + id 
                    + "] is not supported.");
        }
        
        return result;
    }
    
    /**
     * Return RPCLanguage according to the input key.
     *
     * @param key : The input key of language.
     *        Range: Refer to the definition of RPCLanguage.
     *        Unit: String.
     *        Scaling: 1.
     *  
     * return RPCLanguage [out]: The corresponding RPCLanguage.
     *        Range: Valid object of RPCLanguage.
     *        Unit: RPCLanguage.
     *        Scaling: 1.
     *        
     * throws ArgumentErrorException if the input id is not supported.
     */
    public static RPCLanguage getLanguageByKey(String key) throws ArgumentErrorException
    {
        RPCLanguage result = null;
        
        for (RPCLanguage item : RPCLanguage.values())
        {
            if (item.KEY.equalsIgnoreCase(key))
            {
                result = item;
            }
        }
        
        if (null == result)
        {
            throw new ArgumentErrorException("This language [" + key 
                    + "] is not supported.");
        }
        
        return result;
    }
}
// (R20933 2015-10-05 04:34:57 kevenwu)
// ----------------------------------------------------------------------------
// Refine for production parameter.
