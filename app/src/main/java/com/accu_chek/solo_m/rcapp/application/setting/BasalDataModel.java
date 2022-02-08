/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.setting.SafetyChannelSettings
 * Brief: SharedPreference funvtion to deal SafetyChannel Data
 *
 * Create Date: 07/09/2015
 * $Revision: 20551 $ 1
 * $Author: DWYang $ Jackson Huang
 * $Id: BasalDataModel.java 20551 2015-10-01 13:43:53Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.setting;

import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

public class BasalDataModel
{    
    // Uri for ContentResolver
    private static final Uri TBR_URI = Uri.withAppendedPath(
            NugenFrameworkConstants.SP_URI, NugenFrameworkConstants.BASAL_DESTINATION);
    
    // String for Concatenation
    public static final String KEY_CH1_POSTFIX = "_ch1";
    public static final String KEY_CH2_POSTFIX = "_ch2";    
    
    // SharedPreference access
    private static final SharedPreferenceModel mModel = 
            new SharedPreferenceModel(TBR_URI);
    
    /**
     * Delete all contents in this category of SharedPreference 
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *                                              
     * @return SafetyBoolean [out]
     * 
     *         The result of deleting all contents of this category
     *         
     *         SafetyBoolean.TRUE  -- Success
     *         SafetyBoolean.FALSE -- Failure
     *         
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     */    
    public static SafetyBoolean deleteAll(Context context)
    {
        SafetyBoolean isResultOK = mModel.deleteAll(context);

        return isResultOK;
    }

    /**
     * Set string into this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param value [in] SafetyString
     * 
     *        Data for insertion
     *        
     *        Range: Valid SafetyString
     *        Unit: SafetyString
     *        Scaling: 1
     *                                              
     * @return None
     *         
     */    
    public static void setString(Context context, String key, SafetyString value)
    {
        mModel.setString(context, key, value);
    }

    /**
     * Set boolean into this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param value [in] SafetyBoolean
     * 
     *        Data for insertion
     *        
     *        Range: Valid SafetyBoolean
     *        Unit: SafetyBoolean
     *        Scaling: 1
     *                                              
     * @return None
     *         
     */    
    public static void setSafetyBoolean(Context context, String key,
            SafetyBoolean isTrue)
    {
        mModel.setSafetyBoolean(context, key, isTrue);
    }

    /**
     * Set integer into this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param value [in] SafetyChannel<Integer>
     * 
     *        Data for insertion
     *        
     *        Range: Valid SafetyChannel<Integer>
     *        Unit: SafetyChannel<Integer>
     *        Scaling: 1
     *                                              
     * @return None
     *         
     */    
    public static void setInt(Context context, String key,
            SafetyChannel<Integer> value)
    {
        // Key of SafetyChannel CH1, CH2 data
        String KEYch1 = key.concat(KEY_CH1_POSTFIX);
        String KEYch2 = key.concat(KEY_CH2_POSTFIX);
        // SafetyNumber Value
        SafetyNumber<Integer> siChValue = null;
        
        // Set CH1 value
        siChValue = new SafetyNumber<Integer>(value.getValueCH1(), 
                                              -value.getValueCH1()); 
        mModel.setInt(context, KEYch1, siChValue);
        
        // Set CH2 value
        siChValue = new SafetyNumber<Integer>(value.getValueCH2(), 
                                              -value.getValueCH2()); 
        mModel.setInt(context, KEYch2, siChValue);

    }
    
    /**
     * Set long into this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param value [in] SafetyChannel<Long>
     * 
     *        Data for insertion
     *        
     *        Range: Valid SafetyChannel<Long>
     *        Unit: SafetyChannel<Long>
     *        Scaling: 1
     *                                              
     * @return None
     *         
     */    
    public static void setLong(Context context, String key,
            SafetyChannel<Long> value)
    {
        // Key of SafetyChannel CH1, CH2 data
        String KEYch1 = key.concat(KEY_CH1_POSTFIX);
        String KEYch2 = key.concat(KEY_CH2_POSTFIX);
        // SafetyNumber Value
        SafetyNumber<Long> slChValue = null;
        
        // Set CH1 value
        slChValue = new SafetyNumber<Long>(value.getValueCH1(), 
                                              -value.getValueCH1()); 
        mModel.setLong(context, KEYch1, slChValue);
        
        // Set CH2 value
        slChValue = new SafetyNumber<Long>(value.getValueCH2(), 
                                              -value.getValueCH2()); 
        mModel.setLong(context, KEYch2, slChValue);

    }    

    /**
     * Get string from this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *                                              
     * @return SafetyString [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *              *         
     *         Range: Valid SafetyString
     *         Unit: SafetyString
     *         Scaling: 1
     *         
     */
    public static SafetyString getString(Context context, String key)
    {
        SafetyString result = mModel.getString(context, key,
                                               null);
        
        return result;
    }

    /**
     * Get string from this category of SharedPreference 
     * with default returned value 
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param sDefault [in] SafetyString
     * 
     *        Default returned data if the data with assigned key does not
     *        exist  
     *        
     *        Range: Valid SafetyString
     *        Unit: SafetyString
     *        Scaling: 1       
     *                                              
     * @return SafetyString [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *         or default returned data
     *              *         
     *         Range: Valid SafetyString
     *         Unit: SafetyString
     *         Scaling: 1
     *         
     */
    public static SafetyString getString(Context context, String key,
            SafetyString sDefault)
    {
        SafetyString result = mModel.getString(context, key,
                sDefault);
        
        return result;
    }

    /**
     * Get boolean from this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *                                              
     * @return SafetyBoolean [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *              *         
     *         Range: Valid SafetyBoolean
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     */    
    public static SafetyBoolean getSafetyBoolean(Context context, String key)
    {
        SafetyBoolean isResultOK = mModel.getSafetyBoolean(context,
                                key, SafetyBoolean.FALSE);
        
        return isResultOK;
    }

    /**
     * Get boolean from this category of SharedPreference 
     * with default returned value 
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param isResultOKDefault [in] SafetyBoolean
     * 
     *        Default returned data if the data with assigned key does not
     *        exist  
     *        
     *        Range: Valid SafetyBoolean
     *        Unit: SafetyBoolean
     *        Scaling: 1       
     *                                              
     * @return SafetyBoolean [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *         or default returned data
     *              *         
     *         Range: Valid SafetyBoolean
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     */    
    public static SafetyBoolean getSafetyBoolean(Context context, String key,
            SafetyBoolean isResultOKDefault)
    {
        SafetyBoolean isResultOK = mModel.getSafetyBoolean(context,
                                                    key, isResultOKDefault);
        
        return isResultOK;
    }

    /**
     * Get integer from this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *                                              
     * @return SafetyChannel<Integer> [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *              *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     */    
    public static SafetyChannel<Integer> getInt(Context context, String key)
    {
        // Key of SafetyChannel CH1, CH2 data
        String KEYch1 = key.concat(KEY_CH1_POSTFIX);
        String KEYch2 = key.concat(KEY_CH2_POSTFIX);
        // SafetInteger channel value
        SafetyNumber<Integer> scCH1 = null;
        SafetyNumber<Integer> scCH2 = null;
        
        // Get two channel data from this category of SharedPreference
        scCH1 = mModel.getInt(context, KEYch1, null);
        scCH2 = mModel.getInt(context, KEYch2, null);
        
        return new SafetyChannel<Integer>(scCH1.get(), scCH2.get());
    }

    
    /**
     * Get integer from this category of SharedPreference 
     * with default returned value 
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param defaultValue [in] SafetyChannel<Integer>
     * 
     *        Default returned data if the data with assigned key does not
     *        exist  
     *        
     *        Range: Valid SafetyChannel<Integer> object
     *        Unit: SafetyChannel<Integer>
     *        Scaling: 1       
     *                                              
     * @return SafetyChannel<Integer> [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *         or default returned data
     *              *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     */    
    public static SafetyChannel<Integer> getInt(Context context, String key,
            SafetyChannel<Integer> defaultValue)
    {
        // Key of SafetyChannel CH1, CH2 data
        String KEYch1 = key.concat(KEY_CH1_POSTFIX);
        String KEYch2 = key.concat(KEY_CH2_POSTFIX);
        // Default SafetyInteger channel value 
        SafetyNumber<Integer> scCH1default = null;
        SafetyNumber<Integer> scCH2default = null;        
        // SafetInteger channel value
        SafetyNumber<Integer> scCH1 = null;
        SafetyNumber<Integer> scCH2 = null;
        
        // Create default SafetyInteger channel value
        scCH1default = new SafetyNumber<Integer>(defaultValue.getValueCH1(),
                                                -defaultValue.getValueCH1());
        
        scCH2default = new SafetyNumber<Integer>(defaultValue.getValueCH2(),
                                                -defaultValue.getValueCH2());        
        
        
        // Get two channel data from this category of SharedPreference
        scCH1 = mModel.getInt(context, KEYch1, scCH1default);
        scCH2 = mModel.getInt(context, KEYch2, scCH2default);        
        
        return new SafetyChannel<Integer>(scCH1.get(), scCH2.get());
    } 
    
    /**
     * Get long from this category of SharedPreference
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *                                              
     * @return SafetyChannel<Long> [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *              *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     */    
    public static SafetyChannel<Long> getLong(Context context, String key)
    {
        // Key of SafetyChannel CH1, CH2 data
        String KEYch1 = key.concat(KEY_CH1_POSTFIX);
        String KEYch2 = key.concat(KEY_CH2_POSTFIX);
        // SafetInteger channel value
        SafetyNumber<Long> scCH1 = null;
        SafetyNumber<Long> scCH2 = null;
        
        // Get two channel data from this category of SharedPreference
        scCH1 = mModel.getLong(context, KEYch1, null);
        scCH2 = mModel.getLong(context, KEYch2, null);
        
        return new SafetyChannel<Long>(scCH1.get(), scCH2.get());
    }

    
    /**
     * Get long from this category of SharedPreference 
     * with default returned value 
     * 
     * @param context [in] Context
     * 
     *        Activity context
     *        
     *        Range: Valid Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @param key [in] String
     * 
     *        Identifier of data
     *        
     *        Range: Valid String
     *        Unit: String
     *        Scaling: 1
     *        
     * @param defaultValue [in] SafetyChannel<Long>
     * 
     *        Default returned data if the data with assigned key does not
     *        exist  
     *        
     *        Range: Valid SafetyChannel<Long> object
     *        Unit: SafetyChannel<Long>
     *        Scaling: 1       
     *                                              
     * @return SafetyChannel<Long> [out]
     * 
     *         Data from this category of SharedPreference by assigned key
     *         or default returned data
     *              *         
     *         Range: Valid SafetyChannel<Long> object
     *         Unit: SafetyChannel<Long>
     *         Scaling: 1
     *         
     */    
    public static SafetyChannel<Long> getLong(Context context, String key,
            SafetyChannel<Long> defaultValue)
    {
        // Key of SafetyChannel CH1, CH2 data
        String KEYch1 = key.concat(KEY_CH1_POSTFIX);
        String KEYch2 = key.concat(KEY_CH2_POSTFIX);
        // Default SafetyInteger channel value 
        SafetyNumber<Long> scCH1default = null;
        SafetyNumber<Long> scCH2default = null;        
        // SafetInteger channel value
        SafetyNumber<Long> scCH1 = null;
        SafetyNumber<Long> scCH2 = null;
        
        // Create default SafetyInteger channel value
        scCH1default = new SafetyNumber<Long>(defaultValue.getValueCH1(),
                                                -defaultValue.getValueCH1());
        
        scCH2default = new SafetyNumber<Long>(defaultValue.getValueCH2(),
                                                -defaultValue.getValueCH2());        
        
        
        // Get two channel data from this category of SharedPreference
        scCH1 = mModel.getLong(context, KEYch1, scCH1default);
        scCH2 = mModel.getLong(context, KEYch2, scCH2default);        
        
        return new SafetyChannel<Long>(scCH1.get(), scCH2.get());
    }    
}
// 1st version
// 1st version of this file
