/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: NugenSettingModel
 * Brief: The Settings Model class for data read/write.
 *
 * Create Date: 10/08/2015
 * $Revision: 23372 $
 * $Author: WilliyChiang $
 * $Id: NugenSettingModel.java 23372 2015-11-05 07:52:14Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.settinginterface;

import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.FixPointFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.SharedPreferenceModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;

public final class NugenSettingModel
{
    
    /*
     * The definition is the path of the share preference file, it used to store
     * the user settings.
     */
    private static final Uri SETTINGS_URI = Uri.withAppendedPath(
            NugenFrameworkConstants.SP_URI, NugenFrameworkConstants.SETTING_DESTINATION);
    
    /**
     * The definition is the default values of a user setting.
     */
    private static final String EMPTY_STRING = "---";

    /*
     * The global variable is used to store the user settings to the share 
     * preference file. 
     */
    private static SharedPreferenceModel mModel = new SharedPreferenceModel(SETTINGS_URI);
    
    /**
     * The constructor of NugenSettingModel. To avoid static code analysis issues,
     * the class needs a private constructor.
     * 
     * @param None
     *       
     * @return None
     */
    private NugenSettingModel()
    {
        // Apply to coding standard
    }
    
    /**
     * Set the SafetyString value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return None
     */   
    public static void setString(final Context context, final UserSettingsKey key, final SafetyString value)
    {
        // Is the parameters valid?
        if ((context != null) && (key != null) && (value != null))
        {
            // Use the handler of the key to set the value to the model of the key
            key.getDataHandler().setString(context, key.getUserSettingsModel(), key, value);    
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
    }

    /**
     * Get the SafetyString value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @return The SafetyString value of a setting.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    public static SafetyString getString(final Context context, final UserSettingsKey key)
    {
        SafetyString result = null;
        
        // Is the parameters valid?
        if ((context != null) && (key != null))
        {
            // Use the handler of the key to get the value from the model of the key
            result = key.getDataHandler().getString(context, key.getUserSettingsModel(), key);
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);            
        }
        
        // Return result
        return result;
    }
    
    /**
     * Set the SafetyNumber<Integer> value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     *       
     * @return None
     */  
    public static void setInteger(final Context context, final UserSettingsKey key, final SafetyNumber<Integer> value)
    {
        // Is the parameters valid?
        if ((context != null) && (key != null) && (value != null))
        {
            // Use the handler of the key to set the value to the model of the key
            key.getDataHandler().setInteger(context, key.getUserSettingsModel(), key, value);
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
    }

    /**
     * Get the SafetyNumber<Integer> value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @return The SafetyNumber<Integer> value of a setting.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     */
    public static SafetyNumber<Integer> getInteger(final Context context, final UserSettingsKey key)
    {
        SafetyNumber<Integer> result = null;
        
        // Is the parameters valid?
        if ((context != null) && (key != null))
        {
            // Use the handler of the key to get the value from the model of the key
            result = key.getDataHandler().getInteger(context, key.getUserSettingsModel(), key);
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        
        // Return result
        return result;
    }
    
    /**
     * Set the SafetyNumber<Long> value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid SafetyNumber<Long> object
     *       Unit: SafetyNumber<Long>
     *       Scaling: 1
     *       
     * @return None
     */  
    public static void setLong(final Context context, final UserSettingsKey key, final SafetyNumber<Long> value)
    {
        // Is the parameters valid?
        if ((context != null) && (key != null) && (value != null))
        {
            // Use the handler of the key to set the value to the model of the key
            key.getDataHandler().setLong(context, key.getUserSettingsModel(), key, value);
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
    }

    /**
     * Get the SafetyNumber<Long> value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @return The SafetyNumber<Long> value of a setting.
     *       Range: Valid SafetyNumber<Long> object
     *       Unit: SafetyNumber<Long>
     *       Scaling: 1
     */
    public static SafetyNumber<Long> getLong(final Context context, final UserSettingsKey key)
    {
        SafetyNumber<Long> result = null;
        
        // Is the parameters valid?
        if ((context != null) && (key != null))
        {
            // Use the handler of the key to get the value from the model of the key
            result = key.getDataHandler().getLong(context, key.getUserSettingsModel(), key);
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        
        // Return result
        return result;
    }
    
    /**
     * Set the FixPointFloat value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid FixPointFloat object
     *       Unit: FixPointFloat
     *       Scaling: 1
     *       
     * @return None
     */  
    public static void setFixPoint(final Context context, final UserSettingsKey key, final FixPointFloat value)
    {
        // Is the parameters valid?
        if ((context != null) && (key != null) && (value != null))
        {
            // Use the handler of the key to set the value to the model of the key
            key.getDataHandler().setFixPoint(context, key.getUserSettingsModel(), key, value);
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
    }

    /**
     * Get the FixPointFloat value by using the attributes of the key.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting key.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @return The FixPointFloat value of a setting.
     *       Range: Valid FixPointFloat object
     *       Unit: FixPointFloat
     *       Scaling: 1
     */
    public static FixPointFloat getFixPoint(final Context context, final UserSettingsKey key)
    {
        FixPointFloat result = null;
        
        // Is the parameters valid?
        if ((context != null) && (key != null))
        {
            // Use the handler of the key to get the value from the model of the key
            result = key.getDataHandler().getFixPoint(context, key.getUserSettingsModel(), key);
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        
        // Return Result
        return result;
    }
    
    /**
     * Reset the values of the all user settings to "" in the XML file of user settings.
     * 
     * @param None
     *       
     * @return None
     */
    public static void reset()
    {
        // Delete the data in the SharedPreference, and get the result of the delete process
        UserSettingsXml.getInstance().setValueToDefault();
    }
    
    /**
     * Set the value to the SharedPreference by the given key.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return None
     */
    public static void setString(final Context context, final String key, final SafetyString value)
    {  
        // Set value of key to the SharedPreference
        mModel.setString(context, key, value);
    }
    
    /**
     * Set the value to the SharedPreference by the given key.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param isTrue The value of a setting.
     *       Range: Valid setSafetyBoolean object
     *       Unit: setSafetyBoolean
     *       Scaling: 1
     */
    public static void setSafetyBoolean(final Context context, final String key,
            final SafetyBoolean isTrue)
    {
        // Set value of key to the SharedPreference
        mModel.setSafetyBoolean(context, key, isTrue);
    }

    /**
     * Set the value to the SharedPreference by the given key.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param isTrue The value of a setting.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     *       
     * @return None
     */
    public static void setInt(final Context context, final String key,
            final SafetyNumber<Integer> value)
    {
        // Set value of key to the SharedPreference
        mModel.setInt(context, key, value);
    }

    /**
     * Get the key to the SharedPreference by the given key.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @return The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     */
    private static String getKey(final Context context, final String key)
    {
        // Return key
        return key;
    }

    /**
     * Get the SafetyString value from the SharedPreference by the given key.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return The SafetyString value of the given key.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    public static SafetyString getString(final Context context, final String key)
    {
        // Get the value of key from the SharedPreference
        final SafetyString result = getString(context, getKey(context, key), null);
        
        // Return result
        return result;
    }

    /**
     * Get the SafetyString value from the SharedPreference by the given key,
     * if the data in the SharedPreference is null, the function returns defaultValue.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @param sDefault The SafetyString value for giving the default value.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     * 
     * @return The SafetyString value of the given key.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    public static SafetyString getString(final Context context, final String key,
            final SafetyString defaultValue)
    {
        // Get the value of key from the SharedPreference
        final SafetyString result = mModel.getString(context, getKey(context, key),
                defaultValue);
        
        // Return result
        return result;
    }

    /**
     * Get the SafetyBoolean value from the SharedPreference by the given key,
     * if the data in the SharedPreference is null, the function returns SafetyBoolean.FALSE.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return The SafetyBoolean value of the given key.
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */
    public static SafetyBoolean getSafetyBoolean(final Context context, final String key)
    {
        // Get the value of key from the SharedPreference
        final SafetyBoolean result = getSafetyBoolean(context, getKey(context, key), SafetyBoolean.FALSE);
        
        // Return result
        return result; 
    }

    /**
     * Get the SafetyBoolean value from the SharedPreference by the given key,
     * if the data in the SharedPreference is null, the function returns isDefaultResultOK.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @param isDefaultResultOK The SafetyBoolean value for giving the default value.
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     * 
     * @return The SafetyBoolean value of the given key.
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */
    public static SafetyBoolean getSafetyBoolean(final Context context, final String key,
            final SafetyBoolean isDefaultResultOK)
    {
        // Get the value of key from the SharedPreference
        final SafetyBoolean result = mModel.getSafetyBoolean(context, getKey(context, key),
                isDefaultResultOK);
        
        // Return result
        return result;
    }

    /**
     * Get the SafetyNumber<Integer> value from the SharedPreference by the given key.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return The SafetyNumber<Integer> value of the given key.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     */
    public static SafetyNumber<Integer> getInt(final Context context, final String key)
    {
        // Get the value of key from the SharedPreference
        final SafetyNumber<Integer> result = getInt(context, getKey(context, key), null);
        
        // Return Result
        return result; 
    }

    /**
     * Get the SafetyNumber<Integer> value from the SharedPreference by the given key,
     * if the data in the SharedPreference is null, the function returns defaultValue.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param defaultValue The SafetyNumber<Integer> value for giving the default value.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     * 
     * @return The SafetyNumber<Integer> value of the given key.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     */
    public static SafetyNumber<Integer> getInt(final Context context, final String key,
            final SafetyNumber<Integer> defaultValue)
    {
        // Get the value of key from the SharedPreference
        SafetyNumber<Integer> result = mModel.getInt(context, getKey(context, key), null);
                
        // Is result equal to null?
        if (null == result)
        {
            // Set result to defaultValue
            result = defaultValue;
        }
        
        // Return result
        return result;
    }

    /**
     * Get the value with float type from the SharedPreference by the given key.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return The SafetyFloat value of the given key.
     *       Range: Valid SafetyFloat object
     *       Unit: SafetyFloat
     *       Scaling: 1
     */
    public static SafetyFloat getFloat(final Context context, final String key)
    {
        // Get the value of key from the SharedPreference
        final SafetyFloat result = mModel.getFloat(context, getKey(context, key), null);
        
        // Return the value
        return result;
    }

    /**
     * Get the value with float type from the SharedPreference by the given key,
     * if the data in the SharedPreference is null, the function returns defaultValue.
     *
     * @param context The context of the caller.
     *       Range: Valid context object. 
     *       Unit: Context
     *       Scaling: 1
     * 
     * @param key The key of a setting.
     *       Range: Valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param defaultValue The FixPointFloat value for giving the default value.
     *       Range: Valid FixPointFloat object
     *       Unit: FixPointFloat
     *       Scaling: 1
     * 
     * @return The FixPointFloat value of the given key.
     *       Range: Valid FixPointFloat object
     *       Unit: FixPointFloat
     *       Scaling: 1
     */
    public static FixPointFloat getFixPoint(final Context context, final String key,
            final FixPointFloat defaultValue)
    {
        // Get the value of key from the SharedPreference
        final FixPointFloat result = mModel.getFixPoint(context, key, defaultValue);
        
        // Return the result
        return result;
    }
    
    /*
     * This class IUserSettingsModel is an interface, it provides the abstract functions
     * to get value from the model and set value to model.
     */
    public interface IUserSettingsModel
    {
        /**
         * Get the value of a setting from a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param key The name of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @return The value of a setting.
         *       Range: Valid SafetyString object
         *       Unit: SafetyString
         *       Scaling: 1
         */
        SafetyString getValue(Context context, UserSettingsKey key);
        
        /**
         * Set a value of a setting to a model
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param key The name of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @param value The value of a setting.
         *       Range: Valid SafetyString object
         *       Unit: SafetyString
         *       Scaling: 1
         *       
         * @return None
         */        
        void setValue(Context context, UserSettingsKey key, SafetyString value);
    }
    
    
    public static class SettingsDataHandler
    {
        /**
         * Set the SafetyString value of a setting to a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @param value The value of a setting.
         *       Range: Valid SafetyString object
         *       Unit: SafetyString
         *       Scaling: 1
         *       
         * @return None
         */   
        public final void setString(final Context context, final IUserSettingsModel model, final UserSettingsKey key, final SafetyString value)
        {
            // Is the parameters valid?
            if ((context != null) && (model != null) && (key != null) && (value != null))
            {
                // Set value of key to model
                model.setValue(context, key, value);
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
        }
        
        /**
         * Get the SafetyString value of a setting from a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @return The SafetyString value of a setting.
         *       Range: Valid SafetyString object
         *       Unit: SafetyString
         *       Scaling: 1
         */
        public final SafetyString getString(final Context context, final IUserSettingsModel model, final UserSettingsKey key)
        {
            SafetyString result = null;

            // Is the parameters valid?
            if ((context != null) && (model != null) || (key != null))
            {
                boolean isEqualToEmptyString = false;
                boolean isEqualToDefaultValue = false;
                
                // Get the value of key from model  
                result = model.getValue(context, key);
                
                // Compare the value and an empty string
                isEqualToEmptyString = result.getString().equalsIgnoreCase(EMPTY_STRING);
                
                // Compare the value and the key in the Configuration Matrix
                isEqualToDefaultValue = result.getString().equalsIgnoreCase(key.getConfigurationMatrixKey());
                
                // Is the value of key equal to an empty string?
                if (false == isEqualToEmptyString)
                {
                    // Is the value of key equal to the key in the Configuration Matrix
                    if (true == isEqualToDefaultValue)
                    {
                        // Get the value of key from Configuration Matrix
                        final SafetyString configKey = new SafetyString(key.getConfigurationMatrixKey(),
                                CRCTool.generateCRC16(key.getConfigurationMatrixKey().getBytes()));
                        result = ConfigurationMatrixWrapper.getString(context, configKey);
                    }
                }
                else
                {
                    result = null;
                }
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            
            // Return the result
            return result;
        }
        
        /**
         * Set the SafetyNumber<Long> value of a setting to a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @param value The value of a setting.
         *       Range: Valid SafetyNumber<Long> object
         *       Unit: SafetyNumber<Long>
         *       Scaling: 1
         *       
         * @return None
         */   
        public final void setLong(final Context context, final IUserSettingsModel model, final UserSettingsKey key, final SafetyNumber<Long> value)
        {
            // Is the parameters valid?
            if ((context != null) && (model != null) && (key != null) && (value != null))
            {
                String longValue = null;
                int crc = 0;
            
                // Transfer value with SafetyNumber<Long> type to value with string type
                longValue = String.valueOf(value.get());
                
                // Generate the CRC of value with string type 
                crc = CRCTool.generateCRC16(longValue.getBytes());
                
                // Set value with SafetyString type of key to model
                model.setValue(context, key, new SafetyString(longValue, crc));
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
        }
        
        /**
         * Get the SafetyNumber<Long> value of a setting from a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @return The SafetyNumber<Long> value of a setting.
         *       Range: Valid SafetyNumber<Long> object
         *       Unit: SafetyNumber<Long>
         *       Scaling: 1
         */
        public final SafetyNumber<Long> getLong(final Context context, final IUserSettingsModel model, final UserSettingsKey key)
        {
            SafetyNumber<Long> result = null;
            
            // Is the parameters valid?
            if ((context != null) && (model != null) && (key != null))
            {
                boolean isEqualToEmptyString = false;
                boolean isEqualToDefaultValue = false;
                
                // Get the value of key from model
                final SafetyString stringValue = model.getValue(context, key);
                
                // Compare the value and an empty string
                isEqualToEmptyString = stringValue.getString().equalsIgnoreCase(EMPTY_STRING);
                
                // Compare the value and the key in the Configuration Matrix
                isEqualToDefaultValue = stringValue.getString().equalsIgnoreCase(key.getConfigurationMatrixKey());
                
                // Is the value of key equal to null?
                if (false == isEqualToEmptyString)
                {
                    if (true == isEqualToDefaultValue)
                    {
                        // Get the value of key from Configuration Matrix
                        final SafetyString keyOfDefault = new SafetyString(key.getConfigurationMatrixKey(),
                                CRCTool.generateCRC16(key.getConfigurationMatrixKey().getBytes()));
                        
                        result = ConfigurationMatrixWrapper.getLong(keyOfDefault);
                    }
                    else
                    {
                        // Transfer the value with SafetyString type to the value with SafetyNumber<Long> type
                        final long value = Long.parseLong(stringValue.getString());
                        
                        result = new SafetyNumber<Long>(value, -value);
                    }
                }
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            
            // Return the result
            return result;
        }
        
        /**
         * Set the SafetyNumber<Integer> value of a setting to a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @param value The value of a setting.
         *       Range: Valid SafetyNumber<Integer> object
         *       Unit: SafetyNumber<Integer>
         *       Scaling: 1
         *       
         * @return None
         */   
        public final void setInteger(final Context context, final IUserSettingsModel model, final UserSettingsKey key, final SafetyNumber<Integer> value)
        {
            // Is the parameters valid?
            if ((context != null) && (model != null) && (key != null) && (value != null))
            {
                // Transfer value with SafetyNumber<Integer> type to value with string type
                final String intValue = String.valueOf(value.get());
                
                // Generate the CRC of value with string type
                final int crc = CRCTool.generateCRC16(intValue.getBytes());
                
                // Set the value with SafetyString type of key to model
                model.setValue(context, key, new SafetyString(intValue, crc));        
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);       
            }
        }
        
        /**
         * Get the SafetyNumber<Integer> value of a setting from a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @return The SafetyNumber<Integer> value of a setting.
         *       Range: Valid SafetyNumber<Integer> object
         *       Unit: SafetyNumber<Integer>
         *       Scaling: 1
         */
        public final SafetyNumber<Integer> getInteger(final Context context, final IUserSettingsModel model, final UserSettingsKey key)
        {
            SafetyNumber<Integer> result = null;
            
            // Is the parameters valid?
            if ((context != null) && (model != null) && (key != null))
            {
                boolean isEqualToEmptyString = false;
                boolean isEqualToDefaultValue = false;
                
                // Get the value of key from model
                final SafetyString stringValue = model.getValue(context, key);
                
                // Compare the value and an empty string
                isEqualToEmptyString = stringValue.getString().equalsIgnoreCase(EMPTY_STRING);
                
                // Compare the value and the key in the Configuration Matrix
                isEqualToDefaultValue = stringValue.getString().equalsIgnoreCase(key.getConfigurationMatrixKey());
                
                // Is the value an empty string?
                if (false == isEqualToEmptyString)
                {
                    if (true == isEqualToDefaultValue)
                    {
                        // Get the value of key from Configuration Matrix
                        final SafetyString keyOfDefault = new SafetyString(key.getConfigurationMatrixKey(),
                                CRCTool.generateCRC16(key.getConfigurationMatrixKey().getBytes()));
                        result = ConfigurationMatrixWrapper.getInteger(context, keyOfDefault);                             
                    }
                    else
                    {
                        // Transfer the value with SafetyString to the value with SafetyNumber<Integer> type
                        final int value = Integer.parseInt(stringValue.getString());
                        result = new SafetyNumber<Integer>(value, -value);
                    }
                }
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);   
            }
            
            // Return the result
            return result;
        }
        
        /**
         * Set the FixPointFloat value of a setting to a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @param value The value of a setting.
         *       Range: Valid FixPointFloat object
         *       Unit: FixPointFloat
         *       Scaling: 1
         *       
         * @return None
         */   
        public final void setFixPoint(final Context context, final IUserSettingsModel model, final UserSettingsKey key, final FixPointFloat value)
        {
            // Is the parameters valid?
            if ((context != null) && (model != null) && (key != null) && (value != null))
            {
                // Set value with SafetyString type of key to model
                setInteger(context, model, key, value.getSafetyNumber());
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
        }
        
        /**
         * Get the FixPointFloat value of a setting from a model.
         * 
         * @param context The context of the caller.
         *       Range: Valid Context object
         *       Unit: Context
         *       Scaling: 1
         *       
         * @param model The place where stores data.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1 
         *       
         * @param key The key of a setting.
         *       Range: Valid UserSettingsKey object
         *       Unit: UserSettingsKey
         *       Scaling: 1
         *       
         * @return The FixPointFloat value of a setting.
         *       Range: Valid FixPointFloat object
         *       Unit: FixPointFloat
         *       Scaling: 1
         */
        public final FixPointFloat getFixPoint(final Context context, final IUserSettingsModel model, final UserSettingsKey key)
        {
            FixPointFloat result = null;
            
            // Is the parameters valid?
            if ((context != null) && (model != null) && (key != null))
            {
                // Get the value with SafetyNumber<Integer> type of key from model
                final SafetyNumber<Integer> value = getInteger(context, model, key);
                
                // Is the value of key equal to null
                if (null != value)
                {
                    /* 
                     * Transfer the value with SafetyNumber<Integer> type of key to
                     * the value with FixPointFloat type of key  
                     */
                    result = new FixPointFloat(value);
                }
            }
            else
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            
            // Return the result
            return result;
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
