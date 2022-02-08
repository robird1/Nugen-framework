/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: NugenUserSettingsConstants
 * Brief: Constants definition of user settings.
 *
 * Create Date: 10/15/2015
 * $Revision: 23372 $
 * $Author: WilliyChiang $
 * $Id: NugenUserSettingsConstants.java 23372 2015-11-05 07:52:14Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.settinginterface;

import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.setting.UserSettingsModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel.IUserSettingsModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel.SettingsDataHandler;

public class NugenUserSettingsConstants
{
    
    public enum UserSettingsKey
    {
        /**
         * The enumerator is the key of hyperglycemicThreshold which is used to 
         * get data from the XML file of user settings and the Configuration 
         * Matrix, and it also can be used to set data to the XML file of user 
         * settings.
         */
        HYPER_GLYCEMIC_THRESHOLD(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.HYPER_GLYCEMIC_THRESHOLD,
                ConfigParameter.KEY_HYPER_THRESHOLD_DEFAULT),
                
                
        /**
         * The enumerator is the key of bolusAdviceStatus which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */
        BOLUS_ADVICE_STATUS(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.BOLUS_ADVICE_STATUS,
                ConfigParameter.BOLUS_ADVICE_STATUS_DEFAULT),
                
                
        /**
         * The enumerator is the key of MDIInsulinIncrement which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */    
        MDI_INSULIN_INCREMENT(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.MDI_INSULIN_INCREMENT,
                ConfigParameter.KEY_MDI_INSULIN_INCREMENT),
                
                
        /**
         * The enumerator is the key of MDIMode which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */
        MDI_MODE(null, UserSettingsXml.getInstance(), 
                UserSettingsXmlKey.MDI_MODE,
                ConfigParameter.MDI_MODE_DEFAULT),
                
                
        /**
         * The enumerator is the key of MDIMaxBolusAmount which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */
        MDI_MAX_BOLUS_AMOUNT(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.MDI_MAX_BOLUS_AMOUNT,
                ConfigParameter.KEY_MDI_MAX_BOLUS),
                
                
        /**
         * The enumerator is the key of userBolusThresholdMax which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */
        USER_BOLUS_THRESHOLD_MAX(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.USER_BOLUS_THRESHOLD_MAX,
                ConfigParameter.LOCALIZED_BOLUS_AMOUNT_LIMIT_MAXIMUM),
                
        
        /**
         * The enumerator is the key of reminderbGTestAfterMeal.duration which 
         * is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_MEAL_DURATION(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_MEAL_DURATION,
                ConfigParameter.REMINDER_BG_TEST_AFTER_MEAL_DURATION_DEFAULT),
                
                
        /**
         * The enumerator is the key of reminderbGTestAfterMeal.status which is 
         * used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_MEAL_STATUS(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_MEAL_STATUS,
                ConfigParameter.BG_TEST_SET_STATUS),
                
        
        /**
         * The enumerator is the key of reminderbGTestAfterMeal.signalizationMelody 
         * which is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_MEAL_SOUND(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_MEAL_SIGNALIZATION_MELODY,
                ConfigParameter.INFUSION_SET_TONE),
                
                
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.time which is 
         * used to get data from the XML file of user settings and the Configuration
         * Matrix, and it also can be used to set data to the XML file of user 
         * settings.
         */
        REMINDER_BG_AFTER_HIGH_DURATION(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_HIGH_TIME,
                ConfigParameter.REMINDER_BG_TEST_AFTER_HIGH_DURATION_DEFAULT),
                
        
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.status which is 
         * used to get data from the XML file of user settings and the Configuration 
         * Matrix, and it also can be used to set data to the XML file of user 
         * settings.
         */
        REMINDER_BG_AFTER_HIGH_STATUS(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_HIGH_STATUS,
                ConfigParameter.BG_TEST_SET_STATUS),
                
                
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.signalizationMelody 
         * which is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_HIGH_SOUND(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_HIGH_SIGNALIZATION_MELODY,
                ConfigParameter.INFUSION_SET_TONE),
                
                
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.threshold which 
         * is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_HIGH_THRESHOLD(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_HIGH_THRESHOLD,
                ConfigParameter.KEY_REMINDER_AFTERBG_HIGH_THRESHOLD_DEFAULT),

                
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.time which is 
         * used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_LOW_DURATION(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_LOW_TIME,
                ConfigParameter.REMINDER_BG_TEST_AFTER_LOW_DURATION_DEFAULT),
                
        
                
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.status which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */
        REMINDER_BG_AFTER_LOW_STATUS(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_LOW_STATUS,
                ConfigParameter.BG_TEST_SET_STATUS),
                
                
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.signalizationMelody 
         * which is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_LOW_SOUND(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_LOW_SIGNALIZATION_MELODY,
                ConfigParameter.INFUSION_SET_TONE),
                
                
        /**
         * The enumerator is the key of reminderbGTestAfterHigh.threshold which 
         * is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        REMINDER_BG_AFTER_LOW_THRESHOLD(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.REMINDER_BG_TEST_AFTER_LOW_THRESHOLD,
                ConfigParameter.KEY_REMINDER_AFTER_LOW_THRESHOLD_DEFAULT),
                
                
        /**
         * The enumerator is the key of carbUnitType which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */
        CARB_UNIT_TYPE(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.CARB_UNIT_TYPE, 
                ConfigParameter.KEY_DEFAULT_CARBS_UNIT),

                
		/**
		 * The enumerator is the key of bGMeasurementDisplayUnits 
         * which is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
		 */
        BG_DISPLAY_UNIT(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.BG_DISPLAY_UNIT, 
                ConfigParameter.KEY_BGMEASUREMENT_DISPLAY_UNIT),
        
                
        /**
         * The enumerator is the key of therapyBlock3InsulinSensivityRatioInsulin 
         * which is used to get data from the XML file of user settings and the 
         * Configuration Matrix, and it also can be used to set data to the XML 
         * file of user settings.
         */
        THERAPY_BLOCK3_INSULIN(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.THERAPY_BLOCK3_INSULIN_SENSIVITY_RATIO_INSULIN,
                ConfigParameter.BOLUS_ADVICE_INSULIN_SENSIVITY_RATIO_INSULIN_DEFAULT),
        
        
        /**
         * The enumerator is the key of timeFormat which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */        
        TIME_FORMAT(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.TIME_FORMAT,
                ConfigParameter.KEY_TIME_FORMAT),
        
        
        /**
         * The enumerator is the key of hypoglycemicThreshold which is used to get 
         * data from the XML file of user settings and the Configuration Matrix, 
         * and it also can be used to set data to the XML file of user settings.
         */        
        HYPOGLYCEMIC_THRESHOLD(null, UserSettingsXml.getInstance(),
                UserSettingsXmlKey.HYPOGLYCEMIC_THRESHOLD,
                ConfigParameter.KEY_HYPO_THRESHOLD_DEFAULT);
        
        /**
         * The global variable is used to access data from the XML file of user settings.
         */
        private final SettingsDataHandler mDataHandler;
        
        /**
         * The global variable is he place which is used to store the user settings.
         */
        private final IUserSettingsModel mTargetModel;
        
        /**
         * The key of a setting in the Configuration Matrix.
         */
        private final String mKeyOfConfigurationMatrix;
        
        /**
         * The key of a settings in the XML file of user settings.
         */
        private final String mKeyOfUserSetting;
        
        /**
         * The constructor of the UserSettingsKey enumeration. The constructor 
         * initialize the global variables.
         * 
         * @param handler The data type and format handler which is used to access data.
         *        If handler is null, the handler is set to SettingsDataHandler .
         *        Range: Null or valid SettingsDataHandler object
         *        Unit: SettingsDataHandler
         *        Scaling: 1
         *        
         * @param targetModel The place where stores user settings.
         *        If targetModel is null, the targetModel is set to UserSettingsModel.
         *        Range: Null or valid IUserSettingsModel object
         *        Unit: IUserSettingsModel
         *        Scaling: 1
         *        
         * @param keyOfUserSetting The key in the XML file of user settings. 
         *        Range: Valid String object
         *        Unit: String
         *        Scaling: 1
         *        
         * @param keyOfConfigMatrix The key in the Configuration Matrix.
         *        Range: Valid String object
         *        Unit: String
         *        Scaling: 1
         */
        private UserSettingsKey(final SettingsDataHandler handler, final IUserSettingsModel targetModel, 
                final String keyOfUserSetting, final String keyOfConfigMatrix)
        {
            // Is handler equal to null?
            if (null == handler)
            {
                // Create a SettingsDataHandler instance, and set mDataHandler to it
                mDataHandler = new SettingsDataHandler();
            }
            else
            {
                // Set mDataHandler to handler
                mDataHandler = handler;
            }
            
            // Is targetModel equal to null?
            if (null == targetModel)
            {
                // Get the UserSettingsModel instance, and set mTargetModel to it
                mTargetModel = UserSettingsModel.getInstance();
            }
            else
            {
                // Set mTargetModel to targetModel
                mTargetModel = targetModel;
            }
            
            // Set mKeyOfConfigurationMatrix to keyOfConfigMatrix
            mKeyOfConfigurationMatrix = keyOfConfigMatrix;
            
            // Set mKeyOfUserSetting to keyOfUserSettings
            mKeyOfUserSetting = keyOfUserSetting;
        }
        
        /**
         * Get the SettingsDataHandler instance to access the XML file of user settings.
         *       
         * @return The data handler to access the XML file of user settings.
         *       Range: Valid SettingsDataHandler object
         *       Unit: SettingsDataHandler
         *       Scaling: 1
         */
        public SettingsDataHandler getDataHandler()
        {
            // Return mDataHandler
            return mDataHandler;
        }
        
        /**
         * Get the IUserSettingsModel instance which is used to store the user settings
         *       
         * @return The IUserSettingsModel instance which is used to store the user settings.
         *       Range: Valid IUserSettingsModel object
         *       Unit: IUserSettingsModel
         *       Scaling: 1
         */
        public IUserSettingsModel getUserSettingsModel()
        {
            // Return mTargetModel
            return mTargetModel;
        }
        
        /**
         * Get the key in the Configuration Matrix.
         *       
         * @return The key in the Configuration Matrix.
         *       Range: Valid String object
         *       Unit: String
         *       Scaling: 1
         */
        public String getConfigurationMatrixKey()
        {
            // Return mKeyOfConfigurationMatrix
            return mKeyOfConfigurationMatrix;
        }
        
        /**
         * Get the key in the User Setting XML.
         *       
         * @return The key in the User Setting XML.
         *       Range: Valid String object
         *       Unit: String
         *       Scaling: 1
         */
        public String getUserSettingKey()
        {
            // Return mKeyOfUserSetting
            return mKeyOfUserSetting;
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