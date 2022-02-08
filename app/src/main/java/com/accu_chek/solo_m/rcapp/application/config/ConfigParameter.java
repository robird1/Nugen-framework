/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.config.ConfigParameter
 * Brief: Configuration Matrix Parameter Name constant strings
 *
 * Create Date: 08/14/2015
 * $Revision: 25214 $
 * $Author: SteveSu $
 * $Id: ConfigParameter.java 25214 2015-12-01 06:18:05Z SteveSu $
 */
package com.accu_chek.solo_m.rcapp.application.config;

public class ConfigParameter
{
    // Common
    public static final String KEY_TIME_FORMAT = "timeFormatDefault";
    public static final String KEY_DEFAULT_CARBS_UNIT = "carbUnitTypeDefault";

    // Defined for Settings / Basal Profile Rate
    public static final String KEY_TBR_FACTOR_MIN = "TempBasalRate.FactorMin";

    public static final String KEY_TBR_FACTOR_MAX = "TempBasalRate.FactorMax";

    public static final String KEY_TBR_FACTOR_RESOLUTION = "TempBasalRate.FactorResolution";

    public static final String KEY_TBR_DURATION_MIN = "TempBasalRate.DurationMin";

    public static final String KEY_TBR_DURATION_MAX = "TempBasalRate.DurationMax";

    public static final String KEY_TBR_DURATION_RESOLUTION = "TempBasalRate.DurationResolution";

    public static final String KEY_BRP_IU_RESOLUTION_1 = "basalRateResolution.Value1";

    public static final String KEY_BRP_IU_RESOLUTION_2 = "basalRateResolution.Value2";

    public static final String KEY_BRP_IU_AMOUNT_MIN = "BasalRateFactoryLimit.MinimumAmount";

    public static final String KEY_BRP_IU_AMOUNT_MAX = "BasalRateLimit.MaximumAmount";

    public static final String KEY_TBR_MAX_COUNT = "TempBasalRate.MaxNumber";

    public static final String KEY_BRP_MAX_COUNT = "basalRateLimit.MaxNumberProfiles";

    public static final String KEY_TBR_PERCENT_DEFAULT = "TempBasalRate.FactorDefault";

    public static final String KEY_TBR_DURATION_DEFAULT = "TempBasalRate.DurationDefault";

    public static final String KEY_TBR_NAME_LENGTH = "TempBasalRate.NameLength";

    public static final String KEY_BASAL_DELIVERY_DELAY = "hourlyMinimumBasalDeliveryDelay";

    public static final String KEY_BRP_TIME_BLOCK_MAX_NUMBER = "basalRateLimit.MaxNumberTimeBlocks";

    public static final String KEY_BRP_AMOUNT_DEFAULT = "basalRateAmount.DefaultValue";

    public static final String KEY_BRP_DURATION_DEFAULT = "basalRateDuration.DefaultValue";

    public static final String KEY_BRP_START_TIME = "basalRateDuration.StartTime";

    public static final String KEY_BRP_BASAL_LIMIT = "BasalRateFactoryLimit";

    public static final String KEY_BRP_DURATION_RESOLTION = "basalRateDuration.Resolution";

    // Defined for TimeManagement
    public static final String KEY_DATE_TIME_BACKUP_PERIOD = "dateTimeBackupPeriod";
    public static final String KEY_SYNC_INFO_TIME_DIFFERENCE_RC_VS_MP = "syncInfoTimeDifferenceRCvsMP";
    public static final String KEY_METER_INTERNAL_TIME_SYNC_INTERVAL = "meterInternalTimeSyncInterval";
    public static final String KEY_METER_PUMP_TIME_DIFFERENCE = "meterPumpTimeDifference";
    public static final String KEY_METER_PUMP_TIME_DIFFERENCE_TOLERANCE = "meterPumpTimeDifferenceTolerance";
    public static final String KEY_FORCED_OFFSET_MICRO_PUMP_TIME_BASE = "forcedOffsetMicroPumpTimeBase";
    public static final String KEY_MIN_DATE = "minDate";
    public static final String KEY_MAX_DATE = "maxDate";
    
    // Defined for BGM Control
    public static final String KEY_DISPLAY_LO = "DisplayLevelLO";
    public static final String KEY_DISPLAY_HI = "DisplayLevelHI";
    public static final String KEY_BGMEASUREMENT_DISPLAY_UNIT = "bGMeasurementDisplayUnits";
    public static final String KEY_RESET_DATA_ON_FAT_INSERT = "resetDataOnFATInsert";

    // My Data
    public static final String KEY_GRAMS_MIN = "carbohydratesMinGrams";
    public static final String KEY_GRAMS_MAX = "carbohydratesMaxGrams";
    public static final String KEY_BE_MIN = "carbohydratesMinBE";
    public static final String KEY_BE_MAX = "carbohydratesMaxBE";
    public static final String KEY_KE_MIN = "carbohydratesMinKE";
    public static final String KEY_KE_MAX = "carbohydratesMaxKE";
    public static final String KEY_CC_MIN = "carbohydratesMinCC";
    public static final String KEY_CC_MAX = "carbohydratesMaxCC";
    public static final String KEY_DEFAULT_GRAMS = "carbohydratesDefaultGrams";
    public static final String KEY_DEFAULT_BE = "carbohydratesDefaultBE";
    public static final String KEY_DEFAULT_KE = "carbohydratesDefaultKE";
    public static final String KEY_DEFAULT_CC = "carbohydratesDefaultCC";
    public static final String KEY_MEALTIME_TYPE_DEFAULT = "mealTimeTypeDefault";
    public static final String KEY_NOTE_MAX_SIZE = "noteSizeMax";

    // Defined for Settings / Warning Limits
    public static final String KEY_HYPER_THRESHOLD_MIN = "hyperglycemicThresholdMin";
    public static final String KEY_HYPER_THRESHOLD_MAX = "hyperglycemicThresholdMax";
    public static final String KEY_HYPO_THRESHOLD_MAX = "hypoglycemicThresholdMax";
    public static final String KEY_HYPO_THRESHOLD_MIN = "hypoglycemicThresholdMin";
    public static final String KEY_HYPER_THRESHOLD_DEFAULT = "hyperglycemicThresholdDefault";
    public static final String KEY_HYPO_THRESHOLD_DEFAULT = "hypoglycemicThresholdDefault";

    // Defined for Settings / Reminder
    public static final String KEY_REMINDER_CHANGE_INFUSION_SET_INTERVAL_DEFAULT = "reminderPumpChangeInfusionSetIntervalDefault";
    public static final String KEY_REMINDER_CHANGE_INFUSION_SET_STATUS_DEFAULT = "reminderPumpChangeInfusionSetStatusDefault";
    public static final String KEY_REMINDER_CHANGE_INFUSION_SET_TIME_DEFAULT = "reminderPumpChangeInfusionSetTimeDefault";
    public static final String KEY_REMINDER_STATUS_DEFAULT = "reminderStatusDefault";
    public static final String KEY_REMINDER_AFTER_LOW_THRESHOLD_DEFAULT = "reminderbGTestAfterLowbGThresholdDefault";
    public static final String KEY_REMINDER_AFTERBG_HIGH_THRESHOLD_DEFAULT = "reminderbGTestAfterHighbGThresholdDefault";
    public static final String KEY_REMINDER_BGTEST_MAX = "reminderbGTestMax";
    public static final String KEY_REMINDER_ALARM_CLOCK_DEFAULT = "reminderAlarmClockTimeDefault";
    public static final String KEY_REMINDER_ALARM_CLOCK_REPEAT_DEFAULT = "reminderAlarmClockRepeatStatusDefault";
    public static final String KEY_REMINDER_BASAL_INJECTION_STATE_DEFAULT = "reminderBasalInjectionStatusDefault";
    public static final String KEY_REMINDER_BASAL_INJECTION_STATUS_DEFAULT = "reminderBasalInjectionRepeatStatusDefault";
    public static final String KEY_REMINDER_BASAL_INJECTION_TIME_DEFAULT = "reminderBasalInjectionTimeDefault";
    public static final String KEY_REMINDER_BG_TEST_TIME_DEFAULT = "reminderbGTestTimeDefault";
    public static final String KEY_REMINDER_BG_TEST_STATUS_DEFAULT = "reminderbGTestRepeatStatusDefault";
    public static final String KEY_REMINDER_MISSED_BOLUS_STATUS_DEFAULT = "reminderMissedBolusRepeatStatusDefault";
    public static final String KEY_REMINDER_MISSED_BOLUS_TIME_DEFAULT = "reminderMissedBolusTimeDefault";

    // Defined for Comms config
    public static final String KEY_BLE_CONN_INTERVAL_MIN = "BLE_Conn_Interval_Min";
    public static final String KEY_BLE_CONN_INTERVAL_MAX = "BLE_Conn_Interval_Max";
    public static final String KEY_BLE_CONN_LATENCY = "BLE_Conn_Latency";
    public static final String KEY_BLE_SUPERVISION_TIMEOUT = "BLE_Supervision_Timeout";
    public static final String KEY_BLE_SCAN_TYPE = "BLE_Scan_Type";
    public static final String KEY_BLE_INIT_WINDOWS = "BLE_Init_Window";
    public static final String KEY_BLE_INIT_INTERVAL = "BLE_Init_Interval";
    public static final String KEY_BLE_INIT_FILTER_POLICY_MODE1 = "BLE_Init_Filter_Policy_Mode1";
    public static final String KEY_BLE_INIT_FILTER_POLICY_MODE2 = "BLE_Init_Filter_Policy_Mode2";
    public static final String KEY_BLE_INIT_FILTER_POLICY_MODE3 = "BLE_Init_Filter_Policy_Mode3";
    public static final String KEY_BLE_INIT_STATE_MODE3 = "BLE_Init_State_Mode3";
    public static final String KEY_BLE_INIT_PAUSE_MODE3 = "BLE_Init_Pause_Mode3";
    public static final String KEY_BLE_STANDBY_TIMEOUT = "BLE_Standby_Timeout";
    public static final String KEY_BLE_E2E_RETRY = "BLE_E2E_Retry";
    public static final String KEY_BLE_PROCEDURE_TIMEOUT = "BLE_Procedure_Timeout";
    public static final String KEY_BLE_PROCEDURE_RETRY = "BLE_Procedure_Retry";
    public static final String KEY_BLE_PROCEDURE_TIMEOUT_KES = "BLE_Procedure_Timeout_KeyExchange";
    public static final String KEY_BLE_FAST_RECONNECTION_DURATION = "BLE_Fast_Reconnection_Duration";
    public static final String KEY_BLE_PROCEDURE_TIMEOUT_HISTORY = "BLE_Procedure_Timeout_ReadHistory";
    public static final String KEY_BLE_READ_HISTORY_MAX_COUNT = "BLE_ReadHistory_max_count";
    public static final String KEY_BLE_CHALLENGE_RESPONSE_TIME = "SWWatchdogChallengeResponseTime";
    public static final String KEY_BLE_CHALLENGE_REPEAT_TIME = "SWWatchdogChallengeRepeatTime";
    public static final String KEY_BLE_ROCHE_OUI = "rocheOUIRC";
    public static final String KEY_MP_SW_VERSION = "pumpSoftwareVersionCompatibletoRC";
    public static final String KEY_MP_SYSTEM_CONFIG = "systemConfiguration";
    public static final String KEY_MP_MODEL_NAME = "pumpModelNumberString";

    // Health Events Picker
    public static final String KEY_HEALTHEVENTS_DEFAULT = "healthEventDefault";

    // Start up / Health Events Settings
    public static final String KEY_HEALTHEVENTS_PERCENTAGE_MIN = "healthEventPercentageMin";
    public static final String KEY_HEALTHEVENTS_PERCENTAGE_MAX = "healthEventPercentageMax";
    public static final String KEY_HEALTHEVENTS_PERCENTAGE_RESOLUTION = "healthEventPercentageResolution";
    public static final String KEY_HEALTHEVENTS_PERCENTAGE_DEFAULT = "healthEventPercentageDefault";
    public static final String KEY_HEALTHEVENTS_DEFAULT_ITEM = "healthEventDefault";

    // EMWR config
    public static final String KEY_BUTTON_ACTIVT_BLOCK_TIME = "buttonActiveBlockedTime";

    // MicroPump Replacement
    public static final String KEY_RESERVOIR_AMOUNT_DEFAULT_VALUE = "ReservoirVolume.DefaultValue";
    public static final String KEY_RESERVOIR_AMOUNT_FACTORY_MAX_VALUE = "ReservoirVolumeFactoryMax.Value";
    public static final String KEY_RESERVOIR_AMOUNT_FACTORY_MIN_VALUE = "ReservoirVolumeFactoryMin.Value";

    // Battery threshold and allowable range
    public static final String DEAD_BATTERY_THRESHOLD = "deadBatteryThreshold";
    public static final String EMPTY_BATTERY_THRESHOLD = "emptyBatteryThreshold";
    public static final String LOW_BATTERY_THRESHOLD = "lowBatteryThreshold";
    public static final String BATTERY_RECHARGE_TEMPERATURE_MIN = "batteryRechargeTemperatureMin";
    public static final String BATTERY_RECHARGE_TEMPERATURE_MAX = "batteryRechargeTemperatureMax";

    // Settings
    public static final String KEY_LANGUAGE = "userSelectedLanguageDefault";
    public static final String KEY_BRIGHTNESS = "userBacklightIntensityDefault";
    public static final String KEY_TOUCHSCREEN = "touchScreenFeedbackDefault";

    public static final String KEY_SIGNAL_TYPE_NORMAL = "userSignallingTypeNormalDefault";
    public static final String KEY_SIGNAL_TYPE_VIBRATE = "userSignallingTypeVibrateDefault";
    public static final String KEY_SIGNAL_TYPE_QUIET = "userSignallingTypeQuietDefault";
    public static final String KEY_SIGNAL_TYPE_LOUD = "userSignallingTypeLoudDefault";

    public static final String KEY_VOLUME_NORMAL = "userSignalizationLevelNormalDefault";
    public static final String KEY_VOLUME_VIBRATE = "userSignalizationLevelVibrateDefault";
    public static final String KEY_VOLUME_QUIET = "userSignalizationLevelQuietDefault";
    public static final String KEY_VOLUME_LOUD = "userSignalizationLevelLoudDefault";

    public static final String KEY_MDI_INSULIN_INCREMENT = "MDIInsulinIncrementMax";
    public static final String KEY_MDI_MAX_BOLUS = "MDIMaxBolusAmountDefault";

    public static final String KEY_SIGNALIZATION_MODE = "userSignalizationModeDefault";
    public static final String KEY_SIGNAL_SUSPEND_ENABLE = "SignalizationProfileWSuspendEnable";
    public static final String KEY_SIGNAL_SUSPEND_STATUS = "SignalizationProfileWSuspendRepeat";
    public static final String KEY_SUSPEND_START_TIME = "SignalizationProfileWSuspendStartTime";
    public static final String KEY_SUSPEND_END_TIME = "SignalizationProfileWSuspendEndTime";
    public static final String KEY_SUSPEND_DURATION_MAX = "SignalizationProfileWSuspendDurationMax";

    // Warning Limit
    public static final String HYPER_THRESHOLD = "hyperglycemicThresholdDefault";
    public static final String HYPER_THRESHOLD_INCREMENT = "hyperglycemicThresholdIncrement";
    public static final String HYPER_THRESHOLD_MIN = "hyperglycemicThresholdMin";
    public static final String HYPER_THRESHOLD_MAX = "hyperglycemicThresholdMax";
    public static final String HYPO_THRESHOLD = "hypoglycemicThresholdDefault";
    public static final String HYPO_THRESHOLD_INCREMENT = "hypoglycemicThresholdIncrement";
    public static final String HYPO_THRESHOLD_MIN = "hypoglycemicThresholdMin";
    public static final String HYPO_THRESHOLD_MAX = "hypoglycemicThresholdMax";
    public static final String PUMP_EXPIRY_DAYS = "warningPumpExpiryOffsetTimeDefault";
    public static final String PUMP_EXPIRY_DAYS_MIN = "warningPumpExpiryOffsetTimeMin";
    public static final String PUMP_EXPIRY_DAYS_MAX = "warningPumpExpiryOffsetTimeMax";
    public static final String RESERVOIR_LEVEL = "pumpWarningReservoirLevel";
    public static final String RESERVOIR_LEVEL_MIN = "pumpWarningReservoirLevelMin";
    public static final String RESERVOIR_LEVEL_MAX = "pumpWarningReservoirLevelMax";
    public static final String RESERVOIR_LEVEL_RESOLUTION = "pumpWarningReservoirLevelResolution";
    public static final String AUTO_OFF_TIMEOUT = "AutoOffTimeout";
    public static final String AUTO_OFF_TIMEOUT_MIN = "AutoOffTimeoutMin";
    public static final String AUTO_OFF_TIMEOUT_MAX = "AutoOffTimeoutMax";
    public static final String AUTO_OFF_TIMEOUT_RESOLUTION = "AutoOffTimeoutResolution";
    public static final String AUTO_OFF_ENABLE = "AutoOffEnable";

    // Reminder
    public static final String INFUSION_SET_STATUS = "reminderPumpChangeInfusionSetStatusDefault";
    public static final String INFUSION_SET_INTERVAL = "reminderPumpChangeInfusionSetIntervalDefault";
    public static final String INFUSION_SET_TIME = "reminderPumpChangeInfusionSetTimeDefault";
    public static final String INFUSION_SET_TONE = "reminderSignalizationMelodyDefault";
    public static final String INFUSION_SET_REPEAT_STATUS = "reminderPumpChangeInfusionSetRepeatStatusDefault";
    
    public static final String BG_TEST_SET_STATUS = "reminderStatusDefault";
    public static final String BG_TEST_SET_TIME = "reminderbGTestTimeDefault";
    public static final String BG_TEST_SET_TONE = "reminderSignalizationMelodyDefault";
    public static final String BG_TEST_SET_REPEAT_STATUS = "reminderbGTestRepeatStatusDefault";
    
    public static final String MISSED_BOLUS_STATUS = "reminderStatusDefault";
    public static final String MISSED_BOLUS_TIME = "reminderMissedBolusTimeDefault";
    public static final String MISSED_BOLUS_TONE = "reminderSignalizationMelodyDefault";
    public static final String MISSED_BOLUS_REPEAT_STATUS = "reminderMissedBolusRepeatStatusDefault";

    public static final String BASAL_INJECTION_STATUS = "reminderBasalInjectionStatusDefault";
    public static final String BASAL_INJECTION_TIME = "reminderBasalInjectionTimeDefault";
    public static final String BASAL_INJECTION_TONE = "reminderSignalizationMelodyDefault";
    public static final String BASAL_INJECTION_REPEAT_STATUS = "reminderBasalInjectionRepeatStatusDefault";

    public static final String DEFAULT_REMINDER_STATUS = "reminderStatusDefault";
    public static final String BG_AFTER_HIGH_THRESHOLD = "reminderbGTestAfterHighbGThresholdDefault";
    public static final String BG_AFTER_HIGH_DURATION = "reminderbGTestAfterHighDurationDefault";
    public static final String DEFAULT_REMINDER_TONE = "reminderSignalizationMelodyDefault";
    public static final String BG_AFTER_HIGH_MIN_DURATION = "reminderbGTestAfterHighDurationMin";
    public static final String BG_AFTER_HIGH_MAX_DURATION = "reminderbGTestAfterHighDurationMax";
    public static final String DEFAULT_TIME_RESOLUTION = "reminderTimeResolution";

    public static final String BG_AFTER_LOW_THRESHOLD = "reminderbGTestAfterLowbGThresholdDefault";
    public static final String BG_AFTER_LOW_DURATION = "reminderbGTestAfterLowDurationDefault";
    public static final String BG_AFTER_LOW_MIN_DURATION = "reminderbGTestAfterLowDurationMin";
    public static final String BG_AFTER_LOW_MAX_DURATION = "reminderbGTestAfterLowDurationMax";

    public static final String BG_AFTER_MEAL_DURATION = "reminderbGTestAfterMealDurationDefault";
    public static final String BG_AFTER_MEAL_MIN_DURATION = "reminderbGTestAfterMealDurationMin";
    public static final String BG_AFTER_MEAL_MAX_DURATION = "reminderbGTestAfterMealDurationMax";

    public static final String DOCTOR_VISIT_TIME = "reminderDoctorVisitTimeDefault";

    public static final String LAB_TEST_TIME = "reminderLabVisitTimeDefault";
    
    public static final String ALARM_CLOCK_TIME = "reminderAlarmClockTimeDefault";
    public static final String ALARM_REPEAT_STATUS = "reminderAlarmClockRepeatStatusDefault";

    public static final String REMINDER_CUSTOM_TIME = "reminderAlarmClockTimeDefault";

    public static final String REMINDER_SNOOZE_TIME = "reminderSnoozeTime";

    public static final String MAX_SNOOZE_TIMES = "maxReminderSnoozeTimesNOM";
    
    // system Mode
    public static final String KEY_METER_ACCESS_LEVEL = "meterAccessLevelDefault";
    
    // The key bolusAdviceStatusDefault is used to find a user setting in the Configuration Matrix.
    public static final String BOLUS_ADVICE_STATUS_DEFAULT = "bolusAdviceStatusDefault";
    
    // The key MDIModeDefault is used to find a user setting in the Configuration Matrix.
    public static final String MDI_MODE_DEFAULT = "MDIModeDefault";
    
    // The key LocalizedBolusAmountLimitMaximum is used to find a user setting in the Configuration Matrix.
    public static final String LOCALIZED_BOLUS_AMOUNT_LIMIT_MAXIMUM = 
            "LocalizedBolusAmountLimitMaximum";
    
    // The key reminderbGTestAfterMealDurationDefault is used to find a user setting in the Configuration Matrix.
    public static final String REMINDER_BG_TEST_AFTER_MEAL_DURATION_DEFAULT = 
            "reminderbGTestAfterMealDurationDefault";
    
    // The key reminderbGTestAfterHighDurationDefault is used to find a user setting in the Configuration Matrix.
    public static final String REMINDER_BG_TEST_AFTER_HIGH_DURATION_DEFAULT = 
            "reminderbGTestAfterHighDurationDefault";
    
    // The key reminderbGTestAfterLowDurationDefault is used to find a user setting in the Configuration Matrix.
    public static final String REMINDER_BG_TEST_AFTER_LOW_DURATION_DEFAULT = 
            "reminderbGTestAfterLowDurationDefault";
    
    // The key bolusAdviceInsulinSensivityRatioInsulinDefault is used to find a user setting in the Configuration Matrix.
    public static final String BOLUS_ADVICE_INSULIN_SENSIVITY_RATIO_INSULIN_DEFAULT = 
            "bolusAdviceInsulinSensivityRatioInsulinDefault";
}

/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// First Revision

// Update header comments
// (R15209 2015-08-22 23:31:51 henrytso)
// ----------------------------------------------------------------------------
// Add BRP KEY_BRP_DURATION_RESOLTION
// (R15781 2015-08-31 02:06:57 JacksonHuang)
// ----------------------------------------------------------------------------
// Support note iocn display in SCR0315 screen
// (R15894 2015-08-31 08:53:56 henrytso)
// ----------------------------------------------------------------------------
// [Setting] add MDI mode
// (R18085 2015-09-16 05:09:21 KiddYeh)
// ----------------------------------------------------------------------------
// [Sound setting] add the screen of signal suspension
// (R22528 2015-10-26 05:22:59 KiddYeh)
// ----------------------------------------------------------------------------
// [Reminder] add Reminder module
// (R22528 2015-10-26 05:22:59 KiddYeh)
// ----------------------------------------------------------------------------
// [Reminder] add Reminder module
// (R23058 2015-11-02 21:19:18 KiddYeh)
// ----------------------------------------------------------------------------
// [Reminder] add Reminder module
// (R23369 2015-11-05 03:40:14 WilliyChiang)
// ----------------------------------------------------------------------------
// [Warning Limits] add screens
// (R24195 2015-11-16 06:01:45 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] add screen - missed bolus reminder, basal injection reminder
// (R24347 2015-11-18 05:17:48 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] 1. add bG after high / low / meal reminders
// 2. add doctor visit and lab test reminders
// (R24549 2015-11-20 06:07:24 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] add Alarm Clock / Custom reminders
// (R24607 2015-11-23 02:51:18 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] update Reminder module
// (R24607 2015-11-23 02:51:18 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] update Reminder module
// (R24607 2015-11-23 02:51:18 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] update Reminder module
