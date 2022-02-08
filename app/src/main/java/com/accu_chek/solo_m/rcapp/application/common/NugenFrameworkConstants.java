/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: NugenFrameworkConstants
 * Brief: Record the constant of the Nugen function in the framework layer
 *
 * Create Date: 9/17/2015
 * $Revision: 23798 $
 * $Author: VictorChen $
 * $Id: NugenFrameworkConstants.java 23798 2015-11-11 02:11:30Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.common;

import android.net.Uri;

public class NugenFrameworkConstants
{
    public static final String PACKAGE_NAME = "com.accu_chek.solo_m.rcapp.application";

    public static final String SP_AUTHORITY = PACKAGE_NAME.concat(".sp");
    public static final Uri SP_URI = Uri.parse("content://"
            .concat(SP_AUTHORITY));

    public static final String SETTING_DESTINATION = "setting";
    public static final String GENERAL_DESTINATION = "general";
    public static final String PRODUCTION_DESTINATION = "production";
    public static final String BASAL_DESTINATION = "basal";

    public static final String SP_KEY_CHECK_POSTFIX = "_check";
    public static final String SP_NAME_BACK_POSTFIX = "_backup";

    public static final String KEY_CONTROL_CLASS_NAME = "control_class_name";
    public static final String KEY_BUNDLE_DATA_FOR_UPDATE_ACTIVITY = "bundle_data_for_update_activity";

    public static final String KEY_SAFE_STATE = "safe_state_flag";
    public static final String KEY_POST_RESULT_STATE = "post_result_flag";

    public static final String KEY_FLIGHT_MODE_STATUS = "flight_mode_status";

    public static class BGMConstants
    {
        public static final String KEY_BG_IS_WORKFLOW_INTERRUPTION = "bg_is_workflow_interruption";
        public static final String KEY_BG_VALUE = "bg__test_value";
        public static final String KEY_BG_TIMESTAMP = "bg_test_timestamp";
        public static final String KEY_BG_VERSION = "bgm_verion";
        public static final String KEY_BG_POST = "bgm_post";
        public static final String KEY_BG_STRIP_CONNECTOR = "Strip_Connector";
        public static final String KEY_BG_INSTRUMENT = "Instrument_Name";
    }

    public static class USBConstants
    {
        public static final String KEY_USB_CONNECT = "usb_is_connected";
        public static final String KEY_USB_CONFIGURE = "usb_is_configured";
    }

    public static class UtilConstants
    {
        public static final String KEY_CHANNEL_1 = "channel_1";
        public static final String KEY_CHANNEL_2 = "channel_2";
    }

    public static class SettingConstants
    {
        public static final String KEY_REMINTER_TONE = "reminder_tone";
        public static final String KEY_CLOCK_FORMAT_ID = "clock_format_id";
        public static final String KEY_HOUR = "hour";
        public static final String KEY_MINUTE = "minute";
        public static final String KEY_TIME_FORMAT_ID = "time_format_id";
        public static final String KEY_YEAR = "year";
        public static final String KEY_MONTH = "month";
        public static final String KEY_MONTH_RESOURCE_ID = "month_resource_id";
        public static final String KEY_DAY = "day";

        public static final String LANGUAGE = "language";

    }

    public static class PowerManagerConstants
    {
        public static final String BATTERY_INFO = "battery_info";
        public static final String SAFE_MODE_STRIP = "safe_mode_strip";
        public static final String POWER_BUTTON_PRESSED = "power_button_pressed";
        public static final String STANDBY_TO_ACTIVE_CYCLE = "standby_to_active_cycle";
        public static final String PUMP_BATTRY_STATUS = "pump_battery_status";
    }

    /**
     * Defined for common U/I component usage
     */
    public static class CommonConstants
    {
        // Defined for common value picler
        public static final String KEY_ACTION_BAR_ICON_ID = "action_bar_icon_id";
        public static final String KEY_ACTION_BAR_TEXT_ID = "action_bar_text_id";
        public static final String KEY_PICKER_VALUE = "picker_value";
        public static final String KEY_PICKER_VALUE_UNIT = "picker_value_unit";
        public static final String KEY_MIN = "picker_min";
        public static final String KEY_MAX = "picker_max";
        public static final String KEY_INCREMENT = "picker_increment";
        public static final String KEY_LOGBOOK_ID_CH1 = "logbook_id_ch1";
        public static final String KEY_LOGBOOK_ID_CH2 = "logbook_id_ch2";
        public static final String KEY_CALLER_ACTIVITY = "caller_activity";
        public static final String KEY_IS_STARTUP = "is_start_flow";
        public static final String KEY_SYSTEM_MODE = "system_mode";

        public static final int RETURN_HYPER_LIMIT = 0x000F;
        public static final int RETURN_HYPO_LIMIT = 0x0033;
        public static final int TRUE = 0x000F;
        public static final int FALSE = 0x0033;

        public static final int BG_UNIT_MGDL = 0x0055;
        public static final int BG_UNIT_MMOL = 0x003C;

        public static final int CARBS_UNIT_GRAMS = 0x0356;
        public static final int CARBS_UNIT_BE = 0x0359;
        public static final int CARBS_UNIT_KE = 0x0365;
        public static final int CARBS_UNIT_CC = 0x036A;

        // Defined for SCR0183 meal time item
        public static final int MEAL_TIME_DUMMY = 0x0000;
        public static final int MEAL_TIME_NO_ENTRY = 0x000F;
        public static final int MEAL_TIME_BEFORE_MEAL = 0x0033;
        public static final int MEAL_TIME_AFTER_MEAL = 0x003C;
        public static final int MEAL_TIME_BEDTIME = 0x0055;
        public static final int MEAL_TIME_FASTING = 0x005A;
        public static final int MEAL_TIME_OTHER = 0x0066;
        // U/I item index. Cannot use the Hamming value.
        public static final int MEAL_TIME_ITEM0 = 0;
        public static final int MEAL_TIME_ITEM1 = 1;
        public static final int MEAL_TIME_ITEM2 = 2;
        public static final int MEAL_TIME_ITEM3 = 3;
        public static final int MEAL_TIME_ITEM4 = 4;
        public static final int MEAL_TIME_ITEM5 = 5;

        // Id of the health event items. (Related to Config Matrix)
        public static final int HEALTH_EVENT_NO_VALUE = 0x000F;
        public static final int HEALTH_EVENT_EXERCISE1 = 0x0033;
        public static final int HEALTH_EVENT_EXECRISE2 = 0x003C;
        public static final int HEALTH_EVENT_STRESS = 0x0055;
        public static final int HEALTH_EVENT_ILLNESS = 0x005A;
        public static final int HEALTH_EVENT_PMS = 0x0066;
        public static final int HEALTH_EVENT_CUSTOM1 = 0x0069;
        public static final int HEALTH_EVENT_CUSTOM2 = 0x0096;
        public static final int HEALTH_EVENT_CUSTOM3 = 0x0099;
        // U/I item index. Cannot use the Hamming value.
        public static final int HEALTH_EVENT_ITEM0 = 0;
        public static final int HEALTH_EVENT_ITEM1 = 1;
        public static final int HEALTH_EVENT_ITEM2 = 2;
        public static final int HEALTH_EVENT_ITEM3 = 3;
        public static final int HEALTH_EVENT_ITEM4 = 4;
        public static final int HEALTH_EVENT_ITEM5 = 5;
        public static final int HEALTH_EVENT_ITEM6 = 6;
        public static final int HEALTH_EVENT_ITEM7 = 7;
        public static final int HEALTH_EVENT_ITEM8 = 8;
        // Defined for how many footer button will be supported in this Activity
        public static final int MAX_HEALTH_EVENT_ITEM = 8;
        // Total allowed of the selected items
        public static final int HEALTH_EVENT_ALLOW_SELECTED = 4;

        // Index for UIAutomator usage. No hamming value needed.
        public static final int INDEX_00 = 0;
        public static final int INDEX_01 = 1;
        public static final int INDEX_02 = 2;
        public static final int INDEX_03 = 3;
        public static final int INDEX_04 = 4;
        public static final int INDEX_05 = 5;
        public static final int INDEX_06 = 6;
        public static final int INDEX_07 = 7;
        public static final int INDEX_08 = 8;
        public static final int INDEX_09 = 9;
        public static final int INDEX_10 = 10;
        public static final int INDEX_11 = 11;
        public static final int INDEX_12 = 12;
        public static final int INDEX_13 = 13;

        public static final int TIME_12H = 0x0F;
        public static final int TIME_24H = 0x33;

        public static final int TIME_AM = 0x0F;
        public static final int TIME_PM = 0x33;
        public static final int TIME_NONE = 0x3C;

        // Used by Common Activity (SCR0183, SCR0206, SCR0092, etc.)
        public static final int CALLBY_SCR0183 = 0x0F;
        public static final int CALLBY_SCR0206 = 0x33;
        public static final int CALLBY_SCR0092 = 0x3C;
        public static final int CALLBY_SCR0204 = 0x55;
        public static final int CALLBY_SCR0277 = 0x5A;
        public static final int CALLBY_SCR0315 = 0x66;

        public static final int INTEGER_TYPE = 0x0F;
        public static final int FLOAT_TYPE = 0x33;

    }

    public static class StartupConstants
    {
        public static final String KEY_CLASS_PATH = "resume_class_path";
        public static final String KEY_RESET_CLASS_PATH = "reset_class_path";
        public static final String KEY_FIRST_USE = "first_use";
        public static final String KEY_RESERVOIR_AMOUNT = "reservoir_amount";
        public static final String KEY_LAGUNAGE = "lagunage";
        public static final String KEY_PINCODE = "pin_code";
        public static final String KEY_CARBOHYDRATE_UNIT = "Carbohydrate_Unit";

        // Define the Key of Health Events Settings
        public static final String KEY_EXERCISE1 = "setting_exercise1";
        public static final String KEY_EXERCISE2 = "setting_exercise2";
        public static final String KEY_STRESS = "setting_stress";
        public static final String KEY_ILLNESS = "setting_illness";
        public static final String KEY_PREMENSTRUAL = "setting_premenstrual";
        public static final String KEY_CUSTOMISED1 = "setting_customised1";
        public static final String KEY_CUSTOMISED2 = "setting_customised2";
        public static final String KEY_CUSTOMISED3 = "setting_customised3";
    }

    public static class BTLEConstants
    {
        public static final String KEY_BTADDRESS = "bonding_device";
        public static final String KEY_BLEVERSION = "ble_version";

        public static final String KEY_PUMP_DIS = "key_pump_device_info";
        public static final String KEY_SERIAL_NUMBER = "key_serial_number";
        public static final String KEY_SW_VERSION = "key_sw_version";
        public static final String KEY_HW_VERSION = "key_hw_version";
    }

    public static class EMWRConstants
    {
        public static final String KEY_REPEAT = "emwr_repeat";
    }

    public static class ProductionConstants
    {
        public static final int KEY_TRUE = 0x0A05;
        public static final int KEY_FALSE = 0x09F9;
        public static final String KEY_BG_MEASUREMENT_DISPALY_UNITS = "bGMeasurementDisplayUnits";
        public static final String KEY_USER_SELECT_LANGUAGE_DEFAULT = "userSelectedLanguageDefault";
        public static final String KEY_DATE_DEFAULT = "dateDefault";
        public static final String KEY_LOCALIZED_BOLUSE_AMOUNT_LIMITMAXIMUM = "LocalizedBolusAmountLimitMaximum";
        public static final String KEY_MAX_LANGUAGES = "maxLanguages";
        public static final String KEY_SELECTABLE_LANGUAGE_01 = "selectableLanguage01";
        public static final String KEY_SELECTABLE_LANGUAGE_02 = "selectableLanguage02";
        public static final String KEY_SELECTABLE_LANGUAGE_03 = "selectableLanguage03";
        public static final String KEY_SELECTABLE_LANGUAGE_04 = "selectableLanguage04";
        public static final String KEY_SELECTABLE_LANGUAGE_05 = "selectableLanguage05";
        public static final String KEY_SELECTABLE_LANGUAGE_06 = "selectableLanguage06";
        public static final String KEY_SELECTABLE_LANGUAGE_07 = "selectableLanguage07";
        public static final String KEY_SELECTABLE_LANGUAGE_08 = "selectableLanguage08";
        public static final String KEY_SELECTABLE_LANGUAGE_09 = "selectableLanguage09";
        public static final String KEY_SELECTABLE_LANGUAGE_10 = "selectableLanguage10";
        public static final String KEY_SELECTABLE_LANGUAGE_11 = "selectableLanguage11";
        public static final String KEY_SELECTABLE_LANGUAGE_12 = "selectableLanguage12";
        public static final String KEY_SELECTABLE_LANGUAGE_13 = "selectableLanguage13";
        public static final String KEY_SELECTABLE_LANGUAGE_14 = "selectableLanguage14";
        public static final String KEY_SELECTABLE_LANGUAGE_15 = "selectableLanguage15";
        public static final String KEY_SELECTABLE_LANGUAGE_16 = "selectableLanguage16";
        public static final String KEY_SELECTABLE_LANGUAGE_17 = "selectableLanguage17";
        public static final String KEY_SELECTABLE_LANGUAGE_18 = "selectableLanguage18";
        public static final String KEY_SELECTABLE_LANGUAGE_19 = "selectableLanguage19";
        public static final String KEY_SELECTABLE_LANGUAGE_20 = "selectableLanguage20";
        public static final String KEY_SELECTABLE_LANGUAGE_21 = "selectableLanguage21";
        public static final String KEY_SELECTABLE_LANGUAGE_22 = "selectableLanguage22";
        public static final String KEY_SELECTABLE_LANGUAGE_23 = "selectableLanguage23";
        public static final String KEY_SELECTABLE_LANGUAGE_24 = "selectableLanguage24";
        public static final String KEY_SELECTABLE_LANGUAGE_25 = "selectableLanguage25";
        public static final String KEY_SELECTABLE_LANGUAGE_26 = "selectableLanguage26";
        public static final String KEY_SELECTABLE_LANGUAGE_27 = "selectableLanguage27";
        public static final String KEY_SELECTABLE_LANGUAGE_28 = "selectableLanguage28";
        public static final String KEY_SELECTABLE_LANGUAGE_29 = "selectableLanguage29";
        public static final String KEY_SELECTABLE_LANGUAGE_30 = "selectableLanguage30";
        public static final String KEY_SELECTABLE_LANGUAGE_31 = "selectableLanguage31";
        public static final String KEY_METER_ACCESS_LEVEL_DEFAULT = "meterAccessLevelDefault";
        public static final String KEY_DISPLAY_LEVEL_LO = "DisplayLevelLO";
        public static final String KEY_PRODUCT_IDENTIFIER = "productIdentifier";
        public static final String KEY_METER_MODEL_NUMBER = "meterModelNumber";
        public static final String KEY_MIN_DATE = "minDate";
        public static final String KEY_ROCHE_PIDRC_PHDC = "rochePIDRC_PHDC";
        public static final String KEY_ROCHE_PIDRC_MTP = "rochePIDRC_MTP";
        public static final String KEY_METER_SERIAL_NUMBER = "meterSerialNumber";
        public static final String KEY_SYSTEM_CONFIGURATION = "systemConfiguration";
        public static final String KEY_SECURITY_LOCK_SUPER_PIN = "securityLockSuperPIN";
        public static final String KEY_SECURITY_LOCK_STATUS = "securityLockStatus";
        public static final String KEY_SECURITY_LOCK_PIN = "securityLockPIN";
        public static final String KEY_SECURITY_LOCK_WALLPAPER = "securityLockWallpaper";
        public static final String KEY_RESETDATAONFATINSERT = "resetDataOnFATInsert";

        public static final String KEY_INPRODUCTION = "inproductionmode";
        public static final String KEY_CLEARFATDATA = "cleardata";
        public static final String KEY_HARDWAREVERSION = "hardware_version";

    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Add new constant -- BASAL_DESTINATION
// [NSIQ-95] Add flag to identify who call SCR0009 to prevent HOME function
// enabled in Startup flow
// [Fixed NSIQ-150]
// (R21115 2015-10-06 22:58:35 StanleyWu)
// ----------------------------------------------------------------------------
// add KEY_RESETDATAONFATINSERT.
// (R21665 2015-10-15 22:52:03 StanleyWu)
// ----------------------------------------------------------------------------
// add instrument name and strip Connector for RPC.
// (R23050 2015-11-02 08:46:08 VictorChen)
// ----------------------------------------------------------------------------
// Refine bGMeasurementDisplayUnits hamming distance.
// (R23131 2015-11-03 11:06:43 KayjeanKu)
// ----------------------------------------------------------------------------
// Refine constant for CM V8.
