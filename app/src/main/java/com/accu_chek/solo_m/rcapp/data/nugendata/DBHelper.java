package com.accu_chek.solo_m.rcapp.data.nugendata;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.accu_chek.solo_m.rcapp.application.reminder.ReminderAlarmClockTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderAlarmListTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderBGAfterHighTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderBGAfterLowTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderBGAfterMealTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderBGTestTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderBasalInjectionTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderCustomTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderDoctorVisitTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderInfusionSetTable;
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderLabTestTable;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.operationhandler.SerialNumberGenerator;

public class DBHelper extends SQLiteOpenHelper
{
    
    // table name of bG test reminder
    public static final String REMINDER_ALARM_LIST_TABLE = "reminder_alarm_list_table";
    // table name of bG test reminder
    public static final String REMINDER_BG_TEST_TABLE = "reminder_bg_test_table";
    // table name of change infusion set reminder
    public static final String REMINDER_INFUSION_SET_TABLE = "reminder_infusion_set_table";
    // table name of missed bolus reminder
    public static final String REMINDER_MISSED_BOLUS_TABLE = "reminder_missed_bolus_table";
    // table name of basal injection reminder
    public static final String REMINDER_BASAL_INJECTION_TABLE = "reminder_basal_injection_table";
    // table name
    public static final String REMINDER_BG_AFTER_MEAL_TABLE = "reminder_bg_after_meal_table";
    // table name
    public static final String REMINDER_BG_AFTER_HIGH_TABLE = "reminder_bg_after_high_table";
    // table name
    public static final String REMINDER_BG_AFTER_LOW_TABLE = "reminder_bg_after_low_table";
    // table name
    public static final String REMINDER_DOCTOR_VISIT_TABLE = "reminder_doctor_visit_table";
    // table name
    public static final String REMINDER_LAB_TEST_TABLE = "reminder_lab_test_table";
    // table name
    public static final String REMINDER_ALARM_CLOCK_TABLE = "reminder_alarm_clock_table";
    // table name
    public static final String REMINDER_CUSTOM_TABLE = "reminder_custom_table";

    // history data table name
    static final String HISTORYDATA_TABLE = "history_data_table";
    // log book table name
    static final String LOG_BOOK_TABLE = "log_book_table";
    // log book view name
    static final String LOG_BOOK_VIEW = "log_book_view";
    // bG table name
    static final String BG_TABLE = "bg_table";
    // time segment table name
    static final String TIME_SEGMENT_TABLE = "time_segment_table";
    // cG table name
    static final String CG_TABLE = "cg_table";
    // basal profile table name
    static final String BASAL_PROFILE_TABLE = "basal_profile_table";
    // basal time block table name
    static final String BASAL_TIME_BLOCK_TABLE = "basal_time_block_table";
    // patient record table name
    static final String PATIENT_RECORD_TABLE = "patient_record_table";
    // user setting table name
    static final String USER_SETTING_TABLE = "user_setting_table";
    // patient record time block table name
    static final String PATIENT_RECORD_TIME_BLOCK_TABLE = 
            "patient_record_time_block_table";
    // EMWR log table name
    static final String EMWR_LOG_TABLE = "emwr_log_table";

    // database file name
    private static final String DATABASE_NAME = "NUGEN.db";

    // the current database version
    private static final int NUGEN_VERSION = 13;

    // the instance of NugenDBHelper
    private static DBHelper mInstance = null;

    // SQL command for creating HISTORYDATA_TABLE
    private static final String HISTORYDATA_CREATE = "CREATE TABLE "
            + HISTORYDATA_TABLE + " (" + HistoryDataTable.COLUMN_HISTORYDATA_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + HistoryDataTable.COLUMN_EVENT_TYPE + " TEXT,"
            + HistoryDataTable.COLUMN_SEQUENCE_NUM + " TEXT,"
            + HistoryDataTable.COLUMN_TIME_OFFSET + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_DATA + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_ID + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_TITLE + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_DETAIL + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_BOLUSTYPE + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_BOLUSDOSE + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_BOLUSDURATION + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_BASALBRPPERCENTAGE + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_BASALBRPDURATION + " TEXT,"
            + HistoryDataTable.COLUMN_EVENT_BASALTBRDOSE + " TEXT,"
            + HistoryDataTable.COLUMN_TIMESTAMP + " TEXT NOT NULL,"
            + HistoryDataTable.COLUMN_TIMESTAMP_DB + " INTEGER NOT NULL,"
            + HistoryDataTable.COLUMN_PUMP_ID + " TEXT,"
            + HistoryDataTable.COLUMN_SEGMENT_ID + " TEXT NOT NULL,"
            + HistoryDataTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + HistoryDataTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating LOG_BOOK
    private static final String LOG_BOOK_CREATE = "CREATE TABLE "
            + LOG_BOOK_TABLE + " (" + LogBookTable.COLUMN_LOG_BOOK_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT," 
            + LogBookTable.COLUMN_BG_ID + " TEXT," 
            + LogBookTable.COLUMN_BG_ID_DB + " INTEGER,"
            + LogBookTable.COLUMN_BOLUS_ID + " TEXT,"
            + LogBookTable.COLUMN_BOLUS_ID_DB + " INTEGER,"            
            + LogBookTable.COLUMN_NOTE + " TEXT,"
            + LogBookTable.COLUMN_SEGMENT_ID + " TEXT NOT NULL,"
            + LogBookTable.COLUMN_MEAL_TIME + " TEXT,"
            + LogBookTable.COLUMN_CARB_VALUE + " TEXT,"
            + LogBookTable.COLUMN_BASAL_INSULIN_MDI + " TEXT,"
            + LogBookTable.COLUMN_TIMESTAMP + " TEXT,"
            + LogBookTable.COLUMN_TIMESTAMP_DB + " INTEGER,"            
            + LogBookTable.COLUMN_HEALTH_EVENT_FALGS + " TEXT,"
            + LogBookTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + LogBookTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    
    // SQL command for creating LOG_BOOK_VIEW
    private static final String LOG_BOOK_VIEW_CREATE = "CREATE VIEW "
            + LOG_BOOK_VIEW + " AS select " 
    		+ LogBookTable.COLUMN_LOG_BOOK_ID + ","
            + "log_book_table." + LogBookTable.COLUMN_BG_ID + "," 
            + "log_book_table." + LogBookTable.COLUMN_BOLUS_ID + ","
            + LogBookTable.COLUMN_NOTE + ","
            + LogBookTable.COLUMN_MEAL_TIME + ","
            + LogBookTable.COLUMN_CARB_VALUE + ","
            + LogBookTable.COLUMN_BASAL_INSULIN_MDI + ","
            + "log_book_table." + LogBookTable.COLUMN_TIMESTAMP + ","
            + LogBookTable.COLUMN_HEALTH_EVENT_FALGS + ","
            + BGTable.COLUMN_BG_VALUE + "," 
            + "bg_table." + BGTable.COLUMN_USER_SETTING_ID + ","
            + PatientRecordTable.COLUMN_CONFIRM_TOTAL_BOLUS + ","
            + PatientRecordTable.COLUMN_ABSOLUTE_TIME_STAMP + ","
            + PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP + "," 
            + PatientRecordTable.COLUMN_TEST_FLAGS + "," 
            + PatientRecordTable.COLUMN_RECORD_CONTENTS + ","
            + PatientRecordTable.COLUMN_BOLUS_ACTIVATION_TYPE + ","
            + PatientRecordTable.COLUMN_CONFIRM_CORRECTION_BOLUS + ","
            + PatientRecordTable.COLUMN_CONFIRM_MEAL_BOLUS + ","
            + PatientRecordTable.COLUMN_CONFIRM_TOTAL_BOLUS + ","
            + PatientRecordTable.COLUMN_CARB_SUGGESTION + ","
            + PatientRecordTable.COLUMN_ACTIVE_INSULIN + ","
            + PatientRecordTable.COLUMN_CURRENT_ALLOWED_BG + ","
            + PatientRecordTable.COLUMN_CURRENT_TARGET + ","
            + PatientRecordTable.COLUMN_CORRECTION_MEAL_INCREASE + ","
            + PatientRecordTable.COLUMN_CORRECTION_DELTA_BG + ","
            + PatientRecordTable.COLUMN_CURRENT_DELTA_BG + ","
            + PatientRecordTable.COLUMN_CARB_AMOUNT + ","
            + PatientRecordTable.COLUMN_HEALTH_PERCENTAGE + ","
            + PatientRecordTable.COLUMN_USER_SELECT_CORRECTION_BOLUS + ","
            + PatientRecordTable.COLUMN_USER_SELECT_MEAL_BOLUS + ","
            + PatientRecordTable.COLUMN_USER_SELECT_TOTAL_BOLUS + ","
            + PatientRecordTable.COLUMN_RECOMMEND_CORRECTION_BOLUS + ","
            + PatientRecordTable.COLUMN_RECOMMEND_MEAL_BOLUS + ","
            + PatientRecordTable.COLUMN_RECOMMEND_TOTAL_BOLUS + ","
            + PatientRecordTable.COLUMN_BOLUS_DELIVERY_TYPE + ","
            + PatientRecordTable.COLUMN_IMMEDIATE_INSULIN + ","
            + PatientRecordTable.COLUMN_DELAYED_INSULIN + ","
            + PatientRecordTable.COLUMN_DELAYED_DURATION + ","
            + PatientRecordTable.COLUMN_LAG_TIME + ","
            + PatientRecordTable.COLUMN_BOLUS_DURATION
    		+ " FROM log_book_table LEFT JOIN bg_table "
    		+ " ON log_book_table.bg_id_db = bg_table.bg_id LEFT JOIN patient_record_table "
    		+ " ON log_book_table.bolus_id_db = patient_record_table.record_id";
    
    // SQL command for creating BG_TABLE
    private static final String BG_CREATE = "CREATE TABLE " + BG_TABLE + " ("
            + BGTable.COLUMN_BG_ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + BGTable.COLUMN_BG_VALUE + " TEXT," 
            + BGTable.COLUMN_SEGMENT_ID + " TEXT NOT NULL," 
            + BGTable.COLUMN_TIMESTAMP + " TEXT NOT NULL," 
            + BGTable.COLUMN_TIMESTAMP_DB + " INTEGER NOT NULL,"
            + BGTable.COLUMN_IS_PAIRED_TO_PUMP + " TEXT NOT NULL," 
            + BGTable.COLUMN_TEMP_RESULT + " TEXT NOT NULL," 
            + BGTable.COLUMN_USER_SETTING_ID + " TEXT NOT NULL," 
            + BGTable.COLUMN_BG_RESULT + " TEXT,"
            + BGTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + BGTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating TIME_SEGMENT_TABLE
    private static final String TIME_SEGMENT_CREATE = "CREATE TABLE "
            + TIME_SEGMENT_TABLE + " (" + TimeSegmentTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + TimeSegmentTable.COLUMN_SEGMENT_ID + " TEXT NOT NULL,"
            + TimeSegmentTable.COLUMN_START_TIME + " TEXT NOT NULL,"
            + TimeSegmentTable.COLUMN_END_TIME + " TEXT NOT NULL,"
            + TimeSegmentTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + TimeSegmentTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating CG_TABLE
    private static final String CG_CREATE = "CREATE TABLE " + CG_TABLE + " ("
            + CGTable.COLUMN_CG_ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + CGTable.COLUMN_TIMESTAMP + " TEXT NOT NULL,"
            + CGTable.COLUMN_TIMESTAMP_DB + " INTEGER NOT NULL,"            
            + CGTable.COLUMN_CG_RESULT + " TEXT NOT NULL,"
            + CGTable.COLUMN_SEGMENT_ID + " TEXT NOT NULL,"
            + CGTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + CGTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating CG_TABLE
    private static final String BASAL_PROFILE_CREATE = "CREATE TABLE "
            + BASAL_PROFILE_TABLE + " (" + BasalProfileTable.COLUMN_PROFILE_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + BasalProfileTable.COLUMN_PROFILE_NAME + " TEXT,"
            + BasalProfileTable.COLUMN_TOTAL_BASAL + " TEXT NOT NULL,"
            + BasalProfileTable.COLUMN_IS_PROFILE_ACTIVE + " TEXT NOT NULL,"
            + BasalProfileTable.COLUMN_ORDERING_INFO + " TEXT NOT NULL,"
            + BasalProfileTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + BasalProfileTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating BASAL_TIME_BLOCK_TABLE
    private static final String BASAL_TIME_BLOCK_CREATE = "CREATE TABLE "
            + BASAL_TIME_BLOCK_TABLE + " ("
            + BasalTimeBlockTable.COLUMN_BLOCK_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + BasalTimeBlockTable.COLUMN_PROFILE_ID + " TEXT NOT NULL,"
            + BasalTimeBlockTable.COLUMN_END_TIME + " TEXT NOT NULL,"
            + BasalTimeBlockTable.COLUMN_BASAL_RATE + " TEXT NOT NULL,"
            + BasalTimeBlockTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + BasalTimeBlockTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating PATIENT_RECORD_TABLE
    private static final String PATIENT_RECORD_CREATE = "CREATE TABLE "
            + PATIENT_RECORD_TABLE + " (" + PatientRecordTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + PatientRecordTable.COLUMN_BG_ID + " TEXT,"
            + PatientRecordTable.COLUMN_BG_ID_DB + " INTEGER,"                       
            + PatientRecordTable.COLUMN_LOGBOOK_ID + " TEXT,"                        
            + PatientRecordTable.COLUMN_LOGBOOK_ID_DB + " INTEGER,"            
            + PatientRecordTable.COLUMN_SEGMENT_ID + " TEXT NOT NULL,"            
            + PatientRecordTable.COLUMN_BOLUS_ADVICE_FLAGS + " TEXT," 
            + PatientRecordTable.COLUMN_ABSOLUTE_TIME_STAMP + " TEXT NOT NULL," 
            + PatientRecordTable.COLUMN_ABSOLUTE_TIME_STAMP_DB + " INTEGER NOT NULL,"
            + PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP + " TEXT NOT NULL,"
            + PatientRecordTable.COLUMN_RELATIVE_TIME_STAMP_DB + " INTEGER NOT NULL,"
            + PatientRecordTable.COLUMN_TEST_FLAGS + " TEXT," 
            + PatientRecordTable.COLUMN_RECORD_CONTENTS + " TEXT,"
            + PatientRecordTable.COLUMN_CONCENTRATION + " TEXT,"            
            + PatientRecordTable.COLUMN_CARB_AMOUNT + " TEXT,"
            + PatientRecordTable.COLUMN_HEALTH_PERCENTAGE + " TEXT,"
            + PatientRecordTable.COLUMN_USER_SETTING_ID + " TEXT,"
            + PatientRecordTable.COLUMN_USER_SETTING_ID_DB + " INTEGER,"
            + PatientRecordTable.COLUMN_TIME_BLOCK_ID + " TEXT,"
            + PatientRecordTable.COLUMN_TIME_BLOCK_ID_DB + " INTEGER,"            
            + PatientRecordTable.COLUMN_USER_SELECT_CORRECTION_BOLUS + " TEXT,"
            + PatientRecordTable.COLUMN_USER_SELECT_MEAL_BOLUS + " TEXT,"
            + PatientRecordTable.COLUMN_USER_SELECT_TOTAL_BOLUS + " TEXT,"
            + PatientRecordTable.COLUMN_CONFIRM_CORRECTION_BOLUS + " TEXT,"
            + PatientRecordTable.COLUMN_CONFIRM_MEAL_BOLUS + " TEXT,"
            + PatientRecordTable.COLUMN_CONFIRM_TOTAL_BOLUS + " TEXT,"            
            + PatientRecordTable.COLUMN_RECOMMEND_CORRECTION_BOLUS + " TEXT,"
            + PatientRecordTable.COLUMN_RECOMMEND_MEAL_BOLUS + " TEXT,"
            + PatientRecordTable.COLUMN_RECOMMEND_TOTAL_BOLUS + " TEXT,"            
            + PatientRecordTable.COLUMN_CARB_SUGGESTION + " TEXT,"            
            + PatientRecordTable.COLUMN_CURRENT_TARGET + " TEXT,"
            + PatientRecordTable.COLUMN_CORRECTION_MEAL_INCREASE + " TEXT,"
            + PatientRecordTable.COLUMN_CORRECTION_DELTA_BG + " TEXT,"
            + PatientRecordTable.COLUMN_CURRENT_DELTA_BG + " TEXT,"                        
            + PatientRecordTable.COLUMN_CURRENT_ALLOWED_BG + " TEXT,"                        
            + PatientRecordTable.COLUMN_MAX_ALLOWED_BG + " TEXT,"  
            + PatientRecordTable.COLUMN_ACTIVE_INSULIN + " TEXT,"
            + PatientRecordTable.COLUMN_BOLUS_ID + " TEXT,"
            + PatientRecordTable.COLUMN_BOLUS_ID_DB + " INTEGER,"            
            + PatientRecordTable.COLUMN_BOLUS_ACTIVATION_TYPE + " TEXT,"
            + PatientRecordTable.COLUMN_BOLUS_DELIVERY_TYPE + " TEXT,"            
            + PatientRecordTable.COLUMN_IMMEDIATE_INSULIN + " TEXT,"
            + PatientRecordTable.COLUMN_DELAYED_INSULIN + " TEXT,"
            + PatientRecordTable.COLUMN_BOLUS_DURATION + " TEXT,"
            + PatientRecordTable.COLUMN_LAG_TIME + " TEXT,"            
            + PatientRecordTable.COLUMN_DELAYED_DURATION + " TEXT,"
            + PatientRecordTable.COLUMN_CONTROL_SOLUTION + " TEXT,"
            + PatientRecordTable.COLUMN_TIMESTAMP_INVALID + " TEXT,"
            + PatientRecordTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + PatientRecordTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating USER_SETTING_TABLE
    private static final String USER_SETTING_CREATE = "CREATE TABLE "
            + USER_SETTING_TABLE + " ("
            + UserSettingTable.COLUMN_USER_SETTING_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + UserSettingTable.COLUMN_SNACK_SIZE + " TEXT NOT NULL,"
            + UserSettingTable.COLUMN_MEAL_RISE + " TEXT NOT NULL,"
            + UserSettingTable.COLUMN_ACTING_TIME + " TEXT NOT NULL,"
            + UserSettingTable.COLUMN_OFFSET_TIME + " TEXT NOT NULL,"
            + UserSettingTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + UserSettingTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating PATIENT_RECORD_TIME_BLOCK_TABLE
    private static final String PATIENT_RECORD_TIME_BLOCK_CREATE = 
            "CREATE TABLE " + PATIENT_RECORD_TIME_BLOCK_TABLE + " ("
            + PatientRecordTimeBlockTable.COLUMN_TIME_BLOCK_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + PatientRecordTimeBlockTable.COLUMN_CARB_RATIO_INSULIN + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_CARB_RATIO_CARBS + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_INSULIN_SENSITIVITY_INSULIN
            + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_INSULIN_SENSITIVITY_BG
            + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_BG_LOWER_TARGET + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_BG_UPPER_TARGET + " TEXT,"    
            + PatientRecordTimeBlockTable.COLUMN_START_TIME + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_END_TIME + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_IS_ACTIVE + " TEXT,"
            + PatientRecordTimeBlockTable.COLUMN_SERIAL_NUMBER
            + " INTEGER NOT NULL,"
            + PatientRecordTimeBlockTable.COLUMN_CRC
            + " INTEGER NOT NULL" + ");";

    // SQL command for creating EMWR_LOG_TABLE
    private static final String EMWR_LOG_CREATE = "CREATE TABLE "
            + EMWR_LOG_TABLE + " ("
    		+ EMWRLogTable.COLUMN_EMWR_ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
    		
            + EMWRLogTable.COLUMN_SEGMENT_ID + " TEXT NOT NULL,"
            + EMWRLogTable.COLUMN_TIMESTAMP + " TEXT NOT NULL,"
            + EMWRLogTable.COLUMN_TIMESTAMP_DB + " INTEGER NOT NULL,"            
            + EMWRLogTable.COLUMN_INSTANCE_ID + " TEXT NOT NULL,"
            + EMWRLogTable.COLUMN_EMWR_TYPE + " TEXT NOT NULL,"
            + EMWRLogTable.COLUMN_EMWR_STATUS + " TEXT NOT NULL,"
            + EMWRLogTable.COLUMN_MESSAGE_NAME + " TEXT NOT NULL,"
            + EMWRLogTable.COLUMN_COMMENT + " TEXT,"
            
            + EMWRLogTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + EMWRLogTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating REMINDER_BG_TEST_TABLE
    private static final String REMINDER_BG_TEST_CREATE = "CREATE TABLE "
            + REMINDER_BG_TEST_TABLE + " (" + ReminderBGTestTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderBGTestTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderBGTestTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderBGTestTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderBGTestTable.COLUMN_REPEAT_STATUS + " TEXT NOT NULL,"
            + ReminderBGTestTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderBGTestTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderBGTestTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating REMINDER_INFUSION_SET_TABLE
    private static final String REMINDER_INFUSION_SET_CREATE = "CREATE TABLE "
            + REMINDER_INFUSION_SET_TABLE + " (" + ReminderInfusionSetTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderInfusionSetTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderInfusionSetTable.COLUMN_INTERVAL + " TEXT NOT NULL,"
            + ReminderInfusionSetTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderInfusionSetTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderInfusionSetTable.COLUMN_REPEAT_STATUS + " TEXT NOT NULL,"
            + ReminderInfusionSetTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderInfusionSetTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderInfusionSetTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating REMINDER_MISSED_BOLUS_TABLE
    private static final String REMINDER_MISSED_BOLUS_CREATE = "CREATE TABLE "
            + REMINDER_MISSED_BOLUS_TABLE + " (" + ReminderBGTestTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderBGTestTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderBGTestTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderBGTestTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderBGTestTable.COLUMN_REPEAT_STATUS + " TEXT NOT NULL,"
            + ReminderBGTestTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderBGTestTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderBGTestTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    // SQL command for creating REMINDER_BASAL_INJECTION_TABLE
    private static final String REMINDER_BASAL_INJECTION_CREATE = "CREATE TABLE "
            + REMINDER_BASAL_INJECTION_TABLE + " (" + ReminderBasalInjectionTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderBasalInjectionTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderBasalInjectionTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderBasalInjectionTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderBasalInjectionTable.COLUMN_REPEAT_STATUS + " TEXT NOT NULL,"
            + ReminderBasalInjectionTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderBasalInjectionTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderBasalInjectionTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    private static final String REMINDER_BG_AFTER_MEAL_CREATE = "CREATE TABLE "
            + REMINDER_BG_AFTER_MEAL_TABLE + " (" + ReminderBGAfterMealTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderBGAfterMealTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderBGAfterMealTable.COLUMN_REMIND_AFTER + " TEXT NOT NULL,"
            + ReminderBGAfterMealTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderBGAfterMealTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderBGAfterMealTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderBGAfterMealTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    private static final String REMINDER_BG_AFTER_HIGH_CREATE = "CREATE TABLE "
            + REMINDER_BG_AFTER_HIGH_TABLE + " (" + ReminderBGAfterHighTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderBGAfterHighTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderBGAfterHighTable.COLUMN_BG_THRESHOLD + " TEXT NOT NULL,"
            + ReminderBGAfterHighTable.COLUMN_REMIND_AFTER + " TEXT NOT NULL,"
            + ReminderBGAfterHighTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderBGAfterHighTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderBGAfterHighTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderBGAfterHighTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    private static final String REMINDER_BG_AFTER_LOW_CREATE = "CREATE TABLE "
            + REMINDER_BG_AFTER_LOW_TABLE + " (" + ReminderBGAfterLowTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderBGAfterLowTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderBGAfterLowTable.COLUMN_BG_THRESHOLD + " TEXT NOT NULL,"
            + ReminderBGAfterLowTable.COLUMN_REMIND_AFTER + " TEXT NOT NULL,"
            + ReminderBGAfterLowTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderBGAfterLowTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderBGAfterLowTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderBGAfterLowTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    private static final String REMINDER_DOCTOR_VISIT_CREATE = "CREATE TABLE "
            + REMINDER_DOCTOR_VISIT_TABLE + " (" + ReminderDoctorVisitTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderDoctorVisitTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderDoctorVisitTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderDoctorVisitTable.COLUMN_DATE + " TEXT NOT NULL,"
            + ReminderDoctorVisitTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderDoctorVisitTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderDoctorVisitTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderDoctorVisitTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    private static final String REMINDER_LAB_TEST_CREATE = "CREATE TABLE "
            + REMINDER_LAB_TEST_TABLE + " (" + ReminderLabTestTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderLabTestTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderLabTestTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderLabTestTable.COLUMN_DATE + " TEXT NOT NULL,"
            + ReminderLabTestTable.COLUMN_TONE + " TEXT NOT NULL,"            
            + ReminderLabTestTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderLabTestTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderLabTestTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    private static final String REMINDER_ALARM_CLOCK_CREATE = "CREATE TABLE "
            + REMINDER_ALARM_CLOCK_TABLE + " (" + ReminderAlarmClockTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderAlarmClockTable.COLUMN_NAME + " TEXT NOT NULL,"
            + ReminderAlarmClockTable.COLUMN_STATE + " TEXT NOT NULL,"
            + ReminderAlarmClockTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderAlarmClockTable.COLUMN_TONE + " TEXT NOT NULL,"    
            + ReminderAlarmClockTable.COLUMN_REPEAT_STATUS + " TEXT NOT NULL,"
            + ReminderAlarmClockTable.COLUMN_ALARM_REQUEST_CODE + " INTEGER NOT NULL,"
            + ReminderAlarmClockTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderAlarmClockTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    private static final String REMINDER_CUSTOM_CREATE = "CREATE TABLE "
            + REMINDER_CUSTOM_TABLE + " (" + ReminderCustomTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderCustomTable.COLUMN_NAME + " NOT NULL,"
            + ReminderCustomTable.COLUMN_STATE + " NOT NULL,"
            + ReminderCustomTable.COLUMN_TIME + " NOT NULL,"
            + ReminderCustomTable.COLUMN_DATE + " NOT NULL,"
            + ReminderCustomTable.COLUMN_TONE + " NOT NULL,"            
            + ReminderCustomTable.COLUMN_ALARM_REQUEST_CODE + " NOT NULL,"
            + ReminderCustomTable.COLUMN_SERIAL_NUMBER + " NOT NULL,"
            + ReminderCustomTable.COLUMN_CRC + " NOT NULL" + ");";

    private static final String REMINDER_ALARM_LIST_CREATE = "CREATE TABLE "
            + REMINDER_ALARM_LIST_TABLE + " (" + ReminderAlarmListTable.COLUMN_RECORD_ID
            + " INTEGER PRIMARY KEY AUTOINCREMENT,"
            + ReminderAlarmListTable.COLUMN_TIME + " TEXT NOT NULL,"
            + ReminderAlarmListTable.COLUMN_REPEAT_STATUS + " TEXT NOT NULL,"
            + ReminderAlarmListTable.COLUMN_EMWR_CODE + " TEXT NOT NULL,"
            + ReminderAlarmListTable.COLUMN_ALARM_REQUEST_CODE + " TEXT NOT NULL,"
            + ReminderAlarmListTable.COLUMN_SERIAL_NUMBER + " INTEGER NOT NULL,"
            + ReminderAlarmListTable.COLUMN_CRC + " INTEGER NOT NULL" + ");";

    
    // the Context
    private Context mContext = null;

    
    /**
     * Get the one and only instance of the class DBHelper.
     * 
     * @param context : to use to open or create the database
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return the instance of the class DBHelper
     *         Range: valid object
     *         Unit: DBHelper
     *         Scaling: 1
     * 
     * @see mInstance: the instance of the class DBHelper
     */
    public static synchronized DBHelper getInstance(Context context)
    {   
        CommonUtils.objectCheck(context);
        
        if (mInstance == null)
        {
            mInstance = new DBHelper(context);
        }
        return mInstance;
    }

    /**
     * Class constructor.
     * 
     * @param context : to use to open or create the database
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mContext
     */
    private DBHelper(Context context)
    {
        super(context, DATABASE_NAME, null, NUGEN_VERSION);
        
        // "context" will be checked in the function
        // "getInstance(Context context)"

        mContext = context;
    }

    /**
     * Called when the database is created for the first time. This is where the
     * creation of tables and the initial population of the tables should
     * happen.
     * 
     * @param db : the database instance
     *            Range: valid object
     *            Unit: SQLiteDatabase
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mContext
     */
    @Override
    public void onCreate(SQLiteDatabase db)
    {
        CommonUtils.objectCheck(db);

        db.execSQL(HISTORYDATA_CREATE);
        db.execSQL(LOG_BOOK_CREATE);
        db.execSQL(BG_CREATE);
        db.execSQL(TIME_SEGMENT_CREATE);
        db.execSQL(CG_CREATE);
        db.execSQL(BASAL_PROFILE_CREATE);
        db.execSQL(BASAL_TIME_BLOCK_CREATE);
        db.execSQL(PATIENT_RECORD_CREATE);
        db.execSQL(USER_SETTING_CREATE);
        db.execSQL(PATIENT_RECORD_TIME_BLOCK_CREATE);
        db.execSQL(EMWR_LOG_CREATE);
        db.execSQL(REMINDER_BG_TEST_CREATE);
        db.execSQL(REMINDER_INFUSION_SET_CREATE);
        db.execSQL(REMINDER_MISSED_BOLUS_CREATE);
        db.execSQL(REMINDER_BASAL_INJECTION_CREATE);
        db.execSQL(REMINDER_BG_AFTER_MEAL_CREATE);
        db.execSQL(REMINDER_BG_AFTER_HIGH_CREATE);
        db.execSQL(REMINDER_BG_AFTER_LOW_CREATE);
        db.execSQL(REMINDER_DOCTOR_VISIT_CREATE);
        db.execSQL(REMINDER_LAB_TEST_CREATE);
        db.execSQL(REMINDER_ALARM_CLOCK_CREATE);
        db.execSQL(REMINDER_CUSTOM_CREATE);
        db.execSQL(REMINDER_ALARM_LIST_CREATE);

        db.execSQL(LOG_BOOK_VIEW_CREATE);
        SerialNumberGenerator.reset(mContext);
    }

    /**
     * Called when the database needs to be upgraded. The implementation should
     * use this method to drop tables, add tables, or do anything else it needs
     * to upgrade to the new schema version.
     * 
     * @param db : the database instance
     *            Range: valid object
     *            Unit: SQLiteDatabase
     *            Scaling: 1
     * @param nOldVersion : the old database version
     *            Range: integer value greater than zero
     *            Unit: int
     *            Scaling: 1
     * @param nNewVersion : the new database version
     *            Range: integer value greater than zero
     *            Unit: int
     *            Scaling: 1
     * @return None
     * 
     */
    @Override
    public void onUpgrade(SQLiteDatabase db, int nOldVersion, int nNewVersion)
    {
        // It is unnecessary to check the value range of "nOldVersion" and
        // "nNewVersion" since the two arguments will not be used.

        CommonUtils.objectCheck(db);
        
        db.execSQL("DROP TABLE IF EXISTS " + HISTORYDATA_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + LOG_BOOK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + BG_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + TIME_SEGMENT_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + CG_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + BASAL_PROFILE_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + BASAL_TIME_BLOCK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + PATIENT_RECORD_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + USER_SETTING_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + PATIENT_RECORD_TIME_BLOCK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + EMWR_LOG_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_TEST_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_INFUSION_SET_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_MISSED_BOLUS_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BASAL_INJECTION_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_AFTER_MEAL_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_AFTER_HIGH_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_AFTER_LOW_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_DOCTOR_VISIT_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_LAB_TEST_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_ALARM_CLOCK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_CUSTOM_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_ALARM_LIST_TABLE);

        db.execSQL("DROP VIEW IF EXISTS " + LOG_BOOK_VIEW);
        onCreate(db);
    }

    /**
     * Called when the database needs to be dropped.
     * 
     * @param db : The database instance
     *            Range: valid object
     *            Unit: SQLiteDatabase
     *            Scaling: 1
     * 
     * @return None
     * 
     */
    void dropDB(SQLiteDatabase db)
    {
        CommonUtils.objectCheck(db);

        db.execSQL("DROP TABLE IF EXISTS " + HISTORYDATA_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + LOG_BOOK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + BG_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + TIME_SEGMENT_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + CG_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + BASAL_PROFILE_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + BASAL_TIME_BLOCK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + PATIENT_RECORD_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + USER_SETTING_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + PATIENT_RECORD_TIME_BLOCK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + EMWR_LOG_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_TEST_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_INFUSION_SET_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_MISSED_BOLUS_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BASAL_INJECTION_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_AFTER_MEAL_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_AFTER_HIGH_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_BG_AFTER_LOW_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_DOCTOR_VISIT_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_LAB_TEST_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_ALARM_CLOCK_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_CUSTOM_TABLE);
        db.execSQL("DROP TABLE IF EXISTS " + REMINDER_ALARM_LIST_TABLE);

        db.execSQL("DROP VIEW IF EXISTS " + LOG_BOOK_VIEW);
        onCreate(db);
    }
}
// [Reminder] use DB to store Reminder data
// [Reminder] fix the bug of canceling alarm
// [Reminder] add screen - missed bolus reminder, basal injection reminder
// [Reminder] 1. add bG after high / low / meal reminders
// 2. add doctor visit and lab test reminders
// [Reminder] add Alarm Clock / Custom reminders
// [NSM-2889] Update Bolus Database APIs
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
