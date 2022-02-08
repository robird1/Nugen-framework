package com.accu_chek.solo_m.rcapp.data.nugendata;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.UriMatcher;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class DBProvider extends ContentProvider
{
    // The Logcat tag for debugging.
    public static final String TAG = DBProvider.class.getSimpleName();

    // For building URI
    public static final String SYMBOL = "/";
    
    // The name of Database file
    public static final String DB_NAME = "NUGEN";
    
    // The authority of URI path
    public static final String AUTHORITY = "com.accu_chek.solo_m.rcapp.db.nugen";

    // Database table name
    public static final String HISTORYDATA_TABLE = DBHelper.HISTORYDATA_TABLE;
    // Database table name
    public static final String LOG_BOOK_TABLE = DBHelper.LOG_BOOK_TABLE;
    // Database table name
    public static final String LOG_BOOK_VIEW = DBHelper.LOG_BOOK_VIEW;
    // Database table name
    public static final String BG_TABLE = DBHelper.BG_TABLE;
    // Database table name
    public static final String TIME_SEGMENT_TABLE = DBHelper.TIME_SEGMENT_TABLE;
    // Database table name
    public static final String CG_TABLE = DBHelper.CG_TABLE;
    // Database table name
    public static final String BASAL_PROFILE_TABLE = DBHelper.BASAL_PROFILE_TABLE;
    // Database table name
    public static final String BASAL_TIME_BLOCK_TABLE = 
            DBHelper.BASAL_TIME_BLOCK_TABLE;
    // Database table name
    public static final String PATIENT_RECORD_TABLE = 
            DBHelper.PATIENT_RECORD_TABLE;
    // Database table name
    public static final String USER_SETTING_TABLE = DBHelper.USER_SETTING_TABLE;
    // Database table name
    public static final String PATIENT_RECORD_TIME_BLOCK_TABLE = 
            DBHelper.PATIENT_RECORD_TIME_BLOCK_TABLE;
    // Database table name
    public static final String EMWR_LOG_TABLE = DBHelper.EMWR_LOG_TABLE;
    // Database table name
    public static final String REMINDER_BG_TEST_TABLE = DBHelper.REMINDER_BG_TEST_TABLE;
    // Database table name
    public static final String REMINDER_INFUSION_SET_TABLE = DBHelper.REMINDER_INFUSION_SET_TABLE;
    // Database table name
    public static final String REMINDER_MISSED_BOLUS_TABLE = DBHelper.REMINDER_MISSED_BOLUS_TABLE;
    // Database table name
    public static final String REMINDER_BASAL_INJECTION_TABLE = DBHelper.REMINDER_BASAL_INJECTION_TABLE;
    // Database table name
    public static final String REMINDER_BG_AFTER_MEAL_TABLE = DBHelper.REMINDER_BG_AFTER_MEAL_TABLE;
    // Database table name
    public static final String REMINDER_BG_AFTER_HIGH_TABLE = DBHelper.REMINDER_BG_AFTER_HIGH_TABLE;
    // Database table name
    public static final String REMINDER_BG_AFTER_LOW_TABLE = DBHelper.REMINDER_BG_AFTER_LOW_TABLE;
    // Database table name
    public static final String REMINDER_DOCTOR_VISIT_TABLE = DBHelper.REMINDER_DOCTOR_VISIT_TABLE;
    // Database table name
    public static final String REMINDER_LAB_TEST_TABLE = DBHelper.REMINDER_LAB_TEST_TABLE;
    // Database table name
    public static final String REMINDER_ALARM_CLOCK_TABLE = DBHelper.REMINDER_ALARM_CLOCK_TABLE;
    // Database table name
    public static final String REMINDER_CUSTOM_TABLE = DBHelper.REMINDER_CUSTOM_TABLE;
    // Database table name
    public static final String REMINDER_ALARM_LIST_TABLE = DBHelper.REMINDER_ALARM_LIST_TABLE;

    // The path of HistoryDataTable URI
    public static final String HISTORYDATA_PATH = DB_NAME + SYMBOL
            + HISTORYDATA_TABLE;
    // The path of LogBookTable URI
    public static final String LOG_BOOK_PATH = DB_NAME + SYMBOL
            + LOG_BOOK_TABLE;
    
    // The path of LogBookView URI
    public static final String LOG_BOOK_VIEW_PATH = DB_NAME + SYMBOL
            + LOG_BOOK_VIEW;
    
    // The path of BGTable URI
    public static final String BG_PATH = DB_NAME + SYMBOL + BG_TABLE;
    // The path of TimeSegmentTable URI
    public static final String TIME_SEGMENT_PATH = DB_NAME + SYMBOL
            + TIME_SEGMENT_TABLE;
    // The path of CGTable URI
    public static final String CG_PATH = DB_NAME + SYMBOL + CG_TABLE;
    // The path of BasalProfileTable URI
    public static final String BASAL_PROFILE_PATH = DB_NAME + SYMBOL
            + BASAL_PROFILE_TABLE;
    // The path of BasalTimeBlockTable URI
    public static final String BASAL_TIME_BLOCK_PATH = DB_NAME + SYMBOL
            + BASAL_TIME_BLOCK_TABLE;
    // The path of PatientRecordTable URI
    public static final String PATIENT_RECORD_PATH = DB_NAME + SYMBOL
            + PATIENT_RECORD_TABLE;
    // The path of UserSettingTable URI
    public static final String USER_SETTING_PATH = DB_NAME + SYMBOL
            + USER_SETTING_TABLE;
    // The path of PatientRecordTimeBlockTable URI
    public static final String PATIENT_RECORD_TIME_BLOCK_PATH = DB_NAME
            + SYMBOL + PATIENT_RECORD_TIME_BLOCK_TABLE;
    // The path of EMWRLogTable URI
    public static final String EMWR_LOG_PATH = DB_NAME + SYMBOL
            + EMWR_LOG_TABLE;
    // The path of ReminderBGTestTable URI
    public static final String REMINDER_BG_TEST_PATH = DB_NAME + SYMBOL
            + REMINDER_BG_TEST_TABLE;
    // The path of ReminderInfusionSetTable URI
    public static final String REMINDER_INFUSION_SET_PATH = DB_NAME + SYMBOL
            + REMINDER_INFUSION_SET_TABLE;
    // The path of ReminderMissedBolusTable URI
    public static final String REMINDER_MISSED_BOLUS_PATH = DB_NAME + SYMBOL
            + REMINDER_MISSED_BOLUS_TABLE;
    // The path of ReminderMissedBolusTable URI
    public static final String REMINDER_BASAL_INJECTION_PATH = DB_NAME + SYMBOL
            + REMINDER_BASAL_INJECTION_TABLE;
    
    public static final String REMINDER_BG_AFTER_MEAL_PATH = DB_NAME + SYMBOL
            + REMINDER_BG_AFTER_MEAL_TABLE;
    public static final String REMINDER_BG_AFTER_HIGH_PATH = DB_NAME + SYMBOL
            + REMINDER_BG_AFTER_HIGH_TABLE;
    public static final String REMINDER_BG_AFTER_LOW_PATH = DB_NAME + SYMBOL
            + REMINDER_BG_AFTER_LOW_TABLE;
    public static final String REMINDER_DOCTOR_VISIT_PATH = DB_NAME + SYMBOL
            + REMINDER_DOCTOR_VISIT_TABLE;
    public static final String REMINDER_LAB_TEST_PATH = DB_NAME + SYMBOL
            + REMINDER_LAB_TEST_TABLE;
    public static final String REMINDER_ALARM_CLOCK_PATH = DB_NAME + SYMBOL
            + REMINDER_ALARM_CLOCK_TABLE;
    public static final String REMINDER_CUSTOM_PATH = DB_NAME + SYMBOL
            + REMINDER_CUSTOM_TABLE;
    public static final String REMINDER_ALARM_LIST_PATH = DB_NAME + SYMBOL
            + REMINDER_ALARM_LIST_TABLE;

    // The path which indicates to delete all tables
    public static final String DELETE_ALL = DB_NAME + SYMBOL;

    // The index returned when a URI is matched against the given components
    private static final int I_HISTORYDATA_TABLE = 4;
    // The index returned when a URI is matched against the given components
    private static final int I_LOG_BOOK_TABLE = 5;
    // The index returned when a URI is matched against the given components
    private static final int I_BG_TABLE = 6;
    // The index returned when a URI is matched against the given components
    private static final int I_TIME_SEGMENT_TABLE = 7;
    // The index returned when a URI is matched against the given components
    private static final int I_CG_TABLE = 8;
    // The index returned when a URI is matched against the given components
    private static final int I_BASAL_PROFILE_TABLE = 9;
    // The index returned when a URI is matched against the given components
    private static final int I_BASAL_TIME_BLOCK_TABLE = 10;
    // The index returned when a URI is matched against the given components
    private static final int I_PATIENT_RECORD_TABLE = 11;
    // The index returned when a URI is matched against the given components
    private static final int I_USER_SETTING_TABLE = 12;
    // The index returned when a URI is matched against the given components
    private static final int I_PATIENT_RECORD_TIME_BLOCK_TABLE = 13;
    // The index returned when a URI is matched against the given components
    private static final int I_EMWR_LOG_TABLE = 14;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_BG_TEST_TABLE = 16;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_INFUSION_SET_TABLE = 17;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_MISSED_BOLUS_TABLE = 19;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_BASAL_INJECTION_TABLE = 20;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_BG_AFTER_MEAL_TABLE = 21;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_BG_AFTER_HIGH_TABLE = 22;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_BG_AFTER_LOW_TABLE = 23;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_DOCTOR_VISIT_TABLE = 24;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_LAB_TEST_TABLE = 25;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_ALARM_CLOCK_TABLE = 26;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_CUSTOM_TABLE = 27;
    // The index returned when a URI is matched against the given components
    private static final int I_REMINDER_ALARM_LIST_TABLE = 28;

    // The index returned when a URI is matched against the given components
    private static final int I_DELETE_ALL = 15;
    // The index returned when a URI is matched against the given components
    private static final int I_LOG_BOOK_VIEW = 18;

    // The object of Utility class to aid in matching URIs in content providers. 
    private static final UriMatcher sURIMatcher = new UriMatcher(
            UriMatcher.NO_MATCH);

    // The instance of SQLiteDatabase
    private SQLiteDatabase mSqlitedatabase = null;

    static
    {
        sURIMatcher.addURI(AUTHORITY, HISTORYDATA_PATH, I_HISTORYDATA_TABLE);
        sURIMatcher.addURI(AUTHORITY, DELETE_ALL, I_DELETE_ALL);

        sURIMatcher.addURI(AUTHORITY, LOG_BOOK_PATH, I_LOG_BOOK_TABLE);
        sURIMatcher.addURI(AUTHORITY, LOG_BOOK_VIEW_PATH, I_LOG_BOOK_VIEW);
        sURIMatcher.addURI(AUTHORITY, BG_PATH, I_BG_TABLE);
        sURIMatcher.addURI(AUTHORITY, TIME_SEGMENT_PATH, I_TIME_SEGMENT_TABLE);
        sURIMatcher.addURI(AUTHORITY, CG_PATH, I_CG_TABLE);
        sURIMatcher
                .addURI(AUTHORITY, BASAL_PROFILE_PATH, I_BASAL_PROFILE_TABLE);
        sURIMatcher.addURI(AUTHORITY, BASAL_TIME_BLOCK_PATH,
                I_BASAL_TIME_BLOCK_TABLE);
        sURIMatcher.addURI(AUTHORITY, PATIENT_RECORD_PATH,
                I_PATIENT_RECORD_TABLE);
        sURIMatcher.addURI(AUTHORITY, USER_SETTING_PATH, I_USER_SETTING_TABLE);
        sURIMatcher.addURI(AUTHORITY, PATIENT_RECORD_TIME_BLOCK_PATH,
                I_PATIENT_RECORD_TIME_BLOCK_TABLE);
        sURIMatcher.addURI(AUTHORITY, EMWR_LOG_PATH, I_EMWR_LOG_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_BG_TEST_PATH, I_REMINDER_BG_TEST_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_INFUSION_SET_PATH, I_REMINDER_INFUSION_SET_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_MISSED_BOLUS_PATH, I_REMINDER_MISSED_BOLUS_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_BASAL_INJECTION_PATH, I_REMINDER_BASAL_INJECTION_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_BG_AFTER_MEAL_PATH, I_REMINDER_BG_AFTER_MEAL_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_BG_AFTER_HIGH_PATH, I_REMINDER_BG_AFTER_HIGH_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_BG_AFTER_LOW_PATH, I_REMINDER_BG_AFTER_LOW_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_DOCTOR_VISIT_PATH, I_REMINDER_DOCTOR_VISIT_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_LAB_TEST_PATH, I_REMINDER_LAB_TEST_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_ALARM_CLOCK_PATH, I_REMINDER_ALARM_CLOCK_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_CUSTOM_PATH, I_REMINDER_CUSTOM_TABLE);
        sURIMatcher.addURI(AUTHORITY, REMINDER_ALARM_LIST_PATH, I_REMINDER_ALARM_LIST_TABLE);

    }

    /**
     * Implement this to handle requests to delete one or more rows. The
     * implementation should apply the selection clause when performing
     * deletion, allowing the operation to affect multiple rows in a directory.
     * 
     * @param uri: The full URI to query, including a row ID (if a specific
     *            record is requested).
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * @param selection: An optional restriction to apply to rows when deleting.
     *            Range: null / valid object
     *            Unit: String
     *            Scaling: 1
     * @param selectionArgs: You may include ?s in selection, which will be
     *            replaced by the values from selectionArgs, in order that they
     *            appear in the selection. The values will be bound as Strings.
     *            Range: null / valid object
     *            Unit: String[]
     *            Scaling: 1
     * 
     * @return The number of rows affected.
     *         Range: 0 ~ 5000
     *         Unit: int
     *         Scaling: 1
     * 
     * @throw IllegalArgumentException: an exception caused by unknown Uri
     * 
     * @see mSqlitedatabase: use this global variable to perform database
     *      operation
     */
    @Override
    public int delete(Uri uri, String selection, String[] selectionArgs)
    {
        Debug.printI(TAG, "[delete] enter");

        int nMatch = -1;
        int nDeletenum = 0;

        CommonUtils.objectCheck(uri);

        nMatch = sURIMatcher.match(uri);

        switch (nMatch)
        {
        case I_HISTORYDATA_TABLE:
            nDeletenum = mSqlitedatabase.delete(HISTORYDATA_TABLE, selection,
                    selectionArgs);
            break;
        case I_LOG_BOOK_TABLE:
            nDeletenum = mSqlitedatabase.delete(LOG_BOOK_TABLE, selection,
                    selectionArgs);
            break;
        case I_LOG_BOOK_VIEW:
            nDeletenum = mSqlitedatabase.delete(LOG_BOOK_VIEW, selection,
                    selectionArgs);
            break;
        case I_BG_TABLE:
            nDeletenum = mSqlitedatabase.delete(BG_TABLE, selection,
                    selectionArgs);
            break;
        case I_TIME_SEGMENT_TABLE:
            nDeletenum = mSqlitedatabase.delete(TIME_SEGMENT_TABLE, selection,
                    selectionArgs);
            break;
        case I_CG_TABLE:
            nDeletenum = mSqlitedatabase.delete(CG_TABLE, selection,
                    selectionArgs);
            break;
        case I_BASAL_PROFILE_TABLE:
            nDeletenum = mSqlitedatabase.delete(BASAL_PROFILE_TABLE, selection,
                    selectionArgs);
            break;
        case I_BASAL_TIME_BLOCK_TABLE:
            nDeletenum = mSqlitedatabase.delete(BASAL_TIME_BLOCK_TABLE,
                    selection, selectionArgs);
            break;
        case I_PATIENT_RECORD_TABLE:
            nDeletenum = mSqlitedatabase.delete(PATIENT_RECORD_TABLE,
                    selection, selectionArgs);
            break;
        case I_USER_SETTING_TABLE:
            nDeletenum = mSqlitedatabase.delete(USER_SETTING_TABLE, selection,
                    selectionArgs);
            break;
        case I_PATIENT_RECORD_TIME_BLOCK_TABLE:
            nDeletenum = mSqlitedatabase.delete(
                    PATIENT_RECORD_TIME_BLOCK_TABLE, selection, selectionArgs);
            break;
        case I_EMWR_LOG_TABLE:
            nDeletenum = mSqlitedatabase.delete(EMWR_LOG_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_BG_TEST_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_BG_TEST_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_INFUSION_SET_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_INFUSION_SET_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_MISSED_BOLUS_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_MISSED_BOLUS_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_BASAL_INJECTION_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_BASAL_INJECTION_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_BG_AFTER_MEAL_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_BG_AFTER_MEAL_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_BG_AFTER_HIGH_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_BG_AFTER_HIGH_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_BG_AFTER_LOW_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_BG_AFTER_LOW_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_DOCTOR_VISIT_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_DOCTOR_VISIT_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_LAB_TEST_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_LAB_TEST_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_ALARM_CLOCK_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_ALARM_CLOCK_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_CUSTOM_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_CUSTOM_TABLE, selection,
                    selectionArgs);
            break;
        case I_REMINDER_ALARM_LIST_TABLE:
            nDeletenum = mSqlitedatabase.delete(REMINDER_ALARM_LIST_TABLE, selection,
                    selectionArgs);
            break;
        case I_DELETE_ALL:

            synchronized (mSqlitedatabase)
            {
                DBHelper.getInstance(getContext()).dropDB(mSqlitedatabase);
            }
            nDeletenum = 1;

            break;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }

        // Notify registered observers that a row was updated
        getContext().getContentResolver().notifyChange(uri, null);

        return nDeletenum;
    }

    /**
     * Implement this to handle requests for the MIME type of the data at the
     * given URI. (This function must override from super class)
     * 
     * @param uri: the URI to query
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * 
     * @return a MIME type string, or null if there is no type
     *         Range: null
     *         Unit: String
     *         Scaling: 1
     * 
     */
    @Override
    public String getType(Uri uri)
    {
        // This function must be overridden from super class ContentProvider
        // since the class DBProvider extends ContentProvider. This function is
        // currently not used.
        
        return null;
    }

    /**
     * Implement this to handle requests to insert a new row.
     * 
     * @param uri: The content:// URI of the insertion request. This must not be
     *            null
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * @param values: A set of column_name/value pairs to add to the database.
     *            This must not be null.
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * 
     * @return The URI for the newly inserted item
     *         Range: valid object
     *         Unit: Uri
     *         Scaling: 1
     * 
     * @throw IllegalArgumentException: an exception caused by unknown Uri
     * 
     * @throw SQLException: An exception that indicates there was an error with
     *        SQL parsing or execution.
     * 
     * @see mSqlitedatabase: use this global variable to perform database
     *      operation
     */
    @Override
    public Uri insert(Uri uri, ContentValues values)
    {
        Debug.printI(TAG, "[insert] enter");

        Uri returnUri = null;
        long nRowId = -1L;
        int nMatch = -1;

        CommonUtils.objectCheck(uri, values);

        nMatch = sURIMatcher.match(uri);

        Debug.printI(TAG, "uri: "+ uri);
        Debug.printI(TAG, "uri index: "+ nMatch);

        switch (nMatch)
        {
        case I_HISTORYDATA_TABLE:
            nRowId = mSqlitedatabase.insert(HISTORYDATA_TABLE, null, values);
            break;
        case I_LOG_BOOK_TABLE:
            nRowId = mSqlitedatabase.insert(LOG_BOOK_TABLE, null, values);
            break;
        case I_LOG_BOOK_VIEW:
            nRowId = mSqlitedatabase.insert(LOG_BOOK_VIEW, null, values);
            break;
        case I_BG_TABLE:
            nRowId = mSqlitedatabase.insert(BG_TABLE, null, values);
            break;
        case I_TIME_SEGMENT_TABLE:
            nRowId = mSqlitedatabase.insert(TIME_SEGMENT_TABLE, null, values);
            break;
        case I_CG_TABLE:
            nRowId = mSqlitedatabase.insert(CG_TABLE, null, values);
            break;
        case I_BASAL_PROFILE_TABLE:
            nRowId = mSqlitedatabase.insert(BASAL_PROFILE_TABLE, null, values);
            break;
        case I_BASAL_TIME_BLOCK_TABLE:
            nRowId = mSqlitedatabase.insert(BASAL_TIME_BLOCK_TABLE, null,
                    values);
            break;
        case I_PATIENT_RECORD_TABLE:
            nRowId = mSqlitedatabase.insert(PATIENT_RECORD_TABLE, null, values);
            break;
        case I_USER_SETTING_TABLE:
            nRowId = mSqlitedatabase.insert(USER_SETTING_TABLE, null, values);
            break;
        case I_PATIENT_RECORD_TIME_BLOCK_TABLE:
            nRowId = mSqlitedatabase.insert(PATIENT_RECORD_TIME_BLOCK_TABLE,
                    null, values);
            break;
        case I_EMWR_LOG_TABLE:
            nRowId = mSqlitedatabase.insert(EMWR_LOG_TABLE, null, values);
            break;
        case I_REMINDER_BG_TEST_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_BG_TEST_TABLE, null, values);
            break;
        case I_REMINDER_INFUSION_SET_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_INFUSION_SET_TABLE, null, values);
            break;
        case I_REMINDER_MISSED_BOLUS_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_MISSED_BOLUS_TABLE, null, values);
            break;
        case I_REMINDER_BASAL_INJECTION_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_BASAL_INJECTION_TABLE, null, values);
            break;
        case I_REMINDER_BG_AFTER_MEAL_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_BG_AFTER_MEAL_TABLE, null, values);
            break;
        case I_REMINDER_BG_AFTER_HIGH_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_BG_AFTER_HIGH_TABLE, null, values);
            break;
        case I_REMINDER_BG_AFTER_LOW_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_BG_AFTER_LOW_TABLE, null, values);
            break;
        case I_REMINDER_DOCTOR_VISIT_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_DOCTOR_VISIT_TABLE, null, values);
            break;
        case I_REMINDER_LAB_TEST_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_LAB_TEST_TABLE, null, values);
            break;
        case I_REMINDER_ALARM_CLOCK_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_ALARM_CLOCK_TABLE, null, values);
            break;
        case I_REMINDER_CUSTOM_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_CUSTOM_TABLE, null, values);
            break;
        case I_REMINDER_ALARM_LIST_TABLE:
            nRowId = mSqlitedatabase.insert(REMINDER_ALARM_LIST_TABLE, null, values);
            break;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }

        if (nRowId != -1L)
        {
            returnUri = ContentUris.withAppendedId(uri, nRowId);

            // Notify registered observers that a row was updated
            getContext().getContentResolver().notifyChange(returnUri, null);
        }
        else
        {
            throw new SQLException("Failed to add a record into " + uri);
        }

        return returnUri;
    }

    /**
     * Implement this to initialize your content provider on startup. This
     * method is called for all registered content providers on the application
     * main thread at application launch time. It must not perform lengthy
     * operations, or application startup will be delayed.
     * 
     * @return Returns true if the provider was successfully loaded, false
     *         otherwise
     *         Range: true / false
     *         Unit: boolean
     *         Scaling: 1
     * 
     * @see mSqlitedatabase: use this global variable to perform database
     *      operation
     */
    @Override
    public boolean onCreate()
    {
        Debug.printI(TAG, "[onCreate] enter");
        boolean isResultOK = false;
        mSqlitedatabase = DBHelper.getInstance(getContext())
                .getWritableDatabase();

        if (mSqlitedatabase != null)
        {
            isResultOK = true;
        }
        return isResultOK;
    }

    /**
     * Implement this to handle query requests from clients.
     * 
     * @param uri: The URI to query. This will be the full URI sent by the
     *            client; if the client is requesting a specific record, the URI
     *            will end in a record number that the implementation should
     *            parse and add to a WHERE or HAVING clause, specifying that _id
     *            value.
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * @param projection: The list of columns to put into the cursor. If null
     *            all columns are included.
     *            Range: null / valid object
     *            Unit: String[]
     *            Scaling: 1
     * @param selection: A selection criteria to apply when filtering rows. If
     *            null then all rows are included.
     *            Range: null / valid object
     *            Unit: String
     *            Scaling: 1
     * @param selectionArgs: You may include ?s in selection, which will be
     *            replaced by the values from selectionArgs, in order that they
     *            appear in the selection. The values will be bound as Strings.
     *            Range: null / valid object
     *            Unit: String[]
     *            Scaling: 1
     * @param sortOrder: How the rows in the cursor should be sorted. If null
     *            then the provider is free to define the sort order.
     *            Range: null / "ASC" / "DSC"
     *            Unit: String
     *            Scaling: 1
     * 
     * @return A Cursor object, which is positioned before the first
     *         entry.
     *         Range: valid object
     *         Unit: Cursor
     *         Scaling: 1
     * 
     * @throw IllegalArgumentException: an exception caused by unknown Uri
     * 
     * @see mSqlitedatabase: use this global variable to perform database
     *      operation
     */
    @Override
    public Cursor query(Uri uri, String[] projection, String selection,
            String[] selectionArgs, String sortOrder)
    {
        Debug.printI(TAG, "[query] enter");

        int nMatch = -1;
        Cursor cursor = null;

        CommonUtils.objectCheck(uri);

        nMatch = sURIMatcher.match(uri);

        Debug.printI(TAG, "uri: "+ uri);
        Debug.printI(TAG, "uri index: "+ nMatch);

        switch (nMatch)
        {
        case I_HISTORYDATA_TABLE:
            cursor = mSqlitedatabase.query(HISTORYDATA_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_LOG_BOOK_TABLE:
            cursor = mSqlitedatabase.query(LOG_BOOK_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_LOG_BOOK_VIEW:
            cursor = mSqlitedatabase.query(LOG_BOOK_VIEW, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_BG_TABLE:
            cursor = mSqlitedatabase.query(BG_TABLE, projection, selection,
                    selectionArgs, null, null, sortOrder);
            break;
        case I_TIME_SEGMENT_TABLE:
            cursor = mSqlitedatabase.query(TIME_SEGMENT_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_CG_TABLE:
            cursor = mSqlitedatabase.query(CG_TABLE, projection, selection,
                    selectionArgs, null, null, sortOrder);
            break;
        case I_BASAL_PROFILE_TABLE:
            cursor = mSqlitedatabase.query(BASAL_PROFILE_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_BASAL_TIME_BLOCK_TABLE:
            cursor = mSqlitedatabase.query(BASAL_TIME_BLOCK_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_PATIENT_RECORD_TABLE:
            cursor = mSqlitedatabase.query(PATIENT_RECORD_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_USER_SETTING_TABLE:
            cursor = mSqlitedatabase.query(USER_SETTING_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_PATIENT_RECORD_TIME_BLOCK_TABLE:
            cursor = mSqlitedatabase
                    .query(PATIENT_RECORD_TIME_BLOCK_TABLE, projection,
                            selection, selectionArgs, null, null, sortOrder);
            break;
        case I_EMWR_LOG_TABLE:
            cursor = mSqlitedatabase.query(EMWR_LOG_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_BG_TEST_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_BG_TEST_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_INFUSION_SET_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_INFUSION_SET_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_MISSED_BOLUS_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_MISSED_BOLUS_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_BASAL_INJECTION_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_BASAL_INJECTION_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_BG_AFTER_MEAL_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_BG_AFTER_MEAL_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_BG_AFTER_HIGH_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_BG_AFTER_HIGH_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_BG_AFTER_LOW_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_BG_AFTER_LOW_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_DOCTOR_VISIT_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_DOCTOR_VISIT_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_LAB_TEST_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_LAB_TEST_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_ALARM_CLOCK_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_ALARM_CLOCK_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_CUSTOM_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_CUSTOM_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        case I_REMINDER_ALARM_LIST_TABLE:
            cursor = mSqlitedatabase.query(REMINDER_ALARM_LIST_TABLE, projection,
                    selection, selectionArgs, null, null, sortOrder);
            break;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);

        }

        /**
         * register to watch a content URI for changes
         */
        cursor.setNotificationUri(getContext().getContentResolver(), uri);

        return cursor;
    }

    /**
     * Implement this to handle requests to update one or more rows. The
     * implementation should update all rows matching the selection to set the
     * columns according to the provided values map.
     * 
     * @param uri: The URI to query. This can potentially have a record ID if
     *            this is an update request for a specific record.
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * @param values: A set of column_name/value pairs to update in the
     *            database. This must not be null.
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * @param selection: An optional filter to match rows to update.
     *            Range: null / valid object
     *            Unit: String
     *            Scaling: 1
     * @param selectionArgs: You may include ?s in selection, which will be
     *            replaced by the values from selectionArgs, in order that they
     *            appear in the selection. The values will be bound as Strings.
     *            Range: null / valid object
     *            Unit: String[]
     *            Scaling: 1
     * 
     * @return The number of rows affected
     *         Range: 0 ~ 5000
     *         Unit: int
     *         Scaling: 1
     * 
     * @throw IllegalArgumentException: an exception caused by unknown Uri
     * 
     * @see mSqlitedatabase: use this global variable to perform database
     *      operation
     */
    @Override
    public int update(Uri uri, ContentValues values, String selection,
            String[] selectionArgs)
    {
        Debug.printI(TAG, "[update] enter");

        int nMatch = -1;
        int nUpdatecolumn = 0;

        CommonUtils.objectCheck(uri, values);

        nMatch = sURIMatcher.match(uri);

        Debug.printI(TAG, "uri: "+ uri);
        Debug.printI(TAG, "uri index: "+ nMatch);

        switch (nMatch)
        {
        case I_HISTORYDATA_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(HISTORYDATA_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_LOG_BOOK_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(LOG_BOOK_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_LOG_BOOK_VIEW:
            nUpdatecolumn = mSqlitedatabase.update(LOG_BOOK_VIEW, values,
                    selection, selectionArgs);
            break;
        case I_BG_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(BG_TABLE, values, selection,
                    selectionArgs);
            break;
        case I_TIME_SEGMENT_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(TIME_SEGMENT_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_CG_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(CG_TABLE, values, selection,
                    selectionArgs);
            break;
        case I_BASAL_PROFILE_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(BASAL_PROFILE_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_BASAL_TIME_BLOCK_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(BASAL_TIME_BLOCK_TABLE,
                    values, selection, selectionArgs);
            break;
        case I_PATIENT_RECORD_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(PATIENT_RECORD_TABLE,
                    values, selection, selectionArgs);
            break;
        case I_USER_SETTING_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(USER_SETTING_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_PATIENT_RECORD_TIME_BLOCK_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(
                    PATIENT_RECORD_TIME_BLOCK_TABLE, values, selection,
                    selectionArgs);
            break;
        case I_EMWR_LOG_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(EMWR_LOG_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_BG_TEST_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_BG_TEST_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_INFUSION_SET_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_INFUSION_SET_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_MISSED_BOLUS_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_MISSED_BOLUS_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_BASAL_INJECTION_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_BASAL_INJECTION_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_BG_AFTER_MEAL_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_BG_AFTER_MEAL_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_BG_AFTER_HIGH_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_BG_AFTER_HIGH_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_BG_AFTER_LOW_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_BG_AFTER_LOW_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_DOCTOR_VISIT_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_DOCTOR_VISIT_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_LAB_TEST_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_LAB_TEST_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_ALARM_CLOCK_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_ALARM_CLOCK_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_CUSTOM_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_CUSTOM_TABLE, values,
                    selection, selectionArgs);
            break;
        case I_REMINDER_ALARM_LIST_TABLE:
            nUpdatecolumn = mSqlitedatabase.update(REMINDER_ALARM_LIST_TABLE, values,
                    selection, selectionArgs);
            break;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }

        // Notify registered observers that a row was updated
        getContext().getContentResolver().notifyChange(uri, null);

        return nUpdatecolumn;
    }
}
// [Reminder] use DB to store Reminder data
// [Reminder] add screen - missed bolus reminder, basal injection reminder
// [Reminder] 1. add bG after high / low / meal reminders
// 2. add doctor visit and lab test reminders
// [Reminder] add Alarm Clock / Custom reminders
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
