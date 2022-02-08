package com.accu_chek.solo_m.rcapp.data.operationhandler;

import java.util.HashMap;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.data.nugendata.DBProvider;

public abstract class IDBData
{
    /**
     * The table path to perform database operation.
     * 
     * @return The URL path.
     *         Range: valid object
     *         Unit: Uri
     *         Scaling: 1
     */
    public abstract Uri onUri();

    /**
     * The operation to insert values to database.
     * 
     * @return The inserted values in ContentValues type.
     *         Range: valid object
     *         Unit: ContentValues
     *         Scaling: 1
     */
    public abstract ContentValues onInsertContentValues();

    /**
     * The operation to update values to database.
     * 
     * @return The updated values in ContentValues type.
     *         Range: valid object
     *         Unit: ContentValues
     *         Scaling: 1
     */
    public abstract ContentValues onUpdateContentValues();

    /**
     * Query the data from a certain table by a cursor object.
     * 
     * @param cursor : Use this to perform the query operation.
     *            Range: valid object
     *            Unit: Cursor
     *            Scaling: 1
     * 
     * @return The query result.
     *         Range: valid object
     *         Unit: IDBData
     *         Scaling: 1
     */
    public abstract IDBData onQueryDataFromCursor(Cursor cursor);

    /**
     * Obtain the CRC value of the query record.
     * 
     * @return CRC value
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     */
    public abstract int getCRC();

    /**
     * Generate the CRC value of the current record.
     * 
     * @return The generated CRC value.
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     */
    public abstract int generateCRC();

    /**
     * Get the primary key name of a certain table for the update operation.
     * 
     * @return The primary key name of a certain table.
     *          Range: valid object
     *          Unit: String
     *          Scaling: 1
     * 
     */
    public abstract String getPrimaryKeyName();

    /**
     * Get the table name.
     * 
     * @return The table name of a certain table.
     *          Range: valid object
     *          Unit: String
     *          Scaling: 1
     * 
     */
    protected abstract String getTableName();

    // the instance of the interface IQuerySelectType
    private IQuerySelectType mQuerySelectTypeInterface = null;

    // the instance of the interface IDeleteSelectType
    private IDeleteSelectType mDeleteSelectTypeInterface = null;

    // the instance of the interface IUpdateSelectType
    private IUpdateSelectType mUpdateSelectTypeInterface = null;

    public interface UrlType
    {
        Uri histotydataUri = Uri.parse("content://" + DBProvider.AUTHORITY
                + "/" + DBProvider.HISTORYDATA_PATH);
        Uri timesegmentUri = Uri.parse("content://" + DBProvider.AUTHORITY
                + "/" + DBProvider.TIME_SEGMENT_PATH);
        Uri patientRecordUri = Uri.parse("content://" + DBProvider.AUTHORITY
                + "/" + DBProvider.PATIENT_RECORD_PATH);
        Uri patientRecordTimeBlockUri = Uri.parse("content://"
                + DBProvider.AUTHORITY + "/"
                + DBProvider.PATIENT_RECORD_TIME_BLOCK_PATH);
        Uri userSettingUri = Uri.parse("content://" + DBProvider.AUTHORITY
                + "/" + DBProvider.USER_SETTING_PATH);
        Uri logBookUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.LOG_BOOK_PATH);
        Uri logBookViewUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.LOG_BOOK_VIEW_PATH);
        Uri bgUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.BG_PATH);
        Uri cgUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.CG_PATH);
        Uri basalProfileUri = Uri.parse("content://" + DBProvider.AUTHORITY
                + "/" + DBProvider.BASAL_PROFILE_PATH);
        Uri basalTimeBlockUri = Uri.parse("content://" + DBProvider.AUTHORITY
                + "/" + DBProvider.BASAL_TIME_BLOCK_PATH);
        Uri emwrUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.EMWR_LOG_PATH);
        Uri reminderBGTestUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_BG_TEST_PATH);
        Uri reminderInfusionSetUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_INFUSION_SET_PATH);
        Uri reminderMissedBolusUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_MISSED_BOLUS_PATH);
        Uri reminderBasalInjectionUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_BASAL_INJECTION_PATH);
        Uri reminderBGAfterMealUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_BG_AFTER_MEAL_PATH);
        Uri reminderBGAfterHighUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_BG_AFTER_HIGH_PATH);
        Uri reminderBGAfterLowUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_BG_AFTER_LOW_PATH);
        Uri reminderDoctorVisitUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_DOCTOR_VISIT_PATH);
        Uri reminderLabTestUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_LAB_TEST_PATH);
        Uri reminderAlarmClockUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_ALARM_CLOCK_PATH);
        Uri reminderCustomUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_CUSTOM_PATH);
        Uri reminderAlarmListUri = Uri.parse("content://" + DBProvider.AUTHORITY + "/"
                + DBProvider.REMINDER_ALARM_LIST_PATH);

    }

    public interface SelectionType
    {
        String OPENINTERVAL = " BETWEEN ? and ?";
        String ASSIGNVALUE = "=?";
        String RIGHTANDOPENINTERVAL = " >=? and";
        String LEFTANDOPENINTERVAL = " <=?";
    }

    public interface OrderByType
    {
        String ASC = " ASC";
        String DESC = " DESC";
    }

    public interface IQuerySelectType
    {
        /**
         * The selection of the query operation.
         * 
         * @return The selection of the query operation.
         *         Range: null / valid object
         *         Unit: String
         *         Scaling: 1
         * 
         */
        String onSelection();

        /**
         * The selection arguments of the query operation.
         * 
         * @return The selection arguments of the query operation.
         *         Range: null / valid objects
         *         Unit: String[]
         *         Scaling: 1
         * 
         */
        String[] onSelectionArgs();

        /**
         * The sort order of the query result.
         * 
         * @return The sort order of the query result.
         *         Range: null / "ASC" / "DSC"
         *         Unit: String
         *         Scaling: 1
         * 
         */
        String onOrderBy();
    }

    public interface IDeleteSelectType
    {
        /**
         * The selection of the delete operation.
         * 
         * @return The selection of the delete operation.
         *         Range: null / valid object
         *         Unit: String
         *         Scaling: 1
         * 
         */
        String onSelection();

        /**
         * The selection arguments of the delete operation.
         * 
         * @return The selection arguments of the delete operation.
         *         Range: null / valid objects
         *         Unit: String[]
         *         Scaling: 1
         * 
         */
        String[] onSelectionArgs();
    }

    public interface IUpdateSelectType
    {
        /**
         * The selection of the update operation.
         * 
         * @return The selection of the update operation.
         *         Range: null / valid object
         *         Unit: String
         *         Scaling: 1
         * 
         */
        String onSelection();

        /**
         * The selection arguments of the update operation.
         * 
         * @return The selection arguments of the update operation.
         *         Range: null / valid objects
         *         Unit: String[]
         *         Scaling: 1
         * 
         */
        String[] onSelectionArgs();
    }

    /**
     * Get the selected type for the query operation.
     * 
     * @return The selected type instance.
     *         Range: null / valid object
     *         Unit: IQuerySelectType
     *         Scaling: 1
     * 
     * @see mQuerySelectTypeinterface: Use this global variable for the query
     *      operation.
     */
    public IQuerySelectType getQuerySelectTypeInterface()
    {
        return mQuerySelectTypeInterface;
    }

    /**
     * Get the selected type for the delete operation.
     * 
     * @return The selected type instance.
     *         Range: null / valid object
     *         Unit: IDeleteSelectType
     *         Scaling: 1
     * 
     * @see mDeleteSelectTypeinterface: Use this global variable for the delete
     *      operation.
     */
    public IDeleteSelectType getDeleteSelectTypeInterface()
    {
        return mDeleteSelectTypeInterface;
    }

    /**
     * Get the selected type for the update operation.
     * 
     * @return The selected type instance.
     *         Range: null / valid object
     *         Unit: IUpdateSelectType
     *         Scaling: 1
     * 
     * @see mUpdateSelectTypeinterface: Use this global variable for the update
     *      operation.
     */
    public IUpdateSelectType getUpdateSelectTypeInterface()
    {
        return mUpdateSelectTypeInterface;
    }

    /**
     * Set the selected type for the query operation.
     * 
     * @param selectTypeInterface : The selected type instance.
     *            Range: null / valid object
     *            Unit: IQuerySelectType
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mQuerySelectTypeinterface: Use this global variable for the query
     *      operation.
     */
    public void setQuerySelectTypeInterface(IQuerySelectType selectTypeInterface)
    {
        mQuerySelectTypeInterface = selectTypeInterface;
    }

    /**
     * Set the selected type for the delete operation.
     * 
     * @param selectTypeInterface : The selected type instance.
     *            Range: null / valid object
     *            Unit: IDeleteSelectType
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mDeleteSelectTypeinterface: Use this global variable for the delete
     *      operation.
     */
    public void setDeleteSelectTypeInterface(
            IDeleteSelectType selectTypeInterface)
    {
        mDeleteSelectTypeInterface = selectTypeInterface;
    }

    /**
     * Set the selected type for the update operation.
     * 
     * @param selectTypeInterface : The selected type instance.
     *            Range: null / valid object
     *            Unit: IUpdateSelectType
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mUpdateSelectTypeinterface: Use this global variable for the update
     *      operation.
     */
    public void setUpdateSelectTypeInterface(
            IUpdateSelectType selectTypeInterface)
    {
        mUpdateSelectTypeInterface = selectTypeInterface;
    }

    /**
     * Set the insertion values for the insert operation.
     * 
     * @param context : The Context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param map : The values for the insert operation.
     *            Range: valid object
     *            Unit: HashMap<String, Object>
     *            Scaling: 1
     *            
     * @return None
     */
    public void setInsertionValues(Context context, HashMap<String, Object> map)
    {
        // This function will be overridden by subclass.
    }

    /**
     * Set the values for the update operation.
     * 
     * @param map : The values for the update operation.
     *            Range: valid object
     *            Unit: HashMap<String, Object>
     *            Scaling: 1
     * 
     * @return None
     */
    public void setUpdateValues(HashMap<String, Object> map)
    {
        // This function will be overridden by subclass.
    }

    /**
     * Clear the update value after the update operation.
     * 
     * @return None
     */
    public void clearUpdateValues()
    {
        // This function will be overridden by subclass.
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
