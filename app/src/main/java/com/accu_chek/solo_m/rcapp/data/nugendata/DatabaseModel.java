package com.accu_chek.solo_m.rcapp.data.nugendata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import android.content.Context;
import android.net.Uri;

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
import com.accu_chek.solo_m.rcapp.application.reminder.ReminderMissedBolusTable;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.operationhandler.DBOperateHandler;
import com.accu_chek.solo_m.rcapp.data.operationhandler.DeleteHandler;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IDeleteSelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IUpdateSelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.InsertHandler;
import com.accu_chek.solo_m.rcapp.data.operationhandler.QueryHandler;
import com.accu_chek.solo_m.rcapp.data.operationhandler.UpdateHandler;

public class DatabaseModel
{
    // The Logcat tag for debugging.
    private static final String TAG = "DatabaseModle";

    // Stores the mapping between table uri and table instance.
    private static Map<Uri, IDBData> mTableMap = new HashMap<Uri, IDBData>();
    static
    {
        mTableMap.put(UrlType.basalProfileUri, new BasalProfileTable());
        mTableMap.put(UrlType.basalTimeBlockUri, new BasalTimeBlockTable());
        mTableMap.put(UrlType.bgUri, new BGTable());
        mTableMap.put(UrlType.cgUri, new CGTable());
        mTableMap.put(UrlType.emwrUri, new EMWRLogTable());
        mTableMap.put(UrlType.histotydataUri, new HistoryDataTable());
        mTableMap.put(UrlType.logBookUri, new LogBookTable());
        mTableMap.put(UrlType.logBookViewUri, new LogBookView());
        mTableMap.put(UrlType.patientRecordTimeBlockUri, new PatientRecordTimeBlockTable());
        mTableMap.put(UrlType.patientRecordUri, new PatientRecordTable());
        mTableMap.put(UrlType.timesegmentUri, new TimeSegmentTable());
        mTableMap.put(UrlType.userSettingUri, new UserSettingTable());
        mTableMap.put(UrlType.reminderBGTestUri, new ReminderBGTestTable());
        mTableMap.put(UrlType.reminderInfusionSetUri, new ReminderInfusionSetTable());
        mTableMap.put(UrlType.reminderMissedBolusUri, new ReminderMissedBolusTable());
        mTableMap.put(UrlType.reminderBasalInjectionUri, new ReminderBasalInjectionTable());
        mTableMap.put(UrlType.reminderBGAfterMealUri, new ReminderBGAfterMealTable());
        mTableMap.put(UrlType.reminderBGAfterHighUri, new ReminderBGAfterHighTable());
        mTableMap.put(UrlType.reminderBGAfterLowUri, new ReminderBGAfterLowTable());
        mTableMap.put(UrlType.reminderDoctorVisitUri, new ReminderDoctorVisitTable());
        mTableMap.put(UrlType.reminderLabTestUri, new ReminderLabTestTable());
        mTableMap.put(UrlType.reminderAlarmClockUri, new ReminderAlarmClockTable());
        mTableMap.put(UrlType.reminderCustomUri, new ReminderCustomTable());
        mTableMap.put(UrlType.reminderAlarmListUri, new ReminderAlarmListTable());

    }

    // The reference to certain table instance for performing database operation.
    private IDBData mModel = null;

    /**
     * class constructor
     * 
     * @param path : URI reference of certain table
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mTableMap: Use this global variable for obtaining certain table
     *      instance.
     * @see mModel: Use this global variable for performing database operation
     *      to certain table.
     */
    public DatabaseModel(Uri path)
    {
        mModel = mTableMap.get(path);
    }

    /**
     * For inserting data into database.
     * 
     * @param context : To use the database.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param data : The data for inserting.
     *            Range: valid object
     *            Unit: HashMap<String, Object>
     *            Scaling: 1
     * 
     * @return The URL of the newly created row.
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     *            
     * @see mModel: Use this global variable for performing database operation
     *      to certain table.
     */
    public Uri insertData(Context context, HashMap<String, Object> data)
    {
        DBOperateHandler<Uri> inserthandler = null;

        mModel.setInsertionValues(context, data);
        inserthandler = new InsertHandler(context, null, mModel);

        return inserthandler.start();
    }

    /**
     * For querying data from database.
     * 
     * @param context : to use the database
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param type : the query criteria
     *            Range: valid object
     *            Unit: IQuerySelectType
     *            Scaling: 1
     * 
     * @return the query result.
     *            Range: valid object
     *            Unit: ArrayList
     *            Scaling: 1
     *            
     * @see mModel: Use this global variable for performing database operation
     *      to certain table.
     */
    public ArrayList<IDBData> queryData(Context context, IQuerySelectType type)
    {
        ArrayList<IDBData> queryResult = null;

        DBOperateHandler<ArrayList<IDBData>> handler = null;

        mModel.setQuerySelectTypeInterface(type);

        handler = new QueryHandler<IDBData>(context, null, mModel);
        queryResult = handler.start();

        return queryResult;
    }

    /**
     * For deleting data from database.
     * 
     * @param context : to use the database
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param type : the delete criteria
     *            Range: valid object
     *            Unit: IDeleteSelectType
     *            Scaling: 1
     * 
     * @return the delete count
     *            Range: 0 ~ 5000
     *            Unit: int
     *            Scaling: 1
     *            
     * @see mModel: Use this global variable for performing database operation
     *      to certain table.
     */
    public int deleteData(Context context, IDeleteSelectType type)
    {
        int nDeletecount = -1;
        DBOperateHandler<Integer> handler = null;

        mModel.setDeleteSelectTypeInterface(type);

        handler = new DeleteHandler(context, null, mModel);
        nDeletecount = handler.start();

        Debug.printI(TAG, " nDeletecount = " + nDeletecount);

        return nDeletecount;
    }

    /**
     * For updating database data.
     * 
     * @param context : to use the database
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param data : the data for the update operation
     *            Range: valid object
     *            Unit: HashMap<String, Object>
     *            Scaling: 1
     * @param type : the update criteria
     *            Range: valid object
     *            Unit: IUpdateSelectType
     *            Scaling: 1
     * 
     * @return the update count
     *            Range: 0 ~ 5000
     *            Unit: int
     *            Scaling: 1
     *            
     * @see mModel: Use this global variable for performing database operation
     *      to certain table.
     */
    public int updateData(Context context, HashMap<String, Object> data,
            IUpdateSelectType type)
    {
        int nUpdatecount = -1;
        DBOperateHandler<Integer> handler = null;

        mModel.setUpdateValues(data);
        mModel.setUpdateSelectTypeInterface(type);

        handler = new UpdateHandler(context, null, mModel);
        nUpdatecount = (Integer) handler.start();

        // clear the value of class variable [mUpdateValue]
        mModel.clearUpdateValues();

        Debug.printI(TAG, " nUpdatecount = " + nUpdatecount);

        return nUpdatecount;
    }

    /**
     * Get the last record of certain database table.
     * 
     * @param context : to use the database
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return the object which contains record information
     *         Range: valid object
     *         Unit: IDBData
     *         Scaling: 1
     * 
     * @see mModel: Use this global variable for performing database operation
     *      to certain table.
     */
    public IDBData getLastRecord(Context context)
    {
        ArrayList<IDBData> queryResult = null;
        IDBData record = null;

        mModel.setQuerySelectTypeInterface(new QueryLastRecordArgs());

        queryResult = new QueryHandler<IDBData>(context, null, mModel).start();

        if (queryResult != null)
        {
            int size = queryResult.size();

            if (size > 0)
            {
                record = queryResult.get(size - 1);
            }
        }

        return record;

    }

    class QueryLastRecordArgs implements IQuerySelectType
    {

        /**
         * the selection of the query operation
         * 
         * @return the selection of the query operation
         *         Range: null
         *         Unit: String
         *         Scaling: 1
         */
        @Override
        public String onSelection()
        {
            return null;
        }

        /**
         * the selection arguments of the query operation
         * 
         * @return the selection arguments of the query operation
         *         Range: null
         *         Unit: String[]
         *         Scaling: 1
         * 
         */
        @Override
        public String[] onSelectionArgs()
        {
            return null;
        }

        /**
         * the sort order of the query result
         * 
         * @return the sort order of the query result
         *         Range: null
         *         Unit: String
         *         Scaling: 1
         * 
         * @see mModel
         */
        @Override
        public String onOrderBy()
        {
            return mModel.getPrimaryKeyName().concat(" DESC LIMIT 1");
        }

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
