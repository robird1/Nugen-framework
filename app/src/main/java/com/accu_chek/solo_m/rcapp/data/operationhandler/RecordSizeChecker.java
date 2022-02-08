package com.accu_chek.solo_m.rcapp.data.operationhandler;

import java.util.ArrayList;
import java.util.HashMap;

import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IDeleteSelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public class RecordSizeChecker
{
    // The Logcat tag for debugging.
    private static final String TAG = "RecordSizeChecker";

    // the max record count limit for most database tables
    private static final int MAX_RECORD_DEFAULT = 5000;
    
    // the max record count limit for EMW table
    private static final int MAX_RECORD_EMW = 50;
    
    // the max record count limit for time management table
    private static final int MAX_RECORD_TM = 50;
    
    // the default record count value for each database table
    private static final int RECORD_COUNT_EMPTY = 0;

    // the instance of the class RecordSizeChecker
    private static final RecordSizeChecker INSTANCE = new RecordSizeChecker();

    // HashMap instance to map the record count to each defined table
    private static final HashMap<Uri, Integer> mRecordCountMap = 
            new HashMap<Uri, Integer>();

    // HashMap instance to map the max record count limit to each defined table
    private static final HashMap<Uri, Integer> mMaxRecordMap = 
            new HashMap<Uri, Integer>();

    // the Context
    private static Context mContext = null;

    // the data contains the record information
    private static IDBData mData = null;

    static
    {
        mRecordCountMap.put(UrlType.basalProfileUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.basalTimeBlockUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.bgUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.cgUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.emwrUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.histotydataUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.logBookUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.patientRecordTimeBlockUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.patientRecordUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.timesegmentUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.userSettingUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderBGTestUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderInfusionSetUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderMissedBolusUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderBasalInjectionUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderBGAfterMealUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderBGAfterHighUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderBGAfterLowUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderDoctorVisitUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderLabTestUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderAlarmClockUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderCustomUri, RECORD_COUNT_EMPTY);
        mRecordCountMap.put(UrlType.reminderAlarmListUri, RECORD_COUNT_EMPTY);

        mMaxRecordMap.put(UrlType.basalProfileUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.basalTimeBlockUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.bgUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.cgUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.emwrUri, MAX_RECORD_EMW);
        mMaxRecordMap.put(UrlType.histotydataUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.logBookUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.patientRecordTimeBlockUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.patientRecordUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.timesegmentUri, MAX_RECORD_TM);
        mMaxRecordMap.put(UrlType.userSettingUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderBGTestUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderInfusionSetUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderMissedBolusUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderBasalInjectionUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderBGAfterMealUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderBGAfterHighUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderBGAfterLowUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderDoctorVisitUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderLabTestUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderAlarmClockUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderCustomUri, MAX_RECORD_DEFAULT);
        mMaxRecordMap.put(UrlType.reminderAlarmListUri, MAX_RECORD_DEFAULT);

    }

    /**
     * class constructor
     * 
     * @return None
     */
    private RecordSizeChecker()
    {
        // Empty constructor. This class can not be initialized by other class
    }

    /**
     * Get the one and only instance of the class RecordSizeChecker.
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param data : the data contains the current record information
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return Returns the one and only instance of the class RecordSizeChecker
     *         Range: valid object
     *         Unit: RecordSizeChecker
     *         Scaling: 1
     * 
     * @see mContext
     * @see mData
     */
    public static RecordSizeChecker getInstance(Context context, IDBData data)
    {
        mContext = context;
        mData = data;

        return INSTANCE;
    }

    /**
     * Update the record count of a certain table stored in the HashMap after
     * the insert operation and then check whether the record count exceeds the
     * max limit count or not.
     * 
     * @return Returns true if the record count exceeds the max record limit of
     *         a certain table after the insert operation. If the record count
     *         exceeds the limitation, the oldest record of a certain table will
     *         be removed.
     *         Range: true / false
     *         Unit: boolean
     *         Scaling: 1
     */
    boolean checkAfterInsert()
    {
        updateRecordCountMap();

        return checkRecordCount();
    }

    /**
     * Update the record count of a certain table stored in the HashMap after
     * the delete operation.
     * 
     * @param deleteRecordCount : the deleted record count
     *            Range: 0 ~ 5000
     *            Unit: int
     *            Scaling: 1
     */
    void checkAfterDelete(int deleteRecordCount)
    {
        updateRecordCountMap(deleteRecordCount);
    }

    /**
     * Update the record count of a certain table stored in the HashMap after
     * the insert operation.
     * 
     * @return Returns true if the record count in HashMap is restored by
     *         querying the database. Record count will be restored when process
     *         has been removed or system reboot.
     *         Range: true / false
     *         Unit: boolean
     *         Scaling: 1
     * 
     * @see mRecordCountMap
     * @see mData
     */
    private boolean updateRecordCountMap()
    {
        boolean isRestoreFromDB = false;

        if (mRecordCountMap.get(mData.onUri()) == RECORD_COUNT_EMPTY)
        {
            int recordCountInDB = getRecordCountFromDB();

            mRecordCountMap.put(mData.onUri(), recordCountInDB);

            isRestoreFromDB = true;
        }
        else
        {
            int recordCount = mRecordCountMap.get(mData.onUri());

            mRecordCountMap.put(mData.onUri(), recordCount + 1);

        }

        return isRestoreFromDB;
    }

    /**
     * Get the record count of a certain table by querying database.
     * 
     * @return Returns the record count of a certain database table.
     *            Range: 0 ~ 5000
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mData
     * @see mContext
     */
    private int getRecordCountFromDB()
    {
        Debug.printI(TAG,
                "[Enter] getRecordCountFromDB(). current instance is: "
                        + this.getClass().getSimpleName());

        int recordCountInDB = 0;
        ArrayList<IDBData> queryResult = null;

        // query all record
        mData.setQuerySelectTypeInterface(null);

        queryResult = (ArrayList<IDBData>) new QueryHandler<IDBData>(mContext,
                null, mData).start();

        if (queryResult != null)
        {
            recordCountInDB = queryResult.size();

            Debug.printI(TAG, "queried record count: " + recordCountInDB);
        }

        return recordCountInDB;
    }

    /**
     * Check whether the record count exceeds the max limit count or not. If the
     * record count exceeds the limitation, the oldest record of a certain table
     * will be removed.
     * 
     * @return Returns true if the record count exceeds the max record limit of
     *         a certain table after the insert operation.
     *         Range: true / false
     *         Unit: boolean
     *         Scaling: 1
     * 
     * @see mContext
     * @see mData
     * @see mRecordCountMap
     * @see mMaxRecordMap
     */
    private boolean checkRecordCount()
    {
        Debug.printI(TAG, "[Enter] checkRecordCount(). current instance is: "
                + this.getClass().getSimpleName());

        boolean isOverMaxRecordLimit = false;
        Uri tableUri = mData.onUri();
        int recordCount = mRecordCountMap.get(tableUri);

        if (recordCount > mMaxRecordMap.get(tableUri))
        {
            Debug.printI(TAG,
                    "record count > max size. remove the oldest record.");

            int desiredDeleteCount = recordCount - mMaxRecordMap.get(tableUri);

            Debug.printI(TAG, "desiredDeleteCount: " + desiredDeleteCount);

            // remove the oldest record
            mData.setDeleteSelectTypeInterface(new DeleteArgs(
                    desiredDeleteCount));

            int deleteCount = new DeleteHandler(mContext, null, mData).start();

            Debug.printI(TAG, "deleteCount: " + deleteCount);

            mRecordCountMap.put(tableUri, recordCount - deleteCount);

            isOverMaxRecordLimit = true;

        }
        else
        {
            Debug.printI(TAG, "record count < max size. record count ++");

            Debug.printI(TAG,
                    "mRecordCountMap.get() after check record count: "
                            + mRecordCountMap.get(tableUri));
        }

        return isOverMaxRecordLimit;
    }

    /**
     * Update the record count of a certain table stored in the HashMap after
     * the delete operation.
     * 
     * @param deleteCount : the deleted record count
     *            Range: 0 ~ 5000
     *            Unit: int
     *            Scaling: 1
     * 
     * @return Returns true if the record count in HashMap is restored by
     *         querying the database. Record count will be restored when process
     *         has been removed or system reboot.
     *         Range: true / false
     *         Unit: boolean
     *         Scaling: 1
     * 
     * @see mRecordCountMap
     * @see mData
     */
    private boolean updateRecordCountMap(int deleteCount)
    {
        boolean isRestoreFromDB = false;
        int recordCountBeforeDelete = mRecordCountMap.get(mData.onUri());

        if (recordCountBeforeDelete != RECORD_COUNT_EMPTY)
        {
            mRecordCountMap.put(mData.onUri(), recordCountBeforeDelete
                    - deleteCount);
        }
        else
        {
            int recordCountInDB = getRecordCountFromDB();

            mRecordCountMap.put(mData.onUri(), recordCountInDB);

            isRestoreFromDB = true;
        }

        Debug.printI(TAG,
                "recordCount after delete in updateRecordCountMap(): "
                        + mRecordCountMap.get(mData.onUri()));

        return isRestoreFromDB;
    }

    class DeleteArgs implements IDeleteSelectType
    {
        // the desired count of the deleted record
        private int mDeleteCount = 0;

        /**
         * Class constructor.
         * 
         * @param deleteCount : the desired count of the deleted record
         *            Range: 0 ~ 5000
         *            Unit: int
         *            Scaling: 1
         * 
         * @see mDeleteCount
         */
        private DeleteArgs(int deleteCount)
        {
            mDeleteCount = deleteCount;
        }

        /**
         * the selection of the delete operation
         * 
         * @return the selection of the delete operation
         *         Range: valid object
         *         Unit: String
         *         Scaling: 1
         */
        @Override
        public String onSelection()
        {
            return mData.getPrimaryKeyName() + " IN (SELECT "
                    + mData.getPrimaryKeyName() + " FROM "
                    + mData.getTableName() + " ORDER BY "
                    + mData.getPrimaryKeyName() + " ASC LIMIT " + mDeleteCount
                    + ")";
        }

        /**
         * the selection arguments of the delete operation
         * 
         * @return the selection arguments of the delete operation
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
