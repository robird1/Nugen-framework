package com.accu_chek.solo_m.rcapp.data.operationhandler;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IUpdateSelectType;

public class UpdateCommand implements ISQLCommand<Integer>
{
    // tag for debugging
    private static final String TAG = "UpdateCommand";

    // the data for the update operation
    private final IDBData mData;

    // the ContentResolver to execute update command
    private ContentResolver mResolver = null;

    // the selection for update command
    private String mSelection = null;

    // the selectionArgs for update command
    private String[] mSelectionArgs = null;

    // the instance to implement the interface IUpdateSelectType
    private IUpdateSelectType mUpdateSelectType = null;

    // primary key name of certain table
    private String mPrimaryKeyName = null;

    // the HashMap which contains the record ID and ContentValues for the update
    // operation
    private Map<Integer, ContentValues> mMap = new HashMap<Integer, ContentValues>();

    /**
     * Class constructor.
     * 
     * @param context : The Context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param data : The data for execution.
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mContext: This global variable stores the initial value.
     * @see mData: This global variable stores the initial value.
     * @see mResolver: This global variable stores the initial value.
     * @see mPrimaryKeyName: This global variable stores the initial value.
     * @see mUpadteSelectType: This global variable stores the initial value.
     * @see mSelection: This global variable stores the initial value.
     * @see mSelectionArgs: This global variable stores the initial value.
     */
    public UpdateCommand(Context context, IDBData data)
    {
        Debug.printI(TAG, "[UpdateCommand] enter");

        CommonUtils.objectCheck(context, data);

        mResolver = context.getContentResolver();
        mData = data;
        mPrimaryKeyName = data.getPrimaryKeyName();
        mUpdateSelectType = mData.getUpdateSelectTypeInterface();

        if (mUpdateSelectType != null)
        {
            mSelection = mUpdateSelectType.onSelection();
            mSelectionArgs = mUpdateSelectType.onSelectionArgs();
        }
    }

    /**
     * Execute the update command.
     * 
     * @return The number of rows updated.
     *         Range: 0 ~ 5000
     *         Unit: Integer
     *         Scaling: 1
     * 
     * @see mPrimaryKeyName: Use this global variable to update certain record.
     * @see mMap: Use this global variable to store the record Id for the later
     *      update.
     * @see mData: Use this global variable to perform the update command.
     * @see mResolver: Use this global variable to perform operation on
     *      ContentProvider.
     * 
     */
    @Override
    public Integer execute()
    {
        Debug.printI(TAG, "[execute] enter");

        int rowsUpdated = 0;
        int count = 0;
        Map.Entry<Integer, ContentValues> entry = null;
        String recordId = null;
        ContentValues value = null;
        String selection = mPrimaryKeyName.concat("=?");

        updateCRCData();

        Iterator<Map.Entry<Integer, ContentValues>> iterator = mMap.entrySet()
                .iterator();

        while (iterator.hasNext())
        {
            entry = iterator.next();

            recordId = String.valueOf(entry.getKey());
            value = entry.getValue();

            Debug.printI(TAG, "selection = " + selection);
            Debug.printI(TAG, "selectionArgs(recordId) = " + recordId);
            Debug.printI(TAG, "value = " + value);

            count = mResolver.update(mData.onUri(), value, selection,
                    new String[] { recordId });

            rowsUpdated += count;
        }

        Debug.printI(TAG, "index = " + rowsUpdated);

        return rowsUpdated;
    }

    /**
     * Update the CRC value of a certain record.
     * 
     * @return None
     * 
     * @see mData: Use this global variable to obtain Uri path.
     * @see mResolver: Use this global variable to query certain record for
     *      calculating CRC value.
     * @see mSelection: Use this global variable to obtain the update argument.
     * @see mSelectionArgs: Use this global variable to obtain the update
     *      argument.
     * 
     */
    private void updateCRCData()
    {
        Cursor cursor = mResolver.query(mData.onUri(), null, mSelection,
                mSelectionArgs, null);

        if (cursor != null)
        {
            int recordCount = cursor.getCount();

            Debug.printI(TAG, "cursor.getCount(): " + cursor.getCount());

            putContentValueInHashMap(cursor, recordCount);

            cursor.close();
        }

    }

    /**
     * Put the desired updated ID and updated value into a HashMap.
     * 
     * @param cursor : A Cursor object, which is positioned before the first
     *            entry.
     *            Range: valid object
     *            Unit: Cursor
     *            Scaling: 1
     * @param recordCount : The number of rows in the cursor.
     *            Range: 0 ~ 5000
     *            Unit: int
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mPrimaryKeyName: Use this global variable to update certain record.
     * @see mMap: Use this global variable to store the record ID for the later
     *      update.
     * @see mData: Use this global variable to perform the update command.
     * 
     */
    private void putContentValueInHashMap(Cursor cursor, int recordCount)
    {
        CommonUtils.objectCheck(cursor);

        if (recordCount != 0)
        {
            cursor.moveToFirst();

            int primaryKeyIndex = cursor.getColumnIndex(mPrimaryKeyName);
            int recordId = -1;
            IDBData data = null;
            ContentValues value = null;

            do
            {
                recordId = cursor.getInt(primaryKeyIndex);

                // get the record for the update operation
                data = mData.onQueryDataFromCursor(cursor);

                // get the updated column value and CRC value
                value = data.onUpdateContentValues();

                Debug.printI(TAG, "mRecordDataList add value = " + value);

                mMap.put(recordId, value);

            } while (cursor.moveToNext());

//            cursor.close();
        }
    }
}
// [Fixed NSIQ-55]
