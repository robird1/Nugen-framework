/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.data.QueryCommand
 * Brief:Query command execute.
 * 
 * Create Date: 2013/12/30
 * $Revision: 23851 $
 * $Author: SteveSu $
 * $Id: QueryCommand.java 23851 2015-11-11 08:21:45Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.data.operationhandler;

import java.util.ArrayList;
import java.util.Arrays;

import android.annotation.SuppressLint;
import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class QueryCommand<T extends IDBData> implements
        ISQLCommand<ArrayList<T>>
{
    // tag for debugging
    private static final String TAG = "QueryCommand";

    // the data for performing query command
    private final IDBData mData;

    // the ContentResolver to execute the query command
    private ContentResolver mResolver = null;

    // the URL path to execute the query command
    private Uri mUri = null;

    // the selection for the query command
    private String mSelection = "";

    // the selectionArgs for the query command
    private String[] mSelectionArgs = null;

    // the sort order for the query command
    private String mSortOrder = "";

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
     * @param uri : The URL path for execution.
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * @param selection : A filter declaring which rows to return, formatted as
     *            an SQL WHERE clause (excluding the WHERE itself).
     *            Range: null / valid object
     *            Unit: String
     *            Scaling: 1
     * @param selectionArgs : You may include ?s in selection, which will be
     *            replaced by the values from selectionArgs, in the order that
     *            they appear in the selection.
     *            Range: null / valid object
     *            Unit: String[]
     *            Scaling: 1
     * @param sortOrder : How to order the rows, formatted as an SQL ORDER BY.
     *            Range: null / "ASC" / "DSC"
     *            Unit: String
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mResolver: This global variable stores the initial value.
     * @see mData: This global variable stores the initial value.
     * @see mUri: This global variable stores the initial value.
     * @see mSelection: This global variable stores the initial value.
     * @see mSelectionArgs: This global variable stores the initial value.
     * @see mSortOrder: This global variable stores the initial value.
     * 
     */
    public QueryCommand(
            Context context, IDBData data, Uri uri, String selection,
            String[] selectionArgs, String sortOrder)
    {
        Debug.printI(TAG, "[QueryCommand] enter");

        CommonUtils.objectCheck(context, data, uri);

        mResolver = context.getContentResolver();
        mData = data;
        mUri = uri;
        mSelection = selection;
        if (selectionArgs != null)
        {
            mSelectionArgs = Arrays.copyOf(selectionArgs, selectionArgs.length);
        }
        else
        {
            mSelectionArgs = null;

        }

        mSortOrder = sortOrder;

    }

    /**
     * Execute the query command.
     * 
     * @return ArrayList<T> : The query result.
     *         Range: null / valid object
     *         Unit: ArrayList
     *         Scaling: 1
     * 
     * @see mResolver: Use this global variable to perform operation on
     *      ContentProvider.
     * @see mUri: Use this global variable to obtain the URL path.
     * @see mSelection: Use this global variable as the argument of the query
     *      operation.
     * @see mSelectionArgs: Use this global variable as the argument of the
     *      query operation.
     * @see mSortOrder: Use this global variable as the argument of the query
     *      operation.
     */
    @SuppressLint("UseSparseArrays")
    @Override
    public ArrayList<T> execute()
    {
        Debug.printI(TAG, "[execute] enter");

        ArrayList<T> result = null;
        Cursor cursor = null;

        cursor = mResolver.query(mUri, null, mSelection, mSelectionArgs,
                mSortOrder);

        if (cursor != null)
        {
            int recordCount = cursor.getCount();

            if (recordCount != 0)
            {
                result = queryDataFlow(cursor);
            }

            cursor.close();
        }

        return result;
    }

    /**
     * Query the column values from each table record by a Cursor object.
     * 
     * @param cursor : For the purpose of the query operation.
     *            Range: valid object
     *            Unit: Cursor
     *            Scaling: 1
     * 
     * @return ArrayList<T> : the query result.
     *         Range: null / valid object
     *         Unit: ArrayList
     *         Scaling: 1
     * 
     * @throw DataIntegrityException: An exception caused by different CRC
     *        value.
     * 
     * @see mData: Use this global variable to perform the query operation.
     */
    @SuppressWarnings("unchecked")
    private ArrayList<T> queryDataFlow(Cursor cursor)
    {
        Debug.printI(TAG, "[queryDataFlow] enter");

        IDBData data = null;
        ArrayList<T> result = new ArrayList<T>();

        CommonUtils.objectCheck(cursor);

        cursor.moveToFirst();
        Debug.printI(TAG,
                "cursor.isAfterLast = " + String.valueOf(cursor.isAfterLast()));
        Debug.printI(TAG,
                "cursor.getCount = " + String.valueOf(cursor.getCount()));
        do
        {
            data = mData.onQueryDataFromCursor(cursor);

            if (data != null)
            {
                // generate CRC value from the queried record
                int generatedCRC = data.generateCRC();
                // get CRC value from the column of the queried record
                int recordCRC = data.getCRC();

                if (generatedCRC == recordCRC)
                {
                    result.add((T) data);
                }
                else
                {
                    throw new DataIntegrityException();
                }
            }

        } while (cursor.moveToNext());

//        cursor.close();

        return result;
    }
}
// (R15209 2015-08-22 23:31:51 henrytso)
// ----------------------------------------------------------------------------
// [Fixed NSIQ-55]
