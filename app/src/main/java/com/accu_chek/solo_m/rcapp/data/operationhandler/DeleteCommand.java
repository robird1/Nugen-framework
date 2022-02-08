package com.accu_chek.solo_m.rcapp.data.operationhandler;

import android.content.ContentResolver;
import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IDeleteSelectType;

public class DeleteCommand implements ISQLCommand<Integer>
{
    // tag for debugging
    private static final String TAG = "DeleteCommand";

    // the data to execute the delete command
    private final IDBData mData;

    // the ContentResolver to execute the delete command
    private ContentResolver mResolver = null;

    // the selection for the delete command
    private String mSelection = null;

    // the selectionArgs for the delete command
    private String[] mSelectionArgs = null;

    /**
     * class constructor
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param data : the data for execution
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mResolver: This global variable stores the initial value.
     * @see mData: This global variable stores the initial value.
     */
    public DeleteCommand(Context context, IDBData data)
    {
        Debug.printI(TAG, "[Enter] DeleteCommand()");

        CommonUtils.objectCheck(context, data);

        mResolver = context.getContentResolver();
        mData = data;
    }

    /**
     * Execute the delete command
     * 
     * @return The number of rows deleted.
     *         Range: 0 ~ 5000
     *         Unit: Integer
     *         Scaling: 1
     * 
     * @see mData: Use this global variable to perform the delete operation.
     * @see mResolver: Use this global variable to perform operation on
     *      ContentProvider.
     * @see mSelection: Use this global variable as the argument of the delete
     *      operation.
     * @see mSelectionArgs: Use this global variable as the argument of the
     *      delete operation.
     */
    @Override
    public Integer execute()
    {
        Debug.printI(TAG, "[Enter] execute");

        Debug.printI(TAG, "uri = " + mData.onUri());

        getDeleteParameter();

        Debug.printI(TAG, "where = " + mSelection);

        Debug.printI(TAG, "mSelectionArgs = " + mSelectionArgs);

        int nResult = mResolver.delete(mData.onUri(), mSelection,
                mSelectionArgs);

        return nResult;
    }

    /**
     * Obtain the string for the delete operation.
     * 
     * @return None
     * 
     * @see mData: Use this global variable to perform the delete operation.
     * @see mSelection: Use this global variable as the argument of the delete
     *      operation.
     * @see mSelectionArgs: Use this global variable as the argument of the
     *      delete operation.
     */
    private void getDeleteParameter()
    {
        IDeleteSelectType delteSelectTypeinterface = mData
                .getDeleteSelectTypeInterface();
        if (delteSelectTypeinterface != null)
        {
            mSelection = delteSelectTypeinterface.onSelection();
            mSelectionArgs = delteSelectTypeinterface.onSelectionArgs();
        }
    }

}
