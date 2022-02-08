/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.data.InsertCommand
 * Brief:Execute insert command.
 * 
 * Create Date: 2013/12/30
 * $Revision: 20513 $
 * $Author: DWYang $
 * $Id: InsertCommand.java 20513 2015-10-01 10:25:24Z DWYang $
 */
package com.accu_chek.solo_m.rcapp.data.operationhandler;

import android.content.ContentResolver;
import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class InsertCommand implements ISQLCommand<Uri>
{
    // tag for debugging
    private static final String TAG = "InsertCommand";

    // the URL path to execute the insert command
    private final Uri mUri;

    // the data for the insert operation
    private final IDBData mData;

    // the ContentResolver to execute the insert command
    private ContentResolver mResolver = null;

    /**
     * Class constructor.
     * 
     * @param context : The Context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param uri : The URL path for execution.
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
     * @param data : The data for execution.
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mUri: This global variable stores the initial value.
     * @see mData: This global variable stores the initial value.
     * @see mResolver: This global variable stores the initial value.
     * 
     */
    public InsertCommand(Context context, Uri uri, IDBData data)
    {
        Debug.printI(TAG, "[InsertCommand] enter");

        CommonUtils.objectCheck(context, uri, data);

        mUri = uri;
        mData = data;
        mResolver = context.getContentResolver();
    }

    /**
     * Execute the insert command.
     * 
     * @return The URL of the newly created row.
     *         Range: valid object
     *         Unit: Uri
     *         Scaling: 1
     * 
     * @see mUri: Use this global variable to obtain the URL path.
     * @see mResolver: Use this global variable to perform operation on
     *      ContentProvider.
     * @see mData: Use this global variable to perform the insert operation.
     */
    @Override
    public Uri execute()
    {
        Debug.printI(TAG, "[execute] enter");
        Uri reUri = mResolver.insert(mUri, mData.onInsertContentValues());

        return reUri;
    }
}
