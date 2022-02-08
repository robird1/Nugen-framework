/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.data.InsertHandler
 * Brief:Execute insert flow.
 * 
 * Create Date: 2013/12/30
 * $Revision: 20513 $
 * $Author: DWYang $
 * $Id: InsertHandler.java 20513 2015-10-01 10:25:24Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.data.operationhandler;

import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class InsertHandler extends DBOperateHandler<Uri>
{
    // tag for debugging
    private static final String TAG = "InsertHandler";

    // the URL path to execute the insert flow
    private final Uri mUri;

    /**
     * Class constructor.
     * 
     * @param context : The Context.
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param nextHandler : The handler instance for the next execution.
     *            Range: null / valid object
     *            Unit: DBOperateHandler
     *            Scaling: 1
     * @param data : The data for execution.
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mUri: This global variable stores the URL path for insertion.
     */
    public InsertHandler(
            Context context, DBOperateHandler<SafetyBoolean> nextHandler,
            IDBData data)
    {
        super(context, nextHandler, data);

        Debug.printI(TAG, "[InsertHandler] enter");

        // Range check has been performed in super class.

        mUri = data.onUri();
    }

    /**
     * Execute the insert command.
     * 
     * @return The URL path of the newly created row.
     *         Range: valid object
     *         Unit: Uri
     *         Scaling: 1
     * 
     * @see mContext: Use this global variable to construct the instance of
     *      InsertCommand.
     * @see mData: Use this global variable to construct the instance of
     *      InsertCommand.
     * @see mUri: Use this global variable to construct the instance of
     *      InsertCommand.
     */
    @Override
    public Uri handle()
    {
        Debug.printI(TAG, "[handle] enter");

        ISQLCommand<Uri> command = new InsertCommand(mContext, mUri, mData);
        Uri result = command.execute();

        return result;
    }
}
