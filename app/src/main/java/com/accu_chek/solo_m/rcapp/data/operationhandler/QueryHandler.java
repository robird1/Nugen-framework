/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.data.QueryHandler
 * Brief:Query flow.
 * 
 * Create Date: 2013/12/30
 * $Revision: 15209 $
 * $Author: henrytso $
 * $Id: QueryHandler.java 15209 2015-08-23 03:31:51Z henrytso $
 */

package com.accu_chek.solo_m.rcapp.data.operationhandler;

import java.util.ArrayList;

import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;

import android.content.Context;
import android.net.Uri;

public class QueryHandler<T extends IDBData> extends
        DBOperateHandler<ArrayList<T>>
{
    // the URL path to execute the query flow
    private final Uri mUri;

    // the instance to implement the interface IQuerySelectType
    private IQuerySelectType mSelectType = null;

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
     * @param data : The data for the query operation.
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mUri: This global variable stores the initial value.
     * @see mSelectType: This global variable stores the initial value.
     * 
     */
    public QueryHandler(
            Context context, DBOperateHandler<ArrayList<T>> nextHandler,
            IDBData data)
    {
        super(context, nextHandler, data);

        // Range check has been performed in super class.

        mUri = data.onUri();
        mSelectType = data.getQuerySelectTypeInterface();

        if (mSelectType == null)
        {
            mSelectType = new DefaultSelectType();
        }
    }

    /**
     * Execute the query command.
     * 
     * @return The query result.
     *         Range: null / valid object
     *         Unit: ArrayList
     *         Scaling: 1
     * 
     * @see mContext: Use this global variable as the argument of the query
     *      operation.
     * @see mData: Use this global variable as the argument of the query
     *      operation.
     * @see mUri: Use this global variable as the argument of the query
     *      operation.
     * @see mSelectType: Use this global variable as the argument of the query
     *      operation.
     * 
     */
    @Override
    protected ArrayList<T> handle()
    {
        ArrayList<T> result = null;

        ISQLCommand<ArrayList<T>> command = new QueryCommand<T>(mContext,
                mData, mUri, mSelectType.onSelection(),
                mSelectType.onSelectionArgs(), mSelectType.onOrderBy());

        result = command.execute();

        return result;
    }

    class DefaultSelectType implements IQuerySelectType
    {

        /**
         * The selection of the query operation.
         * 
         * @return The selection of the query operation.
         *         Range: null (query the whole record)
         *         Unit: String
         *         Scaling: 1
         * 
         */
        @Override
        public String onSelection()
        {
            return null;
        }

        /**
         * The selection arguments of the query operation.
         * 
         * @return The selection arguments of the query operation.
         *         Range: null (query the whole record)
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
         * The sort order of the query result.
         * 
         * @return The sort order of the query result.
         *         Range: null (apply default sort order)
         *         Unit: String
         *         Scaling: 1
         * 
         */
        @Override
        public String onOrderBy()
        {
            return null;
        }
    }
}
