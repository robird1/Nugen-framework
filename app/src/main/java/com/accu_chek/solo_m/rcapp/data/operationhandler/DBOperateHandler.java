/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.data.DBOperateHandler
 * Brief:Abstract class, for all DB_Model to control the CRUD flow and
 * read/write SharedPreference flag to record the CRUD step.
 * 
 * Create Date: 2013/12/27
 * $Revision: 20513 $
 * $Author: DWYang $
 * $Id: DBOperateHandler.java 20513 2015-10-01 10:25:24Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.data.operationhandler;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public abstract class DBOperateHandler<T>
{
    // to use to open or create the database
    protected final Context mContext;

    // the data for the database operation
    protected final IDBData mData;

    // the next handler for the database operation
    private DBOperateHandler<?> mNextHandler = null;

    // the previous handler for the database operation
    private DBOperateHandler<?> mPreviorHandler = null;

    /**
     * class constructor
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param nextHandler : handler instance for the next execution
     *            Range: null / valid object
     *            Unit: DBOperateHandler
     *            Scaling: 1
     * @param data : the data for the current handler to reference
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mContext: This global variable stores the initial value.
     * @see mNextHandler: This global variable stores the initial value.
     * @see mData: This global variable stores the initial value.
     * 
     */
    public DBOperateHandler(
            Context context, DBOperateHandler<?> nextHandler, IDBData data)
    {
        CommonUtils.objectCheck(context, data);

        mContext = context;
        mNextHandler = nextHandler;
        if (mNextHandler != null)
        {
            mNextHandler.setPreviousHandler(this);
        }
        mData = data;
    }

    /**
     * Set the previous handler for execution if needed.
     * 
     * @param preHandler : the previous handler instance
     *            Range: null / valid object
     *            Unit: DBOperateHandler
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mPreviorHandler: Use this global variable to set last operation
     *      handler.
     */
    protected final void setPreviousHandler(DBOperateHandler<?> preHandler)
    {
        mPreviorHandler = preHandler;
    }

    /**
     * Execute the handler start function, and then perform record size check if
     * the current handler is insert handler or delete handler.
     * 
     * @return The execution result in generic type.
     * 
     *         1. If the return type is Integer, it means the updated or deleted
     *         record count.
     *         Range: 0 ~ 5000
     *         Unit: Integer
     *         Scaling: 1
     * 
     *         2. if the return type is Uri, it means the path of the newly
     *         created row.
     *         Range: valid object
     *         Unit: Uri
     *         Scaling: 1
     * 
     *         3. If the return type is ArrayList, it means the query result.
     *         Range: null / valid object
     *         Unit: ArrayList
     *         Scaling: 1
     * 
     * @see mContext
     * @see mData
     */
    public T start()
    {
        
        T result = handle();
        
        if (this instanceof InsertHandler)
        {
            RecordSizeChecker.getInstance(mContext, mData).checkAfterInsert();
        }
        else if (this instanceof DeleteHandler)
        {
            RecordSizeChecker.getInstance(mContext, mData).checkAfterDelete(
                    (Integer) result);
        }
        else
        {
            // do nothing
        }
        
        return result;
    }

    /**
     * Execute the certain command of a certain handler.
     * 
     * @return T : The execution result in generic type.
     * 
     *         1. If the return type is Integer, it means the updated or deleted
     *         record count.
     *         Range: 0 ~ 5000
     *         Unit: Integer
     *         Scaling: 1
     * 
     *         2. if the return type is Uri, it means the path of the newly
     *         created row.
     *         Range: valid object
     *         Unit: Uri
     *         Scaling: 1
     * 
     *         3. If the return type is ArrayList, it means the query result.
     *         Range: null / valid object
     *         Unit: ArrayList
     *         Scaling: 1
     */
    protected abstract T handle();

}
