package com.accu_chek.solo_m.rcapp.data.operationhandler;

import android.content.Context;

public class DeleteHandler extends DBOperateHandler<Integer>
{
    /**
     * class constructor
     * 
     * @param context : context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param nextHandler : handler instance for the next execution
     *            Range: null / valid object
     *            Unit: DBOperateHandler
     *            Scaling: 1
     * @param data : the data for execution
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * 
     * @return None
     */
    public DeleteHandler(
            Context context, DBOperateHandler<?> nextHandler, IDBData data)
    {
        super(context, nextHandler, data);

        // Range check has been performed in super class.
    }

    /**
     * Execute the delete command.
     * 
     * @return The number of rows deleted.
     *         Range: 0 ~ 5000
     *         Unit: Integer
     *         Scaling: 1
     * 
     * @see mContext: Use this global variable to construct the instance of
     *      DeleteCommand.
     * @see mData: Use this global variable to construct the instance of
     *      DeleteCommand.
     */
    @Override
    protected Integer handle()
    {
        Integer result = -1;

        ISQLCommand<?> command = new DeleteCommand(mContext, mData);

        result = (Integer) command.execute();

        return result;
    }

}
