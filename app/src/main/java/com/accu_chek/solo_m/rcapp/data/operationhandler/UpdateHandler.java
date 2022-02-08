package com.accu_chek.solo_m.rcapp.data.operationhandler;

import android.content.Context;

public class UpdateHandler extends DBOperateHandler<Integer>
{
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
     */
    public UpdateHandler(
            Context context, DBOperateHandler<?> nextHandler, IDBData data)
    {
        super(context, nextHandler, data);

        // Range check has been performed in super class.
    }

    /**
     * Execute the update command.
     * 
     * @return The number of rows updated.
     *         Range: 0 ~ 5000
     *         Unit: Integer
     *         Scaling: 1
     * 
     * @see mContext: Use this global variable to perform the update operation.
     * @see mData: Use this global variable to perform the update operation.
     */
    @Override
    protected Integer handle()
    {
        Integer result = -1;

        ISQLCommand<?> command = new UpdateCommand(mContext, mData);

        result = (Integer) command.execute();

        return result;
    }
}
