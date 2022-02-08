/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: DBTools
 * Brief: 
 *
 * Create Date: 11/5/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.reminder;

import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IDeleteSelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IUpdateSelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.SelectionType;

public class DBTools
{
    
    private static final String TAG = DBTools.class.getSimpleName();
    
    public static class QueryArgs implements IQuerySelectType
    {

        private int mRecordId = 0;
        
        public QueryArgs(int recordId)
        {
            mRecordId = recordId;
        }
        
        @Override
        public String onOrderBy()
        {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public String onSelection()
        {
            return ReminderInfusionSetTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;
        }

        @Override
        public String[] onSelectionArgs()
        {
            String[] args = {String.valueOf(mRecordId)};
            return args;
        }
        
    }
    
    
    public static class UpdateArgs implements IUpdateSelectType
    {
        
        private int mRecordId = 0;
        
        public UpdateArgs(int recordId)
        {
            mRecordId = recordId;
        }

        @Override
        public String onSelection()
        {
            return ReminderInfusionSetTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;
        }

        @Override
        public String[] onSelectionArgs()
        {
            String[] args = {String.valueOf(mRecordId)};
            return args;
        }
        
    }

    
    public static class DeleteArgs implements IDeleteSelectType
    {

        private int mRecordId = 0;
        
        public DeleteArgs(int recordId)
        {
            mRecordId = recordId;
        }
        
        @Override
        public String onSelection()
        {
            // TODO change Table reference
            return ReminderInfusionSetTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;
        }

        @Override
        public String[] onSelectionArgs()
        {
            String[] args = {String.valueOf(mRecordId)};
            return args;
        }
        
    }

    
    public static class DeleteByAlarmCode implements IDeleteSelectType
    {

        private int mAlarmCode = 0;
        
        public DeleteByAlarmCode(int code)
        {
            mAlarmCode = code;
        }
        
        @Override
        public String onSelection()
        {
            return ReminderAlarmListTable.COLUMN_ALARM_REQUEST_CODE + SelectionType.ASSIGNVALUE;
        }

        @Override
        public String[] onSelectionArgs()
        {
//            Debug.printI(TAG, "mAlarmCode: " + mAlarmCode);
            String sAlarmCode = DatabaseUtil.toSafeInsertionString(mAlarmCode).getString();
            String[] args = { sAlarmCode };
            return args;
        }
        
    }

    
    public static class UpdateByAlarmCode implements IUpdateSelectType
    {
        
        private int mAlarmCode = 0;
        
        public UpdateByAlarmCode(int code)
        {
            mAlarmCode = code;
        }

        @Override
        public String onSelection()
        {
            return ReminderAlarmListTable.COLUMN_ALARM_REQUEST_CODE + SelectionType.ASSIGNVALUE;
        }

        @Override
        public String[] onSelectionArgs()
        {
            String sAlarmCode = DatabaseUtil.toSafeInsertionString(mAlarmCode).getString();
            String[] args = { sAlarmCode };
            return args;
        }
        
    }

    public static class QueryByAlarmCode implements IQuerySelectType
    {

        private int mAlarmCode = 0;
        
        public QueryByAlarmCode(int code)
        {
            mAlarmCode = code;
        }
        
        @Override
        public String onOrderBy()
        {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public String onSelection()
        {
            return ReminderAlarmListTable.COLUMN_ALARM_REQUEST_CODE + SelectionType.ASSIGNVALUE;
        }

        @Override
        public String[] onSelectionArgs()
        {
            Debug.printI(TAG, "mAlarmCode: " + mAlarmCode);
            String sAlarmCode = DatabaseUtil.toSafeInsertionString(mAlarmCode).getString();
            
            Debug.printI(TAG, "JSON string of alarm code: " + sAlarmCode);

            String[] args = { sAlarmCode };
            return args;
        }
        
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [Reminder] use DB to store Reminder data
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
