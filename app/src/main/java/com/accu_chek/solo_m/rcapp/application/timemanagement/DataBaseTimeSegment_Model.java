/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: DataBaseTimeSegment_Model
 * Brief:The class provides inner interface to TimeManagementService for
 * accessing database.
 * 
 * Create Date: 05/01/2015
 * $$Revision: 23920 $$
 * $$Author: TerryHsieh $$
 * $$Id: DataBaseTimeSegment_Model.java 23920 2015-11-12 03:37:55Z TerryHsieh $$
 */
package com.accu_chek.solo_m.rcapp.application.timemanagement;


import android.content.Context;
import java.util.ArrayList;
import java.util.HashMap;

import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.nugendata.TimeSegmentTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IDeleteSelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IUpdateSelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.OrderByType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.SelectionType;

import android.net.Uri;
import android.util.Log;

/**
 * Database model for TimeManagementService to access TimeSegment data in database
 */
public class DataBaseTimeSegment_Model
{
    
    // Tag for debugging
    static final String TAG = "[TM]DataBaseTimeSegment_Model";
    
    // flag for debugging
    private static final boolean isDEBUG = true;
    
    // SQL statement
    private String mSelectStr = null;
    
    // SQL arguments
    private String mArgStr = null;
    
    // Time management URL type
    private DatabaseModel mModel = new DatabaseModel(
            IDBData.UrlType.timesegmentUri);

    // Update type class for TimeManagement update function
    public class UpdateType implements IUpdateSelectType
    {
        /**
         * Override onSelectionArgs() function of IUpdateSelectType
         * 
         * Return selection argument callback
         * 
         * @return String[] [out] selection arguments
         *          Range: valid String[] object
         *          Unit: String[]
         *          Scaling: 1
         */
        @Override
        public String[] onSelectionArgs()
        {
            // Argument string assignment
            String[] argStr = { mArgStr };
            return argStr;
        }

        /**
         * Override onSelection() function of IUpdateSelectType
         * 
         * Return selection string callback
         * 
         * @return String [out] selection string
         *          Range: valid String object
         *          Unit: String
         *          Scaling: 1
         */
        @Override
        public String onSelection()
        {
            // TODO Auto-generated method stub
            return mSelectStr;
        }
    }

    /**
     * Query type class for TimeManagement query function
     */
    public class SelectTypeByRecID implements IQuerySelectType
    {
        /**
         * Override onSelection() function of IQuerySelectType
         * 
         * Return selection string callback
         * 
         * @return String [out] selection string
         *          Range: valid String object
         *          Unit: String
         *          Scaling: 1
         */
        @Override
        public String onSelection()
        {
            // Statement string assignment
            String selectStr = mSelectStr;
            return selectStr;
        }

        /**
         * Override onSelectionArgs() function of IQuerySelectType
         * 
         * Return selection argument callback
         * 
         * @return String[] [out] selection arguments
         *          Range: valid String[] object
         *          Unit: String[]
         *          Scaling: 1
         */
        @Override
        public String[] onSelectionArgs()
        {
            // Argument string assignment
            String[] argStr = { mArgStr };
            return argStr;
        }

        /**
         * Override onOrderBy() function of IQuerySelectType
         * 
         * Return order string callback
         * 
         * @return String [out] order string
         *          Range: valid String object
         *          Unit: String
         *          Scaling: 1
         */
        @Override
        public String onOrderBy()
        {
            // Order string assignment
            String orderStr = TimeSegmentTable.COLUMN_SEGMENT_ID
                    + OrderByType.ASC;
            return orderStr;
        }
    }

    /**
    * Delete type class for TimeManagement delete function
    */
    public class DeleteSingleType implements IDeleteSelectType
    {
        /**
         * Override onSelectionArgs() function of IDeleteSelectType
         * 
         * Return selection argument callback
         * 
         * @return String[] [out] selection arguments
         *          Range: valid String[] object
         *          Unit: String[]
         *          Scaling: 1
         */
        @Override
        public String[] onSelectionArgs()
        {
            // Argument string assignment
            String[] argStr = { mArgStr };
            return argStr;
        }

        /**
         * Override onSelection() function of IDeleteSelectType
         * 
         * Return selection string callback
         * 
         * @return String [out] selection string
         *          Range: valid String object
         *          Unit: String
         *          Scaling: 1
         */
        @Override
        public String onSelection()
        {
            // Statement string assignment
            return mSelectStr;
        }
    }

    /**
     * Insert data into TimeSegmentTable
     * @param Context [in] Activity context
     *          Range: valid Context object
     *          Unit: Context
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] startTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] endTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     *           
     * @return int [out] record id
     * 
     */
    public SafetyChannel<Integer> insertData(Context context, SafetyChannel<Integer> segmentid,
            SafetyChannel<Long> startTime, SafetyChannel<Long> endTime)
    {
        int ival = 0;
        int idx = 0;
        String uriPath = null;
        String idStr = null;
        SafetyString ssval = null;
        HashMap<String, Object> values = new HashMap<String, Object>();
        long ltime = 0;
        Uri uri = null;
        SafetyChannel<Integer> scID = null;
        
        // Null check
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(segmentid);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(startTime);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(endTime);
        // Get original value of SafetyChannel
        ival = CommonUtils.getOriginValue((Integer) segmentid.getValueCH1(),
                (Integer) segmentid.getValueCH2());
        // Transfer data to SafetyString
        ssval = DatabaseUtil.toSafeInsertionString(ival);
        values = new HashMap<String, Object>();
        values.put(TimeSegmentTable.COLUMN_SEGMENT_ID, ssval);
        // Get original value of SafetyChannel
        ltime = CommonUtils.getOriginValue((Long) startTime.getValueCH1(),
                (Long) startTime.getValueCH2());
        // Transfer data to SafetyString
        //Start time
        ssval = DatabaseUtil.toSafeInsertionString(ltime);
        values.put(TimeSegmentTable.COLUMN_START_TIME, ssval);
        // Get original value of SafetyChannel
        ltime = CommonUtils.getOriginValue((Long) endTime.getValueCH1(),
                (Long) endTime.getValueCH2());
        // Transfer data to SafetyString
        //End time
        ssval = DatabaseUtil.toSafeInsertionString(ltime);
        values.put(TimeSegmentTable.COLUMN_END_TIME, ssval);

        //InsertData
        uri = mModel.insertData(context, values);
        //get uri path
        uriPath = uri.getPath();
        //filter out record id string from uri path
        //Get index of record id string
        idx = uri.getPath().lastIndexOf('/');
        //Get substring of record id
        idStr = uriPath.substring(idx+1);
        //translate id string to id value
        //Translate id string to integer
        ival = Integer.valueOf(idStr);
        scID = new SafetyChannel<Integer>(
                CommonUtils.encodeCH1Value(ival), CommonUtils.encodeCH2Value(ival));
		//return record id
        return scID;
    }

    /**
     * Update data into TimeSegmentTable
     * 
     * @param Context [in] Activity context
     *          Range: valid Context object
     *          Unit: Context
     *          Scaling: 1
     * @param SafetyChannel<Integer> [in] record id
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] startTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] endTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * 
     * @return void [out] None
     */
    public void updateData(Context context, SafetyChannel<Integer> recid,
            SafetyChannel<Long> startTime, SafetyChannel<Long> endTime)
    {
        int ival = 0;
        SafetyString ssval = null;
        HashMap<String, Object> values = null;
        long ltime = 0;
        UpdateType type = null;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(recid);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(startTime);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(endTime);
        // Get original value of SafetyChannel
        ival = CommonUtils.getOriginValue((Integer) recid.getValueCH1(),
                (Integer) recid.getValueCH2());
        // Transfer data to SafetyString
        //Record id
        ssval = DatabaseUtil.toSafeInsertionString(ival);
        values = new HashMap<String, Object>();
        values.put(TimeSegmentTable.COLUMN_RECORD_ID, ssval);
        // Get original value of SafetyChannel
        ltime = CommonUtils.getOriginValue((Long) startTime.getValueCH1(),
                (Long) startTime.getValueCH2());
        // Transfer data to SafetyString
        //Start time
        ssval = DatabaseUtil.toSafeInsertionString(ltime);
        values.put(TimeSegmentTable.COLUMN_START_TIME, ssval);
        // Get original value of SafetyChannel
        ltime = CommonUtils.getOriginValue((Long) endTime.getValueCH1(),
                (Long) endTime.getValueCH2());
        // Transfer data to SafetyString
        //End time
        ssval = DatabaseUtil.toSafeInsertionString(ltime);
        values.put(TimeSegmentTable.COLUMN_END_TIME, ssval);
        setUpdateId(recid);
        type = new UpdateType();
        //call updateData
        mModel.updateData(context, values, type);
    }

    /**
     * Delete data from TimeSegmentTable
     * 
     * @param SafetyChannel<Integer> [in] scID
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *          
     * @return void [out] None
     */
    public void deleteData(Context context, SafetyChannel<Integer> scID)
    {
        DeleteSingleType type = null;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scID);
        setDeleteID(scID);
        type = new DeleteSingleType();
        mModel.deleteData(context, type);
    }

    /**
     * Query data from TimeSegmentTable by ID
     * 
     * @param Context [in] Activity context
     *          Range: valid Context object
     *          Unit: Context
     *          Scaling: 1
     * @param SafetyChannel<Integer> [in] scRecordID: record id
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     * @return ArrayList<TimeSegmentTable> [out] data arraly list
     */
    public TimeSegmentTable queryDataByRecID(Context context,
            SafetyChannel<Integer> scRecordID) 
    {
        TimeSegmentTable ts_data = null;
        SelectTypeByRecID type = null;
        ArrayList<IDBData> db_data = null;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scRecordID);
        setQueryID(scRecordID);
        type = new SelectTypeByRecID();
        //Call Query database
        db_data = mModel.queryData(context, type);
        // Check whether the parameter is null or not
        if(null != db_data)
        {
            ts_data = (TimeSegmentTable)db_data.get(0);
        }
        return ts_data;
    }

    /**
     * Query data from TimeSegmentTable by time period
     * 
     * @param Context [in] Activity context
     *          Range: valid Context object
     *          Unit: Context
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] startTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] endTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * 
     * @return ArrayList<TimeSegmentTable> [out] data arraly list
     * 
     * @throws DataIntegrityException
     */
    public ArrayList<TimeSegmentTable> queryDataByTimePeriod(Context context,
            SafetyChannel<Long> startTime, SafetyChannel<Long> endTime)
            throws DataIntegrityException
    {
        int count = 0;
        SelectTypeByRecID type = null;
        ArrayList<IDBData> db_data = null;
        ArrayList<TimeSegmentTable> ts_data = null;

        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(startTime);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(endTime);
        setQueryTimePeriod(startTime, endTime);
        type = new SelectTypeByRecID();
        db_data = mModel.queryData(context, type);
        ts_data = new ArrayList<TimeSegmentTable>();
        count = db_data.size();
        for (int i = 0; i < count; i++)
        {
            boolean isSucess = ts_data.add((TimeSegmentTable) db_data.get(i));
            if (false == isSucess)
            {
                throw new DataIntegrityException(
                        "queryDataByTimePeriod: database add data fail!");
            }
        }
        return ts_data;
    }

    /**
     * Get the last data from TimeSegmentTable
     *
     * @param Context [in] Activity context
     *          Range: valid Context object
     *          Unit: Context
     *          Scaling: 1
     * 
     * @return TimeSegmentTable [out] the last data in database
     */
    public TimeSegmentTable getLastRecord(Context context)
            throws DataIntegrityException
    {
        TimeSegmentTable ts_data = null;
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(context);
        ts_data = (TimeSegmentTable) mModel.getLastRecord(context);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(ts_data);
        return ts_data;
    }

    /**
     * Set the content of update data
     * 
     * @param SafetyChannel<Integer> [in] scRecID: record id
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] startTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] endTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * 
     * @return void [out] None
     */
    protected void setUpdateId(SafetyChannel<Integer> scRecID)
    {
        int ival = 0;

        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scRecID);
        // Get original value of SafetyChannel
        ival = CommonUtils.getOriginValue((Integer) scRecID.getValueCH1(),
                (Integer) scRecID.getValueCH2());
        mSelectStr = TimeSegmentTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;
        mArgStr = String.valueOf(ival);
    }

    /**
     * Set the TimeSegment id for Query data from TimeSegmentTable
     * 
     * @param SafetyChannel<Integer> [in] scRecID: record id
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     * @return ArrayList<TimeSegmentTable> [out] data arraly list
     */
    protected void setQueryID(SafetyChannel<Integer> scRecID)
    {
        int ival = 0;

        printI("Enter setQueryID");
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scRecID);
        mSelectStr = TimeSegmentTable.COLUMN_RECORD_ID
                + SelectionType.ASSIGNVALUE;
        printI(mSelectStr);
        // Get original value of SafetyChannel
        ival = CommonUtils.getOriginValue((Integer) scRecID.getValueCH1(),
                (Integer) scRecID.getValueCH2());
        mArgStr = String.valueOf(ival);
        printI(mArgStr);
        printI("Exit setQueryID");
    }

    /**
     * Set the time period for Query data from TimeSegmentTable
     * 
     * @param SafetyChannel<Long> [in] startTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * @param SafetyChannel<Long> [in] endTime
     *          Range: valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * 
     * @return void [out] None
     */
    protected void setQueryTimePeriod(SafetyChannel<Long> startTime,
            SafetyChannel<Long> endTime) throws DataIntegrityException
    {
        Number ch1Value = 0;
        Number ch2Value = 0;
        long sTime = 0;
        long eTime = 0;

        // Check whether the parameter is null or not
        CommonUtils.objectCheck(startTime);
        // Check whether the parameter is null or not
        CommonUtils.objectCheck(endTime);
        ch1Value = startTime.getValueCH1();
        ch2Value = startTime.getValueCH2();
        // Get original value of SafetyChannel
        sTime = CommonUtils.getOriginValue(ch1Value.longValue(),
                ch2Value.longValue());
        ch1Value = endTime.getValueCH1();
        ch2Value = endTime.getValueCH2();
        // Get original value of SafetyChannel
        eTime = CommonUtils.getOriginValue(ch1Value.longValue(),
                ch2Value.longValue());

        mArgStr = TimeSegmentTable.COLUMN_START_TIME + " >= "
                    + String.valueOf(sTime) + " AND "
                    + TimeSegmentTable.COLUMN_END_TIME + " <= "
                    + String.valueOf(eTime);
    }

    /**
     * Set the TimeSegment id for remove data from TimeSegmentTable
     * 
     * @param SafetyChannel<Integer> [in] scRecID: record id
     *          Range: valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     * @return void [out] None
     */
    protected void setDeleteID(SafetyChannel<Integer> scRecID)
            throws DataIntegrityException
    {
        int ival = 0;

        // Check whether the parameter is null or not
        CommonUtils.objectCheck(scRecID);
        // Get original value of SafetyChannel
        ival = CommonUtils.getOriginValue((Integer) scRecID.getValueCH1(),
                (Integer) scRecID.getValueCH2());
        mSelectStr = TimeSegmentTable.COLUMN_RECORD_ID + SelectionType.ASSIGNVALUE;;
        mArgStr = String.valueOf(ival);
    }

    /**
     * 
     * Function for test to print message to Logcat or log file
     *
     * @param String [in] str
     *          Range: valid String object
     *          Unit: String
     *          Scaling: 1
     * @return void [out] 
     */
    private static void printI(String str)
    {
        if (isDEBUG)
        {
            Log.i(TAG, str);
        }
        else
        {
            // Apply to the coding standard
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
