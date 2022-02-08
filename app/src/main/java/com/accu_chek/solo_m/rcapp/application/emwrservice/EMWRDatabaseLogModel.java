    /** 
     * ===========================================================================
     * Copyright 2013 Roche Diagnostics GmbH
     * All Rights Reserved
     * ===========================================================================
     *
     * Class name: com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRDatabaseLogModel
     * Brief: 
     *
     * Create Date: 2015/6/3
     * $Revision: 20513 $
     * $Author: DWYang $
     * $Id: EMWRDatabaseLogModel.java 20513 2015-10-01 10:25:24Z DWYang $
     */

    package com.accu_chek.solo_m.rcapp.application.emwrservice;

    import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

    import android.content.Context;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.data.nugendata.BGTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.BasalProfileTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.BasalTimeBlockTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.CGTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.EMWRLogTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.HistoryDataTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.LogBookTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTimeBlockTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.TimeSegmentTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.UserSettingTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.DBOperateHandler;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.InsertHandler;
import com.accu_chek.solo_m.rcapp.data.operationhandler.QueryHandler;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

    public class EMWRDatabaseLogModel
    {
        
        interface EMWR_SelectionType
        {
            /**
             * Select command: unequal
             */
            String UNASSIGNVALUE = "<>?";
        }

        // for obtaining certain table instance
        private static Map<Uri, IDBData> mTableMap = new HashMap<Uri, IDBData>();
        static
        {
            mTableMap.put(UrlType.basalProfileUri, new BasalProfileTable());
            mTableMap.put(UrlType.basalTimeBlockUri, new BasalTimeBlockTable());
            mTableMap.put(UrlType.bgUri, new BGTable());
            mTableMap.put(UrlType.cgUri, new CGTable());
            mTableMap.put(UrlType.emwrUri, new EMWRLogTable());
            mTableMap.put(UrlType.histotydataUri, new HistoryDataTable());
            mTableMap.put(UrlType.logBookUri, new LogBookTable());
            mTableMap.put(UrlType.patientRecordTimeBlockUri, new PatientRecordTimeBlockTable());
            mTableMap.put(UrlType.patientRecordUri, new PatientRecordTable());
            mTableMap.put(UrlType.timesegmentUri, new TimeSegmentTable());
            mTableMap.put(UrlType.userSettingUri, new UserSettingTable());
        }
        
        // for performing database operation to certain table
        private IDBData mModel = null;
        
        
        /**
         * class constructor
         * 
         * @param path : URI reference of certain table
         *            Range: valid object
         *            Unit: Uri
         *            Scaling: 1
         * 
         * return None
         * 
         * see mTableMap: Use this global variable for obtaining certain table
         *      instance.
         * see mModel: Use this global variable for performing database opearation
         *      to certain table.
         */
        public EMWRDatabaseLogModel(Uri path)
        {
            mModel = mTableMap.get(path);
        }
        
        /**
         * For inserting data into database.
         * 
         * @param context : to use the database
         * Range: valid object
         * Unit: Context
         * Scaling: 1
         * @param data : the data for inserting
         * Range: valid object
         * Unit: HashMap<String, Object>
         * Scaling: 1
         * 
         * return SafetyString that stores the id in EMWR table
         * 
         * see mModel: Use this global variable for performing database operation
         *      to certain table.
         */
        public SafetyString insertData(Context context, HashMap<String, Object> data)
        {
            DBOperateHandler<Uri> inserthandler = null;
            Uri uri = null;
            SafetyString idString = null;

            mModel.setInsertionValues(context, data);
            inserthandler = new InsertHandler(context, null, mModel);

            uri = inserthandler.start();

            // Debug.printI(TAG, uri.toString());
            if(uri != null)
            {
                String id = uri.getLastPathSegment();
                
                idString = new SafetyString(id, CRCTool.generateCRC16(id.getBytes()));;
            }
            else
            {
                // Apply to the coding standard
            }
            
            return idString;
        }
        
        /**
         * For querying data from database.
         * 
         * @param context : to use the database
         * Range: valid object
         * Unit: Context
         * Scaling: 1
         * @param type : the query criteria
         * Range: valid object
         * Unit: IQuerySelectType
         * Scaling: 1
         * 
         * return ArrayList: the query result.
         * 
         * see mModel: Use this global variable for performing database opearation
         *      to certain table.
         */
        public ArrayList<IDBData> queryData(Context context, IQuerySelectType type)
        {
            ArrayList<IDBData> queryResult = null;
            DBOperateHandler<ArrayList<IDBData>> handler = null;
            
            mModel.setQuerySelectTypeInterface(type);
            
            handler = new QueryHandler<IDBData>(context, null, mModel);
            queryResult = handler.start();
            
            return queryResult;
        }
    }

    /*
     * ===========================================================================
     *
     * Revision history
     *  
     * ===========================================================================
     */
