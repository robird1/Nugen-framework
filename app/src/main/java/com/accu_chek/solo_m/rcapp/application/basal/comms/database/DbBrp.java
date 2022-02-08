/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: DbBrp
 * Brief: Database controller for BRP data query
 *
 * Create Date: 11/06/2015
 * $Revision: 25150 $
 * $Author: JacksonHuang $
 * $Id: DbBrp.java 25150 2015-11-30 09:21:10Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms.database;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.data.nugendata.BasalProfileTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.OrderByType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.SelectionType;

import java.util.ArrayList;

public class DbBrp
{
    // SQL statement
    private String mSelectStr = null;
    // SQL arguments
    private String mArgStr = null;
    // Basal Rate Profile URL type
    private DatabaseModel mModel = new DatabaseModel(
            IDBData.UrlType.basalProfileUri);
    
    /** 
     * Query a BRP from database by profile id
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param recordID [in] SafetyChannel<Integer>
     * 
     *         Profile ID of BRP
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @return BasalProfileTable [out]
     *  
     *          BRP Query Result 
     *          
     *          Range: Valid BasalProfileTable
     *          Unit: BasalProfileTable
     *          Scaling: 1        
     */    
    public BasalProfileTable queryProfile(Context context,
                                              SafetyChannel<Integer> recordID)
    {
        // Query syntax
        QueryType type = null;
        ArrayList<IDBData> dbData = null;
        BasalProfileTable brpData = null;
        // Data number
        int iDataNum = 0;

        // Check null object
        CommonUtils.objectCheck(context);
        CommonUtils.objectCheck(recordID);
        
        // Setup query by id syntax
        setQueryByIdSyntax(recordID);
        type = new QueryType();

        // Query data from database
        dbData = mModel.queryData(context, type);
        
        if (dbData == null)
        {
            brpData = null;
        }
        else
        {
            iDataNum = dbData.size();
         
            if (iDataNum == 1)
            {
                brpData = (BasalProfileTable)dbData.get(0);                
            }
            else
            {
                brpData = null;
            }
        }
        
        return brpData;
    }    
    
    /** 
     * Query a BRP from database by OrderingInfo
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param ordering [in] SafetyChannel<Integer>
     * 
     *         OrderingInfo of BRP
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @return ArrayList<BasalProfileTable> [out]
     *  
     *          Data set of BRP
     *          
     *          Range: Valid ArrayList<BasalProfileTable>
     *          Unit: ArrayList<BasalProfileTable>
     *          Scaling: 1         
     */    
    public BasalProfileTable queryProfileOrderingInfo(Context context,
                                          SafetyChannel<Integer> ordering)
    {
        // Query syntax
        QueryType type = null;
        ArrayList<IDBData> dbData = null;
        BasalProfileTable brpData = null;
        // Data number
        int iDataNum = 0;

        // Check null object
        CommonUtils.objectCheck(context);
        CommonUtils.objectCheck(ordering);
        
        // Setup query by id syntax
        setQueryByOrdingInfoSyntax(ordering);
        type = new QueryType();

        // Query data from database
        dbData = mModel.queryData(context, type);
        
        if (dbData == null)
        {
            brpData = null;
        }
        else
        {
            iDataNum = dbData.size();
            
            if (iDataNum == 1)
            {
                brpData = (BasalProfileTable)dbData.get(0);                
            }
            else
            {
                brpData = null;
                callEmwr(EMWRList.EMW45907);
            }
        }
        
        return brpData;
    }     
    
    /**
     * Setup query by id SQL syntax
     * 
     * @param recordID [in] SafetyChannel<Integer>
     * 
     *         Profile ID of BRP
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return None
     */
    
    protected void setQueryByIdSyntax(SafetyChannel<Integer> recordID)
    {
        // Original value
        int ival = CommonUtils.getOriginValue(recordID.getValueCH1(),
                                              recordID.getValueCH2());

        // Setup query by id syntax
        mSelectStr = BasalProfileTable.COLUMN_PROFILE_ID
                   + SelectionType.ASSIGNVALUE;
        
        mArgStr = String.valueOf(ival);
    }    
    
    /**
     * Setup query by ordering info SQL syntax
     * 
     * @param ordering [in] SafetyChannel<Integer>
     * 
     *         OrderingInfo of BRP
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return None
     */
    
    protected void setQueryByOrdingInfoSyntax(SafetyChannel<Integer> ordering)
    {
        // Original value
        int ival = CommonUtils.getOriginValue(ordering.getValueCH1(),
                                              ordering.getValueCH2());;
        // JSON string for search
        String strArg = DatabaseUtil.toSafeInsertionString(ival).getString();

        // Setup query by id syntax
        mSelectStr = BasalProfileTable.COLUMN_ORDERING_INFO
                   + SelectionType.ASSIGNVALUE;
        
        mArgStr = strArg;
    }     
    
    /**
     * Call EWMR for error handling
     * 
     * @param ErrorCode [in] EMWRList
     * 
     *          Internal Error Code Enum Value of Basal Delivery 
     * 
     *          Range: Valid EMWRList
     *          Unit: EMWRList
     *          Scaling: 1
     * 
     * @return None
     */
    protected void callEmwr(EMWRList ErrorCode)
    {    
        NotifyMessage msg = new NotifyMessage(ErrorCode);
     
        NotifyProxy.showEMWR(msg);
    }
    

    /**
     * Database SQL command "Query" setup class
     * 
     */
    public class QueryType implements IQuerySelectType
    {
        /**
         * Return command syntax of SQL "Select"
         * 
         * @return String [out]
         * 
         *         Syntax of SQL "Select"
         *         
         *         Range: Valid String
         *         Unit: String
         *         Scaling: 1
         */        
        @Override
        public String onSelection()
        {
            // Statement string assignment
            String selectStr = mSelectStr;
            
            return selectStr;
        }

        /**
         * Return arguments of SQL "Select"
         * 
         * @return String [] [out] 
         * 
         *         Arguments array
         *         
         *         Range: Valid String array
         *         Unit: String
         *         Scaling: 1
         */
        @Override
        public String[] onSelectionArgs()
        {
            // Argument string assignment
            String[] argStr = { mArgStr };
            
            return argStr;
        }

        /**
         * Return command syntax of SQL "Orderby"
         * 
         * @return String [out]
         * 
         *         Syntax of SQL "Orderby"
         *         
         *         Range: Valid String
         *         Unit: String
         *         Scaling: 1
         */
        @Override
        public String onOrderBy()
        {
            // Order string assignment
            String orderStr = BasalProfileTable.COLUMN_PROFILE_ID
                    + OrderByType.ASC;
            
            return orderStr;
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
// [New Feature] Basal Delivery comms functions
// [Update] Modify comments
// [Update] Add WaitDelivery for long time delivery time
