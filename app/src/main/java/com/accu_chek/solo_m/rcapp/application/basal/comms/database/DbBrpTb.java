/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: DbBrpTb
 * Brief: Database controller for BRP time block data operation
 *
 * Create Date: 11/06/2015
 * $Revision: 25150 $
 * $Author: JacksonHuang $
 * $Id: DbBrpTb.java 25150 2015-11-30 09:21:10Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms.database;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.BasalTimeBlockTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.OrderByType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.SelectionType;

import java.util.ArrayList;

public class DbBrpTb
{
    // SQL statement
    private String mSelectStr = null;
    // SQL arguments
    private String mArgStr = null;
    // Basal Rate Profile URL type
    private DatabaseModel mModel = new DatabaseModel(
            IDBData.UrlType.basalTimeBlockUri);

    /**
     * Query time blocks from database by profile id
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param pID [in] SafetyChannel<Integer>
     * 
     *         Profile ID of BRP
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @return ArrayList<BasalTimeBlockTable> [out]
     *  
     *          Data set of BRP
     *          
     *          Range: Valid ArrayList<BasalTimeBlockTable>
     *          Unit: ArrayList<BasalTimeBlockTable>
     *          Scaling: 1        
     */    
    public ArrayList<BasalTimeBlockTable> queryTimeBlock(Context context,
                                              SafetyChannel<Integer> pID)
    {
        // Query syntax
        QueryType type = null;
        ArrayList<IDBData> dbData = null;
        ArrayList<BasalTimeBlockTable> tbData = null;
        // Data number
        int iDataNum = 0;

        // Check null object
        CommonUtils.objectCheck(context);
        CommonUtils.objectCheck(pID);
        
        // Setup query by id syntax
        setQueryByIdSyntax(pID);
        type = new QueryType();

        // Query data from database
        dbData = mModel.queryData(context, type);
        
        if (dbData == null)
        {
            tbData = null;
        }
        else
        {
            tbData = new ArrayList<BasalTimeBlockTable>();
            
            iDataNum = dbData.size();
            
            for (int i = 0; i < iDataNum; i++)
            {
                tbData.add((BasalTimeBlockTable)dbData.get(i));
            }
        }
        
        return tbData;
    }

    /**
     * Setup query by id SQL syntax
     * 
     * @param pID [in] SafetyChannel<Integer>
     * 
     *         Profile ID of BRP
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return None
     */
    protected void setQueryByIdSyntax(SafetyChannel<Integer> pID)
    {
        // Original value
        int iVal = CommonUtils.getOriginValue(pID.getValueCH1(),
                                              pID.getValueCH2());
        
        // JSON string for search
        String strArg = DatabaseUtil.toSafeInsertionString(iVal).getString();        

        // Setup query by id syntax
        mSelectStr = BasalTimeBlockTable.COLUMN_PROFILE_ID
                   + SelectionType.ASSIGNVALUE;
        
        mArgStr = strArg;
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
            String orderStr = BasalTimeBlockTable.COLUMN_BLOCK_ID
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
