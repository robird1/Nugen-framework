/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRStorageLogger
 * Brief: 
 *
 * Create Date: 2015/6/3
 * $Revision: 24015 $
 * $Author: KayjeanKu $
 * $Id: EMWRStorageLogger.java 24015 2015-11-12 12:28:54Z KayjeanKu $
 */

package com.accu_chek.solo_m.rcapp.application.emwrservice;

import java.util.ArrayList;
import java.util.HashMap;

import android.content.Context;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRDatabaseLogModel.EMWR_SelectionType;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage.EMWR_MESSAGE_STATE;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;
import com.accu_chek.solo_m.rcapp.data.nugendata.EMWRLogTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.OrderByType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.SelectionType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public class EMWRStorageLogger
{
    /**
     * unused integer fieled in MessageLog table
     */
    private static final int UNUSED_INTEGER_FIELD = -1;
    private static final int THREE_ARGUMENT = 3;
    private static final String TAG = "EMWRStorageLogger";
    
    /**
     * MessageLog table control.
     */
    private static EMWRDatabaseLogModel mModel = new EMWRDatabaseLogModel(UrlType.emwrUri);
    
    public static class EMWRUnconfirmNameQuery implements IQuerySelectType
    {
        EMWRList mItem = null; 

        /**
         * Constructor of EMWRUnconfirmNameQuery to save EMWRList item.
         * 
         * @param item The item in EMWRList.
         * Range: The item in EMWRList.
         * Unit: EMWRList
         * Scaling: 1
         * 
         */
        public EMWRUnconfirmNameQuery(EMWRList item)
        {
            mItem = item;
        }
        
        /**
         * Set selection by Message Name and EMWR Status (not CONFIRMED and not CLOSED FOR REPEAT).
         *
         * return A string that meet SQLite query select.
         * Range: valid String object
         * Unit: String
         * Scaling: 1
         */
        @Override
        public String onSelection()
        {
            String selectstr = EMWRLogTable.COLUMN_MESSAGE_NAME
                    + SelectionType.ASSIGNVALUE + " AND "
                    + EMWRLogTable.COLUMN_EMWR_STATUS
                    + EMWR_SelectionType.UNASSIGNVALUE + " AND "
                    + EMWRLogTable.COLUMN_EMWR_STATUS
                    + EMWR_SelectionType.UNASSIGNVALUE;
            // Debug.printI(TAG, " selectstr =  " + selectstr);
            return selectstr;
        }

        /**
         * Set selection arguments that meet SQLite rules. EMWR Status arguments are CONFIRMED and CLOSED FOR REPEAT.
         * 
         * see mItem [in] This global variable is referred for getting EMWRList item.
         * 
         * return A sting array that stores select arguments. 
         * Range: Valid String array
         * Unit: String
         * Scaling: 1
         */
        @Override
        public String[] onSelectionArgs()
        {
            String[] argStr = new String[THREE_ARGUMENT];

            argStr[0] = mItem.toString();
            argStr[1] = DatabaseUtil.toSafeInsertionString(
                    EMWR_MESSAGE_STATE.STATE_CONFIRMED).getString();
            argStr[2] = DatabaseUtil.toSafeInsertionString(
                    EMWR_MESSAGE_STATE.STATE_CLOSED_FOR_REPEAT).getString();
            // Debug.PrintI(TAG, " selectstr =  " + argStr[0]);
            return argStr;
        }

        /**
         * The SQLite order method of the select result. The order method is ASC here.
         *
         * return A string that is ASC order. 
         * Range: Valid String object
         * Unit: String
         * Scaling: 1
         */
        @Override
        public String onOrderBy()
        {
            String orderStr = EMWRLogTable.COLUMN_EMWR_ID + OrderByType.ASC;
            // Debug.printI(TAG, " orderStr =  " + orderStr);
            return orderStr;
        }

    }
    
    /**
     * Log EMWR message into MessageLog table.
     *
     * @param context Context of caller.
     * Range: Valid Context object
     * Unit: Context
     * Scaling: 1
     * @param message NotifyMessage object that stores EMWR message information.
     * Range: Valid NotifyMessage object
     * Unit: NotifyMessage
     * Scaling: 1
     * 
     * see mModel [in] This global variable is referred for getting MessageLog table control.
     * 
     * return SafetyString of id in MessageLog table.
     * Range: Valid SafetyString object
     * Unit: SafetyString
     * Scaling: 1
     */
    public static SafetyString log(Context context, NotifyMessage message)
    {
        SafetyString id = null;

        if (message != null)
        {
            HashMap<String, Object> values = new HashMap<String, Object>();
            EMWRList item = message.getMessageItem();
            Long timestamp = System.currentTimeMillis();
            		
            values.put(EMWRLogTable.COLUMN_SEGMENT_ID,
                    DatabaseUtil.toSafeInsertionString(1));
            values.put(EMWRLogTable.COLUMN_TIMESTAMP,
                    DatabaseUtil.toSafeInsertionString(timestamp));
            values.put(EMWRLogTable.COLUMN_INSTANCE_ID,
                    DatabaseUtil.toSafeInsertionString(UNUSED_INTEGER_FIELD));
            values.put(EMWRLogTable.COLUMN_EMWR_TYPE,
                    DatabaseUtil.toSafeInsertionString(UNUSED_INTEGER_FIELD));
            values.put(
                    EMWRLogTable.COLUMN_EMWR_STATUS,
                    DatabaseUtil
                            .toSafeInsertionString(EMWR_MESSAGE_STATE.STATE_WAIT_TO_SHOW));
            values.put(
                    EMWRLogTable.COLUMN_MESSAGE_NAME,
                    new SafetyString(item.toString(), CRCTool
                            .generateCRC16(item.toString().getBytes())));
            values.put(EMWRLogTable.COLUMN_COMMENT,
                    NotifyMessage.getMessageLine(message));

            Debug.printE(TAG, mModel + "");

            id = mModel.insertData(context, values);
        }
        else
        {
            // Apply to the coding standard
        }

        return id;
    }
    
    /**
     * Check if a message is unconfirmed.
     *
     * @param context Context of caller.
     * Range: Valid Context object
     * Unit: Context
     * Scaling: 1
     * @param item EMWR message item in EMWRList
     * Range: An item in EMWRList
     * Unit: EMWRList
     * Scaling: 1
     * 
     * see mModel [in] This global variable is referred for getting MessageLog table control.
     * 
     * return SafetyBoolean result of checking unconfirmed message in MessageLog table.
     * Range: Valid SafetyString object.
     * TRUE if the message in MessageLog table is unconfirmed.
     * FALSE if the message in the table is confirmed.
     * Unit: SafetyBoolean
     * Scaling: 1 
     */
    public static SafetyBoolean checkMessageUnconfirm(Context context, EMWRList item)
    {
        IQuerySelectType type = new EMWRUnconfirmNameQuery(item);
        ArrayList<IDBData> recordData = mModel.queryData(context, type);
        SafetyBoolean result = SafetyBoolean.FALSE;
        
        if(recordData != null)
        {
            int recordSize = recordData.size();
            
            if(recordSize > 0)
            {
                result = SafetyBoolean.TRUE;
            }
            else
            {
                // Apply to the coding standard
            }
        }
        else
        {
            // Apply to the coding standard
        }
        
        return result;
    }
    
}
