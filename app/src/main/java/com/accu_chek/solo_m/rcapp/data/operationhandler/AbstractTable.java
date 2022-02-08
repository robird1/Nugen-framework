package com.accu_chek.solo_m.rcapp.data.operationhandler;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import android.content.ContentValues;
import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseUtil;

public abstract class AbstractTable extends IDBData
{
    // Table column name. This column value will be generated automatically.
    public static final String COLUMN_SERIAL_NUMBER = "serial_number";

    // Table column name. This column value will be generated automatically.
    public static final String COLUMN_CRC = "crc";

    // The string notation for indicating empty column value.
    protected static final String EMPTY_COLUMN_VALUE = "";

    // The update value for the update operation.
    private static ContentValues mUpdateValue = null;
    
    // The insert value for the insert operation.
    private ContentValues mInsertValue = null;
    
    // The generated CRC value.
    private int mCRC = 0;

    /**
     * Obtain the CRC value of a certain record.
     * 
     * @return the CRC value
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     * 
     * @see mCRC: Use this global variable for obtaining the CRC value.
     */
    @Override
    public int getCRC() 
    {
        return mCRC;
    }
    
    /**
     * Set the input values for the insert operation
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param map : the inserted values
     *            Range: valid object
     *            Unit: HashMap<String, Object>
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mInsertValue: Use this global variable for the insert operation.
     */
    @Override
    public void setInsertionValues(Context context, HashMap<String, Object> map)
    {
        CommonUtils.objectCheck(context, map);
        
        ContentValues values = extractSafetyValue(map);
        
        // the variable 'values' is called by reference
        SerialNumberGenerator.addSerialNumber(context, this, values);
        
        mInsertValue = values;
    }

    /**
     * Set the update values for the update operation
     * 
     * @param map : the update values
     *            Range: valid object
     *            Unit: HashMap<String, Object>
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mUpdateValue: Use this global variable for the update operation.
     */
    @Override
    public void setUpdateValues(HashMap<String, Object> map)
    {
        CommonUtils.objectCheck(map);

        ContentValues values = extractSafetyValue(map);
        
        mUpdateValue = values;
    }
    
    /**
     * clear the update value after the update operation
     * 
     * @return None
     * 
     * @see mUpdateValue: Use this global variable for the update operation.
     */
    @Override
    public void clearUpdateValues()
    {
        mUpdateValue = null;
    }
    
    /**
     * callback function for the insert operation
     * 
     * @return the inserted values
     *         Range: valid object
     *         Unit: ContentValues
     *         Scaling: 1
     * 
     * @see mInsertValue: Use this global variable for the insert operation.
     * @see mCRC: Use this global variable for the insert operation.
     * 
     */
    @Override
    public ContentValues onInsertContentValues() 
    {
        ContentValues value = mInsertValue;

        // initialize global variables for generating CRC value
        getDataFromContentValue(value);

        // insert CRC value
        mCRC = generateCRC();
        value.put(COLUMN_CRC, mCRC);
        
        return value;
    }

    /**
     * callback function for the update operation
     * 
     * @return the updated values
     *         Range: valid object
     *         Unit: ContentValues
     *         Scaling: 1
     * 
     * @see mUpdateValue: Use this global variable for the insert operation.
     * @see mCRC: Use this global variable for the insert operation.
     * 
     */
    @Override
    public ContentValues onUpdateContentValues() 
    {
        ContentValues values = new ContentValues();
        
        // copy the ContentValues "mUpdateValue" into "values"
        values.putAll(mUpdateValue);

        // update global variables for updating CRC value
        getDataFromContentValue(values);

        // update CRC value
        mCRC = generateCRC();
        values.put(COLUMN_CRC, mCRC);

        return values;
    }
    
    /**
     * set CRC value.
     * 
     * @param value : the CRC value
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mCRC: Use this global variable for storing CRC value.
     */
    protected void setCRC(int value) 
    {
        mCRC = value;
    }

    /**
     * This abstract method is to obtain the column values of the insert or
     * update record for calculating CRC value and will be override by
     * subclasses.
     * 
     * @param values : the insert or update values
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * 
     * @return None
     */
    protected abstract void getDataFromContentValue(ContentValues values);
    
    /**
     * Convert safety type values into the type ContentValues for the insert or
     * update operation.
     * 
     * @param map : contain safety type values.
     *            Range: valid object
     *            Unit: HashMap<String, Object>
     *            Scaling: 1
     * 
     * @return the values for the insert or update operation.
     *         Range: valid object
     *         Unit: ContentValues
     *         Scaling: 1
     * 
     */
    private ContentValues extractSafetyValue(HashMap<String, Object> map)
    {
        ContentValues values = new ContentValues();
        Map.Entry<String, Object> entry = null;
        
        // ensure the input argument "map" is not null
        CommonUtils.objectCheck(map);
        
        Iterator<Map.Entry<String, Object>> iterator = map.entrySet().iterator();
        
        while (iterator.hasNext())
        {
            entry = iterator.next();
            
            DatabaseUtil.convertSafetyValue(values, entry);
        }
        
        // values will be updated in the statement
        // "DataBaseUtil.convertSafetyValue(values, entry);"
        // since ContentValues is "call by reference".
        
        return values;
    }
    
    /**
     * Obtain the name of a certain table for the database operation.
     * 
     * @return the name of a certain table
     *         Range: valid object
     *         Unit: String
     *         Scaling: 1
     */
    @Override
    protected String getTableName()
    {
        // override by subclass
        return "";
    }

}
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
