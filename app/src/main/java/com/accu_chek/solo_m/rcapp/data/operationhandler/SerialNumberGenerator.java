package com.accu_chek.solo_m.rcapp.data.operationhandler;

import android.content.ContentValues;
import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.data.nugendata.EMWRLogTable;

public class SerialNumberGenerator
{
    // The Logcat tag for debugging.
    private static final String TAG = "SerialNumberGenerator";

    // the key for storing EMW counter value in share preference
    private static final String KEY_COUNTER_NUM_EMW = "counter_num_emw";

    // the key for storing meter counter value in share preference
    private static final String KEY_COUNTER_NUM_RC = "counter_num_rc";

    // the default EMW counter value
    private static final int DEFAULT_EMW_COUNTER_VALUE = 40001;

    // the maximum EMW counter value
    private static final int MAX_EMW_COUNTER_VALUE = 65536;

    // the default meter counter value
    private static final int DEFAULT_METER_COUNTER_VALUE = 
            (int) Math.pow(2, 20);

    // the maximum meter counter value
    private static final int MAX_METER_COUNTER_VALUE = 
            (int) (Math.pow(2, 21) - 1);

    // the variable for storing EMW counter value
    private static int mEMWCounterNum = DEFAULT_EMW_COUNTER_VALUE;

    // the variable for storing meter counter value
    private static int mMeterCounterNum = DEFAULT_METER_COUNTER_VALUE;

    /**
     * Reset the values of EMW counter and meter counter if the database tables
     * are recreated.
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return None
     */
    public static void reset(Context context)
    {
        NugenGeneralModel.setInt(context, KEY_COUNTER_NUM_EMW,
                new SafetyNumber<Integer>(DEFAULT_EMW_COUNTER_VALUE,
                        -DEFAULT_EMW_COUNTER_VALUE));

        NugenGeneralModel.setInt(context, KEY_COUNTER_NUM_RC,
                new SafetyNumber<Integer>(DEFAULT_METER_COUNTER_VALUE,
                        -DEFAULT_METER_COUNTER_VALUE));
    }

    /**
     * Add a serial number to the corresponding column of an inserted record and
     * then check whether this serial number exceeds the defined value range.
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param tableInstance : the instance which contains the table name
     *            information
     *            Range: valid object
     *            Unit: IDBData
     *            Scaling: 1
     * @param values : the values to be inserted into a database table
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mEMWCounterNum
     * @see mMeterCounterNum
     */
    static void addSerialNumber(Context context, IDBData tableInstance,
            ContentValues values)
    {
        Debug.printI(TAG, "[Enter] addSerialNumber()");

        CommonUtils.objectCheck(context, values, tableInstance);

        String tableName = tableInstance.getClass().getSimpleName();

        variableCheck(context);

        if (tableName.equals(EMWRLogTable.class.getSimpleName()))
        {
            Debug.printI(TAG, "add SN to EMWRLogTable record");

            values.put(AbstractTable.COLUMN_SERIAL_NUMBER, mEMWCounterNum++);
        }
        else
        {
            Debug.printI(TAG, "add SN to RC record");

            values.put(AbstractTable.COLUMN_SERIAL_NUMBER, mMeterCounterNum++);
        }

        checkSerialNumer();

        storeSerialNumber(context, tableName);
    }

    /**
     * Check the values of the EMW counter and meter counter. Counter value will
     * be restored from share preference when app process has been removed or
     * system reboot.
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mEMWCounterNum
     * @see mMeterCounterNum
     */
    private static void variableCheck(Context context)
    {
        if (mEMWCounterNum == DEFAULT_EMW_COUNTER_VALUE)
        {
            SafetyNumber<Integer> safeNum = new SafetyNumber<Integer>(
                    DEFAULT_EMW_COUNTER_VALUE, -DEFAULT_EMW_COUNTER_VALUE);

            mEMWCounterNum = NugenGeneralModel.getInt(context,
                    KEY_COUNTER_NUM_EMW, safeNum).get();
        }

        if (mMeterCounterNum == DEFAULT_METER_COUNTER_VALUE)
        {
            SafetyNumber<Integer> safeNum = new SafetyNumber<Integer>(
                    DEFAULT_METER_COUNTER_VALUE, -DEFAULT_METER_COUNTER_VALUE);

            mMeterCounterNum = NugenGeneralModel.getInt(context,
                    KEY_COUNTER_NUM_RC, safeNum).get();
        }

    }

    /**
     * Check the serial number of EMW counter and meter counter respectively and
     * reset the counter value to the default value if the stored value exceeds
     * the defined maximum value.
     * 
     * @return None
     * 
     * @see mEMWCounterNum
     * @see mMeterCounterNum
     */
    private static void checkSerialNumer()
    {
        Debug.printI(TAG, "mEMWCounterNum: " + mEMWCounterNum);
        Debug.printI(TAG, "mMeterCounterNum++: " + mMeterCounterNum);

        if (mEMWCounterNum > MAX_EMW_COUNTER_VALUE)
        {
            Debug.printI(TAG, "mEMWCounterNum > 65536");

            mEMWCounterNum = DEFAULT_EMW_COUNTER_VALUE;
        }

        if (mMeterCounterNum > MAX_METER_COUNTER_VALUE)
        {
            Debug.printI(TAG, "mMeterCounterNum > (2^21-1)");

            mMeterCounterNum = DEFAULT_METER_COUNTER_VALUE;
        }
    }

    /**
     * Store the counter value of serial number into share preference.
     * 
     * @param context : the Context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * @param tableName : the name of the database table for determining which
     *            counter value to be stored
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mEMWCounterNum
     * @see mMeterCounterNum
     */
    private static void storeSerialNumber(Context context, String tableName)
    {
        if (tableName.equals(EMWRLogTable.class.getSimpleName()))
        {
            NugenGeneralModel.setInt(context, KEY_COUNTER_NUM_EMW,
                    new SafetyNumber<Integer>(mEMWCounterNum, -mEMWCounterNum));
        }
        else
        {
            NugenGeneralModel.setInt(context, KEY_COUNTER_NUM_RC,
                    new SafetyNumber<Integer>(mMeterCounterNum,
                            -mMeterCounterNum));
        }
    }
}
