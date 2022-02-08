package com.accu_chek.solo_m.rcapp.data.nugendata;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;

import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.UtilConstants;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class DatabaseUtil 
{
    // The total channel number for storing original value and diverse value.
    private static final int CHANNEL_TOTAL_NUMBER = 2;
    
    
    /**
	 * Converts the last path segment to a int.
	 * 
	 * This supports a common convention for content URIs where an ID is stored
	 * in the last segment.
	 * 
	 * @param contentUri : The Uri path.
     *            Range: valid object
     *            Unit: Uri
     *            Scaling: 1
	 * 
	 * @return the long conversion of the last segment or -1 if the path is
	 *         empty
     *            Range: -1 to 5000
     *            Unit: int
     *            Scaling: 1
	 *         
	 */
    public static int parseId(Uri contentUri) 
    {
        String last = contentUri.getLastPathSegment();
        return last == null ? -1 : Integer.parseInt(last);
    }

    /**
     * Encode the input value by 2 channel algorithm and then converting the
     * encoded channel values to JSON string. Return this string in SafetyString
     * type.
     * 
     * @param originValue : the original value for DB insertion.
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1 / 10^2
     * 
     * @return the encoded value for DB insertion.
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     */
    public static SafetyString toSafeInsertionString(int originValue)
    {
        int CH1Value = CommonUtils.encodeCH1Value(originValue);
        int CH2Value = CommonUtils.encodeCH2Value(originValue);
        String jsonString = DatabaseUtil.convertJSONString(CH1Value, CH2Value);
        SafetyString safeString = new SafetyString(jsonString,
                CRCTool.generateCRC16(jsonString.getBytes()));

        return safeString;
    }

    /**
     * Encode the input value by 2 channel algorithm and then converting the
     * encoded channel values to JSON string. Return this string in SafetyString
     * type.
     * 
     * @param originValue : the original value for DB insertion.
     *            Range: -2^63 to (2^63)-1
     *            Unit: long
     *            Scaling: 1
     * 
     * @return the encoded value for DB insertion.
     *         Range: valid object
     *         Unit: SafetyString
     *         Scaling: 1
     */
    public static SafetyString toSafeInsertionString(long originValue)
    {
        long CH1Value = CommonUtils.encodeCH1Value(originValue);
        long CH2Value = CommonUtils.encodeCH2Value(originValue);
        String jsonString = DatabaseUtil.convertJSONString(CH1Value, CH2Value);
        SafetyString safeString = new SafetyString(jsonString,
                CRCTool.generateCRC16(jsonString.getBytes()));

        return safeString;
    }

    /**
     * Obtain original float value by decoding 2 channel values from
     * SafetyChannel object.
     * 
     * @param channel : SafetyChannel object which contains 2 channel values.
     *            Range: valid object
     *            Unit: SafetyChannel
     *            Scaling: 1
     * 
     * @return the original float value
     *         Range: 1.401298e-45 to 3.402823e+38
     *         Unit: float
     *         Scaling: 1
     */
    public static float toOriginFloat(SafetyChannel<Integer> channel)
    {
        int scalingSnackSize = 0;
        float snackSize = 0.0f;
        
        CommonUtils.objectCheck(channel);

        scalingSnackSize = CommonUtils.getOriginValue(
                (Integer) channel.getValueCH1(),
                (Integer) channel.getValueCH2());
        snackSize = CommonUtils.resoreIntToFloat(scalingSnackSize);
        
        return snackSize;
    }

    /**
     * Convert safety type values into ContentValues for the insert or update
     * operation.
     * 
     * @param values : For storing the converted primitive value
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * @param entry : the entry of a Map for storing safety type value
     *            Range: valid object
     *            Unit: Map.Entry<String, Object>
     *            Scaling: 1
     * 
     * @return None
     * 
     * @throw IllegalArgumentException : the exception for indicating
     *        unsupported input safety type value.
     * 
     */
    public static void convertSafetyValue(ContentValues values,
            Map.Entry<String, Object> entry)
    {
        CommonUtils.objectCheck(values, entry);
        
        Object safetyType = entry.getValue();
        String columnName = entry.getKey();
        boolean isSupportType = safetyType.getClass().getSimpleName()
                .equals("SafetyString");
        
        if (isSupportType)
        {
            SafetyString safetyString = (SafetyString) safetyType;
            values.put(columnName, safetyString.getString());
            
            if( columnName.equalsIgnoreCase("timestamp") ){
            	long[] channelValue = DatabaseUtil.restoreChannelLongValue(safetyString.getString());
            	Long timestamp = CommonUtils.getOriginValue(channelValue[0], channelValue[1]);
            	values.put(columnName + "_db", timestamp);
            }
            if( columnName.equalsIgnoreCase("bg_id") ){
            	int[] channelValue = DatabaseUtil.restoreChannelIntValue(safetyString.getString());
            	int bg_id = CommonUtils.getOriginValue(channelValue[0], channelValue[1]);
            	values.put(columnName + "_db", bg_id);
            }
            if( columnName.equalsIgnoreCase("bolus_id") ){
            	int[] channelValue = DatabaseUtil.restoreChannelIntValue(safetyString.getString());
            	int bolus_id = CommonUtils.getOriginValue(channelValue[0], channelValue[1]);
            	values.put(columnName + "_db", bolus_id);
            }
        }
        else
        {
            throw new IllegalArgumentException("Unsupported input safety type.");
        }
    }

    /**
     * Generate a CRC value based on the input record data.
     * 
     * @param columnValueList : the input record data
     *            Range: valid object
     *            Unit: ArrayList<String>
     *            Scaling: 1
     * 
     * @return the CRC value
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     */
    public static int generateCRC(ArrayList<String> columnValueList)
    {
        int nCrcValue = -1;
        byte[] frame = {};
        ArrayList<byte[]> data = new ArrayList<byte[]>();

        CommonUtils.objectCheck(columnValueList);
        
        Iterator<String> iterator = columnValueList.iterator();

        while (iterator.hasNext())
        {
            String columnValue = iterator.next();
            data.add(columnValue.getBytes());
        }

        frame = ByteConverter.buildByteArray(data);

//        nCrcValue = CommsJNI.generateCrc(frame, frame.length);
        nCrcValue = CRCTool.generateCRC16(frame);

        return nCrcValue;
    }

    /**
     * Get the column value from the insert or update values.
     * 
     * @param values : the insert or update values
     *            Range: valid object
     *            Unit: ContentValues
     *            Scaling: 1
     * @param column_id : the column name of certain database table
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * @param defaultValue : the default value for a column of a record
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     *            
     * @return the column value
     *         Range: valid object
     *         Unit: String
     *         Scaling: 1
     */
    public static String getStringValue(ContentValues values, String column_id,
            String defaultValue)
    {
        String ret = defaultValue;

        CommonUtils.objectCheck(values);

        if (values.containsKey(column_id))
        {
            ret = (String) values.get(column_id);
        }

        return ret;
    }

    /**
     * Get the column value from a Cursor object.
     * 
     * @param cursor : Use this to obtain column value of a record
     *            Range: valid object
     *            Unit: Cursor
     *            Scaling: 1
     * @param colum_id : the column id of certain database table
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     *            
     * @return the column value
     *         Range: valid object
     *         Unit: String
     *         Scaling: 1
     */
    public static String getStringValue(Cursor cursor, int colum_id)
    {
        String ret = "";

        CommonUtils.objectCheck(cursor);

        if (!cursor.isNull(colum_id))
        {
            ret = cursor.getString(colum_id);
        }

        return ret;
    }

    /**
     * Get the column value from a Cursor object.
     * 
     * @param cursor : Use this to obtain column value of a record
     *            Range: valid object
     *            Unit: Cursor
     *            Scaling: 1
     * @param colum_id : the column id of certain database table
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     *            
     * @return the column value
     *         Range: -2^31 to (2^31)-1
     *         Unit: String
     *         Scaling: 1
     */
    public static int getIntValue(Cursor cursor, int colum_id)
    {
        int ret = 0;

        CommonUtils.objectCheck(cursor);

        if (!cursor.isNull(colum_id))
        {
            ret = cursor.getInt(colum_id);
        }

        return ret;
    }
    
    /**
     * Restore the values of channel 1 and channel 2 from a compact JSON string.
     * 
     * @param jsonString : the compact JSON string
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * 
     * @return integer array for storing the values of channel 1 and
     *         channel 2
     *         Range: -2^31 to (2^31)-1
     *         Unit: int[]
     *         Scaling: 1
     * 
     */
    public static int[] restoreChannelIntValue(String jsonString)
    {
        int[] channelValue = new int[CHANNEL_TOTAL_NUMBER];
        
        CommonUtils.objectCheck(jsonString);

        try
        {
            JSONObject json = new JSONObject(jsonString);
            channelValue[0] = json.optInt(UtilConstants.KEY_CHANNEL_1, -1);
            channelValue[1] = json.optInt(UtilConstants.KEY_CHANNEL_2, -1);
        }
        catch (JSONException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // do nothing
        }
        
        return channelValue;
    }
    
    /**
     * Restore the values of channel 1 and channel 2 from a compact JSON string.
     * 
     * @param jsonString : the compact JSON string
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * 
     * @return array for storing the values of channel 1 and
     *         channel 2
     *         Range: -2^63 to (2^63)-1
     *         Unit: long[]
     *         Scaling: 1
     */
    public static long[] restoreChannelLongValue(String jsonString)
    {
        long[] channelValue = new long[CHANNEL_TOTAL_NUMBER];
        
        CommonUtils.objectCheck(jsonString);

        try
        {
            JSONObject json = new JSONObject(jsonString);
            channelValue[0] = json.optLong(UtilConstants.KEY_CHANNEL_1, -1);
            channelValue[1] = json.optLong(UtilConstants.KEY_CHANNEL_2, -1);
        }
        catch (JSONException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // do nothing
        }
        
        return channelValue;
    }
    
    /**
     * Convert the values of channel 1 and channel 2 into a compact JSON string.
     * 
     * @param ch1Value : the channel 1 value
     *            Range: int / long
     *            Unit: T (int / long)
     *            Scaling: 1
     * @param ch2Value : the channel 2 value
     *            Range: int / long
     *            Unit: T (int / long)
     *            Scaling: 1
     *            
     * @return the compact JSON string
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     */
    public static <T> String convertJSONString(T ch1Value, T ch2Value)
    {
        JSONObject json = new JSONObject();
        String jsonString = null;
        
        doWriteJSONProcess(UtilConstants.KEY_CHANNEL_1, ch1Value, json);
        doWriteJSONProcess(UtilConstants.KEY_CHANNEL_2, ch2Value, json);
        
        jsonString = json.toString();
        
        return jsonString;
    }

    /**
     * Maps name to value of a JSONObject.
     * 
     * @param name : the key of a JSONObject for mapping
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * @param value : the channel value
     *            Range: int / long
     *            Unit: T (int / long)
     *            Scaling: 1
     * @param json : the instance of JSONObject
     *            Range: valid object
     *            Unit: JSONObject
     *            Scaling: 1
     * 
     * @return None
     */
    private static <T> void doWriteJSONProcess(String name, T value,
            JSONObject json)
    {
        CommonUtils.objectCheck(name, json);

        try
        {
            json.put(name, value);
        
        }
        catch (JSONException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // do nothing.
        }
    }

}
// [Reminder] use DB to store Reminder data
// [Reminder] update Reminder module
// [Reminder] update Reminder module
// [Reminder] update Reminder module
