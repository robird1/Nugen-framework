/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.segment.ParseUtils
 * Brief: 
 *
 * Create Date: 2015/3/31
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: ParseUtils.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.model;

import java.util.Arrays;
import java.util.Calendar;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

/**
 * This class is used to convert the data to byte array.
 */
public class ParseUtils
{    
    /**
     * The bytes number of 16 bit integer.
     */
    public static final int BYTES_OF_INT16 = 2;
    
    /**
     * Return the opposite byte order of original data.
     *
     * @param original : The original data.
     *        Range: Null or valid object of byte array.
     *        Unit: Byte array.
     *        Scaling: 1.
     * 
     * return byte[] [out]: The opposite byte order data.
     *         Range: The length is equal to original.
     *         Unit: Byte array.
     *         Scaling: 1.
     */
    public static byte[] makeLittleEndian(byte[] original)
    {
        byte[] result = null;
        int length = -1;
        
        CommonUtils.objectCheck(original);
        
        length = original.length;
        
        result = new byte[length];
        
        for (int i=0; i<length; i++)
        {
            result[i] = original[length - i - 1];
        }
        
        return result;
    }
    
    /**
     * Parse the 16 bit integer to byte array.
     *
     * @param value : The input integer value.
     *        Range : -2^31 to (2^31)-1.
     *        Unit : Integer.
     *        Scaling : 1.
     * 
     * see BYTES_OF_INT16 [in]
     * 
     * return byte[] [out]: The byte array of input value.
     *         Range: The length shall be 2.
     *         Unit: Byte array.
     *         Scaling: 1.
     */
    public static byte[] parseInt16(int value)
    {
        byte[] result = null;        
        byte[] valueInBytes = CRCTool.getBytes(value);
        
        valueInBytes = makeLittleEndian(valueInBytes);
        
        result = Arrays.copyOfRange(valueInBytes, 0, BYTES_OF_INT16);
        
        return result;
    }
    
    /**
     * Parse the 32 bit integer to byte array.
     *
     * @param value : The input integer value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer
     *        Scaling: 1.
     * return byte[] [out]: The byte array of input value.
     *         Range: The length shall be 4.
     *         Unit: Byte array.
     *         Scaling: 1.
     */
    public static byte[] parseInt32(int value)
    {
        return makeLittleEndian(CRCTool.getBytes(value));
    }
    
    /**
     * Parse the UTC time to absolute time format of Continua (BCD code).
     * The structure is {Century(2 bytes), Year(1 byte), Month(1 byte), Day(1 byte),
     * Hour(1 byte), Minute(1 byte), Second(1 byte), Millisecond(1 byte)}.
     *
     * @param time : The input UTC time.
     *        Range: -2^63 to (2^63)-1.
     *        Unit: millisecond.
     *        Scaling: 1.
     * 
     * return byte[] [out]: The result of absolute time.
     *         Range: The length should be 8.
     *         Unit: Byte array.
     *         Scaling: 1.
     */
    public static byte[] parseAbsoluteTime(long time)
    {
        final int LENGTH_OF_CENTURY = 2;
        final int SCALE_OF_MILLISECOND = 10;
        final int RADIX_OF_HEX = 16;
        
        ByteArrayBuffer result = new ByteArrayBuffer(0);
        String temp = null;
        
        String year = null;
        String century = null;
        
                
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(time);
        
        
        temp = String.valueOf(calendar.get(Calendar.YEAR));        
        century = temp.substring(0, LENGTH_OF_CENTURY);        
        year = temp.substring(LENGTH_OF_CENTURY, temp.length());
        
        result.append(Integer.valueOf(century, RADIX_OF_HEX));
        result.append(Integer.valueOf(year, RADIX_OF_HEX));
        
        // The month is zero-base.
        result.append(Integer.valueOf(String.valueOf(
                calendar.get(Calendar.MONTH) + 1), RADIX_OF_HEX));        
        result.append(Integer.valueOf(String.valueOf(
                calendar.get(Calendar.DAY_OF_MONTH)), RADIX_OF_HEX));
        result.append(Integer.valueOf(String.valueOf(
                calendar.get(Calendar.HOUR_OF_DAY)), RADIX_OF_HEX));
        result.append(Integer.valueOf(String.valueOf(
                calendar.get(Calendar.MINUTE)), RADIX_OF_HEX));
        result.append(Integer.valueOf(String.valueOf(
                calendar.get(Calendar.SECOND)), RADIX_OF_HEX));
        result.append(Integer.valueOf(String.valueOf(
                calendar.get(Calendar.MILLISECOND) / SCALE_OF_MILLISECOND), 
                RADIX_OF_HEX));
        
        return result.toByteArray();
    }
    
    /**
     * Parse the UTC time to absolute time format of Continua RPC defined.
     * The structure is 
     * {
     * YYYY - 4 bytes
     * MM - 4 bytes
     * DD - 4 bytes
     * HH - 4 bytes
     * MM - 4 bytes
     * SS - 4 bytes
     * }.
     *
     * @param time : The input UTC time.
     *        Range: -2^63 to (2^63)-1.
     *        Unit: millisecond.
     *        Scaling: 1.
     * 
     * return byte[] [out]: The result of absolute time.
     *         Range: The length should be 8.
     *         Unit: Byte array.
     *         Scaling: 1.
     */
    public static byte[] convertToRPCAbsTimeFormat(long time)
    {
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
        byte[] year = null;
        byte[] month = null;
        byte[] day = null;
        byte[] hour = null;
        byte[] minute = null;
        byte[] second = null;
        
        Calendar calendar = Calendar.getInstance();
        
        calendar.setTimeInMillis(time);
        
        year = CRCTool.getBytes(calendar.get(Calendar.YEAR));
        month = CRCTool.getBytes(calendar.get(Calendar.MONTH) + 1);
        day = CRCTool.getBytes(calendar.get(Calendar.DAY_OF_MONTH));
        hour = CRCTool.getBytes(calendar.get(Calendar.HOUR_OF_DAY));
        minute = CRCTool.getBytes(calendar.get(Calendar.MINUTE));
        second = CRCTool.getBytes(calendar.get(Calendar.SECOND));
        
        buffer.append(year, 0, year.length);
        buffer.append(month, 0, month.length);
        buffer.append(day, 0, day.length);
        buffer.append(hour, 0, hour.length);
        buffer.append(minute, 0, minute.length);
        buffer.append(second, 0, second.length);
        
        return buffer.toByteArray();
    }
    
    /**
     * Calculate the CRC of input data and append the CRC at the end of original
     * data.
     *
     * @param original : The data which is used to calculate CRC and append on it.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     *        
     * return byte[] [out]: The data which contain the original data and CRC.
     *         Range: Valid object of byte[].
     *         Unit: byte[].
     *         Scaling: 1.
     */
    public static SafetyByteArray appendCRC(byte[] original)
    {
        SafetyByteArray result = new SafetyByteArray();
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        byte[] CRCInBytes = null;
        
        CommonUtils.objectCheck(original);
        
        CRCInBytes = ParseUtils.parseInt16(CRCTool.generateCRC16(original));
        
        buffer.append(original, 0, original.length);
        buffer.append(CRCInBytes, 0, CRCInBytes.length);
        
        result.set(buffer.toByteArray(), 
                CRCTool.generateCRC16(buffer.toByteArray()));
        
        return result;
    }
}
