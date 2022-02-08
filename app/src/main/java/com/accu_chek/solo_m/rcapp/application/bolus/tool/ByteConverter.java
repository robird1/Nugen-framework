/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bolus.util.ByteConverter
 * Brief: 
 *
 * Create Date: 2015¦~5¤ë4¤é
 * $Revision: 20521 $
 * $Author: DWYang $
 * $Id: ByteConverter.java 20521 2015-10-01 11:09:05Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.bolus.tool;

public class ByteConverter
{

    public static String getFormattedString(int crc)
    {
        return String.format("%04X", (int) crc);
    }

    public static byte[] getBytes(int data)
    {
        return new byte[] { (byte) ((data >> 0) & 0xff),
                (byte) ((data >> 8) & 0xff), (byte) ((data >> 16) & 0xff),
                (byte) ((data >> 24) & 0xff), };
    }

    public static byte[] getBytes(short data)
    {
        return new byte[] { (byte) ((data >> 0) & 0xff),
                (byte) ((data >> 8) & 0xff), };
    }

    public static byte[] getBytes(byte data)
    {
        return new byte[] { data };
    }

    public static byte[] getBytes(float data)
    {
        return getBytes(Float.floatToRawIntBits(data));
    }

    // public static byte[] getBytes(double data) {
    // return getBytes(Double.doubleToRawLongBits(data));
    // }

    // public static byte[] getBytes(boolean data) {
    // return new byte[] { (byte) (data ? 0x01 : 0x00) }; // bool -> {1 byte}
    // }

    // public static byte[] getBytes(String data) {
    // return (data == null) ? null : data.getBytes();
    // }

    // public static byte[] getBytes(long data) {
    // return new byte[] {
    // (byte) ((data >> 0) & 0xff),
    // (byte) ((data >> 8) & 0xff),
    // (byte) ((data >> 16) & 0xff),
    // (byte) ((data >> 24) & 0xff),
    // (byte) ((data >> 32) & 0xff),
    // (byte) ((data >> 40) & 0xff),
    // (byte) ((data >> 48) & 0xff),
    // (byte) ((data >> 56) & 0xff),
    // };
    // }

    // public static byte[] getBytes(char data) {
    // return new byte[] {
    // (byte) ((data >> 0) & 0xff),
    // (byte) ((data >> 8) & 0xff),
    // };
    // }
}// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Rearrange packages
