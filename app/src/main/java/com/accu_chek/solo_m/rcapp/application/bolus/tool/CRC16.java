/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bolus.util.CRC16
 * Brief: 
 *
 * Create Date: 2015¦~5¤ë4¤é
 * $Revision: 20521 $
 * $Author: DWYang $
 * $Id: CRC16.java 20521 2015-10-01 11:09:05Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.bolus.tool;


/**
 * @author jmarsh Calculates a CRC16 value.
 */
public class CRC16
{

    private static final char CRC_INIT = 0xFFFF;
    private static final char BIT0 = 0x0001;
    private static final char BIT1 = 0x0002;
    private static final char BIT2 = 0x0004;
    private static final char BIT3 = 0x0008;
    private static final char BIT4 = 0x0010;
    private static final char BIT5 = 0x0020;
    private static final char BIT6 = 0x0040;
    private static final char BIT7 = 0x0080;

    public static char calculateCRC16(byte[] bytes)
    {
        // LogUtilities.dLog("******",
        // "*****************START*****************");
        char crc = CRC_INIT;
        for (int i = 0; i < bytes.length; i++)
        {
            char data = (char) (bytes[i] ^ crc);
            crc = (char) (crc >>> 8);
            if ((data & BIT7) > 0)
            {
                crc ^= 0x8408;
            }
            if ((data & BIT6) > 0)
            {
                crc ^= 0x4204;
            }
            if ((data & BIT5) > 0)
            {
                crc ^= 0x2102;
            }
            if ((data & BIT4) > 0)
            {
                crc ^= 0x1081;
            }
            if ((data & BIT3) > 0)
            {
                crc ^= 0x8C48;
            }
            if ((data & BIT2) > 0)
            {
                crc ^= 0x4624;
            }
            if ((data & BIT1) > 0)
            {
                crc ^= 0x2312;
            }
            if ((data & BIT0) > 0)
            {
                crc ^= 0x1189;
            }
            // LogUtilities.dLog("******", "crc*: " + String.format("%04X",
            // (int) crc));
        }
        // LogUtilities.dLog("******",
        // "******************END******************");
        // LogUtilities.dLog("******", "crc*: " + String.format("%04X", (int)
        // crc));
        return crc;
    }

}
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Rearrange packages
