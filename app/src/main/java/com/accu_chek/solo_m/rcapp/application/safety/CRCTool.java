/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.cgmapp.application.CRCTool
 * Brief: The tool for calculate the CRC16 value for the given byte array.
 *
 * Create Date: 12/6/2013
 * $Revision: 24174 $
 * $Author: StanleySu $
 * $Id: CRCTool.java 24174 2015-11-16 09:31:45Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

import java.nio.ByteBuffer;

public final class CRCTool
{
    
    private static final int SHIFT_8_BIT = 8;
    
    private static final int ONE_BYTE_OPERAND = 0x00FF;
    
    private static final int TWO_BYTE_OPERAND = 0xFFFF;
    
    private static final int[] COMMS_CRC16TABLE = 
    {
    0x0000, 0x1189, 0x2312, 0x329b, 0x4624, 0x57ad, 0x6536, 0x74bf,
    0x8c48, 0x9dc1, 0xaf5a, 0xbed3, 0xca6c, 0xdbe5, 0xe97e, 0xf8f7,
    0x1081, 0x0108, 0x3393, 0x221a, 0x56a5, 0x472c, 0x75b7, 0x643e,
    0x9cc9, 0x8d40, 0xbfdb, 0xae52, 0xdaed, 0xcb64, 0xf9ff, 0xe876,
    0x2102, 0x308b, 0x0210, 0x1399, 0x6726, 0x76af, 0x4434, 0x55bd,
    0xad4a, 0xbcc3, 0x8e58, 0x9fd1, 0xeb6e, 0xfae7, 0xc87c, 0xd9f5,
    0x3183, 0x200a, 0x1291, 0x0318, 0x77a7, 0x662e, 0x54b5, 0x453c,
    0xbdcb, 0xac42, 0x9ed9, 0x8f50, 0xfbef, 0xea66, 0xd8fd, 0xc974,
    0x4204, 0x538d, 0x6116, 0x709f, 0x0420, 0x15a9, 0x2732, 0x36bb,
    0xce4c, 0xdfc5, 0xed5e, 0xfcd7, 0x8868, 0x99e1, 0xab7a, 0xbaf3,
    0x5285, 0x430c, 0x7197, 0x601e, 0x14a1, 0x0528, 0x37b3, 0x263a,
    0xdecd, 0xcf44, 0xfddf, 0xec56, 0x98e9, 0x8960, 0xbbfb, 0xaa72,
    0x6306, 0x728f, 0x4014, 0x519d, 0x2522, 0x34ab, 0x0630, 0x17b9,
    0xef4e, 0xfec7, 0xcc5c, 0xddd5, 0xa96a, 0xb8e3, 0x8a78, 0x9bf1,
    0x7387, 0x620e, 0x5095, 0x411c, 0x35a3, 0x242a, 0x16b1, 0x0738,
    0xffcf, 0xee46, 0xdcdd, 0xcd54, 0xb9eb, 0xa862, 0x9af9, 0x8b70,
    0x8408, 0x9581, 0xa71a, 0xb693, 0xc22c, 0xd3a5, 0xe13e, 0xf0b7,
    0x0840, 0x19c9, 0x2b52, 0x3adb, 0x4e64, 0x5fed, 0x6d76, 0x7cff,
    0x9489, 0x8500, 0xb79b, 0xa612, 0xd2ad, 0xc324, 0xf1bf, 0xe036,
    0x18c1, 0x0948, 0x3bd3, 0x2a5a, 0x5ee5, 0x4f6c, 0x7df7, 0x6c7e,
    0xa50a, 0xb483, 0x8618, 0x9791, 0xe32e, 0xf2a7, 0xc03c, 0xd1b5,
    0x2942, 0x38cb, 0x0a50, 0x1bd9, 0x6f66, 0x7eef, 0x4c74, 0x5dfd,
    0xb58b, 0xa402, 0x9699, 0x8710, 0xf3af, 0xe226, 0xd0bd, 0xc134,
    0x39c3, 0x284a, 0x1ad1, 0x0b58, 0x7fe7, 0x6e6e, 0x5cf5, 0x4d7c,
    0xc60c, 0xd785, 0xe51e, 0xf497, 0x8028, 0x91a1, 0xa33a, 0xb2b3,
    0x4a44, 0x5bcd, 0x6956, 0x78df, 0x0c60, 0x1de9, 0x2f72, 0x3efb,
    0xd68d, 0xc704, 0xf59f, 0xe416, 0x90a9, 0x8120, 0xb3bb, 0xa232,
    0x5ac5, 0x4b4c, 0x79d7, 0x685e, 0x1ce1, 0x0d68, 0x3ff3, 0x2e7a,
    0xe70e, 0xf687, 0xc41c, 0xd595, 0xa12a, 0xb0a3, 0x8238, 0x93b1,
    0x6b46, 0x7acf, 0x4854, 0x59dd, 0x2d62, 0x3ceb, 0x0e70, 0x1ff9,
    0xf78f, 0xe606, 0xd49d, 0xc514, 0xb1ab, 0xa022, 0x92b9, 0x8330,
    0x7bc7, 0x6a4e, 0x58d5, 0x495c, 0x3de3, 0x2c6a, 0x1ef1, 0x0f78
    };    
    
    private static final int SHORT_SIZE = 2;

    private static final int INT_SIZE = 4;

    private static final int FLOAT_SIZE = 4;

    private static final int LONG_SIZE = 8;
    
    private static final int DEFAULT_CRC = 0xffff;
    
    private CRCTool()
    {
        
    }
    
    /**
     * Convert short number to byte array
     * 
     * @param s [in] the short number to be converted
     * @return byte[] [out] the byte array of passed in short number
     */
    public static byte[] getBytes(final short s)
    {
        final int nCapacity = SHORT_SIZE;
        final byte[] nByteArray = ByteBuffer.allocate(nCapacity).putShort(s).array();

        return nByteArray;
    }

    /**
     * Convert integer number to byte array
     * 
     * @param i [in] the integer number to be converted
     * @return byte[] [out] the byte array of passed in integer number
     */
    public static byte[] getBytes(final int i)
    {
        final int nCapacity = INT_SIZE;
        final byte[] nByteArray = ByteBuffer.allocate(nCapacity).putInt(i).array();

        return nByteArray;
    }

    /**
     * Convert float number to byte array
     * 
     * @param f [in] the float number to be converted
     * @return byte[] [out] the byte array of passed in float number
     */
    public static byte[] getBytes(final float f)
    {
        final int nCapacity = FLOAT_SIZE;
        final byte[] nByteArray = ByteBuffer.allocate(nCapacity).putFloat(f).array();

        return nByteArray;
    }

    /**
     * Convert long number to byte array
     * 
     * @param l [in] the long number to be converted
     * @return byte[] [out] the byte array of passed in long number
     */
    public static byte[] getBytes(final long l)
    {
        final int nCapacity = LONG_SIZE;
        final byte[] nByteArray = ByteBuffer.allocate(nCapacity).putLong(l).array();

        return nByteArray;
    }

    /**
     * @Brief Apply the input Byte data with the formula of CRC
     * 
     * @param[in] nCRC. The initial value of the CRC
     * @param[in] nInValue. Byte data. (8 bit Signed)
     * @return 16bit unsigned value of the new CRC.
     */
    private static int crc16Calc(final int nInCRC, final byte nInValue)
    {
        int nCRC = 0;
        int index = 0;
        
        index = (nInCRC ^ nInValue)  & ONE_BYTE_OPERAND; 
        
        nCRC = ((nInCRC >> SHIFT_8_BIT) ^ COMMS_CRC16TABLE[index]) & TWO_BYTE_OPERAND;

        return nCRC;
    }

    /**
     * Call this function to generate the CRC16 value with the given data.
     * 
     * @param input [in]Byte array data for the CRC calculation
     * 
     * @return int [out] CRC16 vale.
     */
    public static int generateCRC16(final byte[] input)
    {
        int nCRCValue = DEFAULT_CRC;

        for (int i = 0; i < input.length; i++)
        {
            nCRCValue = crc16Calc(nCRCValue, input[i]);
        }

        return nCRCValue;
    }   
}
/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Restore the sources to revision 3682.
// [Klocwork]Update for fix Klocwork warning.
// [Klocwork]Update for fix Klocwork warning.
// [JIRA-ID]: N/A
// [Comment]: Add header and footer in java file.
// (5488 2014-03-06 02:05:42Z TerryHsieh)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: comply coding guideline
// (5580 2014-03-07 08:29:24Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: comply coding guideline
// (5620 2014-03-10 02:39:53Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-272
// [Comment]: add comments
// (5644 2014-03-10 05:05:30Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-210, ATOS-290, ATOS-291
// [Comment]: Modify for new CISD
// (7067 2014-04-02 06:16:33Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix Klocwork issues.
// (13984 2014-07-28 13:27:40Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Add comment for the function/method
// (14620 2014-08-01 09:06:47Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (17133 2014-08-27 07:38:47Z SteveSu)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (17426 2014-08-28 11:56:27Z SteveSu)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (17486 2014-08-29 02:54:15Z SteveSu)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (18718 2014-09-11 08:59:47Z SteveSu)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (18852 2014-09-12 06:41:33Z SteveSu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (18852 2014-09-12 06:41:33Z SteveSu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Preform KEYWORD function of SVN for each source files. No Source
// code content will be changed
// (21021 2014-10-03 02:34:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (21467 2014-10-07 14:22:36Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline - Fix rule R17, R88, R100, R102, etc.
