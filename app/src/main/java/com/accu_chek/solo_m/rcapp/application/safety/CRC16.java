/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.application.report.CRC16
 * Brief: This class customized CRC-16 CCITT checksum class for
 * CheckedOutputStream usage
 * 
 * Create Date: 3/04/2014
 * $Revision: 24173 $
 * $Author: StanleySu $
 * $Id: CRC16.java 24173 2015-11-16 09:21:50Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

import java.util.zip.Checksum;

public class CRC16 implements Checksum
{

    /** CCITT polynomial: x^16 + x^12 + x^5 + 1 -> 0x1021 (1000000100001) */

    private static final int MASK_BIT = 0xff;
    
    private int mValue = 0;

    /**
     * Update 16-bit CRC.
     * 
     * @param nCRC
     *            [in] starting CRC value
     * @param nBytes
     *            [in] input byte array
     * @param nOff
     *            [in] start offset to data
     * @param nLen
     *            [in] number of bytes to process
     * @return int[out] 16-bit unsigned CRC
     */
    private int update(final int nCRC, final byte[] nBytes, final int nOff, final int nLen)
    {
        final byte[] b = new byte[nLen];

        for (int i = 0; i < nLen; i++)
        {
            b[i] = nBytes[nOff + i];
        }
        // Leverage CRCTool crc16 calculation
        return CRCTool.generateCRC16(b);
    }

    /**
     * Update 16-bit CRC.
     * 
     * @param nValue
     *            [in] input byte
     * @return void[out]
     */
    public final void update(final int nValue)
    {
        final byte[] nByteArray = {(byte) (nValue & MASK_BIT)};
        mValue = update(mValue, nByteArray, 0, 1);
    }

    /**
     * Update 16-bit CRC.
     * 
     * @param nValue
     *            [in] input byte array
     * @param nOff
     *            [in] starting offset to data
     * @param nLen
     *            [in] number of bytes to process
     * @return void[out]
     */

    public final void update(final byte[] nValue, final int nOff, final int nLen)
    {
        mValue = update(mValue, nValue, nOff, nLen);
    }
    
    /**
     * Get 16-bit CRC
     * 
     * @param void[in]
     * @return long[out] 16-bit CRC value
     */
    public final long getValue()
    {
        return mValue;
    }

    /**
     * Reset 16-bit CRC
     * 
     * @param void[in]
     * @return void[out]
     */
    public final void reset()
    {
        mValue = 0;
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// (5248 2014-03-04 04:44:55Z TerryHsieh)
// ----------------------------------------------------------------------------
// [JIRA-ID]:n/a
// [Comment]:update header and footer info
// (5248 2014-03-04 04:44:55Z TerryHsieh)
// ----------------------------------------------------------------------------
// [JIRA-ID]:n/a
// [Comment]:update header and footer information
// (6544 2014-03-25 10:01:30Z TerryHsieh)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-210, ATOS-290, ATOS-291
// [Comment]: Modify for new CISD
// (13868 2014-07-25 09:09:01Z TerryHsieh)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix Klocwork issues.
// (17951 2014-09-02 03:06:14Z TerryHsieh)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (17951 2014-09-02 03:06:14Z TerryHsieh)
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
// [Comment]: Comply coding guideline. Fix Private class variables shall have a "m" prefix.
// (21512 2014-10-08 10:10:27Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline - Fix rule R17, R88, R100, R102, etc.
