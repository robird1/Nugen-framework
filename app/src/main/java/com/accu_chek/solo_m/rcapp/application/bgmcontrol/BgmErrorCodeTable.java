/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.BgmErrorCodeTable
 * Brief:
 *
 * Create Date: 2015/2/5
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: BgmErrorCodeTable.java 24486 2015-11-20 05:48:56Z VictorChen $
 */
package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import java.util.HashMap;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class BgmErrorCodeTable
{
    /**
     * Transfer Status Value to Error Type
     */
    private static HashMap<String, String> Errorcode = new HashMap<String, String>();
    /**
     * Transfer Error Type to EMWR id
     */
    private static HashMap<String, EMWRList> EMWRType = new HashMap<String, EMWRList>();

    /**
     * 
     * Get the error type.
     * 
     * return String [out] error type
     * Range: valid object
     * Unit: String
     * Scaling: 1
     * 
     * @param key [in] error status.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     */
    public static String getErrorType(String key)
    {
        CommonUtils.objectCheck(key);
        String errortype = Errorcode.get(key);
        return errortype;
    }

    /**
     * 
     * Check the error type whether is defined in ERWR table.
     * 
     * return boolean [out] the error type whether is defined in ERWR table.
     * Range: valid object
     * Unit: boolean
     * Scaling: 1
     * 
     * @param key [in] error type.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     */

    public static boolean checkEMWRtype(String key)
    {
        CommonUtils.objectCheck(key);
        boolean iskey = EMWRType.containsKey(key);
        return iskey;
    }

    /**
     * 
     * Return EMWR id
     * return EMWRList [out] emwr id
     * Range: valid object
     * Unit: EMWRList
     * Scaling: 1
     * 
     * @param key [in] error type.
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     */
    public static EMWRList getEmwrId(String key)
    {
        CommonUtils.objectCheck(key);
        EMWRList value = EMWRType.get(key);
        return value;
    }

    /**
     * Use hash map to convert error code
     * Convert Status value to error type, error type to emwr id.
     * 
     * See Errorcode[out]
     * See EMWRType[out]
     * return void [out] None.
     *
     */
    public static void initialErrorCodeTable()
    {
        Errorcode.put("01", "E0");
        Errorcode.put("02", "E2");
        Errorcode.put("03", "E2");
        Errorcode.put("04", "E0");
        Errorcode.put("05", "E2");
        Errorcode.put("06", "E5");
        Errorcode.put("07", "E0");
        Errorcode.put("08", "E0");
        Errorcode.put("09", "E2");
        Errorcode.put("0A", "E2");
        Errorcode.put("0B", "E2");
        Errorcode.put("0C", "E2");
        Errorcode.put("0D", "E2");
        Errorcode.put("0E", "E2");
        Errorcode.put("0F", "E7");
        Errorcode.put("10", "E0");
        Errorcode.put("11", "E0");
        Errorcode.put("12", "E0");
        Errorcode.put("13", "E0");
        Errorcode.put("14", "E0");
        Errorcode.put("15", "E8");
        Errorcode.put("16", "E0");
        Errorcode.put("17", "E0");
        Errorcode.put("18", "E10");
        Errorcode.put("19", "E10");
        Errorcode.put("1A", "E10");
        Errorcode.put("1B", "E10");
        Errorcode.put("1C", "E10");
        Errorcode.put("1D", "E10");
        Errorcode.put("1E", "E10");
        Errorcode.put("1F", "E10");
        Errorcode.put("20", "E10");
        Errorcode.put("21", "E1");
        Errorcode.put("22", "E1");
        Errorcode.put("23", "E1");
        Errorcode.put("24", "E1");
        Errorcode.put("25", "E1");
        Errorcode.put("26", "E1");
        Errorcode.put("27", "E1");
        Errorcode.put("28", "E1");
        Errorcode.put("29", "E1");
        Errorcode.put("2A", "E3");
        Errorcode.put("2B", "E3");
        Errorcode.put("2C", "E3");
        Errorcode.put("2D", "E3");
        Errorcode.put("2E", "E3");
        Errorcode.put("2F", "E3");
        Errorcode.put("30", "E3");
        Errorcode.put("31", "E3");
        Errorcode.put("32", "E3");
        Errorcode.put("33", "E3");
        Errorcode.put("34", "E3");
        Errorcode.put("35", "E3");
        Errorcode.put("36", "E3");
        Errorcode.put("37", "E3");
        Errorcode.put("38", "E3");
        Errorcode.put("39", "E1");
        Errorcode.put("3A", "E1");
        Errorcode.put("3B", "E0");
        Errorcode.put("3C", "E0");
        Errorcode.put("3D", "E6");
        Errorcode.put("3E", "E4");
        Errorcode.put("3F", "E4");
        Errorcode.put("40", "E4");
        Errorcode.put("41", "E4");
        Errorcode.put("42", "E4");
        Errorcode.put("43", "E4");
        Errorcode.put("44", "E4");
        Errorcode.put("45", "E4");
        Errorcode.put("46", "E7");
        Errorcode.put("47", "E7");
        Errorcode.put("48", "E7");
        Errorcode.put("49", "E7");
        Errorcode.put("4A", "E7");
        Errorcode.put("4B", "E7");
        Errorcode.put("4C", "E7");
        Errorcode.put("4D", "E7");
        Errorcode.put("4E", "E7");
        Errorcode.put("4F", "E7");
        Errorcode.put("50", "E7");
        Errorcode.put("51", "E1");
        Errorcode.put("52", "E1");
        Errorcode.put("53", "E1");
        Errorcode.put("54", "E1");
        Errorcode.put("55", "E0");
        Errorcode.put("56", "E0");
        Errorcode.put("57", "E1");
        Errorcode.put("58", "E1");
        Errorcode.put("59", "E0");
        Errorcode.put("5A", "E0");
        Errorcode.put("5B", "E0");
        Errorcode.put("5C", "E0");
        Errorcode.put("5D", "E0");
        Errorcode.put("5E", "E0");
        Errorcode.put("5F", "E0");
        Errorcode.put("60", "E7");
        Errorcode.put("61", "E7");
        Errorcode.put("62", "E7");
        Errorcode.put("63", "E7");
        Errorcode.put("64", "E7");
        Errorcode.put("65", "E7");
        Errorcode.put("66", "E7");
        Errorcode.put("67", "E7");
        Errorcode.put("68", "E7");
        Errorcode.put("69", "E7");
        Errorcode.put("6A", "E7");
        Errorcode.put("6B", "E7");
        Errorcode.put("6C", "E9");
        Errorcode.put("6D", "E7");
        Errorcode.put("6E", "E7");
        Errorcode.put("6F", "E7");
        Errorcode.put("70", "E7");
        Errorcode.put("71", "E7");
        Errorcode.put("72", "E7");
        Errorcode.put("73", "E7");
        Errorcode.put("74", "E7");
        Errorcode.put("75", "E7");
        Errorcode.put("76", "E7");
        Errorcode.put("77", "E7");
        Errorcode.put("78", "E7");
        Errorcode.put("79", "E7");
        Errorcode.put("7A", "E7");
        Errorcode.put("7B", "E7");
        Errorcode.put("7C", "E7");
        Errorcode.put("7D", "E7");
        Errorcode.put("7E", "E7");
        Errorcode.put("7F", "E7");
        Errorcode.put("80", "E7");
        Errorcode.put("81", "E7");
        Errorcode.put("82", "E7");
        Errorcode.put("83", "E7");
        Errorcode.put("84", "E7");
        Errorcode.put("85", "E7");
        Errorcode.put("86", "E7");
        Errorcode.put("87", "E7");
        Errorcode.put("88", "E7");
        Errorcode.put("89", "E0");
        Errorcode.put("8A", "E7");
        Errorcode.put("8B", "E7");
        Errorcode.put("8C", "E7");
        Errorcode.put("8D", "E7");
        Errorcode.put("8E", "E7");
        Errorcode.put("8F", "E7");
        Errorcode.put("90", "E7");
        Errorcode.put("91", "E7");
        Errorcode.put("92", "E7");
        Errorcode.put("93", "E7");
        Errorcode.put("94", "E7");
        Errorcode.put("95", "E7");
        Errorcode.put("96", "E7");
        Errorcode.put("97", "E7");
        Errorcode.put("98", "E7");
        Errorcode.put("99", "E7");
        Errorcode.put("9A", "E7");
        Errorcode.put("9B", "E7");
        Errorcode.put("9C", "E7");
        Errorcode.put("9D", "E7");
        Errorcode.put("9E", "E7");
        Errorcode.put("9F", "E7");
        Errorcode.put("A0", "E7");
        Errorcode.put("A1", "E7");
        Errorcode.put("A2", "E7");
        Errorcode.put("A3", "E7");
        Errorcode.put("A4", "E7");
        Errorcode.put("A5", "E7");
        Errorcode.put("A6", "E7");
        Errorcode.put("A7", "E7");
        Errorcode.put("A8", "E7");
        Errorcode.put("A9", "E7");
        Errorcode.put("AA", "E0");
        Errorcode.put("AB", "E7");
        Errorcode.put("AC", "E0");
        Errorcode.put("AD", "E7");
        Errorcode.put("AE", "E7");
        Errorcode.put("AF", "E7");
        Errorcode.put("B0", "E7");
        Errorcode.put("B1", "E7");
        Errorcode.put("B2", "E7");
        Errorcode.put("B3", "E7");
        Errorcode.put("B4", "E7");
        Errorcode.put("B5", "E7");
        Errorcode.put("B6", "E7");
        Errorcode.put("B7", "E7");
        Errorcode.put("B8", "E7");
        Errorcode.put("B9", "E7");
        Errorcode.put("BA", "E7");
        Errorcode.put("BB", "E7");
        Errorcode.put("BC", "E7");
        Errorcode.put("BD", "E7");
        Errorcode.put("BE", "E7");
        Errorcode.put("BF", "E7");
        Errorcode.put("C0", "E7");
        Errorcode.put("C1", "E7");
        Errorcode.put("C2", "E7");
        Errorcode.put("C3", "E7");
        Errorcode.put("C4", "E7");
        Errorcode.put("C5", "E7");
        Errorcode.put("C6", "E7");
        Errorcode.put("C7", "E7");
        Errorcode.put("C8", "E7");
        Errorcode.put("C9", "E7");
        Errorcode.put("CA", "E7");
        Errorcode.put("CB", "E7");
        Errorcode.put("CC", "E7");
        Errorcode.put("CD", "E7");
        Errorcode.put("CE", "E7");
        Errorcode.put("CF", "E7");
        Errorcode.put("D0", "E7");
        Errorcode.put("D1", "E7");
        Errorcode.put("D2", "E7");
        Errorcode.put("D3", "E7");
        Errorcode.put("D4", "E7");
        Errorcode.put("D5", "E7");
        Errorcode.put("D6", "E7");
        Errorcode.put("D7", "E7");
        Errorcode.put("D8", "E7");
        Errorcode.put("D9", "E7");
        Errorcode.put("DA", "E7");
        Errorcode.put("DB", "E7");
        Errorcode.put("DC", "E7");
        Errorcode.put("DD", "E7");
        Errorcode.put("DE", "E7");
        Errorcode.put("DF", "E7");
        Errorcode.put("E0", "E7");
        Errorcode.put("E1", "E7");
        Errorcode.put("E2", "E7");
        Errorcode.put("E3", "E7");
        Errorcode.put("E4", "E7");
        Errorcode.put("E5", "E7");
        Errorcode.put("E6", "E7");
        Errorcode.put("E7", "E7");
        Errorcode.put("E8", "E7");
        Errorcode.put("E9", "E0");
        Errorcode.put("EA", "E0");
        Errorcode.put("EB", "E0");
        Errorcode.put("EC", "E0");
        Errorcode.put("ED", "E7");
        Errorcode.put("EE", "E7");
        Errorcode.put("EF", "E7");
        Errorcode.put("F0", "E0");
        Errorcode.put("F1", "E0");
        Errorcode.put("F2", "E0");
        Errorcode.put("F3", "E0");
        Errorcode.put("F4", "E0");
        Errorcode.put("F5", "E0");
        Errorcode.put("F6", "E0");
        Errorcode.put("F7", "E0");
        Errorcode.put("F8", "E0");
        Errorcode.put("F9", "E0");
        Errorcode.put("FA", "E0");
        Errorcode.put("FB", "E0");
        Errorcode.put("FC", "E0");
        Errorcode.put("FD", "E0");
        Errorcode.put("FE", "E0");
        Errorcode.put("FF", "E0");

        EMWRType.put("E1", EMWRList.EMW46003);
        // ErrorType.put("E2", EMWRList.EMW46004);
        EMWRType.put("E3", EMWRList.EMW46004);
        EMWRType.put("E4", EMWRList.EMW46010);
        // ErrorType.put("E5", EMWRList.EMW46003);
        EMWRType.put("E6", EMWRList.EMW46011);
        EMWRType.put("E7", EMWRList.EMW46012);
        EMWRType.put("E8", EMWRList.EMW46001);

    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Refine call EMWR interface.
// (R23923 2015-11-12 01:29:51 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
