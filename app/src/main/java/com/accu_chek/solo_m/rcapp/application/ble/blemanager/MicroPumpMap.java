/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.ResponseData
 * Brief: This class handles the Micro Pumps information.   
 *
 * Create Date: 2015/7/27
 * $Revision: 20558 $
 * $Author: DWYang $
 * $Id: MicroPumpMap.java 20558 2015-10-01 14:02:41Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import android.os.Bundle;

import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class MicroPumpMap
{   
    private static final String TAG = "MicroPumpMap";

    public final static String KEY_MP_ADDRESS = "KEY_MP_ADDRESS";
    
    /**
     *  HashMap of MicroPump <SerialNumber, PumpData>
     */
    private static HashMap<String, Bundle> mSeriPumpDataMap = new HashMap<String, Bundle>();
    
    /**
     *  The list of the MP serial number 
     */
    private static List<String> mList = new ArrayList<String>();
    
    /**
     * 
     * Store the serial number and related pump data
     * 
     * @param serialNumber [in] Serial number of MicroPump
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * @param pumpData [in] pump data
     *            Range: valid object
     *            Unit: Bundle
     *            Scaling: 1
     *            
     * @see mSeriPumpDataMap: use this global variable to store pump data
     * @see mList: use this global variable to store serial number
     */
    public static void storeInSeriPumpDataMap(String serialNumber, Bundle pumpData)
    {
        Debug.printI(TAG, "Serial Number = " + serialNumber);
        mSeriPumpDataMap.put(serialNumber, pumpData);
        mList.add(serialNumber);
    }

    /**
     * Get the HashMap of MicroPump to obtain pump data
     * 
     * @return HashMap [out] HashMap of MicroPump
     *         Range: valid object
     *         Unit: HashMap<String, Bundle>
     *         Scaling: 1
     *         
     * @see mSeriPumpDataMap: use this global variable to store pump data
     */
    public static HashMap<String, Bundle> getSeriPumpDataMap()
    {
        return mSeriPumpDataMap;
    }

    /**
     * Clear the HashMap of MicroPump
     * 
     * @see mSeriPumpDataMap: use this global variable to store pump data
     * @see mList: use this global variable to store serial number
     */
    public static void clearSeriPumpDataMap()
    {
        mSeriPumpDataMap.clear();
        mList.clear();

    }
    
    /**
     * Get the List of MicroPump to obtain serial number of MicroPump
     * 
     * @return List [out] HashMap of MicroPump
     *         Range: valid object
     *         Unit: List<String>
     *         Scaling: 1
     *         
     * @see mList: use this global variable to store serial number
     */
    public static List<String> getPumpSerialList()
    {
        return mList;
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
