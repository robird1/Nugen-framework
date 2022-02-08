/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.comms.ble.ScanInfo
 * Brief: This class handles the scanInfo indications and filters them.
 * 
 * Create Date: 2015/7/24
 * $Revision: 24964 $
 * $Author: IvanHuang $
 * $Id: ScanInfo.java 24964 2015-11-27 02:43:24Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.ble.IResponse;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.ble.response.ScanInfoIndication;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IUpdateListButton;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class ScanInfo implements ResponseProcess
{
    private static final String TAG = "ScanInfo";

    public interface IScanInfoListener
    {
        void onScanInfoUpdated();
    }

    public static int mDeviceCount = 0;

    // Advertising Data Size
    private final static int ADV_DATA_SIZE = 27;

    // Index of Manufacturer Specific Data Start
    private final static int IDX_MANU_SPEC_DATA_START = 12;

    // Index of Manufacturer Specific Data End
    // private final static int IDX_MANU_SPEC_DATA_END = 25;

    private IUpdateListButton mActivityListener = null;

    /**
     * Company identifier <OUI> of Roche Note: This variable needs to be
     * initialized with initRocheManuSpecData() before use.
     */
    private static short mRocheOui = 0x0000;

    /**
     * Serial Number prefix of Roche MicroPump Note: This variable needs to be
     * initialized with initRocheManuSpecData() before use.
     */
    private static String[] mRochePrefix = null;

    /**
     * The compatible character of Roche MicroPump Software revision
     * <CharacterMajor.Minor.Trailer> Note: This variable needs to be
     * initialized with ConfigMatrix before use.
     */
    private static String mCompMpSwVerChar = "";

    /**
     * The compatible major number of Roche MicroPump Software revision
     * <CharacterMajor.Minor.Trailer> Note: This variable needs to be
     * initialized with ConfigMatrix before use.
     */
    private static int mCompMpSwVerMajor = 0;

    /**
     * The compatible minor number of Roche MicroPump Software revision
     * <CharacterMajor.Minor.Trailer> Note: This variable needs to be
     * initialized with ConfigMatrix before use.
     */
    private static int mCompMpSwVerMinor = 0;

    /**
     * The compatible trail number of Roche MicroPump Software revision
     * <CharacterMajor.Minor.Trailer> Note: This variable needs to be
     * initialized with ConfigMatrix before use.
     */
    private static int mCompMpSwVerTrail = 0;

    /**
     * The compatible major number of Roche MicroPump System revision
     * Note: This variable needs to be
     * initialized with ConfigMatrix before use.
     */
    private static int mCompMpSysRevMajor = 0;

    /**
     * The compatible minor number of Roche MicroPump System revision
     * Note: This variable needs to be
     * initialized with ConfigMatrix before use.
     */
    private static int mCompMpSysRevMinor = 0;

    /**
     * This global variable is used for the initialization flag
     */
    private static boolean mIsInitRocheDataDone = false;

    /**
     * The instance of ScanInfo
     */
    private static ScanInfo mInstance = null;

    /**
     * The listener of ScanInfo
     */
    private IScanInfoListener mListener = null;

    /**
     * Get the one and only instance of the class ScanInfo.
     * 
     * @param N/A
     * 
     * @return ScanInfo : the one and only instance of the class ScanInfo
     *         Range: A valid object
     *         Unit: ScanInfo
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class ScanInfo
     */
    public static synchronized ScanInfo getInstance()
    {
        if (mInstance == null)
        {
            mInstance = new ScanInfo();
        }
        else
        {
            // Apply to the coding standard
        }
        return mInstance;
    }

    /**
     * Register ScanInfoListener callback
     * 
     * @param listener
     *            Range: a valid object of IScanInfoListener
     *            Unit: IScanInfoListener
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     * @see mListener
     */
    public void registerCallBack(IScanInfoListener listener)
    {
        mListener = listener;
    }

    /**
     * Manufacturer Specific Data
     */

    /**
     * This method will be called after receiving the certain broadcast.
     * 
     * 
     * @param context
     *            : Unused parameter for this override parameter.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent
     *            : The Intent being received.
     *            Range: a valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     * 
     * @return void [out]
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
        Debug.printI(TAG, "[doProcess]: ScanInfo ");

        ScanInfoIndication response = null;

        SafetyByteArray address = null;

        // Null Object Check
        CommonUtils.objectCheck(intent);

        response = (ScanInfoIndication) getResponse(intent);

        address = response.getRemoteBD();

        byte[] responseData = response.getData().getByteArray();
        // Test data
        byte[] manuSpecData = null;
        byte[] byteAddress = address.getByteArray();

        ManuSpecData msd = null;

        ScanInfo.mDeviceCount++;
        Debug.printD(TAG, "No. of Device = " + ScanInfo.mDeviceCount);
        Debug.printD(TAG, "responseData = ");
        Debug.dumpPayload(TAG, responseData);
        Debug.printD(TAG, "responseData length = " + responseData.length);

        // Initialize Roche Manufacturer Specific Data
        initRocheManuSpecData();

        /**
         * Parse the Scan Info and check whether the device is Roche's MicroPump
         * or not. If the Roche's MicroPump is found, then store the Serial
         * Number and Address to a HashMap.
         */

        if (ADV_DATA_SIZE == responseData.length)
        {
            // For actual use
            manuSpecData = Arrays.copyOfRange(responseData,
                    IDX_MANU_SPEC_DATA_START, responseData.length);

            // For test use
            // manuSpecData = getTestData(responseData);

            msd = parseManuSpecData(manuSpecData);
            CommonUtils.objectCheck(msd);

            checkRocheMP(msd, byteAddress);

        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Get response from intent
     * 
     * @param intent
     *            : The Intent being received.
     *            Range: a valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     * 
     * @return response : the response of request
     *         Range: a valid object of IResponse
     *         Unit: IResponse
     *         Scaling: 1
     */
    protected IResponse getResponse(Intent intent)
    {
        ResponsePack pack = null;

        IResponse response = null;

        // Null Object Check
        CommonUtils.objectCheck(intent);

        pack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        response = pack.getResponse();

        return response;
    }

    /**
     * Update MicroPump List Buttons on UI screen
     * 
     * @param N/A
     * 
     * @return void [out]
     * 
     */
    protected void updateMpListButton()
    {
        IUpdateListButton ulb = ScanInfo.getInstance().getActivityListener();

        if (ulb != null)
        {
            ulb.onUpdateButton(null);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Store the activity instance for the callback operation.
     * 
     * @param activity
     *            : the activity instance
     *            Range: a valid object of IUpdateListButton
     *            Unit: IUpdateListButton
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     * @see mActivityListener: use this global variable to store the instance
     *      for performing the callback operation
     */
    public void setActivityListener(IUpdateListButton activity)
    {
        mActivityListener = activity;
    }

    /**
     * Get the activity instance for the callback operation.
     * 
     * @param N/A
     * 
     * @return IUpdateListButton: the activity instance
     *         Range: a valid object of IUpdateListButton
     *         Unit: IUpdateListButton
     *         Scaling: 1
     * 
     * @see mActivityListener: use this global variable to store the instance
     *      for performing the callback operation
     */
    protected IUpdateListButton getActivityListener()
    {
        return mActivityListener;
    }

    /**
     * Check whether the current scanned serial number of MicroPump is exist in
     * the stored list or not.
     * 
     * @param msd
     *            : The data includes SW version, System revision, OUI,
     *            RochePrefix
     *            Range: a valid object of ManuSpecData
     *            Unit: ManuSpecData
     *            Scaling: 1
     * @param byteAddress : the BLE address
     *            Range: a valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     * 
     * 
     * @return boolean: return true if the current scanned serial number is
     *         exist.
     *         Range: a valid object of boolean
     *         Unit: boolean
     *         Scaling: 1
     */
    protected void checkMpExistInList(ManuSpecData msd, byte[] byteAddress)
    {
        boolean isMpExistInList = false;
        List<String> mpList = MicroPumpMap.getPumpSerialList();
        Iterator<String> iterator = mpList.iterator();
        String serialNumber = null;

        Bundle pumpData = new Bundle();

        String mpSerialNumber = null;

        mpSerialNumber = msd.msdSerPre + msd.msdSerVal;

        while (iterator.hasNext() && (isMpExistInList == false))
        {
            serialNumber = (String) iterator.next();

            if (serialNumber.equals(mpSerialNumber))
            {
                isMpExistInList = true;
            }
            else
            {
                // Apply to the coding standard
            }

        }

        if (isMpExistInList == false)
        {

            pumpData.putByteArray(MicroPumpMap.KEY_MP_ADDRESS, byteAddress);
            Debug.printD(TAG, "SN = " + mpSerialNumber);

            MicroPumpMap.storeInSeriPumpDataMap(mpSerialNumber, pumpData);

            // notify UI
            if (null != mListener)
            {
                mListener.onScanInfoUpdated();
            }
            else
            {
                // Apply to the coding standard
            }

        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * 
     * Initialize the Roche Manufacturer Specific Data
     * 
     * @param N/A
     * 
     * @return void [out]
     * 
     * @see mRocheOui
     * @see mRochePrefix
     * @see mIsInitRocheDataDone
     */
    protected void initRocheManuSpecData()
    {

        /**
         * Serial Number prefix of Roche MicroPump GW: Formal release use
         * (Hardcode) ES: Internal use
         */
        String[] tempPrefix = { "GW", "ES" };

        Log.i(TAG, "mIsInitRocheDataDone = " + mIsInitRocheDataDone);

        if (mIsInitRocheDataDone == false)
        {

            mRochePrefix = tempPrefix.clone();

            // Get the compatible MP Roche OUI from Config_Matrix
            initCompMpOui();

            // Get the compatible MP software version number from Config_Matrix
            initCompMpSwVer();

            // Get the compatible MP system reversion number from Config_Matrix
            initCompMpSysRev();

            mIsInitRocheDataDone = true;
        }
        else
        {
            // Apply to the coding standard
        }

    }

    /**
     * 
     * Initialize the compatible MP OUI
     * 
     * @param N/A
     * 
     * @return void [out]
     * 
     * @see mRocheOui
     * 
     */
    protected void initCompMpOui()
    {
        int rocheOUI = 0;

        // rocheOUI = ReadConfig.getIntegerDataByKey(
        // new SafetyString(ConfigParameter.KEY_BLE_ROCHE_OUI, CRCTool
        // .generateCRC16(ConfigParameter.KEY_BLE_ROCHE_OUI
        // .getBytes()))).get();

        // (Hardcode) Company ID: Internal use 0x170
        // TODO Get the company ID from Config_Matrix (TBD)
        rocheOUI = 0x170;
        mRocheOui = (short) rocheOUI;
    }

    /**
     * 
     * Initialize the compatible MP software version number
     * 
     * @param N/A
     * 
     * @return void [out]
     * 
     * @see mCompMpSwVerChar
     * @see mCompMpSwVerMajor
     * @see mCompMpSwVerMinor
     * @see compMpSwVerTrail
     */
    protected void initCompMpSwVer()
    {

        String strCompMpSwVer = "";
        String strTemp = "";
        String[] strMajMinTra = null;
        String regularExpression = "\\.";

        int idxMaj = 0;
        int idxMin = 1;
        int idxTra = 2;

        strCompMpSwVer = ReadConfig.getStringDataByKey(
                new SafetyString(ConfigParameter.KEY_MP_SW_VERSION, CRCTool
                        .generateCRC16(ConfigParameter.KEY_MP_SW_VERSION
                                .getBytes()))).getString();

        Debug.printD(TAG, "strCompMpSwVer = " + strCompMpSwVer);
        //TODO: Parse Trail
        mCompMpSwVerChar = strCompMpSwVer.substring(0, 1);
        strTemp = strCompMpSwVer.substring(1, strCompMpSwVer.length());
        strMajMinTra = strTemp.split(regularExpression);
        mCompMpSwVerMajor = Integer.parseInt(strMajMinTra[idxMaj]);
        mCompMpSwVerMinor = Integer.parseInt(strMajMinTra[idxMin]);
        // mCompMpSwVerTrail = Integer.parseInt(strMajMinTra[idxTra]);
        Debug.printD(TAG, "mCompMpSwVerChar =" + mCompMpSwVerChar);
        Debug.printD(TAG, "mCompMpSwVerMajor =" + mCompMpSwVerMajor);
        Debug.printD(TAG, "mCompMpSwVerMinor =" + mCompMpSwVerMinor);
        // Debug.printD(TAG, "mCompMpSwVerTrail =" + mCompMpSwVerTrail);
    }

    /**
     * 
     * Initialize the compatible MP System Revision number
     * 
     * @param N/A
     * 
     * @return void [out]
     * 
     * @see mCompMpSysRevMajor
     * @see mCompMpSysRevMinor
     */
    protected void initCompMpSysRev()
    {
        // TODO Get the real value from Config_Matrix (TBD)
        mCompMpSysRevMajor = 1;
        mCompMpSysRevMinor = 2;
    }

    /**
     * 
     * Check the MicroPump SW Compatibility
     * 
     * @param msd
     *            : The data includes SW version, System revision, OUI,
     *            RochePrefix
     *            Range: a valid object of ManuSpecData
     *            Unit: ManuSpecData
     *            Scaling: 1
     * @param byteAddress : the BLE address
     *            Range: a valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     * @see mCompMpSwVerChar: Use this global varaible to store the SW version
     *      character
     * @see mCompMpSwVerMajor: Use this global varaible to store the SW major
     *      version
     * @see mCompMpSwVerMinor: Use this global varaible to store the SW minor
     *      version
     * 
     */
    protected void checkMpSwRev(ManuSpecData msd, byte[] byteAddress)
    {

        boolean isSwCharCompatible = msd.msdSwChar.equals(mCompMpSwVerChar);

        boolean isSwMajorCompatible = (msd.msdSwMajor == mCompMpSwVerMajor);

        boolean isSwMinorCompatible = (msd.msdSwMinor >= mCompMpSwVerMinor);

        boolean isMpCompatible = (isSwCharCompatible && isSwMajorCompatible && isSwMinorCompatible);

        Debug.printI("getMpCompatibility", "msdSwChar = " + msd.msdSwChar);
        Debug.printI("getMpCompatibility", "mCompMpSwVerChar = "
                + mCompMpSwVerChar);
        Debug.printI("getMpCompatibility", "msdSwMajor = " + msd.msdSwMajor);
        Debug.printI("getMpCompatibility", "mCompMpSwVerMajor = "
                + mCompMpSwVerMajor);
        Debug.printI("getMpCompatibility", "msdSwMinor = " + msd.msdSwMinor);
        Debug.printI("getMpCompatibility", "mCompMpSwVerMinor = "
                + mCompMpSwVerMinor);

        // XXX: Temporally disable the part of checking software
        // reversion
        isMpCompatible = true;

        if (isMpCompatible == true)
        {
            checkMpSysRev(msd, byteAddress);
        }
        else
        {
            // Apply to the coding standard
        }

    }

    /**
     * 
     * Check the MicroPump System Revision Compatibility
     * 
     * @param msd
     *            : The data includes SW version, System revision, OUI,
     *            RochePrefix
     *            Range: a valid object of ManuSpecData
     *            Unit: ManuSpecData
     *            Scaling: 1
     * @param byteAddress : the BLE address
     *            Range: a valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     * @see mCompMpSysRevMajor: Use this global varaible to store the System
     *      major Revision
     * @see mCompMpSysRevMinor: Use this global varaible to store the System
     *      minor Revision
     * 
     */
    protected void checkMpSysRev(ManuSpecData msd, byte[] byteAddress)
    {

        boolean isSysMajorCompatible = (msd.msdSysMajor == mCompMpSysRevMajor);

        boolean isSysMinorCompatible = (msd.msdSysMinor == mCompMpSysRevMinor);

        boolean isMpCompatible = (isSysMajorCompatible && isSysMinorCompatible);

        if (isMpCompatible == true)
        {
            checkMpExistInList(msd, byteAddress);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Check whether the BLE device is Roche's MicroPump or not
     * 
     * @param msd
     *            : The data includes SW version, System revision, OUI,
     *            RochePrefix
     *            Range: a valid object of ManuSpecData
     *            Unit: ManuSpecData
     *            Scaling: 1
     * @param byteAddress : the BLE address
     *            Range: a valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     * 
     * @return Is Roche's MicroPump or not
     *         Range: true, false
     *         Unit: boolean
     *         Scaling: 1
     * 
     * @see mRocheOui
     * @see mRochePrefix
     */
    protected void checkRocheMP(ManuSpecData msd, byte[] byteAddress)
    {
        short oui = 0;
        String prefix = null;
        boolean isRocheMP = false;
        int i = 0;

        // Logger.i(this, "prefix = " + prefix);
        // Check Company identifier <OUI>

        oui = msd.msdComId;
        prefix = msd.msdSerPre;

        if (oui == mRocheOui)
        {
            // Check Serial Number Prefix
            for (i = 0; (i < mRochePrefix.length) && (isRocheMP != true); i++)
            {
                isRocheMP = (prefix.compareTo(mRochePrefix[i]) == 0);
                //
                // Logger.i(this, "rochePrefix = " + mRochePrefix[i]);
            }
        }
        else
        {
            // Apply to the coding standard
        }

        if (isRocheMP == true)
        {

            checkMpSwRev(msd, byteAddress);
        }
        else
        {
            // Apply to the coding standard
        }

    }

    /**
     * 
     * Parse the Manufacturer Specific Data
     * 
     * @param data
     *            : the original Manufacturer Specific Data
     *            Range: a valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     * 
     * @return the parsed Manufacturer Specific Data
     *         Range: a valid object of ManuSpecData
     *         Unit: ManuSpecData
     *         Scaling: 1
     */
    protected ManuSpecData parseManuSpecData(byte[] data)
    {
        ByteBuffer buf = ByteBuffer.wrap(data);
        ManuSpecData msd = new ManuSpecData();
        String charserName = "UTF-8";

        int swCharLen = 1;
        byte[] swCharByte = new byte[swCharLen];

        int serPrelen = 2;
        byte[] serPreByte = new byte[serPrelen];

        byte byteMax = (byte) 0xFF;
        int intMax = 0xFFFFFFFF;

        // Advertising Data Length (1 byte)
        msd.advLen = buf.get();

        // Advertising Data Type (1 byte)
        msd.advType = buf.get();

        /* ------ Manufacturer Specific Data <START> ------ */

        // Company identifier <OUI> (uint16, 2 bytes)
        msd.msdComId = Short.reverseBytes(buf.getShort());

        // Software revision character (utf8s, 1 byte)
        buf.get(swCharByte, 0, swCharLen);

        // Software revision major value (uint8, 1 byte)
        msd.msdSwMajor = buf.get() & byteMax;

        // Software revision minor value (uint8, 1 byte)
        msd.msdSwMinor = buf.get() & byteMax;

        // Serial Number prefix (utf8s, 2 bytes)
        buf.get(serPreByte, 0, serPrelen);

        // Serial Number value (uint32, 4 bytes)
        //
        msd.msdSerVal = Integer.toString(Integer.reverseBytes(buf.getInt()
                & intMax));

        // System Revision major value (uint8, 1 byte)
        msd.msdSysMajor = buf.get() & byteMax;

        // System Revision minor value (uint8, 1 byte)
        msd.msdSysMinor = buf.get() & byteMax;

        /**
         * Change Software revision character and Serial Number prefix from byte
         * array to UTF-8 string
         */
        try
        {
            msd.msdSwChar = new String(swCharByte, charserName);
            msd.msdSerPre = new String(serPreByte, charserName);
        }
        catch (UnsupportedEncodingException e)
        {
            /**
             * The software revision character and serialnumber prefix are
             * invalid.
             * Skip this manufacturer specific data.
             */
        }
        finally
        {
            // Apply to the coding standard
        }

        /* ------ Manufacturer Specific Data <END> ------ */

        return msd;
    }
    
    protected class ManuSpecData
    {
        // Advertising Data Length (1 byte)
        byte advLen = 0x00;

        // Advertising Data Type (1 byte)
        byte advType = 0x00;

        /* ------ Manufacturer Specific Data <START> ------ */

        // Company identifier <OUI> (uint16, 2 bytes)
        short msdComId = 0x0000;

        // Software revision character (utf8s, 1 byte)
        String msdSwChar = "";

        // Software revision major value (uint8, 1 byte)
        int msdSwMajor = 0;

        // Software revision minor value (uint8, 1 byte)
        int msdSwMinor = 0;

        // Serial Number prefix (utf8s, 2 bytes)
        String msdSerPre = "";

        // Serial Number value (uint32, 4 bytes)
        String msdSerVal = "";

        // Software revision major value (uint8, 1 byte)
        int msdSysMajor = 0;

        // Software revision minor value (uint8, 1 byte)
        int msdSysMinor = 0;

        /* ------ Manufacturer Specific Data <END> ------ */

        /**
         * Constructor of ManuSpecData
         */
        public ManuSpecData()
        {
            // Constructor of ManuSpecData
        }

    }

}


/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R18085
                                                                                 // 2015-09-16
                                                                                 // 05:09:21
                                                                                 // KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
