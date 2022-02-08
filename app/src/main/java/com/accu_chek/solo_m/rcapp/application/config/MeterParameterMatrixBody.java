/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrixBody
 * Brief: Provide System Constants for Application Usage
 *
 * Create Date: 03/03/2015
 * $Revision: 20524 $
 * $Author: DWYang $
 * $Id: MeterParameterMatrixBody.java 20524 2015-10-01 11:12:52Z DWYang $
 */
package com.accu_chek.solo_m.rcapp.application.config;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.FileChannel;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.NoSuchElementException;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlPullParserFactory;

import com.accu_chek.solo_m.rcapp.application.exception.CrcNoMatchException;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.DataTypeMismatchException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

import android.util.Log;

public class MeterParameterMatrixBody
{
    // SafetyBoolean Byte Value
    private static final byte mByteSafetyTRUE = SafetyBoolean.TRUE.getByte();
    private static final byte mByteSafetyFALSE = SafetyBoolean.FALSE.getByte();
    
    // External Storage Path
    private static String mPathRoot = "/data";

    // Config Matrix Storage Path
    private static File mFileDir = new File(mPathRoot + "/ConfigMatrix/");

    // File Name
    // Name of Meter Parameter File
    private static String mStrMasterFileName = "CM_MeterParameter.xml";
    
    // Name of Meter Parameter Backup File
    private static String mStrBackupFileName = "CM_MeterParameter_Backup.xml";
    
    
    
    // Parameter Saved in RAM
    private HashMap<String, MeterParameter> mParameterSet = new HashMap<String, MeterParameter>();
    private HashMap<String, MeterParameter> mParameterSetBackup = new HashMap<String, MeterParameter>();

    // Meter Parameter
    private MeterParameter mMeterParameter = null;
    
    // File Object
    // File Object of Meter Parameter File
    private File mFileMasterFile = new File(mFileDir, mStrMasterFileName);
    // File Object of Meter Parameter Backup File
    private File mFileBackupFile = new File(mFileDir, mStrBackupFileName);

    // MeterParameterMatrix Initialization Result
    private SafetyBoolean mInitResult = SafetyBoolean.FALSE;

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Return the value of the attribute "sfbInitResult" to 
     * the calling function.
     * 
     * @return SafetyBoolean [out] Return if the process is running
     *         successfully.
     *         SafetyBoolean.TRUE -- Success
     *         SafetyBoolean.FALSE -- Fail
     * 
     *         Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean getInitializationResult()
    {
        return mInitResult;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Load parameters from file system. If loading master data from 
     * master file fails, load master data from backup file. If loading 
     * backup data from backup file fails, load backup data from master file.
     * The result of this function will be recorded at mInitResult.
     * 
     * @return None
     */
    protected void init()
    {
        // Return Result
        SafetyBoolean bFinalResult = SafetyBoolean.FALSE;

        // Function SafetyBoolean Result
        byte bResultLoadFromMaster = mByteSafetyFALSE;
        byte bResultLoadFromBackup = mByteSafetyFALSE;

        // Load Parameter
        mMeterParameter = null;
        bResultLoadFromMaster = loadParameter(mFileMasterFile, mParameterSet)
                .getByte();

        // Load Parameter from Backup file
        mMeterParameter = null;
        bResultLoadFromBackup = loadParameter(mFileBackupFile, mParameterSetBackup)
                .getByte();

        // Check if loading is OK
        if ((bResultLoadFromMaster == mByteSafetyTRUE)
                && (bResultLoadFromBackup == mByteSafetyTRUE))
        {
            // Reset process result to FALSE;
            bFinalResult = SafetyBoolean.TRUE;
        }
        else if ((bResultLoadFromMaster != mByteSafetyTRUE)
                && (bResultLoadFromBackup == mByteSafetyTRUE))
        {
            Log.d("Wlog", "Load Master Fail!!");

            // Load Parameter from Backup
            initLoadParameterMasterFromBackup();

            // Set process result to true;
            bFinalResult = SafetyBoolean.TRUE;

        }
        else if ((bResultLoadFromMaster == mByteSafetyTRUE)
                && (bResultLoadFromBackup != mByteSafetyTRUE))
        {
            Log.d("Wlog", "Load Backup Fail!!");

            // Load Parameter from Backup
            initLoadParameterBackupFromMaster();

            // Set process result to true
            bFinalResult = SafetyBoolean.TRUE;
        }
        else
        {
            // Set process result to false
            bFinalResult = SafetyBoolean.FALSE;

            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }

        mInitResult = bFinalResult;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * When loading backup of parameters from backup file is failed,
     * load backup of parameters from master file and restore corrupted
     * backup file from master file
     * 
     * @return None
     */
    protected void initLoadParameterBackupFromMaster()
    {
        // Reset bResult to FALSE
        byte isSuccessful = mByteSafetyFALSE;

        // Load Parameter Backup from Master File
        isSuccessful = loadParameter(mFileMasterFile, mParameterSetBackup).getByte();

        // Check if loading is ok
        if (isSuccessful == mByteSafetyTRUE)
        {
            // Restore Backup File from Master File
            restoreBackupFileFromMasterFile();

        }
        else
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * When loading master of parameters from master file is failed,
     * load master of parameters from backup file and restore corrupted
     * master file from backup file
     * 
     * @return None
     */
    protected void initLoadParameterMasterFromBackup()
    {
        // Reset bResult to FALSE
        byte isSuccessFul = mByteSafetyFALSE;

        // Load Parameter Backup from Master File
        isSuccessFul = loadParameter(mFileBackupFile, mParameterSet).getByte();

        // Check if loading is ok
        if (isSuccessFul == mByteSafetyTRUE)
        {
            // Restore Backup File from Master File
            restoreMasterFileFromBackupFile();
        }
        else
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Restore corrupted backup file from master file
     * 
     * @return None
     */
    protected void restoreBackupFileFromMasterFile()
    {
        // Function Result
        byte isOK = mByteSafetyFALSE;

        // Rename corrupted backup file name
        isOK = renameCorruptedFile(mFileBackupFile).getByte();

        if (isOK != mByteSafetyTRUE)
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }
        else
        {
            // Apply to the coding standard
        }

        // Copy MasterFile to BackupFile
        isOK = copyFile(mFileMasterFile, mStrBackupFileName).getByte();

        if (isOK != mByteSafetyTRUE)
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }
        else
        {
            // Apply to the coding standard
        }

        // Restore Backup File object
        mFileMasterFile = new File(mFileDir, mStrMasterFileName);
        mFileBackupFile = new File(mFileDir, mStrBackupFileName);

        // Compare Master File and New Backup File
        compareFile(mFileMasterFile, mFileBackupFile);
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Restore corrupted master file from backup file
     * 
     * @return None
     */
    protected void restoreMasterFileFromBackupFile()
    {
        // Function Result
        byte isOK = mByteSafetyFALSE;

        // Rename Master file name
        isOK = renameCorruptedFile(mFileMasterFile).getByte();

        if (isOK != mByteSafetyTRUE)
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }
        else
        {
            // Apply to the coding standard
        }

        // Copy BackupFile to MasterFile
        isOK = copyFile(mFileBackupFile, mStrMasterFileName).getByte();

        if (isOK != mByteSafetyTRUE)
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }
        else
        {
            // Apply to the coding standard
        }

        // Restore Backup File object
        mFileMasterFile = new File(mFileDir, mStrMasterFileName);
        mFileBackupFile = new File(mFileDir, mStrBackupFileName);

        // Compare Master File and Backup File
        compareFile(mFileMasterFile, mFileBackupFile);
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Get Integer type meter parameter from Hashmap and return the value
     * 
     * @param ssIDName Parameter ID Name
     * 
     *            NullPointerException will be handled at the calling function
     * 
     *            Range: Valid SafetyString object
     *            Unit: SafetyString 
     *            Scaling: 1
     * 
     * @return SafetyNumber<Integer> [out] the value of the requested ID Name
     * 
     *         Range: Valid SafetyNumber<Integer> object
     *         Unit: SafetyNumber<Integer>
     *         Scaling: 1
     * 
     * @throws DataIntegrityException [out] Error happens when extracting String
     *             from SafetyBoolean object
     * @throws NoSuchElementException [out] Can't find corresponding element in
     *             HashMap object
     * @throws DataTypeMismatchException [out] Data type of requested parameter
     *             doesn't fit the type of returned value
     */
    public SafetyNumber<Integer> getParameterInteger(SafetyString ssIDName)
            throws DataIntegrityException, NoSuchElementException,
            DataTypeMismatchException
    {
        // CRC Result
        byte isPassed = mByteSafetyFALSE;
        // Return Value
        SafetyNumber<Integer> sfInteger = new SafetyNumber<Integer>();
        // DataValue
        Integer iData = -1;
        // String value of ID Name
        String strIDName = ssIDName.getString();
        // Function Result
        boolean isSame = false;
        // Meter Parameter 
        MeterParameter mpData = null;
        
        
        // Get Meter Parameter from HashMap
        mpData = mParameterSet.get(strIDName);

        if (mpData == null)
        {
            throw new NoSuchElementException(
                    "ID Name can't be found in HashMap");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check Data Type
        isSame = mpData.getType().equalsIgnoreCase("Integer");

        if (isSame != true)
        {
            throw new DataTypeMismatchException(
                    "Wrong Meter Parameter Data Type");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check CRC16
        isPassed = mpData.compareCRC().getByte();

        if (isPassed == mByteSafetyTRUE)
        {
            // Set SafetyInteger content for Output
            iData = Integer.valueOf(mpData.getValue(), 10);

            sfInteger.set(iData, -iData);
        }
        else
        {
            try
            {
                sfInteger = getParameterIntegerFromBackup(ssIDName);
            }
            catch (Exception exception)
            {
                // Switch to Safety Mode
                // Call Safety Mode Function
                callEmwr();
            }
            finally
            {
                // Apply to the coding standard
            }
        }

        return sfInteger;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Get Integer type meter parameter from backup HashMap object and 
     * restore corrupted data of master HashMap object
     * 
     * @param ssIDName Parameter ID Name
     * 
     *            NullPointerException will be handled at the calling function
     * 
     *            Range: Valid SafetyString object
     *            Unit: SafetyString
     *            Scaling: 1
     * 
     * @return SafetyNumber<Integer> [out] the value of the requested ID Name
     * 
     *         Range: Valid SafetyNumber<Integer> object
     *         Unit: SafetyNumber<Integer>
     *         Scaling: 1
     * 
     * @throws DataIntegrityException [out] Error happens when extracting String
     *             from SafetyBoolean object
     * @throws NoSuchElementException [out] Can't find corresponding element in
     *             HashMap object
     * @throws DataTypeMismatchException [out] Data type of requested parameter
     *             doesn't fit the type of returned value
     * @throws CrcNoMatchException [out] Crc comparison of Meter Parameter
     *             object is failed
     */
    protected SafetyNumber<Integer> getParameterIntegerFromBackup(
            SafetyString ssIDName) throws DataIntegrityException,
            NoSuchElementException, DataTypeMismatchException,
            CrcNoMatchException
    {
        // CRC Result
        byte isPassed = mByteSafetyFALSE;
        // Return Value
        SafetyNumber<Integer> sfInteger = new SafetyNumber<Integer>();
        // DataValue
        Integer iData = -1;
        // String value of ID Name
        String strIDName = ssIDName.getString();
        // Function Result
        boolean isSame = false;
        // Check Result
        boolean isNull = false;
        // Meter Parameter
        MeterParameter mpData = null;
        MeterParameter mpDataBackup = null;
        
        // Get Meter Parameter from Both DataSet
        mpData = mParameterSet.get(strIDName);
        mpDataBackup = mParameterSetBackup.get(strIDName);
        isNull = (mpData == null) || (mpDataBackup == null);

        if (isNull == true)
        {
            throw new NoSuchElementException(
                    "ID Name can't be found in HashMap");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check Data Type
        isSame = mpDataBackup.getType().equalsIgnoreCase("Integer");

        if (isSame != true)
        {
            throw new DataTypeMismatchException(
                    "Wrong Meter Parameter Backup Data Type");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check CRC16
        isPassed = mpDataBackup.compareCRC().getByte();

        if (isPassed != mByteSafetyTRUE)
        {
            throw new CrcNoMatchException(
                    "Meter Parameter Integer CRC no Match!!");
        }
        else
        {
            // Apply to the coding standard
        }

        // Set SafetyInteger content for Output
        iData = Integer.valueOf(mpDataBackup.getValue(), 10);

        // Restore Crashed Master Data from Backup
        mpData.setIDName(mpDataBackup.getIDName());
        mpData.setType(mpDataBackup.getType());
        mpData.setValue(mpDataBackup.getValue());
        mpData.setCRC(mpDataBackup.getCRC());

        // Make output result
        sfInteger.set(iData, -iData);

        return sfInteger;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Get String type meter parameter from HashMap and return the value
     * 
     * @param ssIDName Parameter ID Name
     * 
     *            NullPointerException will be handled at the calling function
     * 
     *            Range: Valid SafetyString object
     *            Unit: SafetyString
     *            Scaling: 1
     * 
     * @return SafetyString [out] the value of requested ID
     * 
     *         Range: Valid SafetyString object
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @throws DataIntegrityException [out] Error happens when extracting String
     *             from SafetyBoolean object
     * @throws NoSuchElementException [out] Can't find corresponding element in
     *             HashMap object
     * @throws DataTypeMismatchException [out] Data type of requested parameter
     *             doesn't fit the type of returned value
     * 
     */
    public SafetyString getParameterString(SafetyString ssIDName)
            throws DataIntegrityException, NoSuchElementException,
            DataTypeMismatchException
    {
        // CRC Result
        byte isPassed = mByteSafetyFALSE;
        // Return Value
        SafetyString sfString = new SafetyString();
        // String value of ID Name
        String strIDName = ssIDName.getString();
        // Function Result
        boolean isSame = false;
        // MeterParameter
        MeterParameter mpData = null;
        
        // Get Meter Parameter from HashMap
        mpData = mParameterSet.get(strIDName);

        if (mpData == null)
        {
            throw new NoSuchElementException(
                    "ID Name can't be found in HashMap");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check Data Type
        isSame = mpData.getType().equalsIgnoreCase("String");

        if (isSame != true)
        {
            throw new DataTypeMismatchException(
                    "Wrong Meter Parameter Data Type");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check CRC16
        isPassed = mpData.compareCRC().getByte();

        if (isPassed == mByteSafetyTRUE)
        {
            // Set SafetyString content for Output
            int crc16 = CRCTool.generateCRC16(mpData.getValue().getBytes());

            sfString.set(mpData.getValue(), crc16);
        }
        else
        {
            try
            {
                sfString = getParameterStringFromBackup(ssIDName);
            }
            catch (Exception exception)
            {
                // Switch to Safety Mode
                // Call Safety Mode Function
                callEmwr();
            }
            finally
            {
                // Apply to the coding standard
            }
        }

        return sfString;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Get String type meter parameter from backup HashMap object and 
     * restore corrupted data of master HashMap object
     * 
     * @param ssIDName Parameter ID Name
     * 
     *            NullPointerException will be handled at the calling function
     * 
     *            Range: Valid SafetyString object
     *            Unit: SafetyString
     *            Scaling: 1
     * 
     * @return SafetyString [out] the value of requested ID
     * 
     *         Range: Valid SafetyString object
     *         Unit: SafetyString
     *         Scaling: 1
     * 
     * @throws DataIntegrityException [out] Error happens when extracting String
     *             from SafetyBoolean object
     * @throws NoSuchElementException [out] Can't find corresponding element in
     *             HashMap object
     * @throws DataTypeMismatchException [out] Data type of requested parameter
     *             doesn't fit the type of returned value
     * @throws CrcNoMatchException [out] CRC comparison of Meter Parameter
     *             object is failed
     */
    protected SafetyString getParameterStringFromBackup(SafetyString ssIDName)
            throws DataIntegrityException, NoSuchElementException,
            DataTypeMismatchException, CrcNoMatchException
    {
        // CRC Result
        byte isPassed = mByteSafetyFALSE;
        // Return Value
        SafetyString sfString = new SafetyString();
        // String value of ID Name
        String strIDName = ssIDName.getString();
        // Function Result
        boolean isSame = false;
        // Check Result
        boolean isNull = false;
        // Meter Parameter
        MeterParameter mpData = null;
        MeterParameter mpDataBackup = null;        
        
        // Get Meter Parameter from Both DataSet
        mpData = mParameterSet.get(strIDName);
        mpDataBackup = mParameterSetBackup.get(strIDName);
        isNull = (mpData == null) || (mpDataBackup == null);

        if (isNull == true)
        {
            throw new NoSuchElementException(
                    "ID Name can't be found in HashMap");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check Data Type
        isSame = mpDataBackup.getType().equalsIgnoreCase("String");

        if (isSame != true)
        {
            throw new DataTypeMismatchException(
                    "Wrong Meter Parameter Backup Data Type");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check CRC16
        isPassed = mpDataBackup.compareCRC().getByte();

        if (isPassed != mByteSafetyTRUE)
        {
            throw new CrcNoMatchException(
                    "Meter Parameter String CRC no Match!!");
        }
        else
        {
            // Apply to the coding standard
        }

        // Set SafetyString content for Output
        int crc16 = CRCTool.generateCRC16(mpDataBackup.getValue().getBytes());

        sfString.set(mpDataBackup.getValue(), crc16);

        // Restore Crashed Master Data from Backup
        mpData.setIDName(mpDataBackup.getIDName());
        mpData.setType(mpDataBackup.getType());
        mpData.setValue(mpDataBackup.getValue());
        mpData.setCRC(mpDataBackup.getCRC());

        return sfString;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Get Long type meter parameter from HashMap and return the value
     * 
     * @param ssIDName Parameter ID Name
     * 
     *            NullPointerException will be handled at the calling function
     * 
     *            Range: Valid SafetyString object
     *            Unit: SafetyString
     *            Scaling: 1
     * 
     * @return SafetyNumber<Long> [out] the value of the requested ID Name
     * 
     *         Range: Valid SafetyNumber<Long> object
     *         Unit: SafetyNumber<Long>
     *         Scaling: 1
     * 
     * @throws DataIntegrityException [out] Error happens when extracting String
     *             from SafetyBoolean object
     * @throws NoSuchElementException [out] Can't find corresponding element in
     *             HashMap object
     * @throws DataTypeMismatchException [out] Data type of requested parameter
     *             doesn't fit the type of returned value
     */
    public SafetyNumber<Long> getParameterLong(SafetyString ssIDName)
            throws DataIntegrityException, NoSuchElementException,
            DataTypeMismatchException
    {
        // CRC Result
        byte isPassed = mByteSafetyFALSE;
        // Return Value
        SafetyNumber<Long> sfLong = new SafetyNumber<Long>();
        // String value of ID Name
        String strIDName = ssIDName.getString();
        // Function Result
        boolean isSame = false;
        // Output Data
        long lData = 0;
        // Meter Parameter
        MeterParameter mpData = null;

        // Get Meter Parameter from HashMap
        mpData = mParameterSet.get(strIDName);

        if (mpData == null)
        {
            throw new NoSuchElementException(
                    "ID Name can't be found in HashMap");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check Data Type
        isSame = mpData.getType().equalsIgnoreCase("Long");

        if (isSame != true)
        {
            throw new DataTypeMismatchException(
                    "Wrong Meter Parameter Data Type");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check CRC16
        isPassed = mpData.compareCRC().getByte();

        if (isPassed == mByteSafetyTRUE)
        {
            // Set SafetyLong content for Output
            lData = Long.valueOf(mpData.getValue(), 10);

            sfLong.set(lData, (long) -lData);
        }
        else
        {
            try
            {
                sfLong = getParameterLongFromBackup(ssIDName);
            }
            catch (Exception exception)
            {
                // Switch to Safety Mode
                // Call Safety Mode Function
                callEmwr();
            }
            finally
            {
                // Apply to the coding standard
            }
        }

        return sfLong;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Get Long type meter parameter from backup HashMap object and 
     * restore corrupted data of master HashMap object
     * 
     * @param ssIDName Parameter ID Name
     * 
     *            NullPointerException will be handled at the calling function
     * 
     *            Range: Valid SafetyString object
     *            Unit: SafetyString
     *            Scaling: 1
     * 
     * @return SafetyNumber<Long> [out] the value of the requested ID Name
     * 
     *         Range: Valid SafetyNumber<Long> object
     *         Unit: SafetyNumber<Long>
     *         Scaling: 1
     * 
     * @throws DataIntegrityException [out] Error happens when extracting String
     *             from SafetyBoolean object
     * @throws NoSuchElementException [out] Can't find corresponding element in
     *             HashMap object
     * @throws DataTypeMismatchException [out] Data type of requested parameter
     *             doesn't fit the type of returned value
     * @throws CrcNoMatchException [out] CRC comparison of Meter Parameter
     *             object is failed
     */
    protected SafetyNumber<Long> getParameterLongFromBackup(
            SafetyString ssIDName) throws DataIntegrityException,
            NoSuchElementException, DataTypeMismatchException,
            CrcNoMatchException
    {
        // CRC Result
        byte isPassed = mByteSafetyFALSE;
        // Return Value
        SafetyNumber<Long> sfLong = new SafetyNumber<Long>();
        // DataValue
        long lData = -1;
        // String value of ID Name
        String strIDName = ssIDName.getString();
        // Function Result
        boolean isSame = false;
        // Meter Parameter
        MeterParameter mpData = null;
        MeterParameter mpDataBackup = null;
        // Check Result
        boolean isNull = false;
        
        // Get Meter Parameter from Both DataSet
        mpData = mParameterSet.get(strIDName);
        mpDataBackup = mParameterSetBackup.get(strIDName);
        isNull = (mpData == null) || (mpDataBackup == null);

        if (isNull == true)
        {
            throw new NoSuchElementException(
                    "ID Name can't be found in HashMap");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check Data Type
        isSame = mpDataBackup.getType().equalsIgnoreCase("Long");

        if (isSame != true)
        {
            throw new DataTypeMismatchException(
                    "Wrong Meter Parameter Backup Data Type");
        }
        else
        {
            // Apply to the coding standard
        }

        // Check CRC16
        isPassed = mpDataBackup.compareCRC().getByte();

        if (isPassed != mByteSafetyTRUE)
        {
            throw new CrcNoMatchException("Meter Parameter Long CRC no Match!!");
        }
        else
        {
            // Apply to the coding standard
        }

        // Set SafetyInteger content for Output
        lData = Long.valueOf(mpDataBackup.getValue(), 10);

        // Restore Crashed Master Data from Backup
        mpData.setIDName(mpDataBackup.getIDName());
        mpData.setType(mpDataBackup.getType());
        mpData.setValue(mpDataBackup.getValue());
        mpData.setCRC(mpDataBackup.getCRC());

        // Make output result
        sfLong.set(lData, (long) -lData);

        return sfLong;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Load parameters to assigned HashMap object from file system
     * 
     * @param fFile File object where the file will be parsed.
     * 
     *            This parameter will be the member attributes "mFileMasterFile" 
     *            or "mFileBackupFile". This two File objects will be created at 
     *            MeterParameterMatrix creation period. If the creation fails, 
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     *            Range: Valid File object
     *            Unit: File
     *            Scaling: 1
     * 
     * @param mpSet HashMap the parsed data will be put in.
     * 
     *            This parameter will be the member attributes "mParameterSet" or
     *            "mParameterSetBackup". This two HashMap objects will be created
     *            at MeterParameterMatrix creation period. If the creation fails,
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     *            Range: Valid HashMap object
     *            Unit: HashMap
     *            Scaling: 1
     * 
     * @return SafetyBoolean [out] Return if the process is running
     *         successfully.
     *         SafetyBoolean.TRUE -- Success
     *         SafetyBoolean.FALSE -- Fail
     * 
     *         Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     * 
     */
    protected SafetyBoolean loadParameter(File fFile,
            HashMap<String, MeterParameter> mpSet)
    {
        // Function Result
        SafetyBoolean isSuccessFul = SafetyBoolean.FALSE;

        // InputStream Object for Parser File
        InputStream in_s = null;

        // Parser
        XmlPullParserFactory pullParserFactory = null;
        XmlPullParser parser = null;

        mpSet.clear();

        try
        {
            // Create Parser Object
            pullParserFactory = XmlPullParserFactory.newInstance();
            parser = pullParserFactory.newPullParser();

            // Create new Input Stream Object
            in_s = new BufferedInputStream(new FileInputStream(fFile));

            // Set parser attributes
            parser.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, false);

            parser.setInput(in_s, null);

            // Parse XML
            parseXML(parser, mpSet);
            
            // Close Input STream
            in_s.close();
            in_s = null;

            // Set Function Result
            isSuccessFul = SafetyBoolean.TRUE;
        }
        catch (Exception exception)
        {
            exception.printStackTrace();

            // Set Function Result
            isSuccessFul = SafetyBoolean.FALSE;
        }
        finally
        {
            try
            {
                if (in_s != null)
                {
                    in_s.close();
                }
            }
            catch (IOException e)
            {
                isSuccessFul = SafetyBoolean.FALSE;
            }
            finally
            {
                // Apply to the coding standard
            }
        }

        return isSuccessFul;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Parse assigned XML file and insert data into assigned HashMap object
     * 
     * @param parser XML Parser Object
     * 
     *            NullPointerException will be handled at function loadParameter
     * 
     *            Range: Valid XmlPullParser object
     *            Unit: XmlPullParser
     *            Scaling: 1
     * 
     * @param mpSet HashMap Object for Storing Meter Parameters
     * 
     *            This parameter will be the member attributes "mParameterSet" or
     *            "mParameterSetBackup". This two HashMap objects will be created
     *            at MeterParameterMatrix creation period. If the creation fails,
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     * 
     *            Range: Valid HashMap object
     *            Unit: HashMap
     *            Scaling: 1
     * 
     * @return None
     * 
     * @throws XmlPullParserException [out] XmlPullParser error happens
     * @throws IOException [out] File IO error happens
     * @throws CrcNoMatchException [out] Crc comparison of MeterParameter object
     *             is
     *             failed
     * 
     */
    protected void parseXML(XmlPullParser parser,
            HashMap<String, MeterParameter> mpSet)
            throws XmlPullParserException, IOException, CrcNoMatchException

    {
        // Evnet Type
        int eventType = parser.getEventType();

        // TAG Name
        while (eventType != XmlPullParser.END_DOCUMENT)
        {
            switch (eventType)
            {
            case XmlPullParser.START_DOCUMENT :
                // Apply to the coding standard

                // Log.i("Wlog", "START_DOC");

                break;

            case XmlPullParser.START_TAG :
                // Log.i("Wlog", "START_TAG");
                // Create New Meter Parameter Object and Set Data into
                // this Object
                createNewMeterParameterAndSetData(parser);

                break;

            case XmlPullParser.END_TAG :
                // Log.i("Wlog", "END_TAG");
                // Set Meter Parameter Object into HashMap Data Set
                setMeterParameterIntoHashMap(parser, mpSet);

                break;

            default :
                // Apply to the coding standard

                break;
            }

            // Parse Next Event
            eventType = parser.next();
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Create new MeterParameter object and set data into this object
     * 
     * @param parser Parser for parsing the data file
     * 
     *            NullPointerException will be handled at function loadParameter
     * 
     *            Range: Valid XmlPullParser object
     *            Unit: XmlPullParser
     *            Scaling: 1
     * 
     * @return None
     * 
     * @throws XmlPullParserException [out] XmlPullParser error happens
     * @throws IOException [out] File IO error happens
     */
    protected void createNewMeterParameterAndSetData(XmlPullParser parser)
            throws XmlPullParserException, IOException
    {
        // Get TAG Name
        String strTAGname = parser.getName();

        if (strTAGname.equals("record"))
        {
            // Create new Meter Parameter
            mMeterParameter = new MeterParameter();
        }
        else if (mMeterParameter != null)
        {
            // Set Data into MeterParameter
            setDataIntoMeterParameter(parser, strTAGname);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Set Data into MeterParameter Object
     * 
     * @param parser Parser for parsing the data file
     * 
     *            NullPointerException is handled at function loadParameter
     * 
     *            Range: Valid XmlPullParser object
     *            Unit: XmlPullParser
     *            Scaling: 1
     * 
     * @param strTAGname Parsed XML tag name from the data file
     * 
     *            This parameter comes from XmlPullParser.getName(). The
     *            exceptions will be
     *            handled at function loadParameter.
     * 
     *            Range: Valid String
     *            Unit: String
     *            Scaling: 1
     *
     * @return None
     * 
     * @throws XmlPullParserException [out] XmlPullParser error happens
     * @throws IOException [out] File IO error happens
     */
    protected void setDataIntoMeterParameter(XmlPullParser parser,
            String strTAGname) throws XmlPullParserException, IOException
    {
        // Get TAG Value
        String strTAGValue = parser.nextText();

        // Set Data into Meter Parameter Object by TAG name
        if (strTAGname.equals("Parameter"))
        {
            mMeterParameter.setIDName(strTAGValue);
        }
        else if (strTAGname.equals("Type"))
        {
            mMeterParameter.setType(strTAGValue);
        }
        else if (strTAGname.equals("Value"))
        {
            mMeterParameter.setValue(strTAGValue);
        }
        else if (strTAGname.equals("CRC"))
        {
            mMeterParameter.setCRC(strTAGValue);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Set MeterParameter object into HashMap data set
     * 
     * @param parser Parser for parsing the data file
     * 
     *            NullPointerException will be handled at function
     *            loadParameter.
     * 
     *            Range: Valid XmlPullParser object
     *            Unit: XmlPullParser
     *            Scaling: 1
     * 
     * @param mpSet Data set for storing Meter Parameters parsed from the data
     *            file
     * 
     *            This parameter will be the member attributes "mParameterSet" or
     *            "mParameterSetBackup". This two HashMap objects will be created
     *            at MeterParameterMatrix creation period. If the creation fails,
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     * 
     *            Range: Valid HashMap object
     *            Unit: HashMap
     *            Scaling: 1
     * 
     * @return None
     * 
     * @throws CrcNoMatchException [out] CRC comparison of MeterParameter object
     *             is failed
     */
    protected void setMeterParameterIntoHashMap(XmlPullParser parser,
            HashMap<String, MeterParameter> mpSet) throws CrcNoMatchException
    {
        // Get END TAG Name
        String strEndTAGname = parser.getName();
        boolean isSame = false;

        // Log.i("Wlog", "END_TAG : " + strEndTAGname);

        // Check END_TAG name
        isSame = strEndTAGname.equals("record");
        if ((isSame == true) && (mMeterParameter != null))
        {
            // Compare CRC and Set into HashMap
            checkCRCandSetIntoHashMap(mpSet);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Check CRC; if it is passed, set MeterParameter object into HashMap data
     * set
     * 
     * @param mpSet Data set for storing MeterParameter objects.
     * 
     *            This parameter will be the member attributes "mParameterSet" or
     *            "mParameterSetBackup". This two HashMaps will be created at
     *            MeterParameterMatrix creation period. If the creation fails, 
     *            NullPointerException will be thrown and
     *            be caught by uncaughtException
     * 
     *            Range: Valid HashMap Object
     *            Unit: HashMap
     *            Scaling: 1
     * 
     * @return None
     * @throws CrcNoMatchException [out] CRC comparison of MeterParameter object
     *             is failed
     */
    protected void checkCRCandSetIntoHashMap(
            HashMap<String, MeterParameter> mpSet) throws CrcNoMatchException
    {
        // Function Result
        byte sfbResult = mByteSafetyFALSE;

        // Compare CRC with Data from File
        sfbResult = mMeterParameter.compareCRC().getByte();

        if (sfbResult == mByteSafetyTRUE)
        {
            mpSet.put(mMeterParameter.getIDName(), mMeterParameter);

            mMeterParameter = null;
        }
        else
        {
            throw new CrcNoMatchException("Read Meter Parameter CRC no Match!!");
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Rename corrupted file's name to another one to prevent reusing the flash
     * block.
     * 
     * @param fFile File object where the data is corrupted
     * 
     *            NullPointerException will be caught by uncaughtException
     * 
     *            Range: Valid File object
     *            Unit: File
     *            Scaling: 1
     * 
     * @return SafetyBoolean [out] Return if the process is running
     *         successfully.
     *         SafetyBoolean.TRUE -- Success
     *         SafetyBoolean.FALSE -- Fail
     * 
     *         Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    protected SafetyBoolean renameCorruptedFile(File fFile)
    {
        // DateFormat Assignment as FileName
        Calendar caTime = Calendar.getInstance();
        SimpleDateFormat sdFormat = new SimpleDateFormat("yyyyMMddkkmmss");

        // New FileName for Corrupt File
        String nFileName = sdFormat.format(caTime.getTime()) + ".CRP";

        // Result for rename function running result
        SafetyBoolean isOK = SafetyBoolean.FALSE;

        // Run Rename function
        isOK = renameFile(fFile, nFileName);

        return isOK;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Rename file's name with a new one
     * 
     * @param fFile File object where the data is corrupted
     * 
     *            This parameter will be the member attributes "mFileMasterFile"
     *            or "mFileBackupFile". This two File objects will be created at 
     *            MeterParameterMatrix creation period. If the creation fails, 
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     * 
     *            Range: Valid File object
     *            Unit: File
     *            Scaling: 1
     * 
     * @param strNewFileName New file name for replacement
     * 
     *            NullPointerException will be caught by uncaughtException.
     * 
     *            Range: Valid String
     *            Unit: String
     *            Scaling: 1
     * 
     * @return SafetyBoolean [out] Return if the process is running
     *         successfully.
     *         SafetyBoolean.TRUE -- Success
     *         SafetyBoolean.FALSE -- Fail
     * 
     *         Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    protected SafetyBoolean renameFile(File fFile, String strNewFileName)
    {
        // New file name object
        File nFile = new File(mFileDir, strNewFileName);

        // Result for rename function running result
        boolean isOK = false;
        SafetyBoolean isSuccessful = SafetyBoolean.FALSE;

        // Run Rename function
        isOK = fFile.renameTo(nFile);

        if (isOK)
        {
            isSuccessful = SafetyBoolean.TRUE;
        }
        else
        {
            isSuccessful = SafetyBoolean.FALSE;
        }

        return isSuccessful;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Copy file object to a new one with the destination name
     * 
     * @param fFile Source File Object
     * 
     *            This parameter will be the member attributes "mFileMasterFile"
     *            or "mFileBackupFile". This two File objects will be created at 
     *            MeterParameterMatrix creation period. If the creation fails, 
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     *            Range: Valid File object
     *            Unit: File
     *            Scaling: 1
     * 
     * @param strDestFileName Destination File Name
     * 
     *            This parameter will be the member attributes "mStrMasterFileName" 
     *            or "mStrBackupFileName". This two String objects will be created
     *            at MeterParameterMatrix creation period. If the creation fails,
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     *            Range: Valid String
     *            Unit: String
     *            Scaling: 1
     * 
     * @return SafetyBoolean [out] Return if the process is running successful.
     *         SafetyBoolean.TRUE -- Success
     *         SafetyBoolean.FALSE -- Fail
     * 
     *         Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    protected SafetyBoolean copyFile(File fFile, String strDestFileName)
    {
        // rename backup to other name with time of this action
        File nFile = new File(mFileDir, strDestFileName);

        // Result for rename function running result
        SafetyBoolean isOK = SafetyBoolean.FALSE;

        // FileStream IN/OUT
        FileInputStream fInputStream = null;
        FileOutputStream fOutputStream = null;

        // FileChannel IN/OUT
        FileChannel inChannel = null;
        FileChannel outChannel = null;

        try
        {
            // Create FileStream IN/OUT
            fInputStream = new FileInputStream(fFile);
            fOutputStream = new FileOutputStream(nFile);

            // Get FileChannel IN/OUT
            inChannel = fInputStream.getChannel();
            outChannel = fOutputStream.getChannel();

            // Transfer IN content to OUT
            inChannel.transferTo(0, inChannel.size(), outChannel);

            // Close Channel
            inChannel.close();
            inChannel = null;
            
            outChannel.close();
            outChannel = null;

            // Close Stream
            fInputStream.close();
            fInputStream = null;
            
            fOutputStream.close();
            fOutputStream = null;

            isOK = SafetyBoolean.TRUE;
        }
        catch (IOException ioException)
        {
            isOK = SafetyBoolean.FALSE;
        }
        finally
        {
            // Close InputChannel
            if (inChannel != null)
            {
                try
                {
                    inChannel.close();
                    inChannel = null;
                }
                catch (IOException ioExecption)
                {                    
                    isOK = SafetyBoolean.FALSE;
                }
                finally
                {
                    // Apply to the coding standard
                }
            }

            // Close OutputChannel
            if (outChannel != null)
            {
                try
                {
                    outChannel.close();
                    outChannel = null;
                }
                catch (IOException ioExecption)
                {                    
                    isOK = SafetyBoolean.FALSE;
                }
                finally
                {
                    // Apply to the coding standard
                }
            }

            // Close InputStream
            if (fInputStream != null)
            {
                try
                {
                    fInputStream.close();
                    fInputStream = null;
                }
                catch (IOException ioExecption)
                {
                    isOK = SafetyBoolean.FALSE;
                }
                finally
                {
                    // Apply to the coding standard
                }
            }

            // Close OutputStream
            if (fOutputStream != null)
            {
                try
                {
                    fOutputStream.close();
                    fOutputStream = null;
                }
                catch (IOException ioExecption)
                {
                    isOK = SafetyBoolean.FALSE;
                }
                finally
                {
                    // Apply to the coding standard
                }
            }
        }

        return isOK;
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Compare two files with CRC16. If two files are not the same, call EMWR
     * 
     * 
     * @param fFile1 File Object for Comparison
     * 
     *            This parameter will be the member attributes "mFileMasterFile" 
     *            or "mFileBackupFile". This two File objects will be created at 
     *            MeterParameterMatrix creation period. If the creation fails, 
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     *            Range: Valid File object
     *            Unit: File
     *            Scaling: 1
     * 
     * @param fFile2 File Object for Comparison
     * 
     *            This parameter will be the member attributes "mFileMasterFile" 
     *            or "mFileBackupFile". This two File objects will be created at 
     *            MeterParameterMatrix creation period. If the creation fails, 
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     *            Range: Valid File object
     *            Unit: File
     *            Scaling: 1
     * 
     * @return None
     * 
     */
    protected void compareFile(File fFile1, File fFile2)
    {
        // Get Result of MD5 calculation
        int iChecksum1 = checksumFile(fFile1);
        int iChecksum2 = checksumFile(fFile2);

        // Get Result of compare
        if (iChecksum1 != iChecksum2)
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Status: FDD/Coding/UT Done
     * 
     * Calculate CRC16 of a File
     * 
     * @param fFile File Object for Calculating Checksum
     * 
     *            This parameter will be the member attributes "mFileMasterFile"
     *            or "mFileBackupFile". This two File objects will be created at 
     *            MeterParameterMatrix creation period. If the creation fails, 
     *            NullPointerException will be thrown and be caught by
     *            uncaughtException
     * 
     *            Range: Valid File object
     *            Unit: File
     *            Scaling: 1
     * 
     * @return int [out] CRC16 of the assigned file
     * 
     *         Range: default int range
     *         Unit: int
     *         Scaling: 1
     */
    
    protected int checksumFile(File fFile)
    {
        // Input Stream
        InputStream in_s = null;

        // File Length
        int iFileSize = (int) fFile.length();

        // File read Buffer
        byte[] buf = new byte[iFileSize];

        // CRC16 Checksum
        int crc16 = 0;

        // Read Buffer Counts
        @SuppressWarnings("unused")
        int iReadBuffCounts = 0;

        // Exception
        boolean isException = false;

        try
        {
            // Get New FileInputStream Object
            in_s = new FileInputStream(fFile);

            // File read
            iReadBuffCounts = in_s.read(buf, 0, iFileSize);

            // close FileInputStream
            in_s.close();
            in_s = null;
            
            // Set Exception to false
            isException = false;
        }
        catch (Exception exception)
        {
            isException = true;
        }
        finally
        {
            try
            {
                if (in_s != null)
                {
                    in_s.close();
                    in_s = null;
                }
            }
            catch (IOException e)
            {
                isException = true;
            }
            finally
            {
                // Apply to the coding standard
            }
        }

        if (isException == true)
        {
            // Switch to Safety Mode
            // Call Safety Mode Function
            callEmwr();
        }

        // Calculate CRC16
        crc16 = CRCTool.generateCRC16(buf);

        // Return Calculated MD5
        return crc16;
    }
    
    /**
     * Status: Not Ready for review
     * 
     * Trigger EMWR for error handling
     * 
     * @param None
     * @return None
     * @throws Exception 
     */
    protected void callEmwr()
    {
        // TODO : Fix it when EMWR and Log is ready
        //throw new Exception("Error happens!! Switch to Safe State");
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Update comments for wording refine
// (R14734 2015-08-13 23:21:10 JacksonHuang)
// ----------------------------------------------------------------------------
// The method switchToSafeState change the name to callEmwr
// (R15106 2015-08-20 05:59:06 JacksonHuang)
// ----------------------------------------------------------------------------
// Refine wording of comments (ok --> OK)
