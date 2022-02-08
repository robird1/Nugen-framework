/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Selftest
 * Brief: Provide basic functions as register or run self-test item.
 *
 * Create Date: 07/02/2015
 * $Revision: 21298 $
 * $Author: JamesLee $
 * $Id: Selftest.java 21298 2015-10-12 03:04:06Z JamesLee $
 */

package com.accu_chek.solo_m.rcapp.application.selftest;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.logfile.LogContent;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Selftest
{

    // Defines debug tag.
    private static final String TAG = "SelfTest";

    // Define EMWR's default id.
    private static final EMWRList DEFAULT_EMWRID = EMWRList.EMW41001;

    // Define error number in this self-test cycle.
    private static final int ERROR_NUMBER = 1;

    // Record EMWR's id of self-test item.
    private EMWRList mEmwrID;

    // Define List type to save interface ISelfTestListener
    private List<ISelftestListener> mSelftestListener = new ArrayList<ISelftestListener>();

    // Record all self-test EMWR's id when the self-test is failed.
    private HashMap<EMWRList, String> mLogMap = null;

    // The extra-string is for log to file.
    private SafetyString mSafetyExtraString = null;

    // Allow to enable log to file.
    private SafetyBoolean mEnableLog = SafetyBoolean.FALSE;

    // Point to context activity in android.
    private Context mContext = null;

    /**
     * The constructor of Selftest. Initialize the objects.
     * 
     * @param context
     *            [in] : context of application or activity in android.
     *            Range :context is equal to valid context object
     *            Unit: context
     *            Scaling: 1
     * 
     * @see mContext [in]
     * 
     * @return None
     */
    public Selftest(final Context context)
    {
        Debug.printI(TAG, " initial SelfTest constructor : " + context);

        // initial object
        initial(context);
    }

    /**
     * Initialize the objects
     * 
     * @param context
     *            [in] : the context of activity in android.
     *            Range :valid context object
     *            Unit: context
     *            Scaling: 1
     * 
     * @see mContext [out]
     *      challengeCtrl [out]
     * 
     * @return None
     */
    private void initial(final Context context)
    {
        mContext = context.getApplicationContext();

        // new a hash-map to save error log.
        mLogMap = new HashMap<EMWRList, String>();

        // set to false.
        mEnableLog = SafetyBoolean.FALSE;

        // default value of emwr id.
        mEmwrID = DEFAULT_EMWRID;
    }

    /**
     * Status: Coding
     * 
     * Set the listener point to ISelfTestListener interface
     * 
     * @param listener
     *            [in] : object of interface ISelfTestListener
     *            Range :point to valid ISelfTestListener object.
     *            Unit: ISelfTestListener
     *            Scaling:1
     * @see selftestListener [in]
     * 
     * @return None
     */
    public final void addListener(final ISelftestListener listener)
    {
        // Valid object
        CommonUtils.objectCheck(listener);

        // add to array-list
        mSelftestListener.add(listener);
    }

    /**
     * Execute registered listener self-test items
     * 
     * @see selftestListener [in]
     *      mEMWRID[in]
     * 
     * @return SafetyBoolean [in] return the result of self-test
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     */
    public SafetyBoolean runTest()
    {
        // The byte value of SafetyBoolean FALSE
        final byte byteSafetyFALSE = SafetyBoolean.FALSE.getByte();

        final int listensize = mSelftestListener.size();

        SafetyBoolean isResultOK = SafetyBoolean.TRUE;

        SafetyBoolean isActionResult = SafetyBoolean.TRUE;

        byte actionResult = 0;

        // start to do the items of self-test
        for (int i = 0; i < listensize; i++)
        {
            // do self-test items
            isActionResult = mSelftestListener.get(i).doAction();

            // If this test action result is failed , set self-test result is
            // false.
            actionResult = isActionResult.getByte();

            // if one of the self-test items is failed ,set isResultOK to false.
            if (byteSafetyFALSE == actionResult)
            {
                isResultOK = SafetyBoolean.FALSE;

                // if this test result is failed , write log to file.
                putLogtoFile(mEmwrID);
            }
            else
            {
                // Apply to the coding standard
            }
        }

        return isResultOK;
    }

    /**
     * Log the error of self-test result to file
     *
     * @param emwrID [in] the EMWR list's id
     *            Range: valid EMWRList object
     *            Unit: EMWRList
     *            Scaling: 1
     * 
     * @seem SafetyExtraString[in]
     *       logMap[out]
     *
     * @return None
     */
    private void putLogtoFile(final EMWRList emwrID)
    {
        // The byte value of SafetyBoolean FALSE
        final byte byteSafetyTRUE = SafetyBoolean.TRUE.getByte();

        String logString = "";

        final byte enableLogResult = mEnableLog.getByte();

        LogContent errorInfo = null;

        String strCodeId = null;

        String strdesc = null;

        String strExtra = null;

        StringBuffer strDescription = null;

        // Write error log to file
        if (byteSafetyTRUE == enableLogResult)
        {
            // Get log messages from EMWR list
            logString = emwrID.toString();
            strCodeId = String.valueOf(emwrID.getCodeId());
            strdesc = emwrID.getDesciption();

            if (mSafetyExtraString != null)
            {
                // create a string for EMWR's table.
                strDescription = new StringBuffer(strdesc);

                // Connect extra string.
                strExtra = mSafetyExtraString.getString();
                strDescription.append(strExtra);

                Debug.printI(TAG,
                        "strDescription : " + strDescription.toString());

                // Write error log to file
                // errorInfo = new LogTestError(strCodeId,
                // strDescription.toString());
                // LogFile.getInstance().log(errorInfo);

                // record into hash map
                mLogMap.put(emwrID, strDescription.toString());

                // release string buffer
                strDescription.delete(0, strDescription.length());
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
     * Allow log the error of self-test result to file
     *
     * @param enableFlag [in] enable log to file if set SafetyBoolean.TRUE
     *            Range: valid SafetyBoolean object
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @see mEnableLog[out]
     *
     * @return None
     */
    public void enableLogToFile(final SafetyBoolean enableFlag)
    {
        mEnableLog = enableFlag;
    }

    /**
     * Record the emwr's id of the assigned self-test item.
     *
     * @param emwrID [in] the EMWR list's id
     *            Range: valid EMWRList object
     *            Unit: EMWRList
     *            Scaling: 1
     * 
     * @param extraString [in] the extra error string
     *            Range: Valid String object
     *            Unit: String
     *            Scaling: 1
     * 
     * @see mEMWRID[out]
     *      mSafetyExtraString[out]
     * 
     * @return None
     */
    public final void logEmwrID(final EMWRList emwrID,
            final SafetyString extraString)
    {
        // Valid object
        CommonUtils.objectCheck(emwrID);

        mEmwrID = emwrID;
        mSafetyExtraString = extraString;
    }

    /**
     * Get the emwr's id of the assigned self-test item.
     * 
     * @see logMap[in]
     *      mEMWRID[in]
     * 
     * @return EMWRList [in] return the id of EMWRList
     *         Range: Valid EMWRList object
     *         Unit: EMWRList
     *         Scaling: 1
     */
    public final EMWRList getEmwrID()
    {
        final int size = mLogMap.size();

        EMWRList emwrid = mEmwrID;

        // There are more than one error in this self-test cycle.
        if (size > ERROR_NUMBER)
        {
            emwrid = DEFAULT_EMWRID;
        }
        else
        {
            emwrid = mEmwrID;
        }

        return emwrid;
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */