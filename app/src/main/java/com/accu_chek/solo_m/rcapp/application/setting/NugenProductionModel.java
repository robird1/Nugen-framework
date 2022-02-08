/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.setting.
 * NugenProductionModel
 * Brief:
 *
 * Create Date: 2015¦~7¤ë27¤é
 * $Revision: 23880 $
 * $Author: VictorChen $
 * $Id: NugenProductionModel.java 23880 2015-11-11 09:57:46Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.setting;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrixWriteBody;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class NugenProductionModel
{
    @Deprecated
    public static void setString(Context context, String key, SafetyString value)
    {
        CommonUtils.objectCheck(context, key, value);

        setString(new SafetyString(key, CRCTool.generateCRC16(key.getBytes())),
                value);
    }

    public static void setString(SafetyString key, SafetyString value)
    {
        CommonUtils.objectCheck(key, value);
        MeterParameterMatrixWriteBody.getInstance().setString(key, value);
    }

    @Deprecated
    public static SafetyString getString(Context context, String key)
    {
        CommonUtils.objectCheck(context, key);

        SafetyString result = getString(new SafetyString(key,
                CRCTool.generateCRC16(key.getBytes())));
        return result;
    }

    public static SafetyString getString(SafetyString key)
    {
        CommonUtils.objectCheck(key);

        SafetyString result = ReadConfig.getStringDataByKey(key);
        return result;
    }

    @Deprecated
    public static void setInt(Context context, String key,
            SafetyNumber<Integer> value)
    {
        setInt(new SafetyString(key, CRCTool.generateCRC16(key.getBytes())),
                value);
    }

    public static void setInt(SafetyString key, SafetyNumber<Integer> value)
    {
        CommonUtils.objectCheck(key, value);
        MeterParameterMatrixWriteBody.getInstance().setInteger(key, value);
    }

    @Deprecated
    public static SafetyNumber<Integer> getInt(Context context, String key)
    {
        CommonUtils.objectCheck(context, key);
        SafetyNumber<Integer> result = getInt(new SafetyString(key,
                CRCTool.generateCRC16(key.getBytes())));

        return result;
    }

    public static SafetyNumber<Integer> getInt(SafetyString key)
    {
        CommonUtils.objectCheck(key);

        SafetyNumber<Integer> result = ReadConfig.getIntegerDataByKey(key);

        return result;
    }

    public static void setLong(SafetyString key, SafetyNumber<Long> value)
    {
        CommonUtils.objectCheck(key, value);
        MeterParameterMatrixWriteBody.getInstance().setLong(key, value);
    }

    public static SafetyNumber<Long> getLong(Context context, String key)
    {
        CommonUtils.objectCheck(context, key);
        // get CM
        SafetyNumber<Long> result = ReadConfig
                .getLongDataByKey(new SafetyString(key, CRCTool
                        .generateCRC16(key.getBytes())));
        return result;

    }

}
// (R20551 2015-10-01 09:43:53 DWYang)
// ----------------------------------------------------------------------------
// change ProductionSetting to access property.
// (R21349 2015-10-12 08:40:04 VictorChen)
// ----------------------------------------------------------------------------
// Refine function for setting api
// (R22323 2015-10-22 04:19:27 VictorChen)
// ----------------------------------------------------------------------------
// Refine production model to access CM.XML.
