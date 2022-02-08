/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * Class name: ProductionSetting
 * com.accu_chek.solo_m.rcapp.application.production
 * .ProducitonSetting
 * Brief:
 * Create Date: 08/05/2015
 * $Revision: 21343 $
 * $Author: VictorChen $
 * $Id: ProductionSetting.java 21343 2015-10-12 12:09:54Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.production;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;

public class ProductionSetting
{

    private static final String PERSIST_SYS_SYSTEM_MODE = "persist.sys.system.mode";
    
    private static final String PRODUCTION = "production";
    
    private static final String TAG = "ProducitonSetting";
    
    private static final int CUSTOMER = 0x0F;

    /**
     * Set meter enter production mode.
     * see PERSIST_SYS_SYSTEM_MODE[in]
     * see PRODUCTION[in]
     * 
     */

    public static void setProductionMode()
    {
        final IRCSystemPeoperty bpSetProperty = CustJavaFrameworkManager.getRCSystemPropertyService(null);
        CommonUtils.objectCheck(bpSetProperty);
        try
        {
            bpSetProperty.setProperty(PERSIST_SYS_SYSTEM_MODE, PRODUCTION);
            //TODO
            //call reboot.
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis.
        }
    }

    /**
     * Check the meter boot mode.
     * 
     * see PERSIST_SYS_SYSTEM_MODE[in]
     * see PRODUCTION[in]
     * 
     * @return SafetyBoolean
     *         Range: SafetyBoolean.TRUE, SafetyBoolean.FALSE
     *         Unit: SafetyBoolean
     *         Scaling:1
     */
    public static SafetyBoolean isProductionMode()
    {
        // Check meterAccessLevelDefault
        // CUSTOMER=0x0F, PRODUCTION=0x33, INTERNAL=0x3C
        // PRODUCTION, return true.

        final SafetyString meterAccrssLevel = new SafetyString(
                ConfigParameter.KEY_METER_ACCESS_LEVEL,
                CRCTool.generateCRC16(ConfigParameter.KEY_METER_ACCESS_LEVEL
                        .getBytes()));
        boolean isProductionMode = false;
        int meterLevel = ReadConfig.getIntegerDataByKey(meterAccrssLevel).get();
        String mode = "";
        SafetyBoolean sfResult = SafetyBoolean.FALSE;
        // FIXME
        // set meter level is CUSTOMER.
        meterLevel = 15;
        // check "persist.sys.system.mode"
        final IRCSystemPeoperty bpSetProperty = CustJavaFrameworkManager.getRCSystemPropertyService(null);
        CommonUtils.objectCheck(bpSetProperty);

        try
        {
            mode = bpSetProperty.getProperty(PERSIST_SYS_SYSTEM_MODE, "");
            Debug.printI(TAG, "mode= " + mode);

            // get PERSIST_SYS_SYSTEM_MODE flag and meterAccessLevelDefault
            // flag.
            isProductionMode = (mode.equals(PRODUCTION))
                    || (meterLevel != CUSTOMER);
            if (isProductionMode)
            {
                sfResult = SafetyBoolean.TRUE;
            }
            else
            {
                sfResult = SafetyBoolean.FALSE;
            }
            bpSetProperty.setProperty(PERSIST_SYS_SYSTEM_MODE, "");
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis.
        }
        return sfResult;
    }

 
}
// [NISQ-20] Remove the input argument "context" from
// "CustJavaFrameworkManager.getPCConnectService()".
// (R21031 2015-10-06 02:11:39 VictorChen)
// ----------------------------------------------------------------------------
// change ProductionSetting to access property.
