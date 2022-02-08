/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.BatteryLevel
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 22322 $
 * $Author: WilliyChiang $
 * $Id: BatteryLevelHandler.java 22322 2015-10-22 08:10:30Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import org.json.JSONException;
import org.json.JSONObject;

import android.content.Context;
import android.os.BatteryManager;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.PowerManagerConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;

/**
 * This class is used to handle the command for getting the battery level.
 */
public class BatteryLevelHandler implements IContinuaCommandHandler
{
    /**
     * This function gets the battery level of device from shared preference and transfers it to Continua Agent.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None
     */  
    @Override
    public void handleCommand(ContinuaCommandSet commandSet)
    {
        Context context = commandSet.getController().getContext();        
        SafetyString batteryInfo = NugenSettingModel.getString(context, 
                PowerManagerConstants.BATTERY_INFO);
        
        int level = 0;
        byte[] levelInBytes = null;
        
        try
        {
            JSONObject battery = new JSONObject(batteryInfo.getString());
            
            level = battery.getInt(BatteryManager.EXTRA_LEVEL);
        }
        catch (JSONException e)
        {
            // keep battery level in 0.
            e.printStackTrace();
        }
        finally
        {            
            levelInBytes = ParseUtils.parseInt16(level);
            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.BATTERY_LEVEL, ParseUtils.appendCRC(levelInBytes));
        }
    }
}
