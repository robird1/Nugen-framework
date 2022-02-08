/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.PIDNumberHandler
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: PIDNumberHandler.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

/**
 * This class is used to handle the command for getting the PID number.
 */
public class PIDNumberHandler implements IContinuaCommandHandler
{
    /**
     * Get the PID from production model and transfers it to Continua Agent.
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
        SafetyString key = new SafetyString(ProductionConstants.KEY_ROCHE_PIDRC_PHDC,
                CRCTool.generateCRC16(ProductionConstants.KEY_ROCHE_PIDRC_PHDC.getBytes()));
        SafetyNumber<Integer> pid = NugenProductionModel.getInt(key);
        byte[] PIDInBytes = null;
        
        // Temporary used, wait for production module. 
        if (null == pid)
        {
            pid = new SafetyNumber<Integer>(0x21D6, -0x21D6);
        }
        
        PIDInBytes = ParseUtils.parseInt16(pid.get());
        
        commandSet.getController().setSegmentDataToAgent(ContinuaCommand.PID_NUMBER,
                ParseUtils.appendCRC(PIDInBytes));
    }
}
