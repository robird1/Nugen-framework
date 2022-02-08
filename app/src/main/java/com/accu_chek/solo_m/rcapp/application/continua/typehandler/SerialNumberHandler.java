/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.SerialNumberHandler
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: SerialNumberHandler.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

/**
 * This class is used to handle the command for getting the serial number.
 */
public class SerialNumberHandler implements IContinuaCommandHandler
{
    /**
     * Get the serial number from production model and transfers it to Continua Agent.
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
        SafetyString key = new SafetyString(ProductionConstants.KEY_METER_SERIAL_NUMBER,
                CRCTool.generateCRC16(ProductionConstants.KEY_METER_SERIAL_NUMBER.getBytes()));
        SafetyString number = NugenProductionModel.getString(key);
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        char[] serialNumberInBytes = number.getString().toCharArray();
        
		buffer.append(serialNumberInBytes, 0, serialNumberInBytes.length);        
        
        commandSet.getController().setSegmentDataToAgent(ContinuaCommand.SERIAL_NUMBER,
                ParseUtils.appendCRC(buffer.toByteArray()));
    }
}
