/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.AbsoluteTime
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: AbsoluteTimeHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;

/**
 * This class is used to handle the command for getting the absolute time.
 */
public class AbsoluteTimeHandler implements IContinuaCommandHandler
{
    /**
     * Get the absolute time of device and transfers it to Continua Agent.
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
        byte[] currentTime = ParseUtils.parseAbsoluteTime(System.currentTimeMillis());
        
        commandSet.getController().setSegmentDataToAgent(
                ContinuaCommand.ABSOLUTE_TIME, ParseUtils.appendCRC(currentTime));
    }
}
