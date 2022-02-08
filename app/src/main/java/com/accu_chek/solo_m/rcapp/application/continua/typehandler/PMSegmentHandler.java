/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.typehandler.PMSegmentHandler
 * Brief: 
 *
 * Create Date: 2015/3/30
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: PMSegmentHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.ErrorCode;
import com.accu_chek.solo_m.rcapp.application.continua.confighandler.IContinuaConfigHandler.ConfigureId;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

/**
 * This class is used to handle the command for getting the segment entry.
 */
public class PMSegmentHandler implements IContinuaCommandHandler
{
    /**
     * Query the database for the segment data depend on configuration and segment
     * id, and then transfers it to Continua Agent.
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
        try
        {
            ConfigureId.getConfigureId(commandSet.getConfigId()).getHandler()
                    .getData(commandSet);
        }
        catch (ArgumentErrorException e)
        {
            byte[] error = ParseUtils.parseInt16(ErrorCode.COMMAND_NOT_SUPPORTED);
            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.PM_SEGMENT_ENTRY, ParseUtils.appendCRC(error));
            
            e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.
        }
    }
}
