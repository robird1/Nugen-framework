/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.SoftwareVersion
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: SoftwareVersionHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;

/**
 * This class is used to handle the command for getting the software version.
 */
public class SoftwareVersionHandler implements IContinuaCommandHandler
{
    /**
     * Get the software version of measurement engine and return to Continua Agent.
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
        ByteArrayBuffer swVersion = new ByteArrayBuffer(0);
        
        String version = "V03.01.00";
        char[] versionInChars = null;
        
        versionInChars = version.toCharArray();         
        swVersion.append(versionInChars, 0, versionInChars.length);
        
        commandSet.getController().setSegmentDataToAgent(ContinuaCommand.SW_VERSION, 
                ParseUtils.appendCRC(swVersion.toByteArray()));
    }
}
