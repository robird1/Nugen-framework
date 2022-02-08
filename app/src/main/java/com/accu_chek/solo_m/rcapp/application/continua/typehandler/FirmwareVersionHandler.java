/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.FirmwareVersion
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: FirmwareVersionHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;

/**
 * This class is used to handle the command for getting the firmware version.
 */
public class FirmwareVersionHandler implements IContinuaCommandHandler
{
    /**
     * Get the firmware version of device and transfers it to Continua Agent.
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
        final String PREFIX = "V00.";
        final int LENGTH_OF_FW = 9;
        
        byte[] versionInBytes = new byte[LENGTH_OF_FW];
        
        try
        {        
            Context context = commandSet.getController().getContext();            
            PackageInfo info = context.getPackageManager().getPackageInfo(
                    context.getPackageName(), 0);
            String version = PREFIX + info.versionName;
            int lenghOfVersion = version.length();
            
            for (int i=0; i<lenghOfVersion; i++)
            {
                versionInBytes[i] = (byte) version.charAt(i);
            }
            
            for (int i=lenghOfVersion; i<LENGTH_OF_FW; i++)
            {
                versionInBytes[i] = 0;
            }
        }
        catch (NameNotFoundException exception)
        {
            exception.printStackTrace();
        } 
        finally
        {            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.FW_VERSION, ParseUtils.appendCRC(versionInBytes));
        }
    }
}
