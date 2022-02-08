/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.typehandler.ErrorMessageHandler
 * Brief: 
 *
 * Create Date: 2015/6/9
 * $Revision: 22210 $
 * $Author: kevenwu $
 * $Id: ErrorMessageHandler.java 22210 2015-10-21 08:41:27Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;

/**
 * This class is used to handle the error message from Continua Agent.
 */
public class ErrorMessageHandler implements IContinuaCommandHandler
{
    /**
     * Handle the error message from Continua Agent to show some error screen 
     * or another handling.
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
        final String PREFIX = "EMW";
        int errorId = commandSet.getConfigId();
        
        try
        {
        	NotifyProxy.showEMWR(new NotifyMessage(EMWRList.valueOf(PREFIX + errorId)));
        }
        catch (Exception e)
        {
        	e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.
        }
    }
}
