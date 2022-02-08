/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.EnterUpgradeMode
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 24608 $
 * $Author: JensonChin $
 * $Id: EnterUpgradeMode.java 24608 2015-11-23 07:13:00Z JensonChin $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.fwupdate.Error;
import com.accu_chek.solo_m.rcapp.application.fwupdate.FWUpdate;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class EnterUpgradeMode implements IRPCCommandHandler
{
    
    /**
     * Call firmware upgrade manager to enter upgrade mode.
     * Play communication completed sound after configuration completed.
     * If some exception appear, return application error to Agent.
     * Otherwise, return no error to Agent.
     *
     * @param commandSet :The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param invocation : The data arguments of request RPC command.
     *        Range: Valid object of RPCCommandInvocation.
     *        Unit: RPCCommandInvocation.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    @Override
    public void handle(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {
    	int response = RPCErrorResponse.RPC_ERR_NO_ERRORS;
    	
    	int result = new FWUpdate().enter();
    	
    	if (Error.ERR_OK != result)
    	{
    		response = RPCErrorResponse.RPC_ERR_HARDWARE;
    	}
    	else
    	{
    		CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
    	}
    	    	
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateErrorResponse(response));     
    }
    
}
