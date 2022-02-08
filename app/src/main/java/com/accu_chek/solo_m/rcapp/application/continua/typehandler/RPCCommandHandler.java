/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.typehandler.RPCCommandHandler
 * Brief: 
 *
 * Create Date: 2015/7/28
 * $Revision: 23125 $
 * $Author: kevenwu $
 * $Id: RPCCommandHandler.java 23125 2015-11-03 11:16:42Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import java.nio.BufferUnderflowException;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCCommand;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

public class RPCCommandHandler implements IContinuaCommandHandler
{
    /**
     * Delegate the request RPC command to corresponding handler according to the input command.
     * If the command can't be found, return the unrecognized response to Agent.
     * If the input data is invalid, return the invalid data response to Agent.
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
            RPCCommandInvocation invocation = 
                    RPCCommandInvocation.parse(commandSet.getDataOfCommand());
                    
            RPCCommand.getRPCCommandById(invocation.getCommand().get())
                    .getHandler().handle(commandSet, invocation);
        }
        catch (ArgumentErrorException e)
        {
            e.printStackTrace();
            
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_UNRECOGNIZED_CMD));
        }
        catch (BufferUnderflowException e)
        {
            e.printStackTrace();
            
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_INVALID_DATA));
        }
        finally
        {
            // Do nothing.
        }
    }
}
