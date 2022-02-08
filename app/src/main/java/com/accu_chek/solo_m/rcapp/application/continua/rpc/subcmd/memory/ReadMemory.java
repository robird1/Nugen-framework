/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadMemory
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: ReadMemory.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory;

import java.nio.ByteBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory.IMemoryOperationHandler.MemoryType;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

public class ReadMemory implements IRPCCommandHandler
{
    
    /**
     * Delegate the command to the corresponding sub handler according to input memory type.
     * If the memory type can't be recognized, return unrecognized feature response.
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
        RPCDataArguments argument = invocation.getArguments().get(0);
        ByteBuffer buffer = ByteBuffer.wrap(argument.getValue().getByteArray());
        
        try
        {
            MemoryType.getTypeById(buffer.getShort()).getHandler().read(
                    commandSet, invocation);
        }
        catch (ArgumentErrorException e)
        {
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(
                            RPCErrorResponse.RPC_ERR_UNRECOGNIZED_FEATURE));
            
            e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.
        }
    }
    
}
