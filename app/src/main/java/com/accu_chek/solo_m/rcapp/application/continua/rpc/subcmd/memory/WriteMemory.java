/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.WriteMemory
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: WriteMemory.java 20933 2015-10-05 08:34:57Z kevenwu $
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

public class WriteMemory implements IRPCCommandHandler
{    
    /**
     * Write the data to the corresponding position according to the command.
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
            MemoryType.getTypeById(buffer.getShort()).getHandler().write(
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
