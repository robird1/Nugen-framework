/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadConfigurationParameter
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: ReadConfigurationParameter.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.nio.ByteBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.IConfigurationParameterHandler.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

public class ReadConfigurationParameter implements IRPCCommandHandler
{
    /**
     * Delegate the command to the corresponding configuration parameter handler.
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
        try
        {
            ByteBuffer value = ByteBuffer.wrap(
                    invocation.getArguments().get(0).getValue().getByteArray());
            int parameterId = value.getShort();
            
            ConfigParameter.getParameterById(parameterId).getHandler().read(
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
