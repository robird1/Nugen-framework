/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.ResetDevice
 * Brief: 
 *
 * Create Date: 2015/6/11
 * $Revision: 23531 $
 * $Author: kevenwu $
 * $Id: ResetDevice.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.MainSoftwareReset;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

public class ResetDevice implements IRPCCommandHandler
{
    enum SubCommand
    {
        MAIN_SOFTWARE_RESET(0x0001, new MainSoftwareReset());

        /**
         * The command id of this enumeration.
         */
        private final int mCommandId;
        
        /**
         * The sub command handler of this enumeration.
         */
        private final IRPCCommandHandler mHandler;
        
        /**
         * Put the value of command id and IRPCCommandHandler to the instance.
         * 
         * @param commandId : The value of sub command id.
         *        Range: Refer to the definition of SubCommand.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param handler : The corresponding command handler.
         *        Range: Valid object of IRPCCommandHandler.
         *        Unit: IRPCCommandHandler.
         *        Scaling: 1.
         * 
         * see mCommandId [in]
         * see mHandler [in]
         */
        private SubCommand(int commandId, IRPCCommandHandler handler)
        {
            mCommandId = commandId;
            mHandler = handler;
        }
        
        /**
         * Return the command id of this enumeration.
         *
         * see mCommandId [in]
         *
         * return int [out]: 16 bit integer command id.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        public int getCommandId()
        {
            return mCommandId;
        }
        
        /**
         * Return the sub command handler of this enumeration.
         *
         * return IRPCCommandHandler [out]: The RPC sub command handler.
         *        Range: Valid object of ISubCommandHandler.
         *        Unit: ISubCommandHandler.
         *        Scaling: 1.
         */
        public IRPCCommandHandler getHandler()
        {
            return mHandler;
        }
        
        /**
         * Return the SubCommand enumeration according to the input command id.
         *
         * @param commandId : The request command id.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         * 
         * return SubCommand [out]: The corresponding enumeration of input id.
         *        Range: Valid object of SubCommand.
         *        Unit: SubCommand.
         *        Scaling: 1.
         * 
         * throws ArgumentErrorException if the command is not supported. 
         */
        public static SubCommand getSubCommandById(int commandId) 
                throws ArgumentErrorException
        {
            SubCommand result = null;
            
            for (SubCommand item : SubCommand.values())
            {
                int valueOfId = item.getCommandId();
                
                if (valueOfId == commandId)
                {
                    result = item;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException("This command \"" + commandId
                        + "\" is not supported.");
            }
            
            return result;
        }
    }
    
    /**
     * Delegate the request command to corresponding sub command handler.
     * If the command handler cannot be found, return unrecognized error code.
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
            SubCommand.getSubCommandById(invocation.getSubCommand().get())
                    .getHandler().handle(commandSet, invocation);
        }
        catch (ArgumentErrorException e)
        {
            e.printStackTrace();
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(
                            RPCErrorResponse.RPC_ERR_UNRECOGNIZED_CMD));
        }
        finally
        {
            // Do nothing.
        }
    }
}
