/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCCommand
 * Brief: 
 *
 * Create Date: 2015/6/11
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: IRPCCommandHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;

/**
 * This interface provides a function to handle the RPC command.
 */
public interface IRPCCommandHandler
{    
    /**
     * Delegate the sub command to the corresponding sub command handler.
     *
     * @param commandSet : The instance of Continua command set.
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
    void handle(ContinuaCommandSet commandSet, RPCCommandInvocation invocation);
}
