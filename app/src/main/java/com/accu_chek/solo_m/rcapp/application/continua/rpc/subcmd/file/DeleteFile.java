/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.DeleteFile
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: DeleteFile.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.file;

import java.io.File;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

public class DeleteFile implements IRPCCommandHandler
{
    /**
     * This function deletes the file which name is in the invocation from the system.
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
        SafetyByteArray response = new SafetyByteArray();
        RPCDataArguments argument = invocation.getArguments().get(0);
        String fileName = new String(argument.getValue().getByteArray());
        
        File file = new File(FileTransferHandler.FILE_PATH.concat(fileName));
        
        boolean isSuccess = file.delete();
        
        if (isSuccess)
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS);
        }
        else
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
        }
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }

}
