/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadFile
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22188 $
 * $Author: kevenwu $
 * $Id: ReadFile.java 22188 2015-10-21 07:39:44Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.file;

import java.io.File;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;

public class ReadFile implements IRPCCommandHandler
{
    /**
     * Check the status of required file and trigger the FileTransferHandler to transfer.
     * If the required file is not exist, return application error to Agent. 
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
    	String fileName = new String(argument.getValue().getByteArray());    	
    	File file = new File(FileTransferHandler.FILE_PATH.concat(fileName));
    	boolean isFileExist = file.exists();
    	    	
    	if (isFileExist)
    	{
    		FileTransferHandler.getInstance().startTransfer(commandSet, file);
    	}
    	else
    	{
    		commandSet.getController().setSegmentDataOfRPC(
    				RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION));
    	}
    }
}
