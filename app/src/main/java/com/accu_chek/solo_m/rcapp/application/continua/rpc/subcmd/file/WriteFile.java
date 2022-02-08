/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.WriteFile
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 21920 $
 * $Author: kevenwu $
 * $Id: WriteFile.java 21920 2015-10-19 08:31:28Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.file;

import java.io.File;
import java.nio.ByteBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

public class WriteFile implements IRPCCommandHandler
{	
	/**
	 * The file written mode of new file.
	 */
    public static final int RPC_WRITE_FILE_NEW = 0x0001;
    
    /**
     * The file written mode of overwrite exist file.
     */
    public static final int RPC_WRITE_FILE_OVERWRITE = 0x0002;
    
    /**
     * The file written mode of append the data to exist file.
     */
    public static final int RPC_WRITE_FILE_APPEND = 0x0003;
    
    /**
     * Verify the signature and check the status of received file name in the device.
     * If check success, return no error to Continua Manager and start transferring.
     * Otherwise, return the error response to Continua Manager.
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
        final int INDEX_OF_WRITE_MODE = 1;
        final int INDEX_OF_SIGNATURE = 2;
        
        SafetyByteArray response = null;
        
        RPCDataArguments argumentName = invocation.getArguments().get(0);
        RPCDataArguments argumentMode = invocation.getArguments().get(INDEX_OF_WRITE_MODE);
        RPCDataArguments argumentSignature = invocation.getArguments().get(INDEX_OF_SIGNATURE);
        
        String name = FileTransferHandler.FILE_PATH + new String(argumentName.getValue().getByteArray());
        int mode = ByteBuffer.wrap(argumentMode.getValue().getByteArray()).getShort();
        byte[] signature = argumentSignature.getValue().getByteArray();      
        
    	response = checkAndInitReceivedFile(name, mode, signature);
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
    
    /**
     * Check the file status and compare with write mode.
     * If the status is correct, start to transfer the file.
     * Otherwise, return the error response. 
     * 
     * @param fileName : The received file name.
     * 		  Range: Valid object of String.
     * 		  Unit: String.
     * 		  Scaling: 1.
     * @param mode : The write mode of target file.
     * 		  Range: -2^31 to (2^31)-1.
     * 		  Unit: Integer.
     * 		  Scaling: 1.  
     * 
     * return SafetyByteArray [out]: The response of check result. 
     * It's used to return to Continua Manager.
     * 		  Range: Valid object of SafetyByteArray.
     * 		  Unit: SafetyByteArray.
     * 		  Scaling: 1.
     */
    protected SafetyByteArray checkAndInitReceivedFile(String fileName, int mode, byte[] signature)
    {
    	SafetyByteArray result = null;
    	
    	File file = new File(fileName);
    	boolean isFileExist = file.exists();
        
        switch (mode)
        {
        case RPC_WRITE_FILE_NEW :
            if (isFileExist)
            {
            	result = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);   	
            }
            else
            {
            	// Apply coding standard.
            }
        	
        	break;
        case RPC_WRITE_FILE_OVERWRITE :
        	// Do nothing.        	
            
        	break;
        case RPC_WRITE_FILE_APPEND :
        	if (!isFileExist)
        	{
        		result = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
        	}
        	else
        	{
        		// Apply to coding standard.
        	}

        	break;
        default :
        	result = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_INVALID_PARAMETERS);
        	
        	break;
        }
        
        if (null == result)
        {
        	result = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS);
        	FileTransferHandler.getInstance().initReceiveFile(file, mode, signature);
        }
        
    	return result;
    }
}
