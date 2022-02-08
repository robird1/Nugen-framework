/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadInternalCompInfo
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 21737 $
 * $Author: kevenwu $
 * $Id: ReadInternalCompInfo.java 21737 2015-10-16 10:48:49Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.VersionInfoData;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.VersionInfoData.SpecType;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;

public class ReadInternalCompInfo implements IRPCCommandHandler
{
    /**
     * Read the version of measurement engine from general model and return to Agent.
     * If version can't be found, return application error to Agent.
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
    	SafetyByteArray result = null;
    	SafetyString version = 
    			NugenGeneralModel.getString(commandSet.getController().getContext(), 
    			NugenFrameworkConstants.BGMConstants.KEY_BG_VERSION);
    	    	
    	if (null != version)
    	{
    		RPCDataArguments argument = new RPCDataArguments();
    		VersionInfoData data = new VersionInfoData();
			byte[] versionInfo = null;
    		
			data.setVersionInfo(SpecType.SW_REVISION, 0, version.getString().getBytes());
			versionInfo = data.generateBytes().getByteArray();
			
    		argument.setType(RPCArgumentType.RPC_ARG_TYPE_VERSION_INFO);
    		argument.setLength(new SafetyNumber<Integer>(
    				versionInfo.length, -versionInfo.length));
    		argument.setValue(new SafetyByteArray(
    				versionInfo, CRCTool.generateCRC16(versionInfo)));
    		
    		result = RPCParseUtils.generateRPCResponse(
    				RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
    	}
    	else
    	{
    		result = RPCParseUtils.generateErrorResponse(
    				RPCErrorResponse.RPC_ERR_APPLICATION);
    	}
    	
    	commandSet.getController().setSegmentDataOfRPC(result);
    }
}
