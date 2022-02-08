/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.SSSInstrumentName
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 22827 $
 * $Author: kevenwu $
 * $Id: SSSInstrumentName.java 22827 2015-10-29 09:09:46Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.BGMConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;

public class SSSInstrumentName implements IConfigurationParameterHandler
{
    /**
     * Read the instrument name from general model and return to Agent.
     * If the value can't be found, return application error to Agent.
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
    public void read(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {
        SafetyString name = NugenGeneralModel.getString(commandSet.getController().getContext(), 
                BGMConstants.KEY_BG_INSTRUMENT);
        
        if (null != name)
        {
            byte[] nameInBytes = name.getString().getBytes();
            RPCDataArguments argument = new RPCDataArguments();
            
            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT8_ARRAY);
            argument.setLength(new SafetyNumber<Integer>(nameInBytes.length, -nameInBytes.length));
            argument.setValue(new SafetyByteArray(nameInBytes, CRCTool.generateCRC16(nameInBytes)));
            
            commandSet.getController().setSegmentDataOfRPC(RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
        }
        else
        {
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION));
        }
    }

    /**
     * Device doesn't support this action, it will return error response.
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
    public void change(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateErrorResponse(
                        RPCErrorResponse.RPC_ERR_UNRECOGNIZED_CMD));
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */