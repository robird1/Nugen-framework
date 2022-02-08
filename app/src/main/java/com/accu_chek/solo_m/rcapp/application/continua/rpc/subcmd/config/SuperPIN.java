/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.SuperPIN
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: SuperPIN.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
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
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

public class SuperPIN implements IConfigurationParameterHandler
{
    /**
     * Read super PIN from production model and transfer to Agent.
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
        RPCDataArguments argument = invocation.getArguments().get(0);
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_SECURITY_LOCK_SUPER_PIN,
                CRCTool.generateCRC16(ProductionConstants.KEY_SECURITY_LOCK_SUPER_PIN.getBytes()));
        String pin = NugenProductionModel.getString(key).getString();

        for (char each : pin.toCharArray())
        {
            buffer.append(Integer.parseInt(String.valueOf(each)));
        }
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT8_ARRAY);
        argument.setLength(new SafetyNumber<Integer>(buffer.length(), -buffer.length()));
        argument.setValue(new SafetyByteArray(buffer.toByteArray(), 
                CRCTool.generateCRC16(buffer.toByteArray())));
        
        commandSet.getController().setSegmentDataOfRPC( 
                RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }

    /**
     * Device doesn't support this command, so the error response will be return.
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