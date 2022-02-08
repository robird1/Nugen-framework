/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.ManufacturingProcessFlags
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: ManufacturingProcessFlags.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.nio.ByteBuffer;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants;
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
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ManufacturingProcessFlags implements IConfigurationParameterHandler
{
    /**
     * Read the manufacturing process flags from production model and return to 
     * the Continua Agent.
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
        byte[] result = null;
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_METER_ACCESS_LEVEL_DEFAULT,
                CRCTool.generateCRC16(ProductionConstants.KEY_METER_ACCESS_LEVEL_DEFAULT.getBytes()));
        SafetyNumber<Integer> flag = NugenProductionModel.getInt(key);
        
        result = ParseUtils.parseInt16(flag.get());
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
        argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
        argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }

    /**
     * Change the manufacturing flags in production model and return the response.
     * Play communication completed sound after configuration.
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
        final int INDEX_OF_DATA = 1;
        
        RPCDataArguments argument = invocation.getArguments().get(INDEX_OF_DATA);
        
        int flag = ByteBuffer.wrap(argument.getValue().getByteArray()).getShort();
                
        SafetyString key = new SafetyString(ProductionConstants.KEY_METER_ACCESS_LEVEL_DEFAULT,
                CRCTool.generateCRC16(ProductionConstants.KEY_METER_ACCESS_LEVEL_DEFAULT.getBytes()));
        NugenProductionModel.setInt(key, new SafetyNumber<Integer>(flag, -flag));
        
        CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
        
        commandSet.getController().setSegmentDataOfRPC( 
                RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS));
    }
}
