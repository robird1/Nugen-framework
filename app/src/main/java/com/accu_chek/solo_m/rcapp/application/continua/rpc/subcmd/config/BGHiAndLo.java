/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.BGHiAndLo
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: BGHiAndLo.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.nio.ByteBuffer;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
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

public class BGHiAndLo implements IConfigurationParameterHandler
{
    /**
     * Read the BG HI and LO value from production model and then transfer to Agent.
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
        final short HI_VALUE = 600;
        final int LENGTH_SHORT = 2;
        
        RPCDataArguments argument = new RPCDataArguments();
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        byte[] temp = null;               
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_DISPLAY_LEVEL_LO,
                CRCTool.generateCRC16(ProductionConstants.KEY_DISPLAY_LEVEL_LO.getBytes()));
        SafetyNumber<Integer> loValue = NugenProductionModel.getInt(key);
        
        temp = ByteBuffer.allocate(LENGTH_SHORT).putShort(loValue.get().shortValue()).array();
        buffer.append(temp, 0, temp.length);
        
        temp = ByteBuffer.allocate(LENGTH_SHORT).putShort(HI_VALUE).array();
        buffer.append(temp, 0, temp.length);
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16_ARRAY);
        argument.setLength(new SafetyNumber<Integer>(buffer.length(), -buffer.length()));
        argument.setValue(new SafetyByteArray(buffer.toByteArray(), 
                CRCTool.generateCRC16(buffer.toByteArray())));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }

    /**
     * Write the input BG HI and LO value to production model and return response.
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
        ByteBuffer buffer = ByteBuffer.wrap(argument.getValue().getByteArray());
        
        int low = buffer.getShort();
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_DISPLAY_LEVEL_LO,
                CRCTool.generateCRC16(ProductionConstants.KEY_DISPLAY_LEVEL_LO.getBytes()));
        NugenProductionModel.setInt(key, new SafetyNumber<Integer>(low, -low));
        
        CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS));
    }
}
