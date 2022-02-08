/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadTimeFormat
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22494 $
 * $Author: kevenwu $
 * $Id: ReadTimeFormat.java 22494 2015-10-26 03:35:47Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;

public class ReadTimeFormat implements IRPCCommandHandler
{

    /**
     * To read the time format from setting model and return to Agent.
     * If the format can't be recognized, return application error to Agent.
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
        final int FORMAT_24H = 0x0001;
        final int FORMAT_12H = 0x0002;
        
        byte[] result = null;
        RPCDataArguments argument = new RPCDataArguments();
        RPCEventType eventType = RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE;
        
        SafetyNumber<Integer> format = NugenSettingModel.getInteger(
                commandSet.getController().getContext(), 
                UserSettingsKey.TIME_FORMAT);
        int valueOfFormat = format.get();
        
        if (HammingDistance.SAFETY_NUMBER_VALUE_0046 == valueOfFormat)
        {
            result = ParseUtils.parseInt16(FORMAT_12H);
        }
        else if (HammingDistance.SAFETY_NUMBER_VALUE_0047 == valueOfFormat)
        {
            result = ParseUtils.parseInt16(FORMAT_24H);
        }
        else
        {
            eventType = RPCEventType.MDC_NOTI_RPC_ERROR_RESPONSE;
            result = ParseUtils.parseInt16(RPCErrorResponse.RPC_ERR_APPLICATION);
        }
        
        result = ParseUtils.makeLittleEndian(result);

        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
        argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
        argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(eventType, argument));
    }

}
