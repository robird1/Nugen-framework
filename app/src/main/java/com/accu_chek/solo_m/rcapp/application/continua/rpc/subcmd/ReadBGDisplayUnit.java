/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadBGDisplayUnit
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22494 $
 * $Author: kevenwu $
 * $Id: ReadBGDisplayUnit.java 22494 2015-10-26 03:35:47Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;
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
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ReadBGDisplayUnit implements IRPCCommandHandler
{
    /**
     * To read the bG display unit from setting model and return to Agent.
     * The unit is converted to the Continua defined value.
     * If the unit cannot be recognized, return application error to Agent.
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
    @Override
    public void handle(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {
        final byte[] MGDL = {0x08, 0x52};
        final byte[] MMOL = {0x12, 0x72};
        
        RPCDataArguments argument = new RPCDataArguments();
        byte[] result = null;
        RPCEventType type = RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE;
        
        SafetyNumber<Integer> unit = NugenSettingModel.getInteger(                
                commandSet.getController().getContext(),
                UserSettingsKey.BG_DISPLAY_UNIT);
        
        CommonUtils.objectCheck(unit);
        
        if (HammingValues.HAMMING_HD4_VALUE_0004 == unit.get())
        {
            result = MGDL;
        }
        else if (HammingValues.HAMMING_HD4_VALUE_0003 == unit.get())
        {
            result = MMOL;
        }
        else
        {
            type = RPCEventType.MDC_NOTI_RPC_ERROR_RESPONSE;
            result = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(
                    RPCErrorResponse.RPC_ERR_APPLICATION));
        }        
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
        argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
        argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(type, argument));
    }
}
