/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadBolusAdviceStatus
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22322 $
 * $Author: WilliyChiang $
 * $Id: ReadBolusAdviceStatus.java 22322 2015-10-22 08:10:30Z WilliyChiang $
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
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ReadBolusAdviceStatus implements IRPCCommandHandler
{
    /**
     * Read bolus advice status from setting model and return to Continua Agent.
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
        final int ENABLE = HammingDistance.SAFETY_NUMBER_VALUE_0050;
        final int DISABLE = HammingDistance.SAFETY_NUMBER_VALUE_0051;
        
        final byte[] ENABLE_BYTES = new byte[]{0x00, 0x01};
        final byte[] DISABLE_BYTES = new byte[]{0x00, 0x00};
        
        byte[] result = ENABLE_BYTES;
        RPCDataArguments argument = new RPCDataArguments();
        RPCArgumentType type = RPCArgumentType.RPC_ARG_TYPE_OPERATIONAL_STATE;
        RPCEventType event = RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE;
        
        SafetyNumber<Integer> status = NugenSettingModel.getInteger(
                commandSet.getController().getContext(), UserSettingsKey.BOLUS_ADVICE_STATUS);

        CommonUtils.objectCheck(status);
        
        if (ENABLE == status.get())
        {
            result = ENABLE_BYTES;
        }
        else if (DISABLE == status.get())
        {
            result = DISABLE_BYTES;
        }
        else
        {
            type = RPCArgumentType.RPC_ARG_TYPE_UINT16;
            event = RPCEventType.MDC_NOTI_RPC_ERROR_RESPONSE;
            result = ParseUtils.makeLittleEndian(
                    ParseUtils.parseInt16(RPCErrorResponse.RPC_ERR_APPLICATION));
        }
        
        argument.setType(type);
        argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
        argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(event, argument));
    }
}
