/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadInsulinIncrement
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22322 $
 * $Author: WilliyChiang $
 * $Id: ReadInsulinIncrement.java 22322 2015-10-22 08:10:30Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.FixPointFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ReadInsulinIncrement implements IRPCCommandHandler
{
    /**
     * Read the insulin increment from user settings model and return to Continua Agent.
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
        final int EXPONENT_UNIT = 2;
        
        RPCDataArguments argument = new RPCDataArguments();
        byte[] result = null;
        
        FixPointFloat increment = NugenSettingModel.getFixPoint(
                commandSet.getController().getContext(), UserSettingsKey.MDI_INSULIN_INCREMENT);
        SafetyFloat valueInFloat = null;
        
        CommonUtils.objectCheck(increment);
        
        valueInFloat = new SafetyFloat(increment.getOriginFloat(),
                String.valueOf(increment.getOriginFloat()));
        
        result = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(
                new SFloat(valueInFloat, EXPONENT_UNIT).getValue().get()));
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_SFLOAT);
        argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
        argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
        
        commandSet.getController().setSegmentDataOfRPC(RPCParseUtils.generateRPCResponse(
                RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }
}
