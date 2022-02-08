/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadMaxBolusThreshold
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22322 $
 * $Author: WilliyChiang $
 * $Id: ReadMaxBolusThreshold.java 22322 2015-10-22 08:10:30Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.nio.ByteBuffer;

import android.content.Context;

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
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ReadMaxBolusThreshold implements IRPCCommandHandler
{
    /**
     * Read the flag of MDI mode enable from setting model.
     * If MDI is enable, return MDI max threshold to Continua Agent.
     * Otherwise, return user max threshold to Continua Agent.
     * If MDI status value can't be recognized, return application error to Continua Agent.
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
        final int OFF = HammingDistance.SAFETY_NUMBER_VALUE_0048;
        final int ON = HammingDistance.SAFETY_NUMBER_VALUE_0049;
        final int SCALING = 100;
        final int EXPONENT = 2;
        final int BYTES_SHORT = 2;

        byte[] result = null;
        RPCEventType type = RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE;
        RPCDataArguments argument = new RPCDataArguments();
        SafetyNumber<Integer> threshold = new SafetyNumber<Integer>(0, 0);
        Context context = commandSet.getController().getContext();
        float floatThreshold = 0f;
        
        SafetyNumber<Integer> isMDIEnable = NugenSettingModel.getInteger(context, UserSettingsKey.MDI_MODE);
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_SFLOAT);
        
        CommonUtils.objectCheck(isMDIEnable);
        
        if (ON == isMDIEnable.get())
        {
            threshold = NugenSettingModel.getInteger(context, UserSettingsKey.MDI_MAX_BOLUS_AMOUNT);
        
            CommonUtils.objectCheck(threshold);
            
            floatThreshold = threshold.get() / SCALING;        
            result = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(new SFloat(new SafetyFloat(
                    floatThreshold, String.valueOf(floatThreshold)), EXPONENT).getValue().get()));
        }
        else if (OFF == isMDIEnable.get())
        {
            threshold = NugenSettingModel.getInteger(context, UserSettingsKey.USER_BOLUS_THRESHOLD_MAX);
            
            CommonUtils.objectCheck(threshold);
            
            floatThreshold = threshold.get() / SCALING;        
            result = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(new SFloat(new SafetyFloat(
                    floatThreshold, String.valueOf(floatThreshold)), EXPONENT).getValue().get()));
        }
        else
        {
            type = RPCEventType.MDC_NOTI_RPC_ERROR_RESPONSE;
            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
            result = ByteBuffer.allocate(BYTES_SHORT).putShort(
                    (short) RPCErrorResponse.RPC_ERR_APPLICATION).array();
        }                
                
        argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
        argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(type, argument));
    }
}
