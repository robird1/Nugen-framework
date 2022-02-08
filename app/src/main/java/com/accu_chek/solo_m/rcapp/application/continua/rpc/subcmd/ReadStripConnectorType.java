/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.
 * ReadStripConnectorType
 * Brief:
 * 
 * Create Date: 2015/7/10
 * $Revision: 23946 $
 * $Author: IvanHuang $
 * $Id: ReadStripConnectorType.java 23946 2015-11-12 06:43:33Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import android.content.Context;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.BGMConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
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
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

public class ReadStripConnectorType implements IRPCCommandHandler
{
    /**
     * Read strip connector type from general model and transfer to Agent.
     * If the value can't be found, return application error to Agent.
     * 
     * @param commandSet :The instance of Continua command set.
     *            Range: Valid object of ContinuaCommandSet.
     *            Unit: ContinuaCommandSet.1
     *            Scaling: 1.
     * @param invocation : The data arguments of request RPC command.
     *            Range: Valid object of RPCCommandInvocation.
     *            Unit: RPCCommandInvocation.
     *            Scaling: 1.
     * 
     *            return void [out]: None
     */
    @Override
    public void handle(ContinuaCommandSet commandSet,
            RPCCommandInvocation invocation)
    {
        final int HEX = 16;
        final int MASK_OF_TYPE = 0x0010;
        final byte[] PERFORMA = { 0x00, 0x0F };
        final byte[] AVIVA = { 0x00, 0x33 };
        final byte[] NO_BG = { 0x00, 0x3C };

        SafetyByteArray response = null;
        RPCDataArguments argument = new RPCDataArguments();
        Context context = commandSet.getController().getContext();
        SafetyString keySysConfig = new SafetyString(
                ProductionConstants.KEY_SYSTEM_CONFIGURATION,
                CRCTool.generateCRC16(ProductionConstants.KEY_SYSTEM_CONFIGURATION
                        .getBytes()));
        SafetyNumber<Integer> meterMode = NugenProductionModel
                .getInt(keySysConfig);
        SafetyString type = NugenGeneralModel.getString(context,
                BGMConstants.KEY_BG_STRIP_CONNECTOR);

        if (HammingDistance.SAFETY_NUMBER_VALUE_0002 == meterMode.get())
        {
            byte[] result = NO_BG;

            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
            argument.setLength(new SafetyNumber<Integer>(result.length,
                    -result.length));
            argument.setValue(new SafetyByteArray(result, CRCTool
                    .generateCRC16(result)));

            response = RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
        }
        else if (null != type)
        {
            byte[] result = null;
            int connector = Integer.parseInt(type.getString(), HEX)
                    & MASK_OF_TYPE;

            if (MASK_OF_TYPE == connector)
            {
                result = AVIVA;
            }
            else
            {
                result = PERFORMA;
            }

            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
            argument.setLength(new SafetyNumber<Integer>(result.length,
                    -result.length));
            argument.setValue(new SafetyByteArray(result, CRCTool
                    .generateCRC16(result)));

            response = RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
        }
        else
        {
            response = RPCParseUtils
                    .generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
        }

        commandSet.getController().setSegmentDataOfRPC(response);
    }
}
