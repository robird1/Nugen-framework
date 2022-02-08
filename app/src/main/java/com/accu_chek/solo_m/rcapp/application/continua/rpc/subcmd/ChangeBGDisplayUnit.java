/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ChangeBGDisplayUnit
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 23531 $
 * $Author: kevenwu $
 * $Id: ChangeBGDisplayUnit.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.nio.ByteBuffer;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ChangeBGDisplayUnit implements IRPCCommandHandler
{
    /**
     * Write the input bG display unit to production model and return response.
     * Play communication completed sound after configuration.
     * If the input bG display unit can't be recognized, return unrecognized feature to Agent.
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
        final int MGDL = 0x0852;
        final int MMOL = 0x1272;
        
        SafetyByteArray response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS);
        RPCDataArguments argument = invocation.getArguments().get(0);
        
        int unit = ByteBuffer.wrap(argument.getValue().getByteArray()).getShort();
        
        if (MGDL == unit)
        {
            unit = HammingValues.HAMMING_HD4_VALUE_0004;
            NugenSettingModel.setInteger(commandSet.getController().getContext(),
                    UserSettingsKey.BG_DISPLAY_UNIT,
                    new SafetyNumber<Integer>(unit, -unit));
        }
        else if (MMOL == unit)
        {
            unit = HammingValues.HAMMING_HD4_VALUE_0003;
            NugenSettingModel.setInteger(commandSet.getController().getContext(),
                    UserSettingsKey.BG_DISPLAY_UNIT,
                    new SafetyNumber<Integer>(unit, -unit));
        }
        else
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_UNRECOGNIZED_FEATURE);
        }
        
        CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */