/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.ChangeSetup
 * Brief: 
 *
 * Create Date: 2015/6/11
 * $Revision: 22322 $
 * $Author: WilliyChiang $
 * $Id: ChangeMaxBolusThreshold.java 22322 2015-10-22 08:10:30Z WilliyChiang $
 */
package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.nio.ByteBuffer;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.FixPointFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ChangeMaxBolusThreshold implements IRPCCommandHandler
{
    /**
     * Change the max Bolus threshold of device setting by NugenSettingModel.
     * After change complete, play communication complete sound and return "no error" response.
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
        Context context = commandSet.getController().getContext();
        RPCDataArguments argument = invocation.getArguments().get(0);
        
        ByteBuffer buffer = ByteBuffer.wrap(argument.getValue().getByteArray());
        byte first = buffer.get();
        byte second = buffer.get();
        SafetyFloat threshold = new SFloat(first, second).toDouble();
        FixPointFloat value = new FixPointFloat(threshold.getOriginal(), threshold.getDiverse());
        
        NugenSettingModel.setInteger(context, UserSettingsKey.MDI_MAX_BOLUS_AMOUNT, 
                value.getSafetyNumber());
        NugenSettingModel.setInteger(context, UserSettingsKey.USER_BOLUS_THRESHOLD_MAX, 
                value.getSafetyNumber());
                
        CommonUtils.playSound(RPCConstants.SOUND_PATH, context);
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS));
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */