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
 * $Revision: 23531 $
 * $Author: kevenwu $
 * $Id: ChangeCarbUnits.java 23531 2015-11-06 09:01:33Z kevenwu $
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
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ChangeCarbUnits implements IRPCCommandHandler
{
    enum CarbUnits
    {
        GRAM(0x0000, HammingDistance.SAFETY_NUMBER_VALUE_0020),
        BE(0x0001, HammingDistance.SAFETY_NUMBER_VALUE_0021),
        KE(0x0005, HammingDistance.SAFETY_NUMBER_VALUE_0022),
        CC(0x0004, HammingDistance.SAFETY_NUMBER_VALUE_0023);
        
        /**
         * The value which is defined in continua.
         */
        public final int CONTINUA_VALUE;
        
        /**
         * The value which is defined in configuration matrix.
         */
        public final int DEVICE_VALUE;
        
        /**
         * Put the Continua defined value and device defined value into each item.
         * 
         * @param continuaValue : The carbohydrate units which defined in Continua.
         *        Range: Refer to the definition of Continua specification.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param deviceValue : The carbohydrate units which defined in configuration matrix.
         *        Range: Refer to the definition of configuration matrix.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        private CarbUnits(int continuaValue, int deviceValue)
        {
            CONTINUA_VALUE = continuaValue;
            DEVICE_VALUE = deviceValue;
        }
        
        /**
         * Find the CarbUnits by input Continua defined value.
         *
         * @param value : The value defined in Continua.
         *        Range : -2^31 to (2^31)-1.
         *        Unit : Integer.
         *        Scaling : 1.
         *  
         * return CarbUnits [out]: The corresponding CarbUnits, null if the value is not supported.
         *        Range: Null or valid object of CarbUnits.
         *        Unit: CarbUnits.
         *        Scaling: 1.
         */
        public static CarbUnits getUnitByContinuaValue(int value)
        {
           CarbUnits result = null;
           
           for (CarbUnits each : CarbUnits.values())
           {
               if (each.CONTINUA_VALUE == value)
               {
                   result = each;
               }
           }
           
           return result;
        }
    }
    
    /**
     * Change the carbohydrate unit of device setting and return the response.
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
    public void handle(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {   
		SafetyByteArray response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS);
        Context context = commandSet.getController().getContext();
        RPCDataArguments argument = invocation.getArguments().get(0);
        
        CarbUnits unit = CarbUnits.getUnitByContinuaValue(
                ByteBuffer.wrap(argument.getValue().getByteArray()).getShort());
				
		if (null != unit)
		{
			SafetyNumber<Integer> carbUnit = new SafetyNumber<Integer>(unit.DEVICE_VALUE, -unit.DEVICE_VALUE);
        
			NugenSettingModel.setInteger(context, UserSettingsKey.CARB_UNIT_TYPE, carbUnit);
                
			CommonUtils.playSound(RPCConstants.SOUND_PATH, context);	
		}			
        else
		{
			response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_UNRECOGNIZED_FEATURE);
		}
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */