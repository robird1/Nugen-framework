/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.SupportedLanguages
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: SupportedLanguages.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.util.LinkedList;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCLanguage;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class SupportedLanguages implements IConfigurationParameterHandler
{    
    /**
     * Read supported languages from production model and transfer to Agent.
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
        List<RPCLanguage> supported = RPCLanguage.getSupportedLanguages(
                commandSet.getController().getContext());
        
        RPCDataArguments argument = new RPCDataArguments();        
        ByteArrayBuffer result = new ByteArrayBuffer(0);
        
        for (RPCLanguage item : supported)
        {
            result.append(item.CODE);
        }
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT8_ARRAY);
        argument.setLength(new SafetyNumber<Integer>(result.length(), 
                -result.length()));
        argument.setValue(new SafetyByteArray(result.toByteArray(), 
                CRCTool.generateCRC16(result.toByteArray())));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }

    /**
     * Write the supported languages which from RPC command to production model.
     * Play communication completed sound after configuration.
     * If the value from invocation can't be recognized, return unrecognized feature to Agent.
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
        final int INDEX_OF_LANGUAGES = 1;
                
        Context context = commandSet.getController().getContext();
        RPCDataArguments argument = invocation.getArguments().get(INDEX_OF_LANGUAGES);
        int response = RPCErrorResponse.RPC_ERR_NO_ERRORS;
        List<RPCLanguage> supported = RPCLanguage.getSupportedLanguages(context);
                                       
        try
        {
            List<RPCLanguage> supportedNew = new LinkedList<RPCLanguage>();
            
            // retrieve the new supported languages.
            for (byte item : argument.getValue().getByteArray())
            {                
                RPCLanguage language = RPCLanguage.getLanguageById(item);                
                supportedNew.add(language);
            }

            // reset the original supported languages.
            for (RPCLanguage each : supported)
            {
                SafetyString key = new SafetyString(each.KEY, 
                        CRCTool.generateCRC16(each.KEY.getBytes()));
                SafetyNumber<Integer> no = new SafetyNumber<Integer>(
                        HammingDistance.SAFETY_NUMBER_VALUE_0079, -HammingDistance.SAFETY_NUMBER_VALUE_0079);
                NugenProductionModel.setInt(key, no);;
            }
            
            // set the new supported languages.
            for (RPCLanguage item : supportedNew)
            {   
                SafetyString key = new SafetyString(item.KEY, 
                        CRCTool.generateCRC16(item.KEY.getBytes()));
                SafetyNumber<Integer> yes = new SafetyNumber<Integer>(
                        HammingDistance.SAFETY_NUMBER_VALUE_0080, -HammingDistance.SAFETY_NUMBER_VALUE_0080);
                NugenProductionModel.setInt(key, yes);
            }
            
            CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
        }
        catch (ArgumentErrorException exception)
        {            
            exception.printStackTrace();
            
            response = RPCErrorResponse.RPC_ERR_UNRECOGNIZED_FEATURE;
        }
        finally
        {
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(response));
        }
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R23531 2015-11-06 05:01:33 kevenwu)
// ----------------------------------------------------------------------------
// Refine for production parameter.
