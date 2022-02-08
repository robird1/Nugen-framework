/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.SelectedLanguage
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: SelectedLanguage.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.nio.ByteBuffer;
import java.util.Arrays;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
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
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;
import com.accu_chek.solo_m.rcapp.application.setting.generalsetting.IPCUpdateConfig;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class SelectedLanguage implements IConfigurationParameterHandler
{
    /**
     * Read user selected language from production model and transfer to Agent.
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
        SafetyByteArray result = null;
        RPCDataArguments argument = new RPCDataArguments();
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_USER_SELECT_LANGUAGE_DEFAULT,
                CRCTool.generateCRC16(ProductionConstants.KEY_USER_SELECT_LANGUAGE_DEFAULT.getBytes()));
        SafetyString keyOfSelected = NugenProductionModel.getString(key);
        
        try
        {
            byte[] value = null;
            RPCLanguage selected = 
                    RPCLanguage.getLanguageByKey(keyOfSelected.getString());
                        
            value = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(selected.CODE));
            
            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
            argument.setLength(new SafetyNumber<Integer>(value.length, -value.length));
            argument.setValue(new SafetyByteArray(value, CRCTool.generateCRC16(value)));
            
            result = RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
        }
        catch (ArgumentErrorException e)
        {
            result = RPCParseUtils.generateErrorResponse(
                    RPCErrorResponse.RPC_ERR_UNRECOGNIZED_FEATURE);
            e.printStackTrace();
        }
        catch (NullPointerException e)
        {
            result = RPCParseUtils.generateErrorResponse(
                            RPCErrorResponse.RPC_ERR_APPLICATION);
            e.printStackTrace();
        }
        finally
        {            
            commandSet.getController().setSegmentDataOfRPC(result);
        }
    }

    /**
     * Write the input language to production model and return the response.
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
    public void change(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {
        final int INDEX_OF_DATA = 1;
        
        int response = RPCErrorResponse.RPC_ERR_NO_ERRORS;
        RPCLanguage changed = null;
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_USER_SELECT_LANGUAGE_DEFAULT,
                CRCTool.generateCRC16(ProductionConstants.KEY_USER_SELECT_LANGUAGE_DEFAULT.getBytes()));
        
        RPCDataArguments argument = invocation.getArguments().get(INDEX_OF_DATA);
        
        ByteBuffer buffer = ByteBuffer.wrap(argument.getValue().getByteArray());
        
        int languageId = buffer.getShort();
        
        try
        {
            IPCUpdateConfig pcConfig = CustJavaFrameworkManager.getUpdateConfigService();
            SafetyNumber<Integer> value = new SafetyNumber<Integer>();
            int indexOfLanguage = 1;
                        
            changed = RPCLanguage.getLanguageById(languageId);
                        
            // apply the language change.
            indexOfLanguage = Arrays.binarySearch(RPCLanguage.values(), changed);
            
            CommonUtils.objectCheck(pcConfig);
            
            pcConfig.updateLanguage(indexOfLanguage);
            value.set(changed.VALUE, -changed.VALUE);
            
            NugenProductionModel.setInt(key, value);
                        
            CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
        }
        catch (ArgumentErrorException e)
        {
            response = RPCErrorResponse.RPC_ERR_UNRECOGNIZED_FEATURE;
            e.printStackTrace();
        }
        catch (RemoteException e)
        {
            response = RPCErrorResponse.RPC_ERR_APPLICATION;
            e.printStackTrace();
        }
        finally
        {            
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(response));   
        }
    }
}