/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.ConfigurationTestDate
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: ConfigurationTestDate.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.nio.ByteBuffer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ConfigurationTestDate implements IConfigurationParameterHandler
{
    /**
     * The date format of configuration test date which defined by Continua.
     */
    private final SimpleDateFormat FORMAT = new SimpleDateFormat("dd-MM-yyyy");

    /**
     * The length of configuration test date which defined by Continua.
     */
    private final int LENGTH_OF_DATE = 6;
    
    /**
     * Read the configuration date from production model and return to Agent.
     * If the data can't be found or wrong format, return application error to Agent.
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
     * see LENGTH_OF_DATE [in]        
     * 
     * return void [out]: None.
     */    
    @Override
    public void read(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {        
        SafetyByteArray result = null;
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_DATE_DEFAULT,
                CRCTool.generateCRC16(ProductionConstants.KEY_DATE_DEFAULT.getBytes()));
        SafetyString date = NugenProductionModel.getString(key);
                
        try
        {
            RPCDataArguments argument = invocation.getArguments().get(0);
            ByteArrayBuffer buffer = new ByteArrayBuffer(LENGTH_OF_DATE);
            
            byte[] testDate = ParseUtils.parseAbsoluteTime(FORMAT.parse(
                    date.getString()).getTime());
            
            buffer.append(testDate, 1, LENGTH_OF_DATE);
            
            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT8_ARRAY);
            argument.setLength(new SafetyNumber<Integer>(buffer.length(), -buffer.length()));
            argument.setValue(new SafetyByteArray(buffer.toByteArray(),
                    CRCTool.generateCRC16(buffer.toByteArray())));
            
            result = RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
        }
        catch (ParseException e)
        {
            result = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
            e.printStackTrace();
        }
        catch (NullPointerException e)
        {
            result = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
            e.printStackTrace();
        }
        finally
        {
            commandSet.getController().setSegmentDataOfRPC(result);
        }
    }

    /**
     * Write the configuration date from RPC command to production model.
     * Play communication completed sound after configuration.
     * If the data length is different to defined length, return invalid parameter to Agent.
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
     * see LENGTH_OF_DATE [in]        
     * 
     * return void [out]: None.
     */    
    @Override
    public void change(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {
        final int INDEX_OF_DATA = 1;
        String inputDate = null;
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_DATE_DEFAULT,
                CRCTool.generateCRC16(ProductionConstants.KEY_DATE_DEFAULT.getBytes()));
        SafetyByteArray response = new SafetyByteArray();
        RPCDataArguments argument = invocation.getArguments().get(INDEX_OF_DATA);        
        ByteBuffer buffer = ByteBuffer.wrap(argument.getValue().getByteArray());        
        Calendar calendar = Calendar.getInstance();
        int remainingLength = buffer.remaining();
        
        if (LENGTH_OF_DATE == remainingLength)
        {        
            int year = Integer.valueOf("20" + String.format("%x", buffer.get()));
            int month = Integer.valueOf(String.format("%x", buffer.get()));
            int day = Integer.valueOf(String.format("%x", buffer.get()));
            int hourOfDay = Integer.valueOf(String.format("%x", buffer.get()));
            int minute = Integer.valueOf(String.format("%x", buffer.get()));
            int second = Integer.valueOf(String.format("%x", buffer.get()));
            
            calendar.setTimeInMillis(0);
            calendar.set(year, month, day, hourOfDay, minute, second);
            
            inputDate = FORMAT.format(calendar.getTime());
            
            NugenProductionModel.setString(key, 
                    new SafetyString(inputDate, CRCTool.generateCRC16(inputDate.getBytes())));
            
            CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
            
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS);
        }
        else
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_INVALID_PARAMETERS);
        }
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
}
