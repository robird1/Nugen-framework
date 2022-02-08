/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.MinimumCustomerDate
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: MinimumCustomerDate.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.nio.ByteBuffer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;

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

public class MinimumCustomerDate implements IConfigurationParameterHandler
{
    /**
     * The date format of minimum date in production model.
     */
    private final SimpleDateFormat FORMAT = new SimpleDateFormat("dd-MM-yyyy");
    
    /**
     * Read the data from production model and return to Continua Agent.
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
        final int SIZE_OF_MIN_DATE = 3;
        
        RPCDataArguments arguments = new RPCDataArguments();
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_MIN_DATE,
                CRCTool.generateCRC16(ProductionConstants.KEY_MIN_DATE.getBytes()));
        SafetyString date = NugenProductionModel.getString(key);
                
        try
        {
            ByteBuffer buffer = ByteBuffer.wrap(ParseUtils.parseAbsoluteTime(
                    FORMAT.parse(date.getString()).getTime()));
            byte[] data = new byte[SIZE_OF_MIN_DATE];
            
            buffer.get(data);
            
            arguments.setType(RPCArgumentType.RPC_ARG_TYPE_UINT8_ARRAY);
            arguments.setLength(new SafetyNumber<Integer>(
                    SIZE_OF_MIN_DATE, -SIZE_OF_MIN_DATE));
            arguments.setValue(new SafetyByteArray(data, CRCTool.generateCRC16(data)));
            
            commandSet.getController().setSegmentDataOfRPC( 
                    RPCParseUtils.generateRPCResponse(
                            RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, arguments));
        }
        catch (ParseException e)
        {
            commandSet.getController().setSegmentDataOfRPC( 
                    RPCParseUtils.generateErrorResponse(
                            RPCErrorResponse.RPC_ERR_INVALID_DATA));
            e.printStackTrace();
        }
        catch (NullPointerException e)
        {
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(
                            RPCErrorResponse.RPC_ERR_APPLICATION));
            e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.
        }
    }

    /**
     * Write the data to production model and return the response to Agent.
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
        final int POSITION_OF_DATE_DATA = 1;

        int response = RPCErrorResponse.RPC_ERR_NO_ERRORS;
        String inputDate = null;
        
        SafetyString key = new SafetyString(ProductionConstants.KEY_MIN_DATE,
                CRCTool.generateCRC16(ProductionConstants.KEY_MIN_DATE.getBytes()));
        
        RPCDataArguments argument = invocation.getArguments().get(POSITION_OF_DATE_DATA);
        ByteBuffer date = ByteBuffer.wrap(argument.getValue().getByteArray());
        Calendar calendar = Calendar.getInstance();
        
        // reset the calendar.
        calendar.setTimeInMillis(0);
        
        // set input date to calendar.
        calendar.set(Calendar.YEAR, Integer.valueOf(String.format("%x", date.getShort())));
        calendar.set(Calendar.MONTH, Integer.valueOf(String.format("%x", date.get())));
        
        // format the date to the acceptable format in Production model.
        inputDate = FORMAT.format(calendar.getTime());
        
        NugenProductionModel.setString(key, 
                new SafetyString(inputDate, CRCTool.generateCRC16(inputDate.getBytes())));
        
        CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
        
        commandSet.getController().setSegmentDataOfRPC( 
                RPCParseUtils.generateErrorResponse(response));
    }
}
