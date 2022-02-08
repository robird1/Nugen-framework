/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.NumberOfErrorRecords
 * Brief: 
 *
 * Create Date: 2015/11/11
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import java.util.List;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public class NumberOfErrorRecords implements IConfigurationParameterHandler
{

    /**
     * Read the error records from EMWR table and calculate the count of the records.
     * Return the number of error records in UINT16 type.
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
        Context context = commandSet.getController().getContext();
        RPCDataArguments argument = new RPCDataArguments();
        byte[] result = null;
        int numberRC = 0;
        int numberPump = 0;
        
        List<IDBData> rcRecords = new DatabaseModel(UrlType.emwrUri).queryData(context, null);
        List<IDBData> pumpRecords = new DatabaseModel(UrlType.emwrUri).queryData(context, null);
        
        if (null != rcRecords)
        {
            numberRC = rcRecords.size();
        }
        else
        {
            // Apply to the coding standard
        }
        
        if (null != pumpRecords)
        {
            numberPump = pumpRecords.size();
        }
        else
        {
            // Apply to the coding standard
        }
        
        result = CRCTool.getBytes((short) (numberRC + numberPump));
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
        argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
        argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }

    /**
     * This command is not supported of this device, return unrecognized command response.
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
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateErrorResponse(
                        RPCErrorResponse.RPC_ERR_UNRECOGNIZED_CMD));
    }

}
