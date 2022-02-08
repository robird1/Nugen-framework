/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadStoredLanguage
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: ReadStoredLanguage.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.nio.ByteBuffer;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCLanguage;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public class ReadStoredLanguage implements IRPCCommandHandler
{
    /**
     * Return all language to Continua Agent, all languages are stored into device during manufacture.
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
        final int BYTES_OF_SHORT = 2;
        
        RPCDataArguments argument = new RPCDataArguments();
        
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
        for (RPCLanguage each : RPCLanguage.values())
        {
            byte[] language = ByteBuffer.allocate(BYTES_OF_SHORT).putShort((short) each.CODE).array();
            buffer.append(language, 0, language.length);
        }
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16_ARRAY);
        argument.setLength(new SafetyNumber<Integer>(buffer.length(), -buffer.length()));
        argument.setValue(new SafetyByteArray(buffer.toByteArray(), 
                CRCTool.generateCRC16(buffer.toByteArray())));
        
        commandSet.getController().setSegmentDataOfRPC(RPCParseUtils.generateRPCResponse(
                RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }
}
