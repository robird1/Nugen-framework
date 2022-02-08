/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.FirmewareUpgradeStatus
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: FirmwareUpgradeStatus.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CustSystemProperty;

public class FirmwareUpgradeStatus implements IRPCCommandHandler
{
    
    /**
     * Read the upgrade status from system property, and transfer to Agent.
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
        SafetyByteArray response = null;        
        RPCDataArguments argument = new RPCDataArguments();
        
        String status = CustSystemProperty.getProperty("", "0xD000");
        
        byte[] statusInBytes = CRCTool.getBytes(Short.parseShort(status));
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_BITS_16);
        argument.setLength(new SafetyNumber<Integer>(statusInBytes.length, -statusInBytes.length));
        argument.setValue(new SafetyByteArray(statusInBytes, CRCTool.generateCRC16(statusInBytes)));
        
        response = RPCParseUtils.generateRPCResponse(
                RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
    
}
