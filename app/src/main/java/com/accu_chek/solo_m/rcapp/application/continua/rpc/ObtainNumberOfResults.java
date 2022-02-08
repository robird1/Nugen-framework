/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.ObtainNumberOfResults
 * Brief: 
 *
 * Create Date: 2015/6/24
 * $Revision: 21447 $
 * $Author: kevenwu $
 * $Id: ObtainNumberOfResults.java 21447 2015-10-14 02:27:25Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc;

import java.util.ArrayList;
import java.util.List;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ContinuaModel;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.BloodGlucose;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ControlSolution;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public class ObtainNumberOfResults implements IRPCCommandHandler
{    
    /**
     * Calculate the sum of bG and control result records and return to Agent.
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
        RPCDataArguments argument = new RPCDataArguments();
        byte[] number = null;
        
        List<AbstractContinuaSegment> result = new ArrayList<AbstractContinuaSegment>();
        
        result.addAll(ContinuaModel.getDataByClass(
                commandSet.getController().getContext(), BloodGlucose.class));
        result.addAll(ContinuaModel.getDataByClass(
                commandSet.getController().getContext(), ControlSolution.class));
                
        number = ParseUtils.parseInt16(result.size());
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
        argument.setLength(new SafetyNumber<Integer>(number.length, -number.length));
        argument.setValue(new SafetyByteArray(number, CRCTool.generateCRC16(number)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
    }
}
