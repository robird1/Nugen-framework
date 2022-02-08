/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadTimeBlocks
 * Brief:
 * 
 * Create Date: 2015/7/1
 * $Revision: 22494 $
 * $Author: kevenwu $
 * $Id: ReadTimeblocks.java 22494 2015-10-26 03:35:47Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.FixPointFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTimeBlockTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public class ReadTimeblocks implements IRPCCommandHandler
{
    /**
     * Read Bolus time block from database and transfer to Continua Agent.
     * Response structure:
     * {
     * RpcTime(start time),
     * RpcTimeblock(id, end time, target bG value, carb ratio, insulin
     * sensitivity)...;
     * }
     * 
     * @param commandSet :The instance of Continua command set.
     *            Range: Valid object of ContinuaCommandSet.
     *            Unit: ContinuaCommandSet.1
     *            Scaling: 1.
     * @param invocation : The data arguments of request RPC command.
     *            Range: Valid object of RPCCommandInvocation.
     *            Unit: RPCCommandInvocation.
     *            Scaling: 1.
     * 
     *            return void [out]: None
     */
    @Override
    public void handle(ContinuaCommandSet commandSet,
            RPCCommandInvocation invocation)
    {
        final int LENGTH_OF_TIMEBLOCK = 22;

        List<RPCDataArguments> result = new LinkedList<RPCDataArguments>();
        RPCDataArguments argumentStart = new RPCDataArguments();
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        Context context = commandSet.getController().getContext();
        List<IDBData> timeblocks = new DatabaseModel(
                UrlType.patientRecordTimeBlockUri).queryData(context, null);

        SafetyChannel<Long> startTime = ((PatientRecordTimeBlockTable) timeblocks
                .get(0)).getStartTime();
        byte[] timeInBytes = ParseUtils.parseAbsoluteTime(CommonUtils
                .getOriginValue(startTime.getValueCH1(),
                        startTime.getValueCH2()));

        buffer.append(timeInBytes, 0, timeInBytes.length);
        argumentStart.setType(RPCArgumentType.RPC_ARG_TYPE_TIME);
        argumentStart.setLength(new SafetyNumber<Integer>(buffer.length(),
                -buffer.length()));
        argumentStart.setValue(new SafetyByteArray(buffer.toByteArray(),
                CRCTool.generateCRC16(buffer.toByteArray())));
        result.add(argumentStart);

        for (IDBData each : timeblocks)
        {
            ByteBuffer rpcTimeblock = ByteBuffer.allocate(LENGTH_OF_TIMEBLOCK);
            RPCDataArguments argumentTimeblock = new RPCDataArguments();
            PatientRecordTimeBlockTable timeblock = (PatientRecordTimeBlockTable) each;

            SafetyChannel<Integer> id = timeblock.getRecordId();
            SafetyChannel<Long> endTime = timeblock.getEndTime();
            SafetyChannel<Integer> lowTarget = timeblock.getBgLowerTarget();
            SafetyChannel<Integer> highTarget = timeblock.getBgUpperTarget();
            SafetyChannel<Integer> ratioInsulin = timeblock
                    .getCarbRatioInsulin();
            SafetyChannel<Integer> ratioCarb = timeblock.getCarbRatioCarbs();
            SafetyChannel<Integer> sensitivityInsulin = timeblock
                    .getSensitivityInsulin();
            SafetyChannel<Integer> sensitivityBg = timeblock.getSensitivityBg();

            int idValue = CommonUtils.getOriginValue(id.getValueCH1(),
                    id.getValueCH2());
            long timeValue = CommonUtils.getOriginValue(endTime.getValueCH1(),
                    endTime.getValueCH2());
            int lowValue = CommonUtils.getOriginValue(lowTarget.getValueCH1(),
                    lowTarget.getValueCH2());
            int highValue = CommonUtils.getOriginValue(
                    highTarget.getValueCH1(), highTarget.getValueCH2());

            int ratioInsulinValue = CommonUtils.getOriginValue(
                    ratioInsulin.getValueCH1(), ratioInsulin.getValueCH2());
            FixPointFloat ratioInsulinFixPoint = new FixPointFloat(
                    ratioInsulinValue, -ratioInsulinValue);
            SFloat ratioInsulinSFloat = new SFloat(new SafetyFloat(
                    ratioInsulinFixPoint.getOriginFloat(),
                    String.valueOf(ratioInsulinFixPoint.getOriginFloat())), 1);

            int ratioCarbValue = CommonUtils.getOriginValue(
                    ratioCarb.getValueCH1(), ratioCarb.getValueCH2());

            int sensitivityInsulinValue = CommonUtils.getOriginValue(
                    sensitivityInsulin.getValueCH1(),
                    sensitivityInsulin.getValueCH2());
            FixPointFloat sensitivityInsulinFixPoint = new FixPointFloat(
                    sensitivityInsulinValue, -sensitivityInsulinValue);
            SFloat sensitivityInsulinSFloat = new SFloat(
                    new SafetyFloat(
                            sensitivityInsulinFixPoint.getOriginFloat(),
                            String.valueOf(sensitivityInsulinFixPoint
                                    .getOriginFloat())), 1);

            int sensitivityBgValue = CommonUtils.getOriginValue(
                    sensitivityBg.getValueCH1(), sensitivityBg.getValueCH2());

            rpcTimeblock.putShort((short) idValue);
            rpcTimeblock.put(ParseUtils.parseAbsoluteTime(timeValue));
            rpcTimeblock.putShort((short) lowValue);
            rpcTimeblock.putShort((short) highValue);
            rpcTimeblock.putShort(ratioInsulinSFloat.getValue().get()
                    .shortValue());
            rpcTimeblock.putShort((short) ratioCarbValue);
            rpcTimeblock.putShort(sensitivityInsulinSFloat.getValue().get()
                    .shortValue());
            rpcTimeblock.putShort((short) sensitivityBgValue);

            argumentTimeblock.setType(RPCArgumentType.RPC_ARG_TYPE_TIMEBLOCK);
            argumentTimeblock.setLength(new SafetyNumber<Integer>(
                    LENGTH_OF_TIMEBLOCK, -LENGTH_OF_TIMEBLOCK));
            argumentTimeblock.setValue(new SafetyByteArray(rpcTimeblock.array(), 
                    CRCTool.generateCRC16(rpcTimeblock.array())));
            
            result.add(argumentTimeblock);
        }
        
        commandSet.getController().setSegmentDataOfRPC(RPCParseUtils.generateRPCResponse(
                RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, result.toArray(new RPCDataArguments[result.size()])));
    }
}