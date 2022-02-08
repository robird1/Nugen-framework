/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadHighTarget
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22494 $
 * $Author: kevenwu $
 * $Id: ReadHighTarget.java 22494 2015-10-26 03:35:47Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.util.List;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.nugendata.PatientRecordTimeBlockTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IQuerySelectType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.OrderByType;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

public class ReadHighTarget implements IRPCCommandHandler
{
    /**
     * This function retrieves the high target from Bolus time block database and return to Agent.
     * If no record found, return application error to Agent.
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
        
        Context context = commandSet.getController().getContext();
        DatabaseModel model = new DatabaseModel(UrlType.patientRecordTimeBlockUri);
               
        List<IDBData> patientRecord = model.queryData(context, new IQuerySelectType()
        {
            @Override
            public String onSelection()
            {
                String currentTime = String.valueOf(System.currentTimeMillis());
                
                return PatientRecordTimeBlockTable.COLUMN_START_TIME + " > " + currentTime
                        +" AND " + PatientRecordTimeBlockTable.COLUMN_END_TIME + " < " + currentTime;
            }

            @Override
            public String[] onSelectionArgs()
            {                
                return null;
            }

            @Override
            public String onOrderBy()
            {
                return PatientRecordTimeBlockTable.COLUMN_TIME_BLOCK_ID + OrderByType.DESC;
            }
        });
        
        if (null != patientRecord)
        {
            PatientRecordTimeBlockTable timeblock = (PatientRecordTimeBlockTable) patientRecord.get(0);
            SafetyChannel<Integer> highValue = timeblock.getBgUpperTarget();
            
            int highTarget = CommonUtils.getOriginValue(highValue.getValueCH1(), highValue.getValueCH2());
            byte[] highInBytes = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(highTarget));
            
            RPCDataArguments argument = new RPCDataArguments();
            
            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
            argument.setLength(new SafetyNumber<Integer>(highInBytes.length, -highInBytes.length));
            argument.setValue(new SafetyByteArray(highInBytes, CRCTool.generateCRC16(highInBytes)));
            
            response = RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
        }
        else
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
        }
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
}
