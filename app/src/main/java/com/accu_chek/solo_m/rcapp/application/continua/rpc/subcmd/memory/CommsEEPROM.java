/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory.Comms
 * Brief: 
 *
 * Create Date: 2015/11/12
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory;

import java.nio.ByteBuffer;

import android.content.Context;
import android.os.ConditionVariable;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GetErrorLog;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;

public class CommsEEPROM implements IMemoryOperationHandler
{
        
    class GetErrorLogCallback implements ResponseCallback
    {

        /**
         * Release the timeout locker to collect the error log.
         *
         * @param result : The command result from sub system.
         *        Range: Valid object of SafetyBoolean.
         *        Unit: SafetyBoolean.
         *        Scaling: 1.
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            mIsRequestCompleted = result;
            mLock.open();
        }
        
    }
    
    /**
     * The locker to hold the thread to wait the error log read back.
     */
    private ConditionVariable mLock = new ConditionVariable();
    
    /**
     * Indicates the request to sub system is completed or not.
     */
    private SafetyBoolean mIsRequestCompleted = SafetyBoolean.FALSE;

    /**
     * Call read error log function from BLEController, and read the log from general model.
     * If the request to sub system is not completed, return application error.
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
    public void read(ContinuaCommandSet commandSet,
            RPCCommandInvocation invocation)
    {
        final int TIMEOUT = 10000;
        final String ZERO_STRING = new String(new byte[]{0});
        final SafetyString EMPTY_STRING = new SafetyString(ZERO_STRING, CRCTool.generateCRC16(ZERO_STRING.getBytes()));
        
        Context context = commandSet.getController().getContext();
                
        BLEController.getInstance(context).getErrorLog(new GetErrorLogCallback());
        
        mLock.block(TIMEOUT);
        
        if (SafetyBoolean.TRUE.getByte() == mIsRequestCompleted.getByte())
        {
            RPCDataArguments argument = new RPCDataArguments();
            ByteBuffer buffer = null;
            SafetyString log = NugenGeneralModel.getString(context, 
                    GetErrorLog.COMM_ERROR_LOG, EMPTY_STRING);
            
            int count = 0;
            byte[] logResult = null;
            
            buffer = ByteBuffer.wrap(log.getString().getBytes());

            count = buffer.get();
            logResult = new byte[count];
            
            buffer.get(logResult);
            
            argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT8_ARRAY);
            argument.setLength(new SafetyNumber<Integer>(logResult.length, -logResult.length));
            argument.setValue(new SafetyByteArray(logResult, CRCTool.generateCRC16(logResult)));
            
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateRPCResponse(RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
        }
        else
        {
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION));
        }
    }

    /**
     * Not supported on this device, return invalid data response to Agent.
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
    public void write(ContinuaCommandSet commandSet,
            RPCCommandInvocation invocation)
    {
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_INVALID_DATA));
    }

}
