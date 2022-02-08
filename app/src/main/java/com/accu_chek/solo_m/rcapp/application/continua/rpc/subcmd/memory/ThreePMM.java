/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory.ThreePMM
 * Brief: 
 *
 * Create Date: 2015/8/12
 * $Revision: 25175 $
 * $Author: VictorChen $
 * $Id: ThreePMM.java 25175 2015-11-30 11:25:57Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBGMControl;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ThreePMM implements IMemoryOperationHandler
{
    /**
     * Not ready
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
        // TODO Auto-generated method stub

    }

    /**
     * Write the code key information to the Measurement Engine.
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
        final int INDEX_OF_DATA = 1;
        
        SafetyByteArray response = null;
        SafetyByteArray codeKey = null;
        RPCDataArguments argument = invocation.getArguments().get(INDEX_OF_DATA);        
        RpcWriteMemoryData data = RpcWriteMemoryData.parse(argument);        
        IBGMControl controller = CustJavaFrameworkManager.getBGMControlService(
                commandSet.getController().getContext());
        
        codeKey = new SafetyByteArray(data.getData(), CRCTool.generateCRC16(data.getData()));
        
//        try
//        {
//            CommonUtils.objectCheck(controller);
//            
//            controller.updateCodeKey(codeKey);
//            
//            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS);
//        }
//        catch (RemoteException e)
//        {
//            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
//            e.printStackTrace();
//        }
//        finally
//        {
//            commandSet.getController().setSegmentDataOfRPC(response);
//        }
    }
}
// (R20933 2015-10-05 04:34:57 kevenwu)
// ----------------------------------------------------------------------------
// Add update code key command call back.
