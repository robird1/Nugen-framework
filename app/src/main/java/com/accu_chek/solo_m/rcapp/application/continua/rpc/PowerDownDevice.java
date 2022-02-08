/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.PowerDownDevice
 * Brief: 
 *
 * Create Date: 2015/6/24
 * $Revision: 23531 $
 * $Author: kevenwu $
 * $Id: PowerDownDevice.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc;

import android.content.Context;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.power.ICustPowerManager;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class PowerDownDevice implements IRPCCommandHandler
{    
    /**
     * Play communication completed sound and return no error to Agent.
     * And then call power down function of Power Manager to turn off the device.
     *
     * @param commandSet : The instance of Continua command set.
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
        final long SOUND_WAITING_TIME = 3000L;
        
        RPCDataArguments argument = new RPCDataArguments();
        byte[] response = ParseUtils.parseInt16(RPCErrorResponse.RPC_ERR_NO_ERRORS);
        
        Context context = commandSet.getController().getContext();
        
        ICustPowerManager manager = CustJavaFrameworkManager
                .getCustPowerManagerService(context);
                
        CommonUtils.playSound(RPCConstants.SOUND_PATH, context);
        
        CommonUtils.sleep(SOUND_WAITING_TIME);
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
        argument.setLength(new SafetyNumber<Integer>(response.length, -response.length));
        argument.setValue(new SafetyByteArray(response, CRCTool.generateCRC16(response)));
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument));
        
        try
        {
            CommonUtils.objectCheck(manager);
            manager.shutdown();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard.
        }
    }
}
