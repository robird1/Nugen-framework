/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ResetToFactoryDefaults
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22322 $
 * $Author: WilliyChiang $
 * $Id: ResetToFactoryDefaults.java 22322 2015-10-22 08:10:30Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import android.content.Context;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.StartupConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;

public class ResetToFactoryDefaults implements IRPCCommandHandler
{
    /**
     * Property key of start flag.
     */
    private static final String PERSIST_SYS_START_UP = "persist.sys.system.isStartup";
    
    /**
     * Delete the user settings and restart the device.
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
        SafetyByteArray response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS);
        Context context = commandSet.getController().getContext();
        
        try
        {
            CustJavaFrameworkManager.getRCSystemPropertyService(context)
                    .setProperty(PERSIST_SYS_START_UP, "true");
            NugenGeneralModel.delete(context, StartupConstants.KEY_CLASS_PATH);
            NugenSettingModel.reset();
            
            commandSet.getController().setSegmentDataOfRPC(response);

            CustJavaFrameworkManager.getCustPowerManagerService(context).reboot();
        }
        catch (RemoteException e)
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
            commandSet.getController().setSegmentDataOfRPC(response);
            e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.            
        }
    }
}
