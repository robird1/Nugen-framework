/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ChangeUSBProtocol
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 23531 $
 * $Author: kevenwu $
 * $Id: ChangeUSBProtocol.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;

public class ChangeUSBProtocol implements IRPCCommandHandler
{
    /**
     * Return no error to Agent and then change the USB protocol from PHDC to MTP by system property.
     * Play communication completed sound after configuration completed.
     * If some error appear when communication with System Property service, return application error.
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
        CommonUtils.playSound(RPCConstants.SOUND_PATH, commandSet.getController().getContext());
        
        commandSet.getController().setSegmentDataOfRPC(
                RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_NO_ERRORS));
        
        try
        {
            IRCSystemPeoperty controller = CustJavaFrameworkManager.getRCSystemPropertyService(null);
                    
            CommonUtils.objectCheck(controller);
            
            controller.setProperty("persist.sys.usb.config", "mtp");
        }
        catch (RemoteException e)
        {
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION));
            e.printStackTrace();
        }
        finally
        {
            // Do nothing.
        }
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// [NSIQ-20] Remove the input argument "context" from "CustJavaFrameworkManager.getPCConnectService()".
