/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadCurrentVersions
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 21737 $
 * $Author: kevenwu $
 * $Id: ReadCurrentVersions.java 21737 2015-10-16 10:48:49Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.VersionInfoData;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.VersionInfoData.SpecType;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public class ReadCurrentVersions implements IRPCCommandHandler
{
    /**
     * Read the version of application.
     * And send to the Continua Agent in VersionInfoData structure.
     * If the version can't be found, send application error to Continua Agent.
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
        final String PREFIX_SW = "sw-revision: V00.";
        
        SafetyByteArray response = null;
        Context context = commandSet.getController().getContext();
        
        try
        {
            byte[] result = null;
            RPCDataArguments argument = new RPCDataArguments();            
            ByteArrayBuffer buffer = new ByteArrayBuffer(0);
            VersionInfoData versionInfo = new VersionInfoData();
            String version = null;
            PackageInfo info = context.getPackageManager().getPackageInfo(
                    context.getPackageName(), 0);
            
            version = PREFIX_SW.concat(info.versionName);
            buffer.append(version.toCharArray(), 0, version.length());
            
            versionInfo.setVersionInfo(SpecType.SW_REVISION, 1, buffer.toByteArray());
            
            result = versionInfo.generateBytes().getByteArray();
            
            argument.setType(RPCArgumentType.RPC_ARG_TYPE_VERSION_INFO);
            argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
            argument.setValue(versionInfo.generateBytes());
            
            response = RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
        }
        catch (NameNotFoundException e)
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
            e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.
        }
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
}
