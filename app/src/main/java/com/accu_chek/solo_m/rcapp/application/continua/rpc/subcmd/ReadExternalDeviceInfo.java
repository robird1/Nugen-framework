/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadExternalCompInfo
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 22188 $
 * $Author: kevenwu $
 * $Id: ReadExternalDeviceInfo.java 22188 2015-10-21 07:39:44Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import org.json.JSONException;
import org.json.JSONObject;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.BTLEConstants;
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
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;

public class ReadExternalDeviceInfo implements IRPCCommandHandler
{
    /**
     * Read the device information of pump from general model and return to Agent.
     * If the device information can't be found, return application error to Agent.
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
        SafetyByteArray response = RPCParseUtils.generateErrorResponse(
                RPCErrorResponse.RPC_ERR_APPLICATION);;
        RPCDataArguments argument = new RPCDataArguments();
        
        SafetyString deviceInfo = NugenGeneralModel.getString(
                commandSet.getController().getContext(), 
                BTLEConstants.KEY_PUMP_DIS);
        
        if (null != deviceInfo)
        {
            try
            {
                VersionInfoData infoData = new VersionInfoData();
                byte[] result = null;
                
                JSONObject dis = new JSONObject(deviceInfo.getString());
                
                String serialNumber = dis.getString(BTLEConstants.KEY_SERIAL_NUMBER);
                String hwVersion = dis.getString(BTLEConstants.KEY_HW_VERSION);
                String swVersion = dis.getString(BTLEConstants.KEY_SW_VERSION);
                
                infoData.setVersionInfo(SpecType.SERIAL_NUMBER, 1, serialNumber.getBytes());
                infoData.setVersionInfo(SpecType.HW_REVISION, 1, hwVersion.getBytes());
                infoData.setVersionInfo(SpecType.SW_REVISION, 1, swVersion.getBytes());
                
                result = infoData.generateBytes().getByteArray();
                
                argument.setType(RPCArgumentType.RPC_ARG_TYPE_VERSION_INFO);
                argument.setLength(new SafetyNumber<Integer>(result.length, -result.length));
                argument.setValue(new SafetyByteArray(result, CRCTool.generateCRC16(result)));
                
                response = RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
            }
            catch (JSONException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Apply to coding standard.
            }
        }
        else
        {
            // The response is default value application error.
        }
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
}
