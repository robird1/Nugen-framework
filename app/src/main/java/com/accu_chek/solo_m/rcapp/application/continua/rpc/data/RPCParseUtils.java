/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils
 * Brief: 
 *
 * Create Date: 2015/7/30
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: RPCParseUtils.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.data;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public class RPCParseUtils
{    
    /**
     * Collect the input event type and data to generate the RPC response.
     * The format is {handle, time, event type, total length, argument count, 
     * argument length, argument data}.
     *
     * @param eventType : The enumeration of RPC event type.
     *        Range: Valid object of RPCEventType.
     *        Unit: RPCEventType.
     *        Scaling: 1.
     * @param arguments : The data of response.
     *        Range: Valid object of SafetyByteArray.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     * 
     * return SafetyByteArray [out]: The data in RPC response structure.
     *        Range: Valid object of SafetyByteArray.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     */
    public static SafetyByteArray generateRPCResponse(RPCEventType eventType,
            RPCDataArguments... arguments)
    {
        final int HANDLE = 0x0000;
        final int TIME = 0xFFFFFFFF;
        
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        ByteArrayBuffer bufferOfArg = new ByteArrayBuffer(0);
        
        byte[] handle = ParseUtils.parseInt16(HANDLE);
        byte[] time = ParseUtils.parseInt32(TIME);
        byte[] type = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(eventType.getType()));
        byte[] argCount = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(arguments.length));
        byte[] argLength = null;
        byte[] length = null;
        
        for (RPCDataArguments each : arguments)
        {
            byte[] content = each.generateBytes().getByteArray();
            bufferOfArg.append(content, 0, content.length);
        }
        
        argLength = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(bufferOfArg.length()));
        length = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(
                argCount.length + argLength.length + bufferOfArg.length()));
        
        buffer.append(handle, 0, handle.length);
        buffer.append(time, 0, time.length);
        buffer.append(type, 0, type.length);
        buffer.append(length, 0, length.length);
        buffer.append(argCount, 0, argCount.length);
        buffer.append(argLength, 0, argLength.length);
        buffer.append(bufferOfArg.toByteArray(), 0, bufferOfArg.length());
        
        return ParseUtils.appendCRC(buffer.toByteArray());
    }
    
    /**
     * Collect the input error response code and generate the RPC response by
     * generateRPCResponse.
     *
     * @param errorCode : The value of error response.
     *        Range: Refer to the definition of RPCErrorResponse.
     *        Unit: Integer.
     *        Scaling: 1.
     * 
     * return SafetyByteArray [out]: The data in RPC response structure.
     *        Range: Valid object of SafetyByteArray.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     */
    public static SafetyByteArray generateErrorResponse(int errorCode)
    {
        RPCDataArguments argument = new RPCDataArguments();
        byte[] response = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(errorCode));
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
        argument.setLength(new SafetyNumber<Integer>(response.length, -response.length));
        argument.setValue(new SafetyByteArray(response, CRCTool.generateCRC16(response)));
        
        return generateRPCResponse(RPCEventType.MDC_NOTI_RPC_ERROR_RESPONSE, argument);
    }
}
