/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCDataArgumentsParser
 * Brief: 
 *
 * Create Date: 2015/7/21
 * $Revision: 21447 $
 * $Author: kevenwu $
 * $Id: RPCCommandInvocation.java 21447 2015-10-14 02:27:25Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.data;

import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;

import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public class RPCCommandInvocation
{    
    /**
     * The value of main command.
     */
    private SafetyNumber<Integer> mCommand = new SafetyNumber<Integer>(-1, 1);
    
    /**
     * The value of sub-command.
     */
    private SafetyNumber<Integer> mSubCommand = new SafetyNumber<Integer>(-1, 1);
    
    /**
     * The arguments of this invocation.
     */
    private List<RPCDataArguments> mRPCDataArguments = 
            new LinkedList<RPCDataArguments>();
    
    /**
     * Parse the data from byte array to RPCCommandInvocation object.
     * The structure of invocation:
     * {
     * main command,
     * sub command,
     * number of arguments,
     * length of arguments,
     * arguments data;
     * }
     *
     * @param rpcData : The data from RPC request command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     * 
     * return RPCCommandInvocation [out]: The object contains all input data.
     *        Range: Valid object of RPCCommandInvocation.
     *        Unit: RPCCommandInvocation.
     *        Scaling: 1.
     * 
     * throws ArgumentErrorException if the input argument type is not supported. 
     */
    public static RPCCommandInvocation parse(SafetyByteArray rpcData) throws ArgumentErrorException
    {
        final int MASK_BYTE = 0xFF;
        final int MASK_SHORT = 0xFFFF;
        
        RPCCommandInvocation result = new RPCCommandInvocation();
        
        ByteBuffer buffer = ByteBuffer.wrap(rpcData.getByteArray());
        int command = buffer.get() & MASK_BYTE;
        int subCommand = buffer.get() & MASK_BYTE;
        
        int remaining = -1;
        
        result.mCommand = new SafetyNumber<Integer>(command, -command);
        result.mSubCommand = new SafetyNumber<Integer>(subCommand, -subCommand);
                
        // number of items.
        buffer.getShort();
        
        // length of RpcDataArguments.
        buffer.getShort();
        
        remaining = buffer.remaining();
        
        while (remaining > 0)
        {
            RPCDataArguments argument = new RPCDataArguments();
            byte[] value = null;
            int type = buffer.getShort() & MASK_SHORT;
            int length = buffer.getShort() & MASK_SHORT;
            
            Log.i("Invocation", "type: " + type);
            Log.i("Invocation", "length: " + length);
            
            argument.setType(RPCArgumentType.getTypeById(type));
            argument.setLength(new SafetyNumber<Integer>(length, -length));
            
            value = new byte[length];
            buffer.get(value);
            
            argument.setValue(new SafetyByteArray(value, CRCTool.generateCRC16(value)));
            
            result.mRPCDataArguments.add(argument);
            
            remaining = buffer.remaining();
        }
        
        return result;
    }
    
    /**
     * Return the main command of this invocation.
     *
     * see mCommand [out]
     *
     * return int [out]: The value of main command.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public SafetyNumber<Integer> getCommand()
    {
        return mCommand;
    }
    
    /**
     * Return the sub-command of this invocation.
     *
     * see mSubCommand [out]
     *
     * return int [out]: The value of sub-command.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public SafetyNumber<Integer> getSubCommand()
    {
        return mSubCommand;
    }
    
    /**
     * Return the arguments of this invocation. The first one is command argument.
     *
     * see mRPCDataArguments [out]
     *
     * return List<RPCDataArguments> [out]: The list contains all argument data.
     *        Range: Valid object of List<RPCDataArguments>.
     *        Unit: List<RPCDataArguments>.
     *        Scaling: 1.
     */
    public List<RPCDataArguments> getArguments()
    {
        return new LinkedList<RPCDataArguments>(mRPCDataArguments);
    }
}
