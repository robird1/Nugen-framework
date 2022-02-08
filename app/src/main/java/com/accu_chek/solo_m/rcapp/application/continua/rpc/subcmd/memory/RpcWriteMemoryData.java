/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory.RpcWriteMemoryData
 * Brief: 
 *
 * Create Date: 2015/8/12
 * $Revision: 22210 $
 * $Author: kevenwu $
 * $Id: RpcWriteMemoryData.java 22210 2015-10-21 08:41:27Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory;

import java.nio.ByteBuffer;
import java.util.Arrays;

import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;

public class RpcWriteMemoryData
{
    /**
     * The value of start address.
     */
    private int mStartAddress = -1;
    
    /**
     * The data of write memory command.
     */
    private byte[] mData = null;
    
    /**
     * Parse the input data to the RpcWriteMemoryData format.
     * {UINT32 startAddress, UINT8_Array data}
     *
     * @param argument : The structure contain all input data.
     *        Range: Valid object of RPCDataArguments.
     *        Unit: RPCDataArguments.
     *        Scaling: 1.
     * 
     * return RpcWriteMemoryData [out]: The RpcWriteMemoryData contain the input data.
     *        Range: Valid object of RpcWriteMemoryData.
     *        Unit: RpcWriteMemoryData.
     *        Scaling: 1.
     */
    public static RpcWriteMemoryData parse(RPCDataArguments argument)
    {
        RpcWriteMemoryData result = new RpcWriteMemoryData();
    
        ByteBuffer buffer = ByteBuffer.wrap(argument.getValue().getByteArray());
        byte[] data = new byte[argument.getLength().get()];
        
        result.mStartAddress = buffer.getInt();
        
        buffer.get(data, 0, buffer.remaining());
        result.mData = data;
        
        return result;
    }
    
    /**
     * Return the start address value.
     *
     * see mStartAddress [out]
     * 
     * return int [out]: The value of start address.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public int getStartAddress()
    {
        return mStartAddress;
    }
    
    /**
     * Return the data of write memory command.
     *
     * see mData [out]
     *
     * return byte[] [out]: The byte array data of write memory command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     */
    public byte[] getData()
    {
        return Arrays.copyOf(mData, mData.length);
    }
}
