/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments
 * Brief: 
 *
 * Create Date: 2015/7/21
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: RPCDataArguments.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.data;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public class RPCDataArguments
{
    /**
     * The type of this argument data.
     */
    private RPCArgumentType mType = null;
    
    /**
     * The length of this argument data.
     */
    private SafetyNumber<Integer> mLength = new SafetyNumber<Integer>(-1, 1);
    
    /**
     * The data of this argument.
     */
    private SafetyByteArray mValue = null;
    
    /**
     * Return the value type of this argument.
     *
     * see mType [out]
     *
     * return int [out]: The type value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public RPCArgumentType getType()
    {
        return mType;
    }
    
    /**
     * Set the value type of this argument.
     *
     * see mType [in]
     *
     * @param type : The type value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     *        
     * return void [out]        
     */
    public void setType(RPCArgumentType type)
    {
        mType = type;
    }
    
    /**
     * Return the value length of this argument.
     *
     * see mLength [out]
     *
     * return int [out]: The length value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public SafetyNumber<Integer> getLength()
    {
        return mLength;
    }
    
    /**
     * Set the value length of this argument.
     *
     * see mLength [in]
     *
     * @param length : The length value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     *        
     * return void [out]        
     */
    public void setLength(SafetyNumber<Integer> length)
    {
        mLength = length;
    }
    
    /**
     * Return the value of this argument.
     *
     * see mValue [out]
     *
     * return byte[] [out]: The value of request command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     */
    public SafetyByteArray getValue()
    {
        return mValue;
    }
    
    /**
     * Set the value of this argument.
     *
     * see mValue [in]
     *
     * @param value : The value of request command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     *        
     * return void [out]        
     */
    public void setValue(SafetyByteArray value)
    {
        mValue = value;
    }
    
    /**
     * Collect the parameter of type, length and data to generate a byte array.
     *
     * see mType [out]
     * see mLength [out]
     * see mValue [out]
     *
     * return SafetyByteArray [out]: The byte array contains all necessary parameter.
     *        Range: Valid object of SafetyByteArray.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     */
    public SafetyByteArray generateBytes()
    {
        SafetyByteArray result = new SafetyByteArray();        
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
        byte[] typeInBytes = ParseUtils.makeLittleEndian(
                ParseUtils.parseInt16(mType.getArgumentType()));
        byte[] lengthInBytes = ParseUtils.makeLittleEndian(
                ParseUtils.parseInt16(mLength.get()));
        
        buffer.append(typeInBytes, 0, typeInBytes.length);
        buffer.append(lengthInBytes, 0, lengthInBytes.length);
        buffer.append(mValue.getByteArray(), 0, mValue.getByteArray().length);
        
        result.set(buffer.toByteArray(), CRCTool.generateCRC16(buffer.toByteArray()));
        
        return result;
    }
}
