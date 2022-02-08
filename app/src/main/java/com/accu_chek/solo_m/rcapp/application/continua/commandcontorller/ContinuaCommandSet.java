/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet
 * Brief: 
 *
 * Create Date: 2015/6/29
 * $Revision: 22827 $
 * $Author: kevenwu $
 * $Id: ContinuaCommandSet.java 22827 2015-10-29 09:09:46Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.commandcontorller;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

/**
 * This class used to store the command set from Continua Agent.
 */
public class ContinuaCommandSet
{
    /**
     * The instance of ContinuaCommandContorller.
     */
    private final ContinuaCommandController mController;
    
    /**
     * The value of request command.
     */
    private final SafetyNumber<Integer> mCommand;
    
    /**
     * The value of request configuration id.
     */
    private final SafetyNumber<Integer> mConfigId;
    
    /**
     * The value of request segment id.
     */
    private final SafetyNumber<Integer> mSegmentId;
    
    /**
     * The byte array data of the request command.
     */
    private final SafetyByteArray mDataOfCommand;
    
    /**
     * Constructs the instance of this object and put the controller, command, 
     * configId, segmentId and data into this instance.
     * 
     * @param controller : The ContinuaCommandController instance.
     *        Range: Valid object of ContinuaCommandController.
     *        Unit: ContinuaCommandContorller.
     *        Scaling: 1.
     * @param command : The value of request command.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param configId : The value of configuration id.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param segmentId : The value of segment id.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param data : The data of the request command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     *        
     * see mController [in]
     * see mCommand [in]
     * see mConfigId [in]
     * see mSegmentId [in]
     * see mDataOfCommand [in]
     */
    public ContinuaCommandSet(ContinuaCommandController controller, int command, 
            int configId, int segmentId, byte[] data)
    {
        mController = controller;
        mCommand = new SafetyNumber<Integer>(command, -command);
        mConfigId = new SafetyNumber<Integer>(configId, -configId);
        mSegmentId = new SafetyNumber<Integer>(segmentId, -segmentId);
        mDataOfCommand = verifyCRCAndRetrieveData(data);
    }
    
    /**
     * Constructs the instance of this object and put the controller, command, 
     * configId and segmentId into this instance.
     * 
     * @param controller : The ContinuaCommandController instance.
     *        Range: Valid object of ContinuaCommandController.
     *        Unit: ContinuaCommandContorller.
     *        Scaling: 1.
     * @param command : The value of request command.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param configId : The value of configuration id.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param segmentId : The value of segment id.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * 
     * see mController [in]
     * see mCommand [in]
     * see mConfigId [in]
     * see mSegmentId [in]
     */
    public ContinuaCommandSet(ContinuaCommandController controller, int command, 
            int configId, int segmentId)
    {
        mController = controller;
        mCommand = new SafetyNumber<Integer>(command, -command);
        mConfigId = new SafetyNumber<Integer>(configId, -configId);
        mSegmentId = new SafetyNumber<Integer>(segmentId, -segmentId);
        mDataOfCommand = null;
    }
    
    /**
     * Check the CRC of this input byte array data and store it into SafetyByteArray.
     *
     * @param data : The input data with CRC.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     * 
     * return SafetyByteArray [out]: The CRC protected data.
     *        Range: Valid object of SafetyByteArray.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     */
    private SafetyByteArray verifyCRCAndRetrieveData(byte[] data)
    {       
        final byte LENGTH_OF_CRC = 2;
        final int MASK_SHORT = 0xFFFF;
        
        SafetyByteArray result = new SafetyByteArray();
        
        ByteBuffer buffer = null;
        int length = data.length;
        byte[] original = new byte[length - LENGTH_OF_CRC];
        int crc = -1;
        
        buffer = ByteBuffer.wrap(data);
        buffer.get(original);
        
        buffer.order(ByteOrder.LITTLE_ENDIAN);        
        crc = buffer.getShort() & MASK_SHORT;
        
        result.set(original, crc);
        
        return result;
    }
    
    /**
     * Return the instance of ContinuaCommandController.
     *
     * see mController [out]
     *
     * return ContinuaCommandController [out]: The ContinuaCommandController instance.
     *        Range: Valid object of ContinuaCommandController.
     *        Unit: ContinuaCommandContorller.
     *        Scaling: 1.
     */
    public ContinuaCommandController getController()
    {
        return mController;
    }
    
    /**
     * Return the value of request command.
     *
     * see mCommand [out]
     * 
     * return int [out]: The value of request command.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public int getCommand()
    {
        return mCommand.get();
    }
    
    /**
     * Return the configuration id of request command.
     *
     * see mConfigId [out]
     * 
     * return int [out]: The value of configuration id.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public int getConfigId()
    {
        return mConfigId.get();
    }
    
    /**
     * Return the segment id of request command.
     *
     * see mSegmentId [out]
     *
     * return int [out]: The value of segment id.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public int getSegmentId()
    {
        return mSegmentId.get();
    }
    
    /**
     * Return the data of request command.
     *
     * see mDataOfCommand [out]
     *
     * return byte[] [out]: The data of the request command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     */
    public SafetyByteArray getDataOfCommand()
    {
        return mDataOfCommand;
    }
}
