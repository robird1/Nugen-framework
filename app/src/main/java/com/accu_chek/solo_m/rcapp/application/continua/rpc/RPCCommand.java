/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.typehandler.RPCCommand
 * Brief: 
 *
 * Create Date: 2015/6/24
 * $Revision: 21447 $
 * $Author: kevenwu $
 * $Id: RPCCommand.java 21447 2015-10-14 02:27:25Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc;

import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

/**
 * The enumeration of RPC command, it is used to handle the request command.
 */
public enum RPCCommand
{
    /**
     * The instance of read configuration command.
     */
    READ_CONFIGURATION(0x43, new ReadConfiguration()),
    
    /**
     * The instance of read setup command.
     */
    READ_SETUP(0x53, new ReadSetup()),
    
    /**
     * The instance of change setup command.
     */
    CHANGE_SETUP(0x0C, new ChangeSetup()),
    
    /**
     * The instance of obtain number of results command.
     */
    OBTAIN_NUMBER_OF_RESULTS(0x60, new ObtainNumberOfResults()),
    
    /**
     * The instance of get strip counter command.
     */
    GET_STRIP_COUNTER(0x23, new GetStripCounter()),
    
    /**
     * The instance of power down device command.
     */
    POWER_DOWN_DEVICE(0x1D, new PowerDownDevice()),
    
    /**
     * The instance of reset device command.
     */
    RESET_DEVICE(0x1E, new ResetDevice()),
    
    /**
     * The instance of execute Mannheim local command.
     */
    EXECUTE_MANNHEIM_LOCAL(0x10, new ExecuteMannheimLocal()),
    
    /**
     * The instance of read error log command.
     */
    READ_ERROR_LOG(0x63, new ReadErrorLog()),
    
    /**
     * The instance of read measurement log command.
     */
    READ_MEASUREMENT_LOG(0x64, new ReadMeasurementLog()),
    
    /**
     * The instance of file IO command.
     */
    FILE_IO(0x82, new FileIO()),
    
    /**
     * The instance of firmware upgrade command.
     */
    FIRMWARE_UPGRADE(0x85, new FirmwareUpgrade()),
    
    /**
     * The instance of version information command.
     */
    VERSION_INFO(0x86, new VersionInfo()),
    
    /**
     * The instance of execute algorithm command.
     */
    EXECUTE_ALGO(0x78, new ExecuteAlgo());
    
    /**
     * The command id of this enumeration.
     */
    private final int mCommandId;
    
    /**
     * The command handler of this enumeration.
     */
    private final IRPCCommandHandler mHandler;
    
    /**
     * Put the command id and corresponding handler to the instance.
     * 
     * @param commandId : The supported command id.
     *        Range: Refer to the definition of RPCCommand.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param handler : The RPC command handler.
     *        Range: Valid object of IRPCCommandHandler.
     *        Unit: IRPCCommandHandler.
     *        Scaling: 1.
     *        
     * see mCommandId [out]
     * see mHandler [out]        
     */
    private RPCCommand(int commandId, IRPCCommandHandler handler)
    {
        mCommandId = commandId;
        mHandler = handler;
    }
    
    /**
     * Return the command id of this enumeration.
     *
     * see mCommandId [out]
     *
     * return int [out]: The command id in 16 bit integer.
     *        Range: Refer to the definition of this enumeration.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public int getCommandId()
    {
        return mCommandId;
    }
    
    /**
     * Return the command handler of the enumeration.
     *
     * see mHandler [out]
     *
     * return IRPCCommandHandler [out]: The RPC command handler.
     *        Range: Valid object of IRPCCommandHandler.
     *        Unit: IRPCCommandHandler.
     *        Scaling: 1.     
     */
    public IRPCCommandHandler getHandler()
    {
        return mHandler;
    }
    
    /**
     * Return the RPCCommand according to the input command id.
     *
     * @param commandId : The input request command id.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * 
     * return RPCCommand [out]: The corresponding instance of the input id.
     *        Range: Valid object of RPCCommand.
     *        Unit: RPCCommand.
     *        Scaling: 1.
     * 
     * throws ArgumentErrorException if the command is not supported. 
     */
    public static RPCCommand getRPCCommandById(int commandId) 
            throws ArgumentErrorException
    {
        RPCCommand result = null;
        
        for (RPCCommand command : RPCCommand.values())
        {
            int valueOfId = command.getCommandId();
            
            if (valueOfId == commandId)
            {
                result = command;
            }
        }
        
        if (null == result)
        {
            throw new ArgumentErrorException("This command \"" + commandId
                    + "\" is not supported.");
        }
        
        return result;
    }
}
