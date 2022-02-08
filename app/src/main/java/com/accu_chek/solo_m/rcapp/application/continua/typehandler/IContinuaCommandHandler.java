/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.data.IContinuaCommandParser
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: IContinuaCommandHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.ContinuaRequestType;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

/**
 * This class is used to implement the request command handler for processing the
 * request command.
 */
public interface IContinuaCommandHandler
{
    /**
     * The enumeration of Continua request command.
     */
    enum ContinuaCommand
    {
        /**
         * The instance of get product identifier command.
         */
        PRODUCT_IDENTIFIER(ContinuaRequestType.PRODUCT_IDENTIFIER, new ProductIdentifierHandler()),
        
        /**
         * The instance of get PID number command.
         */
        PID_NUMBER(ContinuaRequestType.PID_NUMBER, new PIDNumberHandler()),
        
        /**
         * The instance of get model number command.
         */
        MODEL_NUMBER(ContinuaRequestType.MODEL_NUMBER, new ModelNumberHandler()),
        
        /**
         * The instance of get count PM segment entry command.
         */
        PM_SEGMENT_ENTRY_COUNT(ContinuaRequestType.PMSEG_ENTRY_COUNT, 
                new PMSegmentEntryCountHandler()),
           
        /**
         * The instance of get PM segment entry command.
         */
        PM_SEGMENT_ENTRY(ContinuaRequestType.PMSEG_ENTRY, new PMSegmentHandler()),
        
        /**
         * The instance of get total segment count command.
         */
        PM_TOTAL_ENTRY_COUNT(ContinuaRequestType.PMSEG_TOTAL_COUNT, 
                new PMTotalCountHandler()),
                
        /**
         * The instance of clear all segment data command.
         */
        PM_SEGMENT_CLEAR(ContinuaRequestType.PMSEG_CLEAR, new ClearSegmentHandler()),
                
        /**
         * The instance of get absolute time command.
         */
        ABSOLUTE_TIME(ContinuaRequestType.ABSOLUTE_TIME, new AbsoluteTimeHandler()),
        
        /**
         * The instance of get power status command.
         */
        POWER_STATUS(ContinuaRequestType.POWER_STATUS, new PowerStatusHandler()),
        
        /**
         * The instance of get battery level command.
         */
        BATTERY_LEVEL(ContinuaRequestType.BATTERY_LEVEL, new BatteryLevelHandler()),
        
        /**
         * The instance of get software version command.
         */
        SW_VERSION(ContinuaRequestType.SW_VERSION, new SoftwareVersionHandler()),
        
        /**
         * The instance of get firmware version command.
         */
        FW_VERSION(ContinuaRequestType.FW_VERSION, new FirmwareVersionHandler()),

        /**
         * The instance of get serial number command.
         */
        SERIAL_NUMBER(ContinuaRequestType.SERIAL_NUMBER, new SerialNumberHandler()),
        
        /**
         * The instance of handling error message.
         */
        EMWR_HANDLE(ContinuaRequestType.EMWR_HANDLE, new ErrorMessageHandler()),
        
        /**
         * The instance of handling RPC commands.
         */
        RPC_COMMAND(0, new RPCCommandHandler());
        
        /**
         * The type of Continua request.
         */
        private final int mType;
        
        /**
         * The command handler of each request type.
         */
        private IContinuaCommandHandler mHandler = null;
        
        /**
         * Construct a new ContinuaCommand instance and assign the command type
         * and command handler to this instance.
         * 
         * @param type : The input command type.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: ContinuaCommand.
         *        Scaling: 1.
         * @param handler : The handler of this command type.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: ContinuaCommand.
         *        Scaling: 1.
         *        
         * return void [out]: None        
         */
        private ContinuaCommand(int type, IContinuaCommandHandler handler)
        {
            mType = type;
            mHandler = handler;
        }
        
        /**
         * Return the command type of this enumeration.
         *
         * see mType [out]
         *
         * return int [out]: The value of request command type.
         *         Range: Refer to the definition of ContinuaRequestType.
         *         Unit: Integer.
         *         Scaling: 1.
         */
        public int getCommandType()
        {
            return mType;
        }
        
        /**
         * Return the command handler of this enumeration.
         * 
         * see mHandler [out]
         *
         * return IContinuaCommandHandler [out]: The command handler.
         *        Range: Valid object of IContinuaCommandHandler.
         *        Unit: IContinuaCommandHandler.
         *        Scaling: 1.
         */
        public IContinuaCommandHandler getHandler()
        {
            return mHandler;
        }
        
        /**
         * According to the input type, this function returns the relevant
         * RequestCommnad.
         *
         * @param type : The value of request command type.
         *        Range: Refer to RequestCommandType.
         *        Unit: Integer.
         *        Scaling: 1.
         * 
         * return RequestCommand [out]: The relevant RequestCommand, but return 
         *     OUT_OF_RANGE if the input value is out of range.  
         * 
         * throws ArgumentErrorException if the command is not supported. 
         */
        public static ContinuaCommand getRequestCommand(int type) 
                throws ArgumentErrorException
        {
            ContinuaCommand request = null;
            
            for (ContinuaCommand command : ContinuaCommand.values())
            {
                int valueOfCommand = command.getCommandType();
                
                if (valueOfCommand == type)
                {
                    request = command;
                }
                else
                {
                    // Apply to coding standard.
                }
            }
            
            if (null == request)
            {
                throw new ArgumentErrorException(
                        "This command: " + type + " is not supported.");
            }
            else
            {
                // Apply to coding standard.
            }
            
            return request;
        }
    }
    
    /**
     * Do the command handle procedure.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *
     * return void [out]: None
     */
    void handleCommand(ContinuaCommandSet commandSet);
}
