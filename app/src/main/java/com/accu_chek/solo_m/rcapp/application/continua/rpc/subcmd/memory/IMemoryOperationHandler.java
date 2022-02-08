/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory.IMemoryOperationHandler
 * Brief: 
 *
 * Create Date: 2015/8/11
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: IMemoryOperationHandler.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.memory;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

public interface IMemoryOperationHandler
{
    enum MemoryType
    {
        THREE_PMM(0x0007, new ThreePMM()),
        COMMS_EEPROM(0x0004, new CommsEEPROM());
        
        /**
         * The value of the instance which is defined by Continua.
         */
        private final int VALUE;
        
        /**
         * The memory operation handler.
         */
        private final IMemoryOperationHandler mHandler;
        
        /**
         * Put the defined value into the instance.
         * 
         * @param value : The value of the type.
         *        Range: Refer to the definition of MemoryType.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param handler : The handler of memory operation.
         *        Range: Refer to the definition of MemoryType.
         *        Unit: IMemoryOperationHandler.
         *        Scaling: 1.
         * 
         * see VALUE [in]
         * see mHandler [in]
         */
        private MemoryType(int value, IMemoryOperationHandler handler)
        {
            VALUE = value;
            mHandler = handler;
        }
        
        /**
         * Return the type value of this instance.
         *
         * see VALUE [out]
         *
         * return int [out]: The value of memory type.
         *        Range: Refer to the definition of MemoryType.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        public int getValue()
        {
            return VALUE;
        }
        
        /**
         * Return the corresponding handler of this instance.
         *
         * see mHandler [out]
         *
         * return IMemoryOperationHandler [out]: The handler instance.
         *        Range: Valid object of IMemoryOperationHandler.
         *        Unit: IMemoryOperationHandler.
         *        Scaling: 1.
         */
        public IMemoryOperationHandler getHandler()
        {
            return mHandler;
        }
        
        /**
         * Return the instance of this enumeration according to the input id.
         *
         * @param id : The id of defined memory type.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         *  
         * return MemoryType [out]: The instance of MemoryType.
         *        Range: Valid object of MemoryType.
         *        Unit: MemoryType.
         *        Scaling: 1.
         * 
         * throws ArgumentErrorException if the id is not supported.
         */
        public static MemoryType getTypeById(int id) throws ArgumentErrorException
        {
            MemoryType result = null;
            
            for (MemoryType item : MemoryType.values())
            {
                int valueOfId = item.getValue();
                
                if (valueOfId == id)
                {
                    result = item;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException("This id [" + id + "] is not supported.");
            }
            
            return result;
        }
    }
    
    /**
     * Read the data from the assigned position according to the command.
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
    void read(ContinuaCommandSet commandSet, RPCCommandInvocation invocation);
    
    /**
     * Write the data to the assigned position according to the command.
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
    void write(ContinuaCommandSet commandSet, RPCCommandInvocation invocation);
}
