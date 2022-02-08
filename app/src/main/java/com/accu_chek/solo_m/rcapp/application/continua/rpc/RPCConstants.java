/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants
 * Brief: 
 *
 * Create Date: 2015/7/3
 * $Revision: 23531 $
 * $Author: kevenwu $
 * $Id: RPCConstants.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc;

import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

public interface RPCConstants
{    
    public static final String SOUND_PATH = "/system/altek/Sounds/Comm_Complete.wav";
    
    public enum RPCArgumentType
    {
        RPC_ARG_TYPE_EMPTY(0x0001),
        RPC_ARG_TYPE_INT16(0x0002),
        RPC_ARG_TYPE_UINT16(0x0003),
        RPC_ARG_TYPE_UINT16_ARRAY(0x0004),
        RPC_ARG_TYPE_SFLOAT(0x0005),
        RPC_ARG_TYPE_STRING(0x0006),
        RPC_ARG_TYPE_DATE(0x0007),
        RPC_ARG_TYPE_TIME(0x0008),
        RPC_ARG_TYPE_CODEKEY(0x0009),
        RPC_ARG_TYPE_BG_REMINDER(0x000A),
        RPC_ARG_TYPE_TIME_REMINDER(0x000B),
        RPC_ARG_TYPE_DATE_REMINDER(0x000C),
        RPC_ARG_TYPE_INFUSION_REMINDER(0x000D),
        RPC_ARG_TYPE_TARGET_RANGE(0x000E),
        RPC_ARG_TYPE_ST_PARAMETER(0x000F),
        RPC_ARG_TYPE_OPERATIONAL_STATE(0x0010),
        RPC_ARG_TYPE_UINT8_ARRAY(0x0011),
        RPC_ARG_TYPE_UINT32(0x0012),
        RPC_ARG_TYPE_VERSION_INFO(0x0013),
        RPC_ARG_TYPE_CARB_CONVERSION(0x0014),
        RPC_ARG_TYPE_ST_REMINDER(0x0015),
        RPC_ARG_TYPE_TIMEBLOCK(0x0016),
        RPC_ARG_TYPE_HEALTH_EVENT(0x0017),
        RPC_ARG_TYPE_SIGNAL_MODE(0x0018),
        RPC_ARG_TYPE_REMINDER_SOUND_NAME(0x0019),
        RPC_ARG_TYPE_BITS_16(0x001A),
        RPC_ARG_TYPE_BITS_32(0x001B),
        RPC_ARG_TYPE_INT32(0x001C),
        RPC_ARG_TYPE_UINT32_ARRAY(0x001D),
        RPC_ARG_TYPE_WRITE_MEMORY_DATA(0x1004);
        
        /**
         * The argument type of this enumeration.
         */
        public final int mArgumentType;
        
        /**
         * Put the argument type value into the enumeration.
         * 
         * @param argumentType : The value of argument type.
         *        Range: Refer to the definition of RPCArgumentType.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        private RPCArgumentType(int argumentType)
        {
            mArgumentType = argumentType;
        }
        
        /**
         * Return the argument type of this enumeration.
         *
         * see mArgumentType [out]
         *
         * return int [out]: The value of argument type.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        public int getArgumentType()
        {
            return mArgumentType;
        }
        
        /**
         * Return the RPCArgumentType according to the input id.
         *
         * @param typeId : The value of type id.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         * 
         * return RPCArgumentType [out]: 
         * 
         * throws ArgumentErrorException if the input type id is not supported.
         */
        public static RPCArgumentType getTypeById(int typeId) throws ArgumentErrorException
        {
            RPCArgumentType result = null;
            
            for (RPCArgumentType item : RPCArgumentType.values())
            {
                int valueOfId = item.getArgumentType();
                
                if (valueOfId == typeId)
                {
                    result = item;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException("This type [" + typeId + "] is not supported.");
            }
            
            return result;
        }
    }
    
    public enum RPCEventType
    {
        /**
         * Indicates the response of received read RPC command.
         */
        MDC_NOTI_RPC_COMMAND_RESPONSE(0xF003),
        
        /**
         * Indicates the response of received write RPC command.
         */
        MDC_NOTI_RPC_ERROR_RESPONSE(0xF004);
        
        /**
         * The value of RPC event type.
         */
        private final int mEventType;
        
        /**
         * Put the event type value to the enumeration instantce.
         * 
         * @param eventType : The value of event type.
         *        Range: Refer to the definition of RPCEventType.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * see mEventType [in]        
         */
        private RPCEventType(int eventType)
        {
            mEventType = eventType;
        }
        
        /**
         * Return the type of this enumeration.
         *
         * see mEventType [out]
         *
         * return int [out]: The value of event type.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        public int getType()
        {
            return mEventType;
        }
    }
    
    public interface RPCErrorResponse
    {
        /**
         * Indicates that no errors have occurred during execution of the command.
         */
        public static final int RPC_ERR_NO_ERRORS = 0x0000;
        
        /**
         * Indicates a formatting error in the APDU, when lengths of data entities 
         * are not correct.
         */
        public static final int RPC_ERR_BLOCK_LENGTH = 0x0002;
        
        /**
         * Either the number of parameters or the content is not valid.
         */
        public static final int RPC_ERR_INVALID_PARAMETERS = 0x0003;
        
        /**
         * Indicates that the APDU and command is accepted, but that the command
         * parameters are not valid, e.g. out of range for the device.
         */
        public static final int RPC_ERR_INVALID_DATA = 0x0004;
        
        /**
         * Indicates that the device does not support this command.
         */
        public static final int RPC_ERR_UNRECOGNIZED_CMD = 0x0005;
        
        /**
         * Indicates that the command is recognized, but the parameter is not.
         */
        public static final int RPC_ERR_UNRECOGNIZED_FEATURE = 0x0006;
        
        /**
         * Indicates that a command was aborted due to e.g. user intervention 
         * or the device becomes busy with another task.
         */
        public static final int RPC_ERR_ABORTED_CMD = 0x0007;
        
        /**
         * Indicates that the command cannot be completed due to a hardware error.
         * Manager must invoke Read and Clear Status command to retrieve extended
         * error information.
         */
        public static final int RPC_ERR_HARDWARE = 0x0008;
        
        /**
         * Indicates that the command cannot be completed due to an application 
         * error. Manager must invoke Read and Clear Status command to retrieve
         * extended error information.
         */
        public static final int RPC_ERR_APPLICATION = 0x0009;
        
        /**
         * Indicates that a manager issued a command that does not exist for the
         * currently authorized role, or that the digital signature check of a file fails.
         */
        public static final int RPC_ERR_SECURITY_ERROR = 0x000A;
        
        /**
         * Indicates that the timeout value specified for the command has been exceeded.
         */
        public static final int RPC_ERR_COMMAND_TIMEOUT_EXCEEDED = 0x000B;
    }
}
