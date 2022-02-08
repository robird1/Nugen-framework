/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.ContinuaConstants
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: ContinuaConstants.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.commandcontorller;

import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;

/**
 * All constants of Continua used. 
 */
public interface ContinuaConstants
{
    /**
     * The request type of Continua Agent module.
     */
    public interface ContinuaRequestType
    {
        /**
         * The definition of request serial number.
         */
        public static final int PRODUCT_IDENTIFIER = 0x0066;
        
        /**
         * The definition of request PID number.
         */
        public static final int PID_NUMBER = 0x0069;
        
        /**
         * The definition of request model number.
         */
        public static final int MODEL_NUMBER = 0x0096;
        
        /**
         * The definition of request the count PM segment entry.
         */
        public static final int PMSEG_ENTRY_COUNT = 0x0099;
        
        /**
         * The definition of request the total count of segment.
         */
        public static final int PMSEG_TOTAL_COUNT = 0x005A;
        
        /**
         * The definition of request PM segment entry.
         */
        public static final int PMSEG_ENTRY = 0x0303;
        
        /**
         * The definition of clear all segment data.
         */
        public static final int PMSEG_CLEAR = 0x0055;
        
        /**
         * The definition of request absolute time.
         */
        public static final int ABSOLUTE_TIME = 0x00A5;
        
        /**
         * The definition of request power status.
         */
        public static final int POWER_STATUS = 0x00AA;
        
        /**
         * The definition of request battery level.
         */
        public static final int BATTERY_LEVEL = 0x00C3;
        
        /**
         * The definition of request software version.
         */
        public static final int SW_VERSION = 0x00CC;
        
        /**
         * The definition of request firmware version.
         */
        public static final int FW_VERSION = 0x00F0;
        
        /**
         * The definition of request serial number in string.
         */
        public static final int SERIAL_NUMBER = 0x003C;
        
        /**
         * The definition of error message handler.
         */
        public static final int EMWR_HANDLE = 0x00FF;
    }
    
    /**
     * The configuration id of Continua 10417.
     */
    public interface GlucoseConfigId
    {
        /**
         * The configuration id of standard configuration.
         */
        public static final int STANDARD = 0x06A5;
        
        /**
         * The configuration id of extend configuration without RPC.
         */
        public static final int EXTEND_WITHOUT_RPC = 0x4000;
        
        /**
         * The configuration id of extend configuration with RPC.
         */
        public static final int EXTEND_WITH_RPC = 0x5000;
    }
    
    /**
     * The configuration id of Continua 10417.
     */
    public interface InsulinConfigId
    {
        /**
         * The configuration id of standard configuration.
         */
        public static final int STANDARD = 0x076C;
        
        /**
         * The configuration id of extend configuration without RPC.
         */
        public static final int EXTEND_WITHOUT_RPC = 0x4001;
        
        /**
         * The configuration id of extend configuration with RPC.
         */
        public static final int EXTEND_WITH_RPC = 0x5001;
    }
    
    /**
     * The segment id of Continua 10417.
     */
    public interface GlucoseSegmentId
    {
        /**
         * The segment id of blood glucose.
         */
        public static final int BLOOD_GLUCOSE = HammingDistance.SAFETY_NUMBER_VALUE_0001;
        
        /**
         * The segment id of control solution.
         */
        public static final int CONTROL_SOLUTION = HammingDistance.SAFETY_NUMBER_VALUE_0002;
        
        /**
         * The segment id of context carbohydrate.
         */
        public static final int CONTEXT_CARBOHYDRATE = HammingDistance.SAFETY_NUMBER_VALUE_0005;
        
        /**
         * The segment id of context meal.
         */
        public static final int CONTEXT_MEAL = HammingDistance.SAFETY_NUMBER_VALUE_0004;
        
        /**
         * The segment id of device status.
         */
        public static final int DEVICE_STATUS = HammingDistance.SAFETY_NUMBER_VALUE_0003;
        
        /**
         * The segment id of health events.
         */
        public static final int HEALTH_EVENTS = HammingDistance.SAFETY_NUMBER_VALUE_0006;
        
        /**
         * The segment id of patient record.
         */
        public static final int PATIENT_RECORD = HammingDistance.SAFETY_NUMBER_VALUE_0007;
        
        /**
         * The segment id of note record.
         */
        public static final int NOTE_RECORD = HammingDistance.SAFETY_NUMBER_VALUE_0008;
        
        /**
         * The segment id of control glucose record.
         */
        public static final int CONTROL_GLUCOSE = HammingDistance.SAFETY_NUMBER_VALUE_0009;
        
        /**
         * The segment id of all data in the relevant configuration id.
         */
        public static final int ALL = HammingDistance.SAFETY_NUMBER_VALUE_0010;
    }
    
    /**
     * The error code of Continua communication.
     */
    public interface ErrorCode
    {
        /**
         * The error code of no record found.
         */
        public static final int NO_RECORD_FOUND = 0xFFFF;
        
        /**
         * The error code of command not supported.
         */
        public static final int COMMAND_NOT_SUPPORTED = 0xFFFE;
        
        /**
         * The error code of application error.
         */
        public static final int ERROR_APPLICATION = 0xFFFD;
    }
    
    /**
     * The key type of authentication role. 
     */
    public interface KeySelectorType
    {
        /**
         * Key type of Roche IM.
         */
        public static final int ROCHE_IM = 0x0003;
        
        /**
         * Key type of Roche internal.
         */
        public static final int ROCHE_INTERNAL = 0x0004;
    }
    
    /**
     * The name of file which stored Roche IM role public key of Continua.
     */
    public static final String NAME_IM_KEY = "RocheIMPubKey";
    
    /**
     * The name of file which stored Roche internal role public key of Continua.
     */
    public static final String NAME_INTERNAL_KEY = "RochInternalPubKey";
}
