/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.IContinuaSegmentHandler
 * Brief: 
 *
 * Create Date: 2015/3/20
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: IContinuaConfigHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.confighandler;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseConfigId;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

/**
 * The interface is used to implement a configuration handler to handle each
 * configuration id.
 */
public interface IContinuaConfigHandler
{
    /**
     * Enum of device configuration id handler, it will delegate the command to
     * relevant configuration handler.
     */
    enum ConfigureId
    {
        /**
         * The instance of glucose standard configuration id.
         */
        STANDARD(GlucoseConfigId.STANDARD, new GlucoseStandard()),
        
        /**
         * The instance of glucose extend without RPC configuration id.
         */
        EXTEND_WITHOUT_RPC(GlucoseConfigId.EXTEND_WITHOUT_RPC, new GlucoseExtendWithoutRPC()),
        
        /**
         * The instance of glucose extend with RPC configuration id.
         */
        EXTEND_WITH_RPC(GlucoseConfigId.EXTEND_WITH_RPC, new GlucoseExtendWithRPC());
        
        /**
         * The value of configuration id.
         */
        private final int mConfigId;
        
        /**
         * The handler is used to handle the command of each configuration id.
         */
        private IContinuaConfigHandler mHandler = null;
        
        /**
         * Constructs a new ConfigureId and assigns the target configuration id 
         * and configuration handler.
         * 
         * @param configId : The target configuration id.
         *        Range: Refer to this enumeration definition.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param handler : The target configuration handler.
         *        Range: Refer to this enumeration definition.
         *        Unit: IContinuaConfigHandler.
         *        Scaling: 1.
         *        
         * see mConfigId [out]
         * see mHandler [out]        
         */
        private ConfigureId(int configId, IContinuaConfigHandler handler)
        {
            mConfigId = configId;
            mHandler = handler;
        }
        
        /**
         * Return the configuration id.
         *
         * see mConfigId [out]
         *
         * return int [out]: The configuration id of this enumeration.
         *         Range: Refer to the definition of this enumeration.
         *         Unit: Integer.
         *         Scaling: 1.
         */
        public int getConfigId()
        {
            return mConfigId;
        }
        
        /**
         * Return the command handler of this enumeration.
         * 
         * see mHandler [out]
         *
         * return IContinuaConfigHandler [out]: The command handler.
         *        Range: Valid object of IContinuaConfigHandler.
         *        Unit: IContinuaConfigHandler.
         *        Scaling: 1.
         */
        public IContinuaConfigHandler getHandler()
        {
            return mHandler;
        }
        
        /**
         * According to the input configuration id to return the relevant 
         * enumeration.
         *
         * @param configId : The input configId for getting the relevant
         *     enumeration.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * return ConfigureId [out]: The corresponding enumeration of the input
         *     configuration id.
         *         Range: Valid object of ContigureId.
         *         Unit: ConfigureId.
         *         Scaling: 1.
         * 
         * throw ArgumentErrorException if the configure id is not supported. 
         */
        public static ConfigureId getConfigureId(int configId) 
                throws ArgumentErrorException
        {
            ConfigureId result = null;
            
            for (ConfigureId configureId : ConfigureId.values())
            {
                int valueOfId = configureId.getConfigId();
                
                if (configId == valueOfId)
                {
                    result = configureId;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException(
                        "This configure id: " + configId + " is not supported.");
            }
            
            return result;
        }
    }
    
    /**
     * According to the segment id to do the corresponding action for Continua.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None
     */
    void getData(ContinuaCommandSet commandSet);
    
    /**
     * According to the segment id to calculate the count for Continua.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None
     */
    void getCount(ContinuaCommandSet commandSet);
    
    /**
     * According to the configuration id to clear the data from database.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    void clearAllSegment(ContinuaCommandSet commandSet);
}
