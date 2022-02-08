/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config.IConfigurationParameterHandler
 * Brief: 
 *
 * Create Date: 2015/7/10
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: IConfigurationParameterHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.config;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;

public interface IConfigurationParameterHandler
{
    public enum ConfigParameter
    {
        RPC_MANUFACTURING_PROCESS_FLAGS(0x0001, new ManufacturingProcessFlags()),
        RPC_CONFIGURATION_TEST_DATE(0x000B, new ConfigurationTestDate()),
        RPC_MINIMUM_CUSTOMER_DATE(0x0015, new MinimumCustomerDate()),
        RPC_SUPPORTED_LANGUAGES(0x0016, new SupportedLanguages()),
        RPC_SELECTED_LANGUAGE(0x0018, new SelectedLanguage()),
        RPC_BG_HI_LO(0x001F, new BGHiAndLo()),
        RPC_SSS_INSTRUMENT_NAME(0x0009, new SSSInstrumentName()),
        RPC_MODEL_NUMBER(0x0006, new ModelNumber()),
        RPC_SUPERPIN(0x0022, new SuperPIN());
        
        /**
         * The id of configuration parameter enumeration.
         */
        private final int PARAMETER_ID;
        
        /**
         * The command handler of configuration parameter.
         */
        private final IConfigurationParameterHandler mHandler;
        
        /**
         * Put the parameter id and handler to the instance.
         * 
         * @param id : The id of configuration parameter.
         *        Range: Refer to the definition of ConfigParameter.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param handler : The corresponding handler.
         *        Range: Valid object of IConfigurationParameterHandler.
         *        Unit: IConfigurationParameterHandler.
         *        Scaling: 1.
         * 
         * see PARAMETER_ID [in]
         * see mHandler [in]
         */
        private ConfigParameter(int id, IConfigurationParameterHandler handler)
        {
            PARAMETER_ID = id;
            mHandler = handler;
        }
        
        /**
         * Return the id of this enumeration.
         *
         * see PARAMETER_ID [out]
         *
         * return int [out]: The enumeration id.
         */
        public int getParameterId()
        {
            return PARAMETER_ID;
        }
        
        /**
         * Return the command handler of this enumeration.
         *
         * see mHandler [out]
         *
         * return IConfigurationParameterHandler [out]: The command handler.
         *        Range: Valid object of IConfigurationParameterHandler.
         *        Unit: IConfigurationParameterHandler.
         *        Scaling: 1.
         */
        public IConfigurationParameterHandler getHandler()
        {
            return mHandler;
        }
        
        /**
         * Return the enumeration depend on the input parameter id.
         *
         * @param parameterId : The input id of configuration parameter.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         * 
         * return ConfigParameter [out]: The corresponding ConfigParameter instance.
         *        Range: Valid object of ConfigParameter.
         *        Unit: ConfigParameter.
         *        Scaling: 1.
         * throws ArgumentErrorException if the input parameter id is not supported. 
         */
        public static ConfigParameter getParameterById(int parameterId) 
                throws ArgumentErrorException
        {
            ConfigParameter result = null;
            
            for (ConfigParameter item : ConfigParameter.values())
            {
                int valueOfId = item.getParameterId();
                
                if (valueOfId == parameterId)
                {
                    result = item;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException("This parameter " + parameterId
                        + " is not supported.");
            }
            
            return result;
        }
    }
    
    /**
     * Delegate the read request command to corresponding parameter handler.
     *
     * @param commandSet : The instance of Continua command set.
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
     * Delegate the change request command to corresponding parameter handler.
     *
     * @param commandSet : The instance of Continua command set.
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
    void change(ContinuaCommandSet commandSet, RPCCommandInvocation invocation);
}
