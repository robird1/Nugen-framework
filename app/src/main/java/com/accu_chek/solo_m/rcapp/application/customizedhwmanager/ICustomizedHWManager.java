/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustmizedHWManager
 * Brief: The class provides an interface to modules in RC APP to get customized HW service.
 *
 * Create Date: 2015/3/9
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: ICustomizedHWManager.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.customizedhwmanager;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.CustomizedHWManager.CUSTOMIZED_SERVICE_NAME;

/**
 * The class provides an interface to modules in RC APP to get customized HW service.
 */
public abstract class ICustomizedHWManager
{
    /**
     * UART Service name
     */
    public static final String UART_SERVICE = CUSTOMIZED_SERVICE_NAME.UART_SERVICE;
    /**
     * LED Service name
     */
    public static final String LED_SERVICE = CUSTOMIZED_SERVICE_NAME.LED_SERVICE;
    /**
     * Vibration Service name
     */
    public static final String VIBRATION_SERVICE = CUSTOMIZED_SERVICE_NAME.VIBRATION_SERVICE;
    /**
     * ADC Reader Service name
     */
    public static final String ADCREADER_SERVICE = CUSTOMIZED_SERVICE_NAME.ADCREADER_SERVICE;
    
    
    /**
     * Given service name, the function will get the service manager from service fetcher map. 
     *
     * @param serviceName [in] service string index in ICustomizedHWManager 
     * Range: UART_SERVICE, LED_SERVICE, VIBRATION_SERVICE or ADCREADER_SERVICE in ICustomizedHWManager
     * Unit: String
     * Scaling: 1
     * 
     * return Object that is the manager of the service. Otherwise, return null if service name does not exist or service manager cannot be created.
     * Range: null, UARTManager, LEDManager, VIBRATIONManager or ADCManager (Object can transfer into service manager)
     * Unit: Object
     * Scaling: 1
     */
    public final static Object getSystemService(String serviceName)
    {
        Object manager = CustomizedHWManager.getSystemService(serviceName);
        
        return manager;
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
