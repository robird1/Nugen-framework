/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.customizedhwmanager.
 * CustomizedHWManager
 * Brief:
 * The class defines service names, stores service fetcher map and services map
 * for reusing the service.
 * 
 * Create Date: 2015/1/19
 * $Revision: 22746 $
 * $Author: VictorChen $
 * $Id: CustomizedHWManager.java 22746 2015-10-28 10:11:38Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.customizedhwmanager;

import java.util.ArrayList;
import java.util.HashMap;

import android.os.IBinder;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader.ADCManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader.IADC;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.ILED;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.IHaptic;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.VibrationManager;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.IUART;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;

/**
 * The class defines service names, stores service fetcher map and services map
 * for reusing the service.
 */
final class CustomizedHWManager
{
    /**
     * Customized HW manager service names
     */
    interface CUSTOMIZED_SERVICE_NAME
    {
        /**
         * UART Service name
         */
        static final String UART_SERVICE = "uartinterface";

        /**
         * LED Service name
         */
        static final String LED_SERVICE = "ledcontrol";

        /**
         * Vibration Service name
         */
        static final String VIBRATION_SERVICE = "hapticcontrol";
        
        static final String ADCREADER_SERVICE = "adcreader";
    }

    /**
     * Service manager map
     */
    private static final ArrayList<Object> SERVICE_MAP = new ArrayList<Object>();

    /**
     * Service fetcher map
     */
    private static final HashMap<String, ServiceFetcher> SERVICE_FETCHER_MAP = new HashMap<String, ServiceFetcher>();

    /**
     * Total service quantity
     */
    private static int mIndexCounter = 0;

    /**
     * The class can find or create a service manager
     */
    private static class ServiceFetcher
    {
        /**
         * The service's index.
         */
        private Integer nIndex = -1;

        /**
         * ServiceFetcher's constructor
         * 
         * @param None [in]
         */
        public ServiceFetcher()
        {
            // Empty for static code analysis
        }

        /**
         * Set the index of ServiceFetcher object.
         * 
         * see mIndexCounter [in] This global variable in CustomizedHWManager is
         * referred for the range of the maximum index value.
         * see nIndex [in] This global variable in ServiceFetcher is referred
         * for recording the index of the ServiceFetcher object.
         * 
         * @param index [in] index number
         * Range: 0 .. mIndexCounter - 1
         * Unit: int
         * Scaling: 1
         * 
         * return None
         */
        void setIndex(int index)
        {
            synchronized (nIndex)
            {
                nIndex = index;
            }
        }

        /**
         * Check if the manager has been in SERVICE_MAP and get it, or create a service manager and put into SERVICE_MAP.
         * 
         * see SERVICE_MAP [in] This global variable in CustomizedHWManager is referred for getting items in the map.
         * see nIndex [in] This global variable in CustomizedHWManager is referred for getting or setting Service Fetcher object into SERVICE_MAP.
         * see mIndexCounter [in] This global variable in CustomizedHWManager is referred for service quantity.
         * 
         * @param None [in]
         * 
         * return Object that is the manager of the service. Otherwise, return null if the service does not exist.
         * Range: null, UARTManager, LEDManager, VibrationManager or ADCManager (Object can transfer into service manager)
         * Unit: Object
         * Scaling: 1
         * 
         * throw OperationFailException if the method of creating service manager is not override.
         */
        Object getService() throws OperationFailException
        {
            ArrayList<Object> cache = SERVICE_MAP;
            Object service = null;

            synchronized (nIndex)
            {
                synchronized (cache)
                {
                    int serviceSize = cache.size();

                    if (serviceSize == 0)
                    {

                        // Initialize the cache vector on first access.
                        // At this point sNextPerContextServiceCacheIndex
                        // is the number of potential services that are
                        // cached per-Context.
                        for (int i = 0; i < mIndexCounter; i++)
                        {
                            cache.add(null);
                        }
                    }
                    else
                    {
                        service = cache.get(nIndex);
                    }

                    if (service == null)
                    {
                        service = createService();

                        cache.set(nIndex, service);
                    }
                    else
                    {
                        // Empty for static code analysis
                    }

                    return service;
                }
            }
        }

        /**
         * The function should be override to create service. Otherwise, it throws OperationFailException.
         * 
         * @param None [in]
         * 
         * return Object that always throw RuntimeException and never return. Object is provided by Android SDK.
         * Range: Valid Android Object
         * Unit: Object
         * Scaling: 1
         * 
         * throw OperationFailException if the function is not override.
         * 
         */
        Object createService() throws OperationFailException
        {
            throw new OperationFailException("The function should be override.");
        }
    }

    static
    {
        addServiceFetcher(CUSTOMIZED_SERVICE_NAME.UART_SERVICE,
                new ServiceFetcher()
                {
                    /**
                     * Get UART service and put it into UART manager.
                     * 
                     * see CUSTOMIZED_SERVICE_NAME.UART_SERVICE [in] This global variable in CustomizedHWManager is referred for the service name of UART.
                     * 
                     * @param None [in]
                     * return Object that is UART manager.
                     *         Range: UARTManager (Object can transfer into service manager)
                     *         Unit: Object
                     *         Scaling: 1
                     * @throws OperationFailException if UART service is null.
                     */
                    public Object createService() throws OperationFailException
                    {
                        IBinder binder = CustJavaFrameworkManager.getCustomizedManager(CUSTOMIZED_SERVICE_NAME.UART_SERVICE);
                        
                        if(binder == null)
                        {
                            throw new OperationFailException("UART service is null.");
                        }
                        else
                        {
                            // Empty for static code analysis
                        }

                        return new UARTManager(IUART.Stub.asInterface(binder));
                    }
                });

        addServiceFetcher(CUSTOMIZED_SERVICE_NAME.LED_SERVICE,
                new ServiceFetcher()
                {
                    /**
                     * Get LED service and put it into LED manager.
                     * 
                     * see CUSTOMIZED_SERVICE_NAME.LED_SERVICE [in] This global variable in CustomizedHWManager is referred for the service name of LED.
                     * 
                     * @param None [in]
                     *            
                     * return Object that is LED manager.
                     * Range: LEDManager (Object can transfer into service manager)
                     * Unit: Object
                     * Scaling: 1
                     * 
                     * @throws OperationFailException if LED service is null.
                     */
                    public Object createService() throws OperationFailException
                    {
                        IBinder binder = CustJavaFrameworkManager.getCustomizedManager(CUSTOMIZED_SERVICE_NAME.LED_SERVICE);
                        
                        if(binder == null)
                        {
                            throw new OperationFailException("LED service is null.");
                        }
                        else
                        {
                            // Empty for static code analysis
                        }
                        
                        return new LEDManager(ILED.Stub.asInterface(binder));
                    }
                });

        addServiceFetcher(CUSTOMIZED_SERVICE_NAME.VIBRATION_SERVICE,
                new ServiceFetcher()
                {
                    /**
                     * Get vibration service and put it into vibration manager.
                     * 
                     * see CUSTOMIZED_SERVICE_NAME.VIBRATION_SERVICE [in] This global variable in CustomizedHWManager is referred for the service name of Vibration.
                     * 
                     * @param None [in]
                     * 
                     * return Object that is vibration manager.
                     * Range: VibrationManager (Object can transfer into service manager)
                     * Unit: Object
                     * Scaling: 1
                     * 
                     * throw OperationFailException if Haptic service is null.
                     */
                    public Object createService() throws OperationFailException
                    {
                        IBinder binder = CustJavaFrameworkManager.getCustomizedManager(CUSTOMIZED_SERVICE_NAME.VIBRATION_SERVICE);
                        
                        if(binder == null)
                        {
                            throw new OperationFailException("Haptic service is null.");
                        }
                        else
                        {
                            // Empty for static code analysis
                        }

                        return new VibrationManager(IHaptic.Stub
                                .asInterface(binder));
                    }
                });
        
        addServiceFetcher(CUSTOMIZED_SERVICE_NAME.ADCREADER_SERVICE,
                new ServiceFetcher()
                {
                    /**
                     * Get adc reader service and put it into adc manager.
                     * 
                     * see CUSTOMIZED_SERVICE_NAME.ADCREADER_SERVICE [in] This global variable in CustomizedHWManager is referred for the service name of ADC Reader.
                     * 
                     * @param None [in]
                     * 
                     * return Object that is ADC Reader.
                     * Range: ADCManager (Object can transfer into service manager)
                     * Unit: Object
                     * Scaling: 1
                     * 
                     * throw OperationFailException if ADC service is null.
                     */
                    public Object createService() throws OperationFailException
                    {
                        IBinder binder = CustJavaFrameworkManager.getCustomizedManager(CUSTOMIZED_SERVICE_NAME.ADCREADER_SERVICE);
                        
                        if(binder == null)
                        {
                            throw new OperationFailException("ADC service is null.");
                        }
                        else
                        {
                            // Empty for static code analysis
                        }

                        return new ADCManager(IADC.Stub.asInterface(binder));
                    }
                });
    }

    /**
     * Given service name and ServiceFetcher object, the function put the service into service fetcher map.
     * 
     * see mIndexCounter [in] This global variable is referred for recording service number and providing the number to be service index.
     * see SERVICE_FETCHER_MAP [in] This global variable is referred for putting service fetcher into this global map.
     * 
     * @param serviceName [in] service string index in CustomizedHWManager
     *            Range: CUSTOMIZED_SERVICE_NAME (UART_SERVICE, LED_SERVICE, VIBRATION_SERVICE or ADCREADER_SERVICE in CustomizedHWManager)
     *            Unit: String
     *            Scaling: 1
     * @param fetcher [in] Service fetcher object that define the function for finding and creating service manager. For detail of this object, see ServiceFetcher.
     *            Range: Valid ServiceFetcher object
     *            Unit: ServiceFetcher
     *            Scaling: 1
     * 
     * return None [out]
     */
    private static void addServiceFetcher(String serviceName,
            ServiceFetcher fetcher)
    {
        fetcher.setIndex(mIndexCounter);
        SERVICE_FETCHER_MAP.put(serviceName, fetcher);

        mIndexCounter++;
    }

    /**
     * Given service name, the function will get the service manager from service fetcher map.
     * 
     * see SERVICE_FETCHER_MAP [in] This global variable is referred for getting service fetcher from this global map.
     * 
     * @param serviceName[in] service string index in CustomizedHWManager
     * Range: CUSTOMIZED_SERVICE_NAME (UART_SERVICE, LED_SERVICE, VIBRATION_SERVICE or ADCREADER_SERVICE in CustomizedHWManager)
     * Unit: String
     * Scaling: 1
     * 
     * return Object that is the manager of the service. Otherwise, return null if the service or creating method of the service does not exist.
     * Range: null, UARTManager, LEDManager, VibrationManager or ADCManager (Object can transfer into service manager)
     * Unit: Object
     * Scaling: 1
     */
    static Object getSystemService(String serviceName)
    {
        ServiceFetcher fetcher = SERVICE_FETCHER_MAP.get(serviceName);
        Object fetcherObject = null;

        if (fetcher != null)
        {
            try
            {
                fetcherObject = fetcher.getService();
            }
            catch (OperationFailException e)
            {
                fetcherObject = null;
            }
            finally
            {
                // Empty for static code analysis
            }
        }
        else
        {
            // Empty for static code analysis
        }

        return fetcherObject;
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
