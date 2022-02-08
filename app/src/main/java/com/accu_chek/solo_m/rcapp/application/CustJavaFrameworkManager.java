/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: CustJavaFrameworkManager
 * Brief: The initial entry of the system function
 *
 * Create Date: 9/17/2015
 * $Revision: 25192 $
 * $Author: KiddYeh $
 * $Id: CustJavaFrameworkManager.java 25192 2015-12-01 02:34:08Z KiddYeh $
 */
 
package com.accu_chek.solo_m.rcapp.application;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import android.content.Context;
import android.content.Intent;
import android.os.IBinder;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmControl;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBGMControl;
import com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrix;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.adcreader.ADCManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.vibrationcontrol.VibrationManager;
import com.accu_chek.solo_m.rcapp.application.fwupdate.FWUpdateMain;
import com.accu_chek.solo_m.rcapp.application.fwupdate.FWUpdateResume;
import com.accu_chek.solo_m.rcapp.application.fwupdate.IFWUpdateMain;
import com.accu_chek.solo_m.rcapp.application.power.CustPowerManager;
import com.accu_chek.solo_m.rcapp.application.power.ICustPowerManager;
import com.accu_chek.solo_m.rcapp.application.selftest.IPOSTManager;
import com.accu_chek.solo_m.rcapp.application.selftest.POSTManager;
import com.accu_chek.solo_m.rcapp.application.setting.generalsetting.IPCUpdateConfig;
import com.accu_chek.solo_m.rcapp.application.setting.generalsetting.UpdateConfig;
import com.accu_chek.solo_m.rcapp.application.timemanagement.ITimeManagementService;
import com.accu_chek.solo_m.rcapp.application.timemanagement.TimeManagementService;
import com.accu_chek.solo_m.rcapp.application.usbconnection.IPCConnect;
import com.accu_chek.solo_m.rcapp.application.usbconnection.PCConnect;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;
import com.accu_chek.solo_m.rcapp.application.util.RCSystemProperty;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;
import com.accu_chek.solo_m.rcapp.communication.uartuicp.UICPControl;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.IUICommandDispatcher;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.UICommandDispatcher;

/**
* <h1>Cust java framework manager</h1>
* The CustJavaFrameworkManager program implements an application that
* provide service interface to application layer
* 
* @author  Name
* @version 1.0
* @since   YYYY-MM-DD 
*/
public final class CustJavaFrameworkManager
{
    private static final String TAG = "CustJavaFrameworkManager";
    private static final String TAG_SERVICE_THREAD = "CustServiceThread";
    
    public static final String SERVICE_READY = "com.accu_chek.solo_m.rcapp.service.ready";
    
    private static String mClassName = "android.os.ServiceManager";
    
    static 
    {
        System.loadLibrary("custframeworksvr_jni");
    }
    
    
    private static class CustServerThread extends Thread 
    {
    
    	
        
        private Context mContext = null;
    	
        
        public interface TYPE
    	{
        		String POWER = "java.cust.framework.powermanager";
        		String BT = "java.cust.framework.btcontrol";
        		String BGM= "java.cust.framework.bgmcontrolservice";
        		String PCCONNECT = "java.cust.framework.pcconnectservice";
        		String UART = "java.cust.framework.".concat(ICustomizedHWManager.UART_SERVICE);
        		String LED = "java.cust.framework.".concat(ICustomizedHWManager.LED_SERVICE);
        		String VIBRATOR = "java.cust.framework.".concat(ICustomizedHWManager.VIBRATION_SERVICE);
        		String ADC = "java.cust.framework.".concat(ICustomizedHWManager.ADCREADER_SERVICE);
        		String UICP = "java.cust.framework.uicpbinder";
        		String POST = "java.cust.framework.postmanager";
        		String CONFIG_LANGUAGE = "java.cust.framework.languageconfig";
        		String RC_SYSTEM_PROPERTY = "java.cust.framework.rc.system.property";
        		String FW_UPDATE = "java.cust.framework.fwupdate";
    	}
    
    
    	public CustServerThread(Context context) 
    	{
    		mContext = context;
    	}
    	
        @Override
        public void run() 
        {
            
            try {
                
                Debug.printI(TAG_SERVICE_THREAD,"system property service");
                addService(TYPE.RC_SYSTEM_PROPERTY, new RCSystemProperty(mContext));
                
                Debug.printI(TAG_SERVICE_THREAD,"power manager framework service");
                CustPowerManager pm = new CustPowerManager(mContext);
                addService(TYPE.POWER, pm);
                
                Debug.printI(TAG_SERVICE_THREAD,"customized hw manager");
                setCustomizedManager();
                
                Debug.printI(TAG_SERVICE_THREAD,"uicp manager");
                setUICP();
                
//                Debug.printI(TAG_SERVICE_THREAD,"command dispatcher framework service");
//                AddService("java.cust.framework.commanddispatcherservice", new CommandDispatcherService(mContext));
//                
//                Debug.printI(TAG_SERVICE_THREAD,"Comms Subsys control framework service");
//                AddService("java.cust.framework.commscontrolservice", new CommsControlService(mContext));
                
                Debug.printI(TAG_SERVICE_THREAD, "UI command dispatcher");
                UICommandDispatcher uiCommandDispatcher = UICommandDispatcher.getInstance(mContext);
                addService(TYPE.BT, uiCommandDispatcher);
                
                Debug.printI(TAG_SERVICE_THREAD,"Bgm control framework service");
                BgmControl bgmcontrol = BgmControl.getInstance(mContext);
                addService(TYPE.BGM, bgmcontrol);

                
//                Debug.printI(TAG_SERVICE_THREAD,"Pump control framework service");
//                AddService("java.cust.framework.pumpcontrolservice", new PumpControlService(mContext));

                Debug.printI(TAG_SERVICE_THREAD,"time management framework service");
                addService("java.cust.framework.timemanagementservice", new TimeManagementService(mContext));
                
                Debug.printI(TAG_SERVICE_THREAD,"pc connect framework service");
                addService(TYPE.PCCONNECT, new PCConnect());
                
                Debug.printI("CustServerThread",
                        "POST manager framework service");
                addService(TYPE.POST, new POSTManager(
                        mContext));
                
                addService(TYPE.CONFIG_LANGUAGE, new UpdateConfig(mContext));
                
                Debug.printI(TAG, "Add FW update...");
                addService(TYPE.FW_UPDATE, new FWUpdateMain(mContext));

                SendReadyBroadCast();
                       
                initialComms();
                initialBgm();
                initialPOST();
                
                
            } catch (RuntimeException e) {
                
               Debug.printE(TAG_SERVICE_THREAD,"******************************************");
               Debug.printE(TAG_SERVICE_THREAD,"************ Failure starting core service" + e);
            }
        }

        private void SendReadyBroadCast()
        {
            Debug.printI(TAG_SERVICE_THREAD,"[SendReadyBroadCast] enter");
            Intent intent = new Intent();
            intent.setAction(SERVICE_READY);
            mContext.sendBroadcast(intent);
        }    

        
        private void initialComms()
        {
        	Debug.printI(TAG_SERVICE_THREAD,"initialComms");
        	UICommandDispatcher uiCommandDispatcher= UICommandDispatcher.getInstance(mContext);
        	uiCommandDispatcher.init();
        	
        	//start Bt server
        	Intent intent = new Intent();
        	intent.setAction("nugen.bluetooth.IBlutooth");
        	mContext.startService(intent);
        }
        
        private void initialBgm()
        {
            Debug.printI(TAG_SERVICE_THREAD,"initialBgm");
            IBGMControl bgmBinderProxy = CustJavaFrameworkManager.getBGMControlService(mContext);
            try
            {
                bgmBinderProxy.initBgm();
            }
            catch (RemoteException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        
        private void initialPOST()
        {
    
            Debug.printI("CustServerThread", "initial POST");
    
            IPOSTManager postBinderProxy = CustJavaFrameworkManager
                    .getPOSTService(mContext);
    
            try
            {
                postBinderProxy.initial();
                postBinderProxy.runTest();
            }
            catch (RemoteException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        
        }
        
    
    /**
     * 
     * Function Description
     *
     * @param context
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public static final void init(Context context) 
    {
    	Debug.printI(TAG, "Entered the Cust system java framework server!");
        
    	FWUpdateResume fwr = new FWUpdateResume();
        Thread thr = new CustServerThread(context);
        thr.setName("cust.framework.server.CustServerThread");
        thr.start();
		
		MeterParameterMatrix.getMeterParameterMatrixInstance();
		
		//Resume F/W update procedure if needed
		Debug.printI(TAG, "Resume FW update Procedure!");
		fwr.resume();
		
    }
    
    /**
     * 
     * Function Description
     *
     * @param context
     * @return
     * @return ICustPowerManager [out] Delete pre line return if exist. Parameter Description
     */
    public static ICustPowerManager getCustPowerManagerService(Context context) 
    {
        Debug.printI(TAG, "#### java cust power manger service ####");
        
        ICustPowerManager javaSvr = ICustPowerManager.Stub.asInterface(
                    getService(CustServerThread.TYPE.POWER));

        return javaSvr; 
    }
    
    /**
     * 
     * Function Description
     *
     * @param context
     * @return
     * @return ICustPowerManager [out] Delete pre line return if exist. Parameter Description
     */
    public static IRCSystemPeoperty getRCSystemPropertyService(Context context) 
    {
        Debug.printI(TAG, "#### java cust power manger service ####");
        
        IRCSystemPeoperty javaSvr = IRCSystemPeoperty.Stub.asInterface(
                    getService(CustServerThread.TYPE.RC_SYSTEM_PROPERTY));

        return javaSvr; 
    }
    
    /**
     * 
     * Function Description
     *
     * @param context
     * @return
     * @return IUICommandDispatcher [out] Delete pre line return if exist. Parameter Description
     */
    public static IUICommandDispatcher getUICommandDispatcher(Context context) 
    {
    	Debug.printI(TAG, "#### Comms BT control framework service ####");
    	
    	IUICommandDispatcher javaSvr = IUICommandDispatcher.Stub.asInterface(
    			    getService(CustServerThread.TYPE.BT));

    	return javaSvr;	
    }
    
    /**
     * 
     * Function Description
     *
     * @param context
     * @return
     * @return IBGMControl [out] Delete pre line return if exist. Parameter Description
     */
    public static IBGMControl getBGMControlService(Context context) 
    {
    	Debug.printI(TAG, "#### Commmand BGM framewrok service ####");
        
        IBGMControl javaSvr = IBGMControl.Stub.asInterface(
                    getService(CustServerThread.TYPE.BGM));

        return javaSvr; 
    }
    /**
     * The interface of PumpControlService for remote access. 
     * @param mContext
     * @return
     * @author EDLiu
     */
//	public static IPumpControlService getPumpControlService(Context mContext) {
//
//		IPumpControlService javaSvr = IPumpControlService.Stub.asInterface(
//				GetService("java.cust.framework.pumpcontrolservice"));
//		return javaSvr;
//	} 
    /**
     * The interface of TimeManagementService for remote access. 
     * @param mContext
     * @return
     * @author EDLiu
     */
	public static ITimeManagementService getTimeManagementService(Context mContext) {

		ITimeManagementService javaSvr = null;
		javaSvr = ITimeManagementService.Stub.asInterface(
					getService("java.cust.framework.timemanagementservice"));
		return javaSvr;
	} 
	
    /**
     * The interface of POSTManager for selftest access.
     * 
     * @param mContext
     * @return
     * @author JamesLee
     */
    public static IPOSTManager getPOSTService(Context context)
    {
        Debug.printI(TAG, "#### Commmand POST framewrok service ####");

        IPOSTManager javaSvr = IPOSTManager.Stub
                .asInterface(getService("java.cust.framework.postmanager"));

        return javaSvr;
    }
    
	/**
     * The interface of PCConnectService for remote access. 
     * @return
     * @author AdamChen
     */
    public static IPCConnect getPCConnectService() 
    {
        IPCConnect javaSvr = null;
        javaSvr = IPCConnect.Stub.asInterface(
                    getService(CustServerThread.TYPE.PCCONNECT));
        return javaSvr;
    } 
    
	/**
     * Get customized service binder. 
     * Android customized UART service name: "java.cust.framework.uartinterface".
     * Android customized LED service name: "java.cust.framework.ledcontrol".
     * Android customized Vibration service name: "java.cust.framework.hapticcontrol".
     *
     * @param servicename [in] Service name string in ICustomizedHWManager.
     * Range: CUSTOMIZED_SERVICE_NAME  (UART_SERVICE, LED_SERVICE or VIBRATION_SERVICE in ICustomizedHWManager)
     * Unit: N/A
     * Scaling: N/A
     * @return IBinder [out] Return customized service binder.
     * Range: IUART, ILED or IHaptic (IBinder can transfer into service binder)
     * Unit: N/A
     * Scaling: N/A
     */
    public static IBinder getCustomizedManager(String servicename)
    {
        IBinder javaSvr = getService("java.cust.framework.".concat(servicename));
         
         return javaSvr;
    }
    
	/**
     * The interface of UpdateConfigService for remote access.
     *  
     * @return None
     */
    public static IPCUpdateConfig getUpdateConfigService() 
    {

    	return IPCUpdateConfig.Stub.asInterface(
                getService(CustServerThread.TYPE.CONFIG_LANGUAGE));
    } 

    /**
     * The interface to get the FW update handler for remote access.
     *  
     * @return None
     */
    public static IFWUpdateMain getFWUpdateHandler() 
    {

    	return IFWUpdateMain.Stub.asInterface(
                getService(CustServerThread.TYPE.FW_UPDATE));
    } 
    
    /**
     * Add customized service into Android service. 
     * Android customized UART service name: "java.cust.framework.uartinterface".
     * Android customized LED service name: "java.cust.framework.ledcontrol".
     * Android customized Vibration service name: "java.cust.framework.hapticcontrol".
     *
     * @return None [out] 
     */
    static void setCustomizedManager()
    {
        try
        {
            IBinder binder = null;
            
            // uart
            binder = (IBinder) UARTManager.getInterface();
            
            Debug.printI(TAG_SERVICE_THREAD,"uart manager");
            addService(CustServerThread.TYPE.UART, binder);
            
            // led
            binder = (IBinder) LEDManager.getInterface();
            
            Debug.printI(TAG_SERVICE_THREAD,"led control");
            addService(CustServerThread.TYPE.LED, binder);
            
            // vibration
            binder = (IBinder) VibrationManager.getInterface();
            
            Debug.printI(TAG_SERVICE_THREAD,"vibration control");
            addService(CustServerThread.TYPE.VIBRATOR, binder);
            
            // adc
            binder = (IBinder) ADCManager.getInterface();
            
            Debug.printI(TAG_SERVICE_THREAD,"adc reader");
            addService(CustServerThread.TYPE.ADC, binder);
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // the section does nothing
        }
        
    }
    
    /**
     * Get UICP service binder. 
     * Android UICP service name: "java.cust.framework.uicpbinder".
     *
     * @return IBinder [out] Return UICP service binder.
     */
    public static IBinder getUICPBinder()
    {
        IBinder javaSvr = getService(CustServerThread.TYPE.UICP);
         
         return javaSvr;
    }
    
    /**
     * Add UICP service into Android service. 
     * Android UICP service name: "java.cust.framework.uicpbinder".
     *
     * @return None [out] 
     */
    static void setUICP()
    {
        try
        {
            IBinder binder = null;
            
            binder = (IBinder) UICPControl.getInterface();
            
            Debug.printI(TAG_SERVICE_THREAD,"uicp manager");
            
            addService(CustServerThread.TYPE.UICP, binder);
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // the section does nothing
        }
    }
    
    
    /**
     *  add service in service list by service manager
     * @param name
     * @param object
     */
    private static void addService(String name, IBinder object) 
    {
    	
    	Class<?> c = null;
		Method method_addservice = null;
		
		Class<?>[] parameterTypes = {			
				String.class,
				IBinder.class
		};
		
		try {
			c = Class.forName(mClassName);
			method_addservice = c.getDeclaredMethod("addService", parameterTypes);
			method_addservice.setAccessible(true);
			method_addservice.invoke(null, name, object);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	
    }
    
    /**
     *  get service form service list by service manager
     * @param name
     */
    private static IBinder getService(String name) 
    {
    	
    	Class<?> c = null;
		Method method_getservice= null;
		IBinder Bpbinder = null;
		
		Class<?>[] parameterTypes = {			
				String.class
		};
		
		try {
			c = Class.forName(mClassName);
			method_getservice = c.getDeclaredMethod("getService", parameterTypes);
			method_getservice.setAccessible(true);
			Bpbinder = (IBinder) method_getservice.invoke(null, name);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	
    	return Bpbinder;
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [NISQ-20] Remove the input argument "context" from "CustJavaFrameworkManager.getPCConnectService()".
// [NISQ-20] Remove the input argument "context" from "PCConnect()".
// (R20525 2015-10-01 07:16:30 DWYang)
// ----------------------------------------------------------------------------
// [Setting] rename class name
