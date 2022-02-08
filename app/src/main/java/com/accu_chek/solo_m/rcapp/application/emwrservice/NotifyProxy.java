/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRProxy
 * Brief: 
 *
 * Create Date: 2015/5/25
 * $Revision: 24951 $
 * $Author: KayjeanKu $
 * $Id: NotifyProxy.java 24951 2015-11-26 14:02:32Z KayjeanKu $
 */

package com.accu_chek.solo_m.rcapp.application.emwrservice;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.logfile.LogContent;
import com.accu_chek.solo_m.rcapp.application.logfile.LogEmwr;
import com.accu_chek.solo_m.rcapp.application.logfile.LogFile;
import com.accu_chek.solo_m.rcapp.application.power.ICustPowerManager;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class NotifyProxy implements ServiceConnection
{
    enum ACTION_REQUIRE
    {
        /**
         * Require EMWR to show message
         */
        SHOW_MESSAGE
        {

            @Override
            public void handler(Context context, NotifyMessage message)
                    throws RemoteException
            {
                SafetyString id = EMWRStorageLogger.log(context, message);
                if (id != null)
                {
                    mService.requireEMWR(id, message);
                }
                else
                {
                    // Apply to the coding standard
                }
            }

        },

        REPEAT_MESSAGE
        {

            @Override
            public void handler(Context context, NotifyMessage message)
                    throws RemoteException
            {
                mService.repeatEMWR();
            }

        },

        /**
         * Push unconfirmed EMWR messages
         */
        PUSH_HISTORY
        {

            @Override
            public void handler(Context context, NotifyMessage message)
                    throws RemoteException
            {
            	Log.i("kayjean", "putHistory prepare " );
                mService.putHistory();
            }

        };

        public void handler(Context context, NotifyMessage message)
                throws RemoteException
        {
            // Apply to the coding standard
        }

    }

    private static final String TAG = "NotifyProxy";
    
    private static String EMWR_PACKAGE = "com.accu_chek.solo_m.rcapp.application.emwrservice";
    private static String EMWR_SERVICE_NAME = "com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRService";
    private static IEMWRService mService = null;
    
    private static Context mContext = null;
    private NotifyMessage mMessage = null;
    private ACTION_REQUIRE mAction = null;
    
    /**
     * Bind EMWR service to call requiring interface in EMWR service.
     * 
     * If fail to bind, the function will log the EMWR message and enter safe state (set safe state flag).
     * If the status has been safe state, it tries to use information dialog to show binding error message.
     *
     * see mService [in] This global variable is referred for getting service binder.
     *
     * @param context Context of caller
     * Range: valid Context object
     * Unit: Context
     * Scaling: 1
     * @param action ACTION_REQUIRE item
     * Range: valid item in ACTION_REQUIRE
     * Unit: ACTION_REQUIRE
     * Scaling: 1
     * @param message NotifyMessage that includes message item, sub content (option) and callback (option).
     * Range: valid NotifyMessage
     * Unit: NotifyMessage
     * Scaling: 1
     * 
     * return None
     */
    private static void bindEMWR(Context context, ACTION_REQUIRE action, NotifyMessage message)
    {
    	Log.i("kayjean" , "bindEMWR 1");
        if(mService == null)
        {
        	Log.i("kayjean" , "bindEMWR 2");
            ServiceConnection servConn = new NotifyProxy(context, action, message);
            Intent intent = new Intent();
            boolean bindingResult = false;
            Context appContext =  context.getApplicationContext();
            
            intent.setComponent(new ComponentName(EMWR_PACKAGE, EMWR_SERVICE_NAME));
            //context.startService(intent);
            appContext.startService(intent);
            //bindingResult = context.bindService(intent, servConn, Context.BIND_AUTO_CREATE);
            bindingResult = appContext.bindService(intent, servConn, Context.BIND_AUTO_CREATE);
            
            Log.i("kayjean" , "bindEMWR 3");
            if(bindingResult == false)
            {
            	Log.i("kayjean" , "bindEMWR 4");
                //EMWRStorageLogger.log(context, message);
                //failBindEMWR(context);
                EMWRStorageLogger.log(appContext, message);
                Log.i("kayjean" , "bindEMWR 5");
                failBindEMWR(appContext);
                
            }
            else
            {
            	Log.i("kayjean" , "bindEMWR 6");
                //
            }
        }
        else
        {
             //
        }
    }
    
    /**
     * Bind EMWR service to show EMWR message in EMWR service.
     * 
     * If fail to bind, the function will log the EMWR message and enter safe state (set safe state flag).
     * If the status has been safe state, it tries to use information dialog to show binding error message.
     *
     * see mService [in] This global variable is referred for getting service binder.
     *
     * @param context Context of caller
     * Range: valid Context object
     * Unit: Context
     * Scaling: 1
     * @param message NotifyMessage that includes message item, sub content (option) and callback (option).
     * Range: valid NotifyMessage
     * Unit: NotifyMessage
     * Scaling: 1
     * 
     * return None
     */
    public static void showEMWR(Context context, NotifyMessage message)
    {
//    	try{   	    
//    		LogFile logFile = LogFile.getInstance(context);
//    		String logmessage = "";
//    		logmessage += Integer.toString(message.getMessageItem().getCodeId());
//    		logmessage += message.getMessageItem().getDesciption();
//    		logmessage += message.getMessageItem().getDesciption();
//    		logmessage += Log.getStackTraceString( new Exception() );
//    		
//    		LogContent EmwrInfo = new LogEmwr(message.getMessageItem().toString(), logmessage );
//    		LogFile.getInstance().log(EmwrInfo);
//    	}
//    	catch(Exception e)
//    	{
//    		
//    	}
        emwrProcess(context, ACTION_REQUIRE.SHOW_MESSAGE, message);
    }

    public static void showEMWR(NotifyMessage message)
    {
        emwrProcess(mContext, ACTION_REQUIRE.SHOW_MESSAGE, message);
    }
    
    
    /**
     * Bind EMWR service to show EMWR message in EMWR service. This method just shows the message that has been stored in EMWR queue.
     * 
     * If fail to bind, the function will log the EMWR message and enter safe state (set safe state flag).
     * If the status has been safe state, it tries to use information dialog to show binding error message.
     *
     * see mService [in] This global variable is referred for getting service binder.
     *
     * @param context Context of caller
     * Range: valid Context object
     * Unit: Context
     * Scaling: 1
     * @param message NotifyMessage that includes message item, sub content (option) and callback (option).
     * Range: valid NotifyMessage
     * Unit: NotifyMessage
     * Scaling: 1
     * 
     * return None
     */
   public static void showEMWR(Context context)
    {
       emwrProcess(context, ACTION_REQUIRE.REPEAT_MESSAGE, null);
    }
    
    /**
     * Bind EMWR service to put unconfirmed EMWR message into EMWR service.
     * 
     * If fail to bind, the function will log the EMWR message and enter safe state (set safe state flag).
     * If the status has been safe state, it tries to use information dialog to show binding error message.
     *
     * see mService [in] This global variable is referred for getting service binder.
     *
     * @param context Context of caller
     * Range: valid Context object
     * Unit: Context
     * Scaling: 1
     * @param message NotifyMessage that includes message item, sub content (option) and callback (option).
     * Range: valid NotifyMessage
     * Unit: NotifyMessage
     * Scaling: 1
     * 
     * return None
     */
    public static void enableEMWRService(Context context)
    {
    	Log.i("kayjean", "enableEMWRService prepare " );
    	mContext = context;
        emwrProcess(context, ACTION_REQUIRE.PUSH_HISTORY, null);
        Log.i("kayjean", "enableEMWRService finish " );
    }
    
    /**
     * 
     * Function Description
     *
     * @param context
     * @param action
     * @param message
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private static void emwrProcess(Context context, ACTION_REQUIRE action, NotifyMessage message)
    {
    	Log.i("kayjean" , "emwrProcess 1");
        if(mService == null)
        {
        	Log.i("kayjean" , "emwrProcess 2");
            bindEMWR(context, action, message);
            Log.i("kayjean" , "emwrProcess 3");
        }
        else
        {
        	Log.i("kayjean" , "emwrProcess 4");
            runAction(context, action, message);
            Log.i("kayjean" , "emwrProcess 5");
        }
        Log.i("kayjean" , "emwrProcess 6");
    }
    
    /**
     * Call EMWR service to execute the interface in EMWR service. Before requiring the interface of EMWR service,
     * the message should insert into MessageLog table.
     * 
     * If fail to call EMWR service interface, the function will log the EMWR message and enter safe state (set safe state flag).
     * If the status has been safe state, it tries to use information dialog to show binding error message.
     * 
     * see mService [in] This global variable is referred for getting service binder.
     * 
     * @param context Context of caller
     * Range: valid Context object
     * Unit: Context
     * Scaling: 1
     * @param action ACTION_REQUIRE item
     * Range: valid item in ACTION_REQUIRE
     * Unit: ACTION_REQUIRE
     * Scaling: 1
     * @param message NotifyMessage that includes message item, sub content (option) and callback (option).
     * Range: valid NotifyMessage
     * Unit: NotifyMessage
     * Scaling: 1
     * 
     * return None
     */
    static void runAction(Context context, ACTION_REQUIRE action, NotifyMessage message)
    {
        try
        {           
            action.handler(context, message);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            failBindEMWR(context);
        }
        finally
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * 
     * Constructor of NotifyProxy object.
     * 
     * see mMessage [in] This global variable is referred for saving message item.
     * see mContext [in] This global variable is referred for saving service binder.
     * see mAction [in] This global variable is referred for saving EMWR action.
     * 
     * @param context Context of caller
     * Range: valid Context object
     * Unit: Context
     * Scaling: 1
     * @param action ACTION_REQUIRE item
     * Range: valid item in ACTION_REQUIRE
     * Unit: ACTION_REQUIRE
     * Scaling: 1
     * @param message NotifyMessage that includes message item, sub content (option) and callback (option).
     * Range: valid NotifyMessage
     * Unit: NotifyMessage
     * Scaling: 1
     */
    NotifyProxy(Context context, ACTION_REQUIRE action, NotifyMessage message)
    {
        mContext = context;
        mAction = action;
        mMessage = message;
    }
    

    
    /**
     * When succeed to bind, EMWR service binder is stored and call it to execute expected interface in EMWR service.
     *
     * see mMessage [in] This global variable is referred for saving message item.
     * see mService [in] This global variable is referred for saving service binder.
     * see mContext [in] This global variable is referred for getting service binder.
     * see mAction [in] This global variable is referred for getting EMWR action.
     *
     * @param name The concrete component name of the service that has been connected.
     * Range: valid ComponentName that is provided by Android
     * Unit: ComponentName
     * Scaling: 1
     * @param service The IBinder of the Service's communication channel, which you can now make calls on.
     * Range: valid IBinder that can be stubbed with IEMWRService
     * Unit: IBinder
     * Scaling: 1
     */
    @Override
    public void onServiceConnected(ComponentName name, IBinder service)
    {
Log.i("kayjean" , "onServiceConnected 1");
        Debug.printE(TAG, "bind EMWR Service");
        mService = IEMWRService.Stub.asInterface(service);
Log.i("kayjean" , "onServiceConnected 2");
        NotifyProxy.runAction(mContext, mAction, mMessage);
Log.i("kayjean" , "onServiceConnected 3");
        
    }
    
    /**
     * When disconnect with EMWR service, EMWR service binder is set to null and call it to unbind EMWR service.
     *
     * see mService [in] This global variable is referred for saving service binder.
     * see mContext [in] This global variable is referred for getting service binder.
     *
     * @param name The concrete component name of the service that has been connected.
     * Range: valid ComponentName that is provided by Android
     * Unit: ComponentName
     * Scaling: 1
     */
    @Override
    public void onServiceDisconnected(ComponentName name)
    {
        Debug.printE(TAG, "Unbind EMWR Service unexpectedly");

        mService = null;

        mContext.getApplicationContext().unbindService(this);
    }
    
    /**
     * Fail binding EMWR Service. 
     * Simulate EMWR service entering safe state.
     * If the binding still fail in safe state, EMWR service will try show message view without EMWR service.
     *
     * @param context Context of caller
     * Range: Valid Context object
     * Unit: Context
     * Scaling: 1
     * 
     * return None
     */
    private static void failBindEMWR(Context context)
    {
        NotifyMessage failBinder = new NotifyMessage(EMWRList.EMW47301);
        SafetyBoolean isSafeState = NugenGeneralModel.getSafetyBoolean(context, NugenFrameworkConstants.KEY_SAFE_STATE, SafetyBoolean.FALSE);
        boolean normalState = (isSafeState.getByte() == SafetyBoolean.FALSE.getByte());
        
        if(normalState == true)
        {
            ICustPowerManager powerManager = CustJavaFrameworkManager.getCustPowerManagerService(context);
            
            // call emwr: Fail to bind EMWR service.
            EMWRStorageLogger.log(context, failBinder);
            
            // set safe state flag
            NugenGeneralModel.setSafetyBoolean(context, NugenFrameworkConstants.KEY_SAFE_STATE, SafetyBoolean.TRUE);
            
            // shut down app
            try
            {
                if(powerManager != null)
                {
                    powerManager.reboot();
                }
                else
                {
                    // Apply to the coding standard
                }
            }
            catch (RemoteException e)
            {
                // Apply to the coding standard
                e.printStackTrace();
            }
            finally
            {
                // Apply to the coding standard
            }
        }
        else
        {
            // show information dialog for E57
        }
    }
}
