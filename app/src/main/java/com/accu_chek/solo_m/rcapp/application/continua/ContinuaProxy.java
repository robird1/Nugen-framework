/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController
 * Brief: 
 *
 * Create Date: 2015/3/18
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: ContinuaProxy.java 20933 2015-10-05 08:34:57Z kevenwu $
 */
package com.accu_chek.solo_m.rcapp.application.continua;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.os.RemoteException;

public class ContinuaProxy implements ServiceConnection
{
	/**
	 * The package name of ContinuaService.
	 */
    private static final String PACKAGE_CONTINUA = "com.accu_chek.solo_m.rcapp.application.continua";
    
    /**
     * The Service name of ContinuaService.
     */
    private static final String NAME_CONTINUA = PACKAGE_CONTINUA + ".ContinuaService";
    
    /**
     * The instance of IContinuaService.
     */
    private static IContinuaService mService = null;
    
    /**
     * The instance of application context.
     */
    private static Context mContext = null;
    
    /**
     * The instance of ContinuaProxy, it's used to handle the connection of service binding.
     */
    private static ContinuaProxy mInstance = new ContinuaProxy();
    
    /**
     * Initial the Continua Agent and start to associate with manager.
     * Initiate the binder of ContinuaService if it is not initiated yet.
     * 
     * @param context : The application context.
     * 		  Range: Valid object of Context.
     * 		  Unit: Context.
     * 		  Scaling: 1.
     * 
     * return void [out]: None.
     * 
     * throws RemoteException if ContinuaService is binding failed.
     */
    public static void startContinua(Context context) throws RemoteException
    {
        if (null == mService)
        {
            bindContinuaService(context);
        }
        else
        {
            mService.startContinua();
        }
    }
    
    /**
     * Stop the ContinuaAgent and unbind ContinuaService. 
     * After that, the instance of binder is set to null.
     * 
     * @param context : The application context.
     * 		  Range: Valid object of Context.
     * 		  Unit: Context.
     * 		  Scaling: 1.
     * 
     * return void [out]: None.
     * 
     * throws RemoteException if ContinuaService is binding failed.
     */
    public static void stopContinua(Context context) throws RemoteException
    {
        if (null != mService)
        {
            mService.stopContinua();
            
            mContext.unbindService(mInstance);
            mService = null;
        }
        else
        {
            // Apply to coding standard.
        }
    }
    
    /**
     * Start to bind the ContinuaService.
     * 
     * @param context : The application context.
     * 		  Range: Valid object of Context.
     * 		  Unit: Context.
     * 		  Scaling: 1.
     * 
     * return void [out]: None.
     */
    private static void bindContinuaService(Context context)
    {
        if (null == mService)
        {
            Intent intent = new Intent();
            
            mContext = context.getApplicationContext();
            
            intent.setComponent(new ComponentName(PACKAGE_CONTINUA, NAME_CONTINUA));
            mContext.bindService(intent, mInstance, Context.BIND_AUTO_CREATE);
        }
        else
        {
            // Apply to coding standard.
        }
    }
    
    /**
     * This function is called when ContinuaService is binded successful.
     * 
     * Initiate the Continua Agent to start association.
     * 
     * @param name : The service component name.
     * 		  Range: Valid object of ComponentName.
     * 		  Unit: ComponentName.
     * 		  Scaling: 1. 
     * @param service : The binder of ContinuaService.
     * 		  Range: Valid object of IBinder.
     * 		  Unit: IBinder.
     * 		  Scaling: 1.
     * 
     * return void [out]: None.
     */
    @Override
    public void onServiceConnected(ComponentName name, IBinder service)
    {
        mService = IContinuaService.Stub.asInterface(service);
        
        try
        {
            mService.startContinua();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.
        }
    }

    /**
     * This function is called when ContinuaService is disconnected.
     * 
     * Assign the ContinuaService binder to null and unbind ContinuaService.
     * 
     * @param name : The service component name.
     * 		  Range: Valid object of ComponentName.
     * 		  Unit: ComponentName.
     * 		  Scaling: 1.
     * 
     * return void [out]: None.
     */
    @Override
    public void onServiceDisconnected(ComponentName name)
    {
        mService = null;        
        mContext.unbindService(mInstance);
    }
}
