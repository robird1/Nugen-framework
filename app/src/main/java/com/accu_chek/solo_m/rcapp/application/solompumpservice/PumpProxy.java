package com.accu_chek.solo_m.rcapp.application.solompumpservice;

import java.util.List;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.solompumpservice.IServiceCallback;
import com.accu_chek.solo_m.rcapp.application.solompumpservice.SoloMServiceAidl;


import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

public class PumpProxy implements ServiceConnection
{
	private static SoloMServiceAidl mService = null;
	private static PumpBasalCallback pumpbasalcallback = null;
	private static PumpBgmCallback pumpbgmcallback = null;
	private static Context mContext = null;
    private static String SOLOM_PACKAGE      = "com.accu_chek.solo_m.rcapp.application.solompumpservice";
    private static String SOLOM_SERVICE_NAME = "com.accu_chek.solo_m.rcapp.application.solompumpservice.SoloMPumpService";
	
    PumpProxy(Context context)
    {
        mContext = context;
    }
	
    @Override
    public void onServiceConnected(ComponentName name, IBinder service)
    {
        mService = SoloMServiceAidl.Stub.asInterface(service);
        try {
            if(mService.isInited()) {
                mService.registerCallback(mCallback);
            }
            else {
                try {
                    if(mService != null)
                    {
                        mService.unregisterCallback(mCallback);
                    }
                } catch (RemoteException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        } catch (RemoteException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        if( prevfunc.equalsIgnoreCase("basal71"))
        	function_basal71();
        if( prevfunc.equalsIgnoreCase("bgmmdi"))
        	function_bgmmdi();
    }

    @Override
    public void onServiceDisconnected(ComponentName name)
    {
        mService = null;
        mContext.getApplicationContext().unbindService(this);
    }
    
    private static void bindEMWR(Context context)
    {
        if(mService == null)
        {
            ServiceConnection servConn = new PumpProxy(context);
            Intent intent = new Intent();
            boolean bindingResult = false;
            Context appContext =  context.getApplicationContext();
            intent.setComponent(new ComponentName(SOLOM_PACKAGE, SOLOM_SERVICE_NAME));
            //context.startService(intent);
            appContext.startService(intent);
            //bindingResult = context.bindService(intent, servConn, Context.BIND_AUTO_CREATE);
            bindingResult = appContext.bindService(intent, servConn, Context.BIND_AUTO_CREATE);
            if(bindingResult == false)
            {
            	Log.i("kayjean", "bindEMWR error" );            	
                //failBindEMWR(appContext);
            }
            else
            {
                //
            }
        }
        else
        {
             //
        }
    }
    
    ///////////////////////////call back//////////////////////////
    
    private IServiceCallback mCallback = new IServiceCallback.Stub() {

        @Override
        public void handlerBolusEvent(int msgID, int param) throws RemoteException {
        }

        @Override
        public void handlerEMWREvent(int msgID, List<String> strList) throws RemoteException {
        	NotifyMessage mNotifyMsg = new NotifyMessage(EMWRList.EMW41001);
        	NotifyProxy.showEMWR(mContext.getApplicationContext(), mNotifyMsg);
        }

        @Override
        public void handlerSyncEvent(int msgID, List<String> strList) throws RemoteException {
        	pumpbgmcallback.onReturn( true );
        }
        
        @Override
        public void handlerBasalEvent(int msgID, List<String> strList) throws RemoteException {
        	Log.i("kayjean","handlerBasalEvent !!!!!msgID " + msgID + " strList " + strList.get(0) );
        	pumpbasalcallback.onReturn( true );
        }
    };
    
    /////////////////////////////function//////////////////////////
    
    public static String prevfunc = "";

    public static boolean initPumpService(Context context)
    {
        if(mService == null)
        {
        	Log.i("kayjean", "mService is null");        	
            bindEMWR(context);
        }
        else
        {
        	try{
        		return mService.initPumpService();
        	}
        	catch(Exception e){
        	}
        }
        return false;
    }
    
	public static boolean isBonded(Context context)
	{
        if(mService == null)
        {
        	Log.i("kayjean", "mService is null");        	
            bindEMWR(context);
        }
        else
        {
        	try{
        		return mService.isBonded();
        	}
        	catch(Exception e){
        	}
        }
        return false;
	}
    
	public static boolean isConnected(Context context)
	{
        if(mService == null)
        {
        	Log.i("kayjean", "mService is null");        	
            bindEMWR(context);
        }
        else
        {
        	try{
        		return mService.isConnected();
        	}
        	catch(Exception e){
        	}
        }
        return false;
	}
    
    
	public static void bgmmdi(Context context, PumpBgmCallback callback  )
	{
		pumpbgmcallback = callback;
		prevfunc = "bgmmdi";
        if(mService == null)
        {
        	Log.i("kayjean", "mService is null");        	
            bindEMWR(context);
        }
        else
        {
        	Log.i("kayjean", "mService function_basic");
        	function_basal71();
        }
	}
    
	public static void function_bgmmdi(){
        try{
        	mService.runMDI();
        }
        catch(Exception e){
        	e.printStackTrace();
        }
	}
    
	public static void basal71(Context context, PumpBasalCallback callback  )
	{
		pumpbasalcallback = callback;
		prevfunc = "basal71";
        if(mService == null)
        {
        	Log.i("kayjean", "mService is null");        	
            bindEMWR(context);
        }
        else
        {
        	Log.i("kayjean", "mService function_basic");
        	function_basal71();
        }
	}
	
	public static void function_basal71(){
		//BLEController.getInstance().BLEACT71(new ResponseACT71());
        Log.i("kayjean", "handle function_basal71");
        //Freq f = new Freq( 1, 2, 3);
        try{
        	BasalBRP brp = new BasalBRP(mContext);
        	brp.createBrpDataArray();
        	List<String> ret = brp.getBrp();
        	mService.runBRP( brp.getBrp() );
        }
        catch(Exception e){
        	e.printStackTrace();
        }
	}
}
