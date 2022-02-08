package com.accu_chek.solo_m.rcapp.application.solompumpservice; 


import java.util.List;

import com.accu_chek.solo_m.rcapp.application.solompumpservice.IServiceCallback;


interface SoloMServiceAidl {

	boolean isInited();
	
	void registerCallback(IServiceCallback cb);
    void unregisterCallback(IServiceCallback cb);
    
    boolean isBonded();
    boolean isConnected();
    boolean initPumpService();
	void runBRP(in List< String > bytes);
	void runMDI();
}  