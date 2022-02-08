package com.accu_chek.solo_m.rcapp.application.solompumpservice;

/**
 * a callback interface used by dvbService to send
 * synchronous notifications back to its clients.  Note that this is a
 * one-way interface so the server does not block waiting for the client.
 */
oneway interface IServiceCallback {
	
	/*
	 * handler message from service
	 */
    void handlerBolusEvent(int msgID, int param);
    
    /*
	 * handler message from service
	 */
    void handlerBasalEvent(int msgID, in List<String> strList);
    
    /*
	 * handler emwr message from service
	 */
    void handlerEMWREvent(int msgID, in List<String> strList);

    /*
	 * handler system sync message from service
	 */
    void handlerSyncEvent(int msgID, in List<String> strList);
   
}