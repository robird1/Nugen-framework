package com.accu_chek.solo_m.rcapp.application.usbconnection;


/**
 * {@hide}
 */
interface IUsbListener
{
    //call back function
    void onUsbConnect();
    
    void onDisConnect();
    
    void onConfigure();
    
    void onUnConfigure();
    
}