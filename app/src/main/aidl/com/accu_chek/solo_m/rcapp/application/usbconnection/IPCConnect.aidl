package com.accu_chek.solo_m.rcapp.application.usbconnection;

import com.accu_chek.solo_m.rcapp.application.usbconnection.IUsbListener;


/**
 * {@hide}
 */
interface IPCConnect
{
    
    //Register USB listener
    void setOnUsbListener(IUsbListener listener);
    
}