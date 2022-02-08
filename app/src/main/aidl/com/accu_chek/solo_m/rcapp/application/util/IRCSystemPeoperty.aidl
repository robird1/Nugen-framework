package com.accu_chek.solo_m.rcapp.application.util;


/**
 * {@hide}
 */
interface IRCSystemPeoperty
{
    //Set property
    void setProperty(String propName, String val);
    
    //Get property
    String getProperty(String propName, String defVal);

    
}