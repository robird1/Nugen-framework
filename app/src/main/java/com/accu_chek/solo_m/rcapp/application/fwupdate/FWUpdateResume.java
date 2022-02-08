package com.accu_chek.solo_m.rcapp.application.fwupdate;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;

import android.os.RemoteException;

public class FWUpdateResume
{
    static final String TAG = FWUpdate.class.getSimpleName();
    
    /**
     * PHDC mode value for the android system property "persist.sys.default.pcconnect"
     * Range: "phdc,adb"
     * Unit: String
     * Scaling: 1
     */
    private static final String PHDC_ADB_PROPERTY_VALUE = "phdc,adb";
    
    /**
     * 
     */
    private static final String USBCONFIGPROP = "persist.sys.usb.config";
    
    /**
     * Resume firmware update procedure after system update has been done. 
     * If update status is "in firmware mode", enable UBS connection with PHDC.
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int resume()
    {
        int err = Error.ERR_OK;
        int stat = 0;
        IRCSystemPeoperty prop = null;
        
        Debug.printI(TAG, "[resume] enter...");
        
        stat = FWUpdateStatus.getStatus();
        
        Debug.printI(TAG, "Status is " + Integer.toString(stat));
        
        //if status is "updating", it means the firmware update procedure was 
        //completed and system reboots in firmware update mode rather 
        //than normal mode. We enable the USB connection accordingly.
        if(stat == FWUpdateStatus.UPDATE_IN_PROGRESS)
        {
            prop = CustJavaFrameworkManager.getRCSystemPropertyService(null);

            if(prop != null)
            {
                Debug.printI(TAG, "Enable USB");
                try
                {
                    prop.setProperty(USBCONFIGPROP, PHDC_ADB_PROPERTY_VALUE);
                }
                catch (RemoteException e)
                {
                    err = Error.ERR_RESUME_FAIL;
                }
                finally
                {
                    // Apply to the coding standard
                }
            }
            else
            {
                err = Error.ERR_RESUME_FAIL;
            }
        }
        else
        {
            //The status is not readable. We have no means to handle it.
            //So, trigger Error message via EMWR module
            
        }
        return err;
    }
}
