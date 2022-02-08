package com.accu_chek.solo_m.rcapp.application.fwupdate;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;

import android.os.RemoteException;

public class FWUpdateStatus
{
    static final String TAG = FWUpdateStatus.class.getSimpleName();
    
    private static final int UPDATE_FEATURE_ENBLED = 0x8000; //
    private static final int UPDATE_CAPABLE = 0x4000; //
    private static final int IN_UPDATE_MODE = 0x2000; //
    private static final int PROGRMMING_DEVICE = 0x1000; //
    private static final int PROGRMMING_ERROR = 0x0800; //
    private static final int PROGRMMING_COMPLETED = 0x0400; //
    
    /**
     * Status: initial status
     */
    public static final int UPDATE_INITIAL = UPDATE_FEATURE_ENBLED + UPDATE_CAPABLE;
    public static final String UPDATE_INITIAL_STR = "49152";
    public static final int UPDATE_IN_PROGRESS = UPDATE_FEATURE_ENBLED + UPDATE_CAPABLE + IN_UPDATE_MODE;

    /**
     * 
     */
    static final String PERSIST_SYS_FWUPDATE = "persist.sys.fwupdate.status";
    
    /**
     * Update firmware update status by updating the Android system property
     *
     * @param stat
     * @return int [out] error code
     */
    public static int statusUpdate(int stat)
    {
        Debug.printI(TAG, "[statusUpdate]:" + Integer.toString(stat));
        int err = Error.ERR_OK;
        
        IRCSystemPeoperty iprop = CustJavaFrameworkManager
                .getRCSystemPropertyService(null);
        
        try
        {
            iprop.setProperty(PERSIST_SYS_FWUPDATE, Integer.toString(stat));
        }
        catch (RemoteException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            err = Error.ERR_UPDATE_STATUS_FAIL;
        }
        
        return err;
    }
    
    /**
     * Get firmware update status by reading the Android system property
     *
     * @return int [out] firmware update status
     * 						Range: UPDATE_INITIAL, UPDATE_IN_PROGRESS
     * 						Unit: int
     * 						Scaling:1
     */
    public static int getStatus()
    {
        Debug.printI(TAG, "[getStatus] enter");
        String stat = UPDATE_INITIAL_STR;
        int stat_int = 0;
        
        IRCSystemPeoperty iprop = CustJavaFrameworkManager
                .getRCSystemPropertyService(null);
        
        try
        {
            stat = iprop.getProperty(PERSIST_SYS_FWUPDATE, UPDATE_INITIAL_STR);
        }
        catch (RemoteException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            stat = UPDATE_INITIAL_STR;
        }
        
        Debug.printI(TAG, "Status:" + stat);
        stat_int = Integer.parseInt(stat);
        Debug.printI(TAG, "exit");
        return stat_int;
    }
}