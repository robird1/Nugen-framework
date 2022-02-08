package com.accu_chek.solo_m.rcapp.application.fwupdate;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.common.HammingValues;
import com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrix;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.fwupdate.FWUpdateMain;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;

import android.os.RemoteException;

/**
 *
 */
public final class FWUpdate
{
    static final String TAG = FWUpdate.class.getSimpleName();
    
    /**
     * Class object instance
     */
    private static FWUpdate mInstance = null;

    // global error code It is update by thread
    int mError = Error.ERR_OK;

    // Message buffer length
    static final int MAX_MESSAGE_LEN = 1024;

    String mContent = null;
    
   
    /**
     * PHDC mode value for the android system property "persist.sys.default.pcconnect"
     * Range: "phdc,adb"
     * Unit: String
     * Scaling: 1
     */
    private static final String PHDC_ADB_PROPERTY_VALUE = "phdc,adb";
    
    static final String UPDATE_FEATURE_ENBLED = "0x8000"; //
    static final String UPDATE_CAPABLE = "0x4000"; //
    static final String IN_UPDATE_MODE = "0x2000"; //
    static final String PROGRMMING_DEVICE = "0x1000"; //
    static final String PROGRMMING_ERROR = "0x0800"; //
    static final String PROGRMMING_COMPLETED= "0x0400"; //
    /**
     * Status: initial status
     */
    static final String UPDATE_INITIAL = "49152"; //UPDATE_FEATURE_ENBLED+UPDATE_CAPABLE+PROGRMMING_COMPLETED
    static final String UPDATE_IN_PROGRESS = "57344";//UPDATE_INITIAL+IN_UPDATE_MODE
    
    
//    public FWUpdate()
//    {
//   
//    }
    
    /**
     * Update Firmware for both processors.
     * If the update FW parameter in configuration matrix is set to disable, method returns with 
     * error code -  
     * There is a precondition for this method before calling it:
     * The FW package should be already transmitted to internal storage (eMMC)
     * with specified path:
     * \storage\sdcard0
     *
     * @@param None
     * @return int [out] error code in hamming values. 1:FW update functionality
     *         is disable according to Configuration matrix
     *         Range: HAMMING_HD4_VALUE_0001, HAMMING_HD4_VALUE_0006
     *         Unit: defined error code
     *         Scaling: 1
     */
    public int enter()
    {
        int ret = Error.ERR_OK;
        ExecutorService executor = null;
        Runnable worker = null;
        FWUpdateZip zip = new FWUpdateZip("/storage/sdcard0/altek72_we_jb3-ota-eng.jenson.zip", "/storage/sdcard0/");

        Debug.printI(TAG, "[update] enter...");
        // Check if the flag in configuration is set to enable

        // unzip the FW package - several files output from the zip
        zip.unzip();
        
        // Main processor image
        // Comms image
        // Version info

        // Start to update communication processor
        executor = Executors.newSingleThreadExecutor();
        if (executor != null)
        {
            worker = new runUpdateThread();
            Debug.printI(TAG, "execute ...");
            executor.execute(worker);
            executor.shutdown();
        }
        else
        {
            Debug.printI(TAG, "failed to new thread!");
            ret = Error.ERR_THREAD_EXE_FAIL;
        }

        return ret;
    }

    /**
     * Set the status to the initial status
     * It will change the mode to avoid reboot in FW update process    j. 
     * @param None
     * @return int [out] error code in hamming values. 1:FW update is terminated successfully
     *          Range: HAMMING_HD4_VALUE_0001, HAMMING_HD4_VALUE_0008
     *          Unit: int
     *          Scaling: 1
     */
    public int exit() 
    {   
        int err = Error.ERR_OK;
        
        err = FWUpdateStatus.statusUpdate(FWUpdateStatus.UPDATE_INITIAL);
        
        if(err !=  Error.ERR_OK)
        {
            err = Error.ERR_EXIT_FAIL;
        }
     
        return err;
    }
    
//    /**
//     * Resume firmware update procedure after system update has been done. 
//     * If update status is "in firmware mode", enable UBS connection with PHDC.
//     *
//     * @return
//     * @return int [out] Delete pre line return if exist. Parameter Description
//     */
//    public int resume()
//    {
//        int err = Error.ERR_OK;
//        int stat = 0;
//        IRCSystemPeoperty prop = null;
//        
//        Debug.printI(TAG, "[resume] enter...");
//        
//        stat = mStatus.getStatus();
//        Debug.printI(TAG, "Status is " + Integer.toString(stat));
//        
//        //if status is "updating", it means the firmware update procedure was 
//        //completed and system reboots in firmware update mode rather 
//        //than normal mode. We enable the USB connection accordingly.
//        if(stat == mStatus.UPDATE_IN_PROGRESS)
//        {
//            prop = CustJavaFrameworkManager.getRCSystemPropertyService(null);
//
//            if(prop != null)
//            {
//                Debug.printI(TAG, "Enable USB");
//                try
//                {
//                    prop.setProperty(USBCONFIGPROP, PHDC_ADB_PROPERTY_VALUE);
//                }
//                catch (RemoteException e)
//                {
//                    err = Error.ERR_RESUME_FAIL;
//                }
//                finally
//                {
//                    // Apply to the coding standard
//                }
//            }
//            else
//            {
//                err = Error.ERR_RESUME_FAIL;
//            }
//        }
//        else
//        {
//            //The status is not readable. We have no means to handle it.
//            //So, trigger Error message via EMWR module
//            
//        }
//        return err;
//    }
    
    /**
     *
     */
    protected class runUpdateThread implements Runnable
    {

        /**
         * A thread to execute the update for the two processors
         *
         * @param None
         * @return none
         */
        @Override
        public void run()
        {
            Debug.printI(TAG, "[runCommsUpdateThread] enter...");
            
            //do comms procesor update firstly
            Debug.printI(TAG, "Set status to" + Integer.toString(FWUpdateStatus.UPDATE_IN_PROGRESS));
            FWUpdateStatus.statusUpdate(FWUpdateStatus.UPDATE_IN_PROGRESS);
            if (mError == Error.ERR_OK)
            {
                Debug.printI(TAG, "Start to update comms...");
                mError = doCommsUpdate();
            }
            
            if(mError == Error.ERR_OK)
            {
                Debug.printI(TAG, "Start to update main proc...");
                mError = doMainUpdate();
            }
            
//            //Update status for RPC accordingly
//            if(mError == Error.ERR_OK)
//            {
//                mStatus.statusUpdate(PROGRMMING_COMPLETED);
//            }
//            else
//            {
//                mStatus.statusUpdate(PROGRMMING_ERROR);
//            }
        }
    }

    /**
     * Update communication processor
     * There is a precondition for this method before calling it
     * The FW package should be already transmitted to internal storage (eMMC)
     * via MTP
     * In additional, corresponding history will be created for traceability purpose.
     * @param None
     * @return int [out] error code in hamming values
     *         Range: HAMMING_HD4_VALUE_0001, HAMMING_HD4_VALUE_0002
     *         Unit: defined error code
     *         Scaling: 1
     */
    private int doCommsUpdate()
    {
        int err = Error.ERR_OK;

        Debug.printI(TAG, "[doCommsUpdate] enter...");

        // Release uart port from HW manager
        err = releaseUartPort();
        if (err == Error.ERR_OK)
        {
            // Perform comms fw update
            err = updateComm();
        }
        
        //create history

        return err;
    }

    /**
     * Release uart port that should be occupied by other applications
     *
     * @param None
     * @return int [out] module internal error code
     *         Range: ERR_OK, ERR_UART_RELESE_FAIL
     *         Unit: defined error code
     *         Scaling: 1
     */
    private int releaseUartPort()
    {
        UARTManager uart = null;
        int err = Error.ERR_OK;
        // Intent intent = new Intent();

        Debug.printI(TAG, "[releaseUartPort] enter...");

        uart = (UARTManager) ICustomizedHWManager
                .getSystemService(ICustomizedHWManager.UART_SERVICE);

        if(uart != null)
        {
            // release UART port from other applications
            try
            {
                uart.close(0xF);
            }
            catch (OperationFailException e1)
            {
                err = Error.ERR_UART_RELESE_FAIL;
                e1.printStackTrace();
            }
            // CommonUtils.objectCheck(intent);
            // intent.setAction(mMessage);
            // mContext.sendBroadcast(intent);
        }
        else
        {
            err = Error.ERR_UART_RELESE_FAIL;
        }

        return err;
    }

    /**
     * Perform the update of communication processor. This method assums the UART 
     * port is released from other application.
     *
     * @param None
     * @return int [out] module internal error code
     *         Range: ERR_OK, ERR_COMMS_UPDATE_FAIL
     *         Unit: defined error code
     *         Scaling: 1
     */
    private int updateComm()
    {
        String line = null;
        int err = Error.ERR_OK;

        Debug.printI(TAG, "[updateComm] enter...");
        
        try
        {
            Process process = new ProcessBuilder("comms_update").start();

            BufferedReader bufferedReader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()),
                    MAX_MESSAGE_LEN);

            StringBuilder log = new StringBuilder();

            while ((line = bufferedReader.readLine()) != null)
            {
                log.append(line + "\n");
                Debug.printI(TAG, line);
            }
            Debug.printI(TAG, "All STDIO messages have been read");

            mContent = log.toString();
            bufferedReader.close();
            process.destroy();

            // check if the update has been done successfully
            // COMMS_UPDATE_DONE.compareTo(mContent);

        }
        catch (IOException e)
        {

            Debug.printI(TAG, "read log exception = " + e.getMessage());
            e.printStackTrace();
            err = Error.ERR_COMMS_UPDATE_FAIL;
        }

        return err;
    }

    /**
     * Perform the update of main processor
     *
     * @param None
     * @return int [out] module internal error code
     *         Range: ERR_OK, ERR_MAIN_UPDATE_FAIL
     *         Unit: defined error code
     *         Scaling: 1
     */
    private int doMainUpdate()
    {
        Debug.printI(TAG, "[doMainUpdate] enter...");
        int err = Error.ERR_OK;
        final IFWUpdateMain ifwm = CustJavaFrameworkManager
                .getFWUpdateHandler();

        // Check object
        if(ifwm != null)
        {
            try
            {
                ifwm.update();
            }
            catch (RemoteException e)
            {
                Debug.printI(TAG, "failed to enter!");
                e.printStackTrace();
                err = Error.ERR_MAIN_UPDATE_FAIL;
            }
        }
        else
        {
            err = Error.ERR_MAIN_UPDATE_FAIL;
        }

        return err;
    }
    
    
}
