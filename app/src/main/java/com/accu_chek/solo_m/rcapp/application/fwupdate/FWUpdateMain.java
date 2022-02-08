/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ${package_name}.${type_name}
 * Brief: 
 *
 * Create Date: ${date}
 * $$Revision: 22760 $$
 * $$Author: JensonChin $$
 * $$Id: FWUpdate.java 22760 2015-10-28 15:07:17Z JensonChin $$
 */
package com.accu_chek.solo_m.rcapp.application.fwupdate;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.common.HammingValues;
import com.accu_chek.solo_m.rcapp.application.continua.ContinuaProxy;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.fwupdate.IFWUpdateMain;
import com.accu_chek.solo_m.rcapp.application.power.ICustPowerManager;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;

import android.content.Context;
import android.content.Intent;
import android.os.RemoteException;
import android.util.Log;
import android.os.Environment;
import android.os.RecoverySystem;

/**
* <h1>java cust framework service name.</h1>
* Description class imfornation.
* 
* @author  Name
* @version 1.0
* @since   YYYY-MM-DD 
*/
public final class FWUpdateMain extends IFWUpdateMain.Stub{
    static final String TAG = FWUpdateMain.class.getSimpleName();
    
    Context mContext = null;
    String mPathOTA = "/storage/sdcard0/altek72_we_jb3-ota-eng.jenson.zip";
    //String mPathOTA = "/sdcard/altek72_we_jb3-ota-eng.jenson.zip"; 
    //String mPathOTA = "/cache/altek72_we_jb3-ota-eng.jenson.zip";
    
    String mMessage = "com.accu_chek.solo_m.rcapp.uartservice.close";
    
    /**
     * Constructor
     *
     * ${tags}
     * @return ${return_type} [out] Delete pre line return if exist. Parameter Description
     */
    public FWUpdateMain(Context context) {
       Debug.printI(TAG, "FW update class has been constructed!");
       
       //save context object for later use
       mContext = context;
    }
    
    /**
     * Main flow for main processor software updating procedure. 
     * @param None
     * @return int [out] error code in hamming values. 1:FW update functionality is disable according to Configuration matrix
     *  		Range: HAMMING_HD4_VALUE_0001, HAMMING_HD4_VALUE_0002
     *          Unit: defined error code
     *          Scaling: 1
     */
    public int update() {
        int ret = Error.ERR_OK;
        
        Debug.printI(TAG, "[update] enter...");       
        
        //Verify the OTA package
        
        //reboot in Recovery
        Debug.printI(TAG, "reboot into recovery console...");
        ret =doMainProcUpdate();
        
        return ret;
    }
    
    /**
     * Perform the update of main processor by installing the OTA package 
     * in the recovery console 
     *
     * @param None
     * @return int [out] module internal error code
     *  		Range: ERR_OK, ERR_MAIN_UPDATE_FAIL
     *          Unit: defined error code
     *          Scaling: 1
     */
    private int doMainProcUpdate()
    {
    	int err = Error.ERR_OK;
    	File f = new File(mPathOTA);
    	
    	//CommonUtils.objectCheck(f);
    	if(f != null)
    	{
    	    Debug.printI(TAG, "Install package:" + mPathOTA);
    	    try 
    	    {
    	        RecoverySystem.installPackage(mContext, f);
    	    } 
    	    catch (IOException e) 
    	    {
    	        // TODO Auto-generated catch block
    	        Debug.printI(TAG, "failed to install!");
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
    
    /**
     * Move data from /sdcard0 to /cache since RCFM is allowed to 
     * put the update package to sdcard0 due to limitation of MTP
     *
     * @param None
     * @return int [out] module internal error code
     *  		Range: ERR_OK, ERR_FILE_COPY_FAIL
     *          Unit: defined error code
     *          Scaling: 1
     */
    
//    private void copyPackage()
//    { 
//    	Boolean r = false;
//    	File src = null;
//    	File dest = null;
//    	
//    	Debug.printI(TAG, "[copyPackage] enter...");
//    	
//    	src = new File ("/storage/sdcard0/fw.bin");
//    	if(src.canRead() == false)
//    	{
//    		Debug.printI(TAG,"source can not be read!");
//    	}
//    	dest = new File ("/cache/fw.bin");
//    	if(dest.canWrite() == false)
//    	{
//    		Debug.printI(TAG,"dest can not be written!");
//    	}
//    	if((src != null) || (dest != null))
//    	{
//    		r = src.renameTo(dest);
//        	if(r == false)
//        	{
//        		Debug.printI(TAG,"copy failed!");
//        	}
//    	}
//    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
