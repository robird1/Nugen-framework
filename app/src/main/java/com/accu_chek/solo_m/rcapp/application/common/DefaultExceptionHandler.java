/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: DefaultExceptionHandler
 * Brief: Used for catch runtime exception.
 *
 * Create Date: 7/01/2015
 * $Revision: 20513 $
 * $Author: DWYang $
 * $Id: DefaultExceptionHandler.java 20513 2015-10-01 10:25:24Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.common;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.power.ICustPowerManager;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.os.Looper;
import android.os.RemoteException;
import android.os.SystemClock;
import android.view.WindowManager;
import android.widget.Toast;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.Thread.UncaughtExceptionHandler;
import java.text.SimpleDateFormat;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class DefaultExceptionHandler implements UncaughtExceptionHandler
{

    private static final String TAG = "DefaultExceptionHandler";

    private static DefaultExceptionHandler mInstance = null;
    
    private Context mContext = null;
    
    private Thread.UncaughtExceptionHandler mUnCauchException = null;
       
    
    private DefaultExceptionHandler()
    {
        //do not new by other module directly
    }
    
    /**
     * 
     * Function Description
     *
     * @param ctx
     * @return
     * @return DefaultExceptionHandler [out] Delete pre line return if exist. Parameter Description
     */
    public static synchronized DefaultExceptionHandler getInstance()
    {
        Debug.printI(TAG, "[getInstance] enter");
        
        if(mInstance == null)
        {
            mInstance = new DefaultExceptionHandler();
        }
        
        Debug.printI(TAG, "mInstance = " + mInstance);
        
        return mInstance;
    }
    
    /*
     * 
     */
    public void init(Context context)
    {
        mContext = context;
        mUnCauchException = Thread.getDefaultUncaughtExceptionHandler();
        Thread.setDefaultUncaughtExceptionHandler(this);
    }
    
    /**
     * 
     *
     * @param thread
     * @param ex
     */
    
    @Override
    public void uncaughtException(Thread thread, Throwable ex)
    {
        Debug.printI(TAG, "[uncaughtException] enter");
        
        Debug.printI(TAG, "ex string = " + ex.toString());
        
        ex.printStackTrace();
        
        handleException(ex);
             
        if(mUnCauchException != null)
        {
            Debug.printI(TAG, "System handle exception");
        }
        else
        {
            Debug.printI(TAG, "Application hancle exception");
        }
        
    }

    
    
    /**
     * 
     * Function Description
     *
     * @param ex
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter Description
     */
    private void handleException(final Throwable ex) 
    {  

        ExecutorService executor = Executors.newSingleThreadExecutor();
        ShowInformationThread workThrd = new ShowInformationThread();
        workThrd.setContext(mContext, ex);
        
        //start work thread
        executor.execute(workThrd);

    }
    
    /**
     * 
     */
    class ShowInformationThread implements Runnable
    {

        private Context mContext = null;
        
        private StackTraceElement[] mStack = null;
        
        private String mMessage = null;
        
        private String mContent = null;
        
        private Throwable mException = null;
        
        private StringBuffer mSbf = new StringBuffer();
        
        /**
         * 
         * Function Description
         *
         * @param context
         * @return void [out] Delete pre line return if exist. Parameter Description
         */
        public void setContext(Context context, final Throwable ex)
        {
            mContext = context;
            
            mException = ex;
            
            mStack = ex.getStackTrace();
            mMessage = ex.getMessage();
            
        }
        
        
        /**
         * 
         *
         */
        @Override
        public void run()
        {
            Looper.prepare();

            Toast.makeText(mContext, "Crash information:" + mMessage,
                    Toast.LENGTH_LONG).show();

            // write information to file
            writeToLogFile();

            debugExceptionInformation(mStack);

            // go to safe State
            goToSafeSate();
            
            
            Looper.loop();
        }

        /**
         * 
         * Function Description
         *
         * @return void [out] Delete pre line return if exist. Parameter Description
         * @throws IOException 
         */
        private void writeToLogFile()
        {
            Debug.printI(TAG, "[writeToLogFile] enter");
            String fileName = "crash-" + System.currentTimeMillis()
                    + ".log";
            String path = "/data/nugen/exception/"+fileName;
//            File file = new File(
//                    "/data/exceptionlog/",
//                    fileName);
//            
//            if(!file.exists())
//            {
//                file.getParentFile().mkdirs();
//                try {
//                    file.createNewFile();
//                } catch (IOException e) {
//                    // TODO Auto-generated catch block
//                    e.printStackTrace();
//                }
//            }
//            
//            file.setWritable(true);
            
            ReadLog();
            
            SystemClock.sleep(500L);

            FileWriter fos = null;
            try
            {
                fos = new FileWriter(path, false);

                BufferedWriter out = new BufferedWriter(fos);
                    
                out.write(mContent+"\n");
                out.newLine();
                out.flush();
                out.close();
                
//                if(mMessage != null)
//                {
//                    fos.write(mMessage.getBytes());
//                    
//                }
//                else
//                {
//                    fos.write("Null exception".getBytes());
//                }
//                
//                fos.write("\n".getBytes());
//                
//                for (int i = 0; i < mStack.length; i++)
//                {
//                    fos.write(mStack[i].toString().getBytes());
//                    fos.write("\n".getBytes());
//                }

//                fos.flush();
                fos.close();

                Debug.printI(TAG, "[writeToLogFile] completedly");
            }
            catch (Exception e)
            {
                Debug.printI(TAG,
                        "[Exception] e = " + e.getMessage());
            }
            finally
            {
                Debug.printI(TAG, "[writeToLogFile] finally");

            }

        }

        /**
         * 
         * Function Description
         *
         * @return void [out] Delete pre line return if exist. Parameter Description
         * @throws RemoteException 
         */
        private void goToSafeSate()
        {
            Debug.printI(TAG, "[goToSafeSate] enter");
            
            //Temporary used, remove it int the future {
            AlertDialog dialog = warningMessageDialog(mContext, "Dangerous", mSbf.toString());
            dialog.getWindow().setType(WindowManager.LayoutParams.TYPE_SYSTEM_ALERT);
            dialog.show();
            //} Temporary used, remove it int the future
            

//            SafetyBoolean isSafeState = NugenGeneralModel.getSafetyBoolean(
//                    mContext, NugenFrameworkConstants.KEY_SAFE_STATE,
//                    SafetyBoolean.FALSE);
//            boolean normalState = (isSafeState.getByte() == SafetyBoolean.FALSE
//                    .getByte());
//
//            if (normalState == true)
//            {
                                             
//                ICustPowerManager powerManager = CustJavaFrameworkManager
//                        .getCustPowerManagerService(mContext);
//
//                // shut down app
//                // set safe state flag
//                if(ExceptionInDevelopment.HANDLEEXCEPTION)
//                {
//                    ExceptionInDevelopment.setTestFlag(mContext);
//                }
//                else
//                {
//                    NugenSettingModel.setSafetyBoolean(mContext,
//                        NugenFrameworkConstants.KEY_SAFE_STATE,
//                        SafetyBoolean.TRUE);
//                }
//                
//                if (powerManager != null)
//                {
//                    powerManager.reboot();
//                }
//                else
//                {
//                    // Apply to the coding standard
//                }
//            }
//            else
//            {
//                // show information dialog for E57
//            }

        }
        
        /**
         * Temporary used, remove it int the future
         * @param context
         * @param title
         * @param message
         * @return
         */
        private AlertDialog warningMessageDialog(Context context, String title, String message)
        {
            AlertDialog.Builder builder = new AlertDialog.Builder(context);
            
            builder.setTitle(title);
            builder.setMessage(message);
            
            builder.setPositiveButton("ok", new OnClickListener(){

                @Override
                public void onClick(DialogInterface dialog, int which)
                {
                    ICustPowerManager powerManager = CustJavaFrameworkManager
                            .getCustPowerManagerService(mContext);
                      
                      try
                      {
                          powerManager.reboot();
                      }
                      catch (RemoteException e)
                      {
                          // TODO Auto-generated catch block
                          e.printStackTrace();
                      }
                    
                }
                
            });
            
            return builder.create();
        }
        
        
        /**
         * 
         * @param stack
         */
        private void debugExceptionInformation(StackTraceElement[] stack)
        {
        	mSbf.append("Time: ")
        	.append(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
        			.format(System.currentTimeMillis()))
        	.append("\n")
        	.append(mException)
        	.append("\n")
        	.append(mMessage)
        	.append("\n");
        	
        	for (StackTraceElement element : stack)
            {
        		mSbf.append("at ").append(element.getClassName()).append(".")
                        .append(element.getMethodName()).append("(")
                        .append(element.getFileName()).append(":")
                        .append(element.getLineNumber()).append(")\n");
            }
        	
        	if (mException.getCause() != null)
        	{
        		mSbf.append("Caused by: \n");
        		
        		for (StackTraceElement element : mException.getCause().getStackTrace())
                {
        			mSbf.append("at ").append(element.getClassName()).append(".")
                            .append(element.getMethodName()).append("(")
                            .append(element.getFileName()).append(":")
                            .append(element.getLineNumber()).append(")\n");
                }
        	}
        	
//            mSbf.append(mException.toString());
//            mSbf.append("\n");          
//            for(int i = 0; i < stack.length; i++)
//            {
//                Debug.printI(TAG, "statck{" + i +"] = " + stack[i].toString());
//                mSbf.append("statck{" + i +"] = " + stack[i].toString());
//                mSbf.append("\n");
//            }
//            
//            mSbf.append(mMessage);
        }
        
        /**
         * 
         * Function Description
         *
         * @return void [out] Delete pre line return if exist. Parameter Description
         */
        private void ReadLog()
        {
            // read log
            try
            {

                String line = null;
               

//                Process process = Runtime.getRuntime().exec("logcat -v time -d");
                
                Process process = new ProcessBuilder("logcat", "-v", "time", "-d").start();
                
                BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(
                        process.getInputStream()), 1024);
                
                StringBuilder log = new StringBuilder();
                           
                while ((line = bufferedReader.readLine()) != null)
                {
                    log.append(line + "\n");
                }

                mContent = log.toString();
                bufferedReader.close();
                process.destroy();
                Debug.printI(TAG, "read log completedly ");
            }
            catch (IOException e)
            {
            	Debug.printI(TAG, "read log exception = " + e.getMessage());
            	e.printStackTrace();
            }
        }
            
     }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
 
