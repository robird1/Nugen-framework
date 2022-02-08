/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: UARTAccessWithFlowControl
 * 
 * Brief:
 * The class defines the actions after getting UICP interrupt and provides send
 * interface to modules in RC APP.
 * 
 * Create Date: 2015/2/2
 * $Revision: 22785 $
 * $Author: WillLong $
 * $Id: UARTAccessWithFlowControl.java 22785 2015-10-29 02:20:50Z WillLong $
 */

package com.accu_chek.solo_m.rcapp.communication.uartuicp;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.IBinder;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager.UARTPort;
import com.accu_chek.solo_m.rcapp.communication.uartuicp.IUICP;
import com.accu_chek.solo_m.rcapp.communication.uartuicp.UICP.REMOTE_SEND_STATUS;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.CommsJNI;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.UICommandDispatcher;

/**
 * The class defines the actions after getting UICP interrupt and provides send
 * interface to modules in RC APP.
 */
public class UARTAccessWithFlowControl
{
    
    private static final String TAG = "UARTAccessWithFlowControl";
    /**
     * expected data length when receiving data
     */
    private static final int EXPECTED_LENGTH = 256;

    /**
     * Allowed port is COMMSUART in UARTPort.
     */
    private static final int COMMS_PORT = UARTPort.COMMSUART;

    /**
     * UARTAccessWithFlowControl singleton object
     */
    private static UARTAccessWithFlowControl mControl = null;

    /**
     * the interface of CommandDispatcherService to send data to Dispatcher
     */
    private static UICommandDispatcher mCommandDispatcher = null;

    /**
     * Maximum number for retry Rx
     */
    private static final int MAX_RX_SHORT_TIME_RETRY = 6;

    /**
     * Delay short time (50ms) for retry Rx
     */
    private static final long DELAY_RX_SHORT_TIME = 50L;

    /**
     * Delay long time (1000ms) for retry Rx
     */
    private static final long DELAY_RX_LONG_TIME = 1000L;

    /**
     * array initial size
     */
    private final int ARRAY_INITIAL_SIZE = 0;

    /**
     * Save Context of constructor input
     */
    private Context mContext = null;

    /**
     * UICP control
     */
    private UICPControl mUICPControl = null;

    /**
     * A flag to record the Uart close state.
     * Range: true, false
     * Unit: boolean
     * Scaling: 1
     */
    private boolean mIsUartClosed = false;

    /**
     * A flag to record the Uart restart.
     * Range: true, false
     * Unit: boolean
     * Scaling: 1
     */
    private boolean mIsUartRestart = false;

    /**
     * A flag to record the current Tx state.
     * Range: true, false
     * Unit: boolean
     * Scaling: 1
     */
    private boolean mIsTxOpened = false;

    /**
     * A flag to record the current Rx state.
     * Range: true, false
     * Unit: boolean
     * Scaling: 1
     */
    private boolean mIsRxOpened = false;

    /**
     * Retry times
     * Range: true, false
     * Unit: int
     * Scaling: 1
     */
    private int mRxRetryTimes = 0;

    /**
     * UART manager
     */
    private UARTManager mUARTManager = null;
    
    private static Handler mHandler = null;
    

    private static ReceiveDataRunnable mRecieveDataRun = null;
    
    static {
        
        mHandler = new Handler();
        mRecieveDataRun = new ReceiveDataRunnable(null);
        
    }

    /**
     * Runnable for UARTAccessWithFlowControl receive Data.
     */
    static class ReceiveDataRunnable implements Runnable
    {
        
        /**
         * Save Context of constructor input
         */
        private Context mContext = null;

        /**
         * ReceiveDataRunnable Constructor
         * 
         * see mContext [in] This global variable is referred for saving Context of caller.
         * 
         * @param context [in] Context of caller. Context object is provided by Android SDK.
         * Range: Valid Context object in Android
         * Unit: Context
         * Scaling: 1
         */
        public ReceiveDataRunnable(Context context)
        {
            mContext = context;
        }

        @Override
        public void run()
        {
            boolean isOkForReceiveData;
            Debug.printI(TAG, "[ReceiveDataRunnable] run");
            UARTAccessWithFlowControl control = UARTAccessWithFlowControl
                    .getInstance(mContext);

            if (control != null )
            {
                try
                {
                    int isClosed;

                    isClosed = control.isUartClosed();
                    if(true == control.mIsUartRestart)
                    {
                        control.mIsUartRestart = false;

                        // Delay some time to receive data again
                        mHandler.postDelayed(mRecieveDataRun, 1000);

                    }
                    else if(HammingDistance.SAFETY_BOOLEAN_FALSE == isClosed)
                    {
                        // Remove call back first
                        mHandler.removeCallbacks(mRecieveDataRun);

                        // open RX to receive data
                        isOkForReceiveData = control.receiveStart();

                        if( isOkForReceiveData == true )
                        {
                            // Recieve Rx data
                            control.receiveData(EXPECTED_LENGTH);
                        }

                        // Retry Rx
                        control.retryRx();
                    }
                    else
                    {
                        // Apply to the coding standard
                    }
                    
                }
                catch (OperationFailException e)
                {
                    e.printStackTrace();
                    
                }
                catch (ArgumentErrorException e)
                {
                    e.printStackTrace();
                    
                    // call emwr
                }
                finally
                {
                   // Apply to coding standard
                }
            }
            else
            {
                // Apply to coding standard
            }
            
        }
        
    }

    /**
     * The receiver just receives the interrupt with FALLING state.
     */
    public static class UARTReceiverWithUICP extends BroadcastReceiver
    {
        /**
         * Uevent state field
         */
        private static final String USTATE = "UEVT_STATE";

        /**
         * Expected Uevent state
         */
        private static final String UEVT_FAILING = "FALLING";

        /**
         * When FALLING state starts to receive, COMMS UART receiving with UICP is executed. 
         * New timer of receiving data is started when get FALLING state.
         * 
         * see EXPECT_USTATE [in] This global variable is referred for expected Uevent state.
         * 
         * @param context [in] Context of caller. Context object provided by
         *            Android SDK.
         * Range: Valid Context object in Android
         * Unit: Context
         * Scaling: 1
         * @param intent [in] Intent from Android. Intent object provided by
         *            Android SDK.
         * Range: Valid Intent object in Android
         * Unit: Intent
         * Scaling: 1           
         * 
         * return None
         */
        @Override
        public void onReceive(Context context, Intent intent)
        {
            String state = intent.getStringExtra(USTATE);
            Debug.printI(TAG, "onReceive: UEVENT= " + state);

            if (UEVT_FAILING.equals(state))
            {
                // Remove call back first
                mHandler.removeCallbacks(mRecieveDataRun);

                // Delay some time to receive data again
                mHandler.postDelayed(mRecieveDataRun, DELAY_RX_SHORT_TIME);
            }
            else
            {
                // Apply to the coding standard
            }
        }

    }


    /**
     * Singleton for UARTAccessWithFlowControl. Create and return
     * UARTAccessWithFlowControl instance.
     * 
     * @param context [in] Context of caller. Context object provided by Android
     *            SDK.
     * Range: Valid Context object in Android
     * Unit: Context
     * Scaling: 1  
     *            
     * see mControl [in] This global variable is referred for getting UARTAccessWithFlowControl singleton object.
     * 
     * return UARTAccessWithFlowControl instance. For detail of this object, see UARTAccessWithFlowControl.
     * Range: Valid UARTAccessWithFlowControl object
     * Unit: UARTAccessWithFlowControl
     * Scaling: 1    
     */
    private static synchronized UARTAccessWithFlowControl getInstance(
            Context context)
    {
        
        if (mControl == null)
        {
            try
            {
                Debug.printI(TAG, "New");
                mControl = new UARTAccessWithFlowControl(context);
            }
            catch (OperationFailException e)
            {
                Debug.printI(TAG, "getInstance OperationFailException");
                e.printStackTrace();

                // call emwr
            }
            finally
            {
                // Apply to coding standard
            }

        }
        else
        {
            // Apply to coding standard
        }

        return mControl;
    }

    /**
     * Singleton for UARTAccessWithFlowControl. Return UARTAccessWithFlowControl
     * instance.
     * 
     * @param None [in]
     * 
     * see mControl [in] This global variable is referred for getting UARTAccessWithFlowControl singleton object.
     * 
     * return UARTAccessWithFlowControl instance. For detail of this object, see UARTAccessWithFlowControl.
     * Range: Valid UARTAccessWithFlowControl object
     * Unit: UARTAccessWithFlowControl
     * Scaling: 1 
     * 
     * throw DataIntegrityException when UARTAccessWithFlowControl object can't be got.
     */
    public static synchronized UARTAccessWithFlowControl getInstance()
    {
        if (mControl == null)
        {
            Debug.printI(TAG, "getInstance DataIntegrityException");
            throw new DataIntegrityException(
                    "UART Access with Flow Control is null. It should be created by COMMS POWER ON.");
        }
        else
        {
            // Apply to coding standard
        }

        return mControl;
    }

    /**
     * UARTAccessWithFlowControl Constructor initializes UICP manager, UART manager and Command Dispatcher. Finally, open COMMS UART port.
     * 
     * @param context [in] Context of caller. Context object provided by Android SDK.
     *            
     * see mUICPControl [in] This global variable is referred for getting UICP Control.
     * see mUARTManager [in] This global variable is referred for getting UART Manager.
     * see mCommandDispatcher [in] This global variable is referred for getting the interface of CommandDispatcherService to send data to Dispatcher.
     * see ICustomizedHWManager.UART_SERVICE [in] This global variable is referred for getting UART service's name.
     *            
     * throw OperationFailException if fail to get UICP binder or fail to open UART port.
     */
    private UARTAccessWithFlowControl(Context context)
            throws OperationFailException
    {
                
        IBinder binder = CustJavaFrameworkManager.getUICPBinder();

        mContext = context;

        mUICPControl = new UICPControl(IUICP.Stub.asInterface(binder));

        mUARTManager = (UARTManager) ICustomizedHWManager
                .getSystemService(ICustomizedHWManager.UART_SERVICE);

        if (mUARTManager != null)
        {
            Debug.printI(TAG, "Open UART");
       
            mUARTManager.open(UARTPort.COMMSUART);
        }
        else
        {
            throw new OperationFailException("Fail to access UART service.");
        }
    }

    /**
     * Send data with UICP. (Only support COMMS UART port)
     * 
     * see mUICPControl [in] This global variable is referred for getting UICP Control.
     * see mUARTManager [in] This global variable is referred for getting UART Manager.
     * 
     * @param data [in] safety byte array of sending data byte array in SafetyByteArray:
     *              byte array in SafetyByteArray
     *              Range: 0 ... max int number
     *              Unit: byte
     *              Scaling: 1
     * @param int isFinal: The flag shows that if it is a final command.
     *              Range: HammingDistance.SAFETY_BOOLEAN_FALSE
     *                  /HammingDistance.SAFETY_BOOLEAN_TRUE
     *              Unit: int 
     *              Scaling: 1
     * 
     * throws OperationFailException when sending data has any exceptions.
     * 
     * return None 
     */
    public void send(final SafetyByteArray data, int isFinal) throws OperationFailException
    {
        // Remove Rx data call back first
        mHandler.removeCallbacks(mRecieveDataRun);

        if(false == mIsUartClosed)
        {
            // Close Rx
            receiveEnd();

            // Start Tx
            Debug.printI(TAG, "[Tx] Start");
            mIsTxOpened = true;
            mUICPControl.openTx();
            mUARTManager.send(COMMS_PORT, data);
            if( HammingDistance.SAFETY_BOOLEAN_TRUE == isFinal )
            {
                // Set uart closed
                mIsUartClosed = true;
                mUICPControl.seFunctionMode();
            }else
            {
                mUICPControl.closeTx();
            }
            mIsTxOpened = false;
            Debug.printI(TAG, "[Tx] End");
        }
        else
        {
            // Apply to the coding standard
            Debug.printI(TAG, "[Tx] Uart closed...");
        }

    }

    /**
     * Uart enter to sleep mode
     * 
     * 
     * @param none
     * 
     * throws OperationFailException when sending data has any exceptions.
     * 
     * return None 
     */
    public void enterSleepMode() throws OperationFailException
    {
        Debug.printI(TAG, "enterSleepMode");
        // Set uart closed
        mIsUartClosed = true;

        // Close UICP
        if( mUICPControl != null )
        {
            // Close Rx
            receiveEnd();
        }
        else
        {
            // Apply to the coding standard
        }

        // Close Uart port
        mUARTManager.close(UARTPort.COMMSUART);

        // Power Off Comms
        CommsJNI.powerOffComms();
        Debug.printI(TAG, "PowerOff Comms");

    }

    /**
     * Uart restart
     * 
     * @param none
     * 
     * Set uart restart flag
     * 
     * return None 
     */
    public void restartUART() throws OperationFailException
    {
        Debug.printI(TAG, "restartUART");
        // Set uart closed
        mIsUartClosed = false;
        mIsUartRestart = true;

        // Power On Comms
        CommsJNI.powerOnComms();
        Debug.printI(TAG, "PowerOn Comms");

        // Open Uart port
        mUARTManager.open(UARTPort.COMMSUART);

        // Reset UICP
        if ( mUICPControl != null )
        {
            mUICPControl.reset();
        }

    }

    /**
     * Receive data via UART with UICP. (Only support COMMS UART port), and then transfer to Command Dispatcher in RC meter.
     * 
     * @param none
     * 
     * return boolean, is Rx opened
     * Range: true or false
     * Unit: boolean
     * Scaling: 1
     * 
     * throw OperationFailException when sending data has any exceptions.
     * 
     */
    private boolean receiveStart() throws OperationFailException
    {
        if ( mUICPControl != null )
        {
            if( mIsRxOpened == false )
            {
                // Wait Tx Closed
                if( mIsTxOpened == false )
                {
                    mUICPControl.openRx();
                    mIsRxOpened = true;
                    mRxRetryTimes = 0;
                    Debug.printI(TAG, "[Rx] Start");
                }
                else
                {
                    // Apply to the coding standard
                }
            }
            else
            {
                // Apply to the coding standard
            }
        }
        else
        {
            // Apply to the coding standard
        }
        return mIsRxOpened;
    }

    /**
     * Recieve end and close UICP RX
     * 
     * @param none
     * 
     * throw OperationFailException when sending data has any exceptions.
     * 
     * return None 
     * @throws ArgumentErrorException 
     */
    private void receiveEnd() throws OperationFailException
    {
        if ( mUICPControl != null )
        {
            if( mIsRxOpened == true )
            {
                mUICPControl.closeRx();
                mIsRxOpened = false;
                mRxRetryTimes = 0;
                Debug.printI(TAG, "[Rx] End");
            }
            else
            {
                // Apply to the coding standard
            }
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Receive data via UART. (Only support COMMS UART port)
     * If the function get any data from COMMS UART port, the data is sent to Command Dispatcher in RC meter.
     * 
     * see mUARTManager [in] This global variable is referred for getting UART Manager.
     * see mCommandDispatcher [in] This global variable is referred for getting the interface of CommandDispatcherService to send data to Dispatcher.
     * 
     * @param byteLength [in] expected byte array size.
     * Range: 1 ... 512
     * Unit: byte
     * Scaling: 1
     * 
     * throw OperationFailException when sending data has any exceptions.
     * 
     * return None 
     * @throws OperationFailException 
     */
    private void receiveData(int byteLength) throws ArgumentErrorException, OperationFailException
    {

        if (mUARTManager != null)
        {
            SafetyByteArray readDataSafety = null;
            byte[] readData = new byte[ARRAY_INITIAL_SIZE];

            // Increase rx times
            mRxRetryTimes = mRxRetryTimes + 1;

            readDataSafety = mUARTManager.receive(COMMS_PORT, byteLength);

            readData = readDataSafety.getByteArray();

            Debug.printI(TAG, "receiveData: readData.length=" + readData.length);

            if (readData.length > 0)
            {
                // Reset retry times
                mRxRetryTimes = 0;

                // Get UICommandDispatcher
                if( mCommandDispatcher == null )
                {
                    mCommandDispatcher = UICommandDispatcher.getInstance(mContext);
                }
                else
                {
                    // Apply to the coding standard
                }

                // Send frame to UICommandDispatcher
                mCommandDispatcher.receiveFrame(readDataSafety);
            }
            else
            {
                // Empty for static code analysis
            }

        }
        else
        {
            // Empty for static code analysis
        }
    }

    /**
     * Get Rx open status
     * 
     * @param None [in]
     * 
     * return: none
     * 
     */
    private void retryRx() throws OperationFailException, ArgumentErrorException
    {
        if( mIsRxOpened )
        {
            int status;
            status = mUICPControl.getSendingStatus();
            if( status == REMOTE_SEND_STATUS.SENDING )
            {
                // Check Rx every 50 ms. If there is no data within 300ms, then check data every 1sec. 
                if( mRxRetryTimes < MAX_RX_SHORT_TIME_RETRY )
                {
                    mHandler.postDelayed(mRecieveDataRun, DELAY_RX_SHORT_TIME);
                }
                else
                {
                    mHandler.postDelayed(mRecieveDataRun, DELAY_RX_LONG_TIME);
                }
            }
            else
            {
                // Close Rx
                receiveEnd();
            }
        }
        else
        {
            // Delay some time to check again
            mHandler.postDelayed(mRecieveDataRun, DELAY_RX_SHORT_TIME);
        }
    }

    /**
     * Get Uart Close status
     * 
     * @return int isUartClosed: The flag shows that if uart is closed.
     *          Range: HammingDistance.SAFETY_BOOLEAN_FALSE
     *                  /HammingDistance.SAFETY_BOOLEAN_TRUE
     *          Unit: int 
     *          Scaling: 1
     */
    private int isUartClosed()
    {
        int isClosed;
        
        if( true == mIsUartClosed )
        {
            isClosed = HammingDistance.SAFETY_BOOLEAN_TRUE;
        }
        else
        {
            isClosed = HammingDistance.SAFETY_BOOLEAN_FALSE;
        }
        return isClosed;
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
