/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.BLERequestHandler
 * Brief: This class is used to handle the request in the BLE state request list.
 *
 * Create Date: 2015/8/10
 * $Revision: 25206 $
 * $Author: KiddYeh $
 * $Id: BLERequestHandler.java 25206 2015-12-01 05:08:00Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import android.content.Context;
import android.content.IntentFilter;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.Looper;
import android.os.Message;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ConnectionStateObserver.IConnectionListener;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction.CommandResponse;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLECallbackContext;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * 
 * This class is used to handle the request in the BLE state request list.
 *
 */
public class BLERequestHandler implements ResponseCallbackWithData, IConnectionListener
{  
    private static final String TAG = "BLERequestHandler";
    
    /**
     * Request Max retry
     */
    private static final int RETRY_TIMES = 0;
    
//    /**
//     *  Key Exchange process timeout
//     */
//    private final int KES_TIMEOUT = 60000; 
    
    /**
     *  Discovery process timeout
     */
    private final int DISCOVERY_TIMEOUT = 60000; 
    
    /**
     *  System Sync process timeout
     */
    private final int SYSTEM_SYNC_TIMEOUT = 10000; 
    
    /**
     *  ResetConnection timeout
     */
    private final int RESET_CONNECTION_TIMEOUT = 20000; 
    
    /**
     * The request-handler to handle the timeout of the 
     * request
     */
    private Handler mHandler = null;  
    
    /**
     *  The application context
     */
    private Context mContext = null;
    
    /**
     * The current request
     */
    private Request mCurrentRequest = null;
    
    /**
     * The list of request
     */
    private List<Request> mRequestList = new LinkedList<Request>();
    
    /**
     * The listener of request timeout 
     */
    private BLECallbackContext mListener = null;
    
    /**
     * The synchronize lock of request timeout 
     */
    private Object mLock = new Object();

    /**
     * The synchronize lock of request timeout 
     */
    private Object mWholeLock = new Object();
    
    
    /**
     * The flag of the start of request
     */
    private AtomicReference<SafetyBoolean> mIsStarted = 
            new AtomicReference<SafetyBoolean>(SafetyBoolean.FALSE);
    
    /**
     *  The request retry counter
     */
    private AtomicInteger mRetryCounter = new AtomicInteger(0);            
    
    private final class RequestHandler extends Handler
    {
        private RequestHandler(Looper looper)
        {
            super(looper);
        }

        @Override
        public void handleMessage(Message msg)
        {
            boolean isNotCompleted = false;
            Request request = (Request) msg.obj;
            IBLERequest process = request.mRequest;
            
            process.request(request.mParameter, BLERequestHandler.this);
            
            synchronized (mLock)
            {
                try
                {
                    mLock.wait(request.mTimeout);
                }
                catch (InterruptedException e)
                {
                    e.printStackTrace();
                }
                finally
                {
                    // Apply to the coding standard
                }
            }
            
            
            isNotCompleted = (SafetyBoolean.FALSE.getByte() == request.mIsCompleted.getByte());
            
            if (isNotCompleted)
            {
                mIsStarted.set(SafetyBoolean.FALSE);
                mListener.onRequestTimeout(process, request.mParameter);
            }
            else
            {
                // Apply to the coding standard
            }
        }
    }

    /**
     *  Request timeout listener
     */
    public interface IOnTimeoutListener
    {
        void onRequestTimeout(IBLERequest request, BLERequestParameter parameter);
    }

    /**
     *  Request interface 
     */
    public interface IBLERequest extends ResponseProcess
    {
        void request(BLERequestParameter parameter, ResponseCallback callback);
    }
    
    /**
     *  Request class
     */
    class Request
    {   
    	/**
    	 *  Request 
    	 */
        private IBLERequest mRequest = null;
        
        /**
         *  Request parameter
         */
        private BLERequestParameter mParameter = null;
        
        /**
         *  Response callback 
         */
        private ResponseCallback mCallback = null;
        
        /**
         *  Request timeout
         */
        private long mTimeout = 6000;
        
        /**
         *  Request complete flag
         */
        private SafetyBoolean mIsCompleted = SafetyBoolean.FALSE;
    }
    
    
    /**
     *  Request handler
     *  
     * @param context : the application context
     *        Range: a valid object of Context
     *        Unit: Context
     *        Scaling: 1
     * @param listener : the request timeout listener
     *        Range: a valid object of IOnTimeoutListener
     *        Unit: IOnTimeoutListener
     *        Scaling: 1 
     *        
     * @return void [out]
     * 
     * @see mContext
     * @see mListener
     *         
     */
    public BLERequestHandler(Context context, BLECallbackContext listener)
    {
        IntentFilter filter = new IntentFilter();
        
        filter.addAction(CommandResponse.BT_ATTR_WRITE);
        
        mContext = context.getApplicationContext();
        mListener = listener;
        // register connection listener
        ConnectionStateObserver.getInstance(mContext).registerConnectionListener(this);
//        mContext.registerReceiver(mWriteResponseReceiver, filter);
    }
    
    
    /**
     * Get the context.
     * 
     * @param N/A
     * 
     * @return mContext
     *        Range: a valid object of Context
     *        Unit: Context
     *        Scaling: 1
     *        
     * @see mContext       
     */
    public Context getContext()
    {
        return mContext;
    }
    
    
    /**
     * Check the BLE-Device-Address length is zero or not and return result.
     * If the BD-Address length is not zero, it is bonded to BLE-Device.
     *   
     *
     * @param N/A
     *  
     * @return result : the result of MP is bonded or not
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1 
     *                      
     * 
     */
    public SafetyBoolean isBonded()
    {
        SafetyBoolean result = SafetyBoolean.FALSE;
        
        byte[] address = BLEController.getBDAddress(mContext);
        
        if (address.length != 0)
        {
            result = SafetyBoolean.TRUE;
        }
        else
        {
            // Apply to the coding standard
        }
        
        return result;
    }
    
    
    /**
     * This method initiates the request-handler thread to handle the timeout of the 
     * request in the list. 
     *
     * @param N/A
     *
     * @return void [out]  
     * 
     * @see mHandler
     * @see mLock
     * @see mIsStarted
     * @see mListener
     *                       
     */
    public void init()
    {
        if (null == mHandler)
        {
            HandlerThread thread = new HandlerThread("BLERequestHandler");
            thread.start();
            
            mHandler = new RequestHandler(thread.getLooper());
        }
    }
    
    
    /**
     * This method adds the request at the end of list. 
     *
     * @param process: the response handler
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1         
     * @param parameter: the request parameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1             
     * @param callback: the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                       
     * @return void [out]
     * @see mRequestList
     * 
     *                       
     */
    public void addRequest(IBLERequest process, BLERequestParameter parameter, ResponseCallback callback)
    {
        addRequestAt(mRequestList.size(), process, parameter, callback);
    }
    
    
    /**
     * This method adds the request at the specific position in the list.
     * If the request is KeyExchange or Discovery ,it sets timeout to the specific
     * time. 
     *
     * @param position: the position in the request list
     *            Range : -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * @param process: the response handler
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1
     * @param parameter: the request parameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1            
     * @param callback: the callback function
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1
     *                        
     * return void [out]
     * 
     * @see mRequestList
     * @see mRequest
     * @see mParameter
     * @see mTimeout
     * @see mCallback
     * 
     *                       
     */
    public void addRequestAt(int position, IBLERequest process, BLERequestParameter parameter, ResponseCallback callback)
    {
//        Log.i("Kidd", "Request: " + process.getClass().getSimpleName());
        
        int size = -1;
        Request request = new Request();
        
        request.mRequest = process;
        request.mParameter = parameter;
        request.mCallback = callback;
        
        size = mRequestList.size();
        
//        Log.i("Kidd", "Size: " + size);
        
        if (position >= size)
        {
            position = size;
        }
        else
        {
            // Apply to the coding standard
        }
        
//        if (process instanceof KeyExchange)
//        {
//        	// Set Key Exchange request timeout to 60sec
//            request.mTimeout = KES_TIMEOUT;
//        }        
//        else 
        if (process instanceof Discovery)
        {
        	// Set Discovery request timeout to 60sec
            request.mTimeout = DISCOVERY_TIMEOUT;
        }
        else if (process instanceof SetSystemSync)
        {
            // Set SetSystemSync request timeout to 100sec
            request.mTimeout = SYSTEM_SYNC_TIMEOUT;
        }
        else if (process instanceof ResetConnection)
        {
            // Set ResetConnection request timeout to 100sec
            request.mTimeout = RESET_CONNECTION_TIMEOUT;
        }
        else
        {
            // Apply to the coding standard
        }
        
        mRequestList.add(position, request);
    }
    
    
    /**
     * This method executes the next request if the request-list is not empty.
     *
     * @param N/A
     *                        
     * @return void [out]
     *                       
     */
    private void executeNextRequest()
    {
        
        if (!mRequestList.isEmpty())
        {
            Message message = Message.obtain();
            
            mCurrentRequest = mRequestList.remove(0);
            message.obj = mCurrentRequest;
            Debug.printD(TAG, "mCurrentRequest = " + mCurrentRequest.mRequest.getClass().getSimpleName());   
            
            for (Request each : mRequestList)
            {
                Debug.printD(TAG, each.mRequest.getClass().getSimpleName());
            }
            
            mHandler.sendMessage(message);    
        }
        else
        {
            mIsStarted.set(SafetyBoolean.FALSE);
            synchronized(mWholeLock)
            {
            	mWholeLock.notifyAll();
            }
        }
    }
    
    /**
     * This method start to execute the request in the list.
     *
     * @param N/A
     *                        
     * @return void [out]
     *                       
     */
    public void startHandleRequest()
    {
        boolean isNotStarted = (SafetyBoolean.FALSE.getByte() == mIsStarted.get().getByte());
        
        if (isNotStarted)
        {
            mIsStarted.set(SafetyBoolean.TRUE);
            executeNextRequest();
        }
        else
        {
            // Apply to the coding standard
        }
    }
    

    public void startHandleRequestAndWait( long msec )
    {
        boolean isNotStarted = (SafetyBoolean.FALSE.getByte() == mIsStarted.get().getByte());
        
        if (isNotStarted)
        {
            mIsStarted.set(SafetyBoolean.TRUE);
            executeNextRequest();
            synchronized (mWholeLock)
            {
                try
                {
                	mWholeLock.wait(msec);
                }
                catch (InterruptedException e)
                {
                    e.printStackTrace();
                }
                finally
                {
                    // Apply to the coding standard
                }
            }
            
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * This method removes all requests in the list.
     *
     * @param N/A
     *                        
     * @return void [out]
     *                       
     */
    public void clearRequest()
    {
        synchronized(mRequestList)
        {
            mRequestList.clear();
        }
    }
    
    
    /**
     * This method remove all the request if the connection-state is changed to disconnected. 
     *
     * @param isConnected: the connection state is connected or not
    *            Range: a valid object of SafetyBoolean
     *           Unit: SafetyBoolean
     *           Scaling: 1           
     *                        
     * @return void [out]
     *                       
     */
    @Override
    public void onConnectionStateChanged(int state)
    {

        boolean isNotConnected = (( CommsConstant.BtState.DISCONNECTED == state ) || ( CommsConstant.BtState.CONNECTIONLOST == state ));
        
        if (isNotConnected)
        {            
            if (null != mCurrentRequest)
            {
                mCurrentRequest.mIsCompleted = SafetyBoolean.TRUE;
            }
            
            synchronized(mLock)
            {
                mLock.notifyAll();
            }
            
            clearRequest();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    
    /**
     * This method notifies the request is finished and executes the next 
     * request. 
     *
     * @param result: the result of request is done or not.
    *            Range: a valid object of SafetyBoolean
     *           Unit: SafetyBoolean
     *           Scaling: 1  
     *                        
     * @return void [out]
     * @see mRetryCounter
     * @see mCallback
     *                       
     */
    @Override
    public void onRequestCompleted(SafetyBoolean result)
    {        
        Request request = mCurrentRequest;
        
        request.mIsCompleted = SafetyBoolean.TRUE;
        
        synchronized(mLock)
        {
            mLock.notifyAll();
        }
        
        boolean isResultOK = SafetyBoolean.TRUE.getByte() == result.getByte();
		boolean isOverRetryCount = mRetryCounter.incrementAndGet() > RETRY_TIMES;
		if (isOverRetryCount || isResultOK)
        {
            mRetryCounter.set(0);
            request.mCallback.onRequestCompleted(result);
        }
        else
        {
            Debug.printD(TAG, "Retry Request = " + request.mRequest.getClass().getSimpleName());
            
            addRequestAt(0, request.mRequest, request.mParameter, request.mCallback);
        }
        
        executeNextRequest();
    }


    @Override
    public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
    {
        Request request = mCurrentRequest;
        
        request.mIsCompleted = SafetyBoolean.TRUE;
        
        synchronized(mLock)
        {
            mLock.notifyAll();
        }
        
        if (null != request.mCallback )
        {
            ((ResponseCallbackWithData) request.mCallback).onRequestCompleted(result, pack);
        }

        executeNextRequest();
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R16949 2015-09-10 03:50:14 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
