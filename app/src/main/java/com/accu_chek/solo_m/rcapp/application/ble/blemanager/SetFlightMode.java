/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.SetFlightMode
 * Brief: This class handles SetFlightMode request and its response.
 *
 * Create Date: 2015/7/29
 * $Revision: 25196 $
 * $Author: IvanHuang $
 * $Id: SetFlightMode.java 25196 2015-12-01 02:52:47Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.CommandCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.request.FlightModeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.CauseOnlyResponse;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.IUICommandDispatcher;

/**
* This class handles SetFlightMode request and its response.
*/
public class SetFlightMode implements IBLERequest
{
    private static final String TAG = "SetFlightMode";
    
    /**
     * The instance of SetFlightMode class
     */
    private static volatile SetFlightMode mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;
    
    /**
     * Get the one and only instance of the class SetFlightMode.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class SetFlightMode
     *         Range: A valid object of SetFlightMode
     *         Unit: SetFlightMode
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class SetFlightMode
     */
    public static SetFlightMode getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new SetFlightMode(context);
        }
        else
        {
            // Apply to the coding standard
        }
        return mInstance;
    }
    
    /**
     * The class constructor
     * 
     * @param context: an application context
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mCallback             
     *            
     */
    protected SetFlightMode(Context context)
    {
        mContext = context.getApplicationContext();
    }
    
    /**
     * This method is called after receiving the CauseOnlyResponse broadcast.
     * It gets the cause of the response to check if the SetFlightMode is successful or not.
     * It returns the result of response via callback function.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: a valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     *            
     * @return void [out] 
     * 
     * @see mCallback             
     *            
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
        Debug.printD(TAG, "[SetFlightMode]: Response enter ");
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        CauseOnlyResponse response = (CauseOnlyResponse) pack.getResponse();
        
        BLEResponseReceiver.getInstance(context).unregisterReceiver();
        
        int cause = response.getCause().get();
        int command = response.getCommand().get();
        SafetyBoolean isResult = SafetyBoolean.FALSE;
            
        if ((0 == cause) && (CommandCode.COMM_SET_FLIGHT_MODE == command) )
        {
            isResult = SafetyBoolean.TRUE;  
        }
        else
        {
            // Apply to the coding standard
        }
        returnResult(isResult);
    }   
    
    
    /**
     * This method shall check if the callback exists or not. 
     * If yes, it returns the response result via callback
     * 
     * @param isResult [in] the result of response of current request  
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1
     *                      
     * @return void [out] 
     * 
     * @see mCallback  
     */
    protected void returnResult(SafetyBoolean isResult)
    {
        if( null != mCallback )
        {
            mCallback.onRequestCompleted(isResult);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    
    /**
     * This method handles enable Flight mode.
     * 
     * @param callback : the callback function of Response
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1             
     *            
     * @return void [out] 
     * 
     * @see mCallback 
     * @see mContext
     */
    protected void enableFlightMode(ResponseCallback callback)
    {
  
        FlightModeRequest request = (FlightModeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.COMM_SET_FLIGHT_MODE);

        mCallback = callback;
        
        if ( null != request )
        {
         
            BLEResponseReceiver.getInstance(mContext).registerReceiver(
                    ResponseAction.CommandResponse.COMM_SET_FLIGHT_MODE, this);
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            returnResult(SafetyBoolean.FALSE);
        }
    }
    
    /**
     * This method handles the Flight mode request.
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the callback function of response   
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1   
     *                      
     * @return void [out] 
     * 
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[SetFlightMode]: Request enter ");
        boolean isEnable = (SafetyBoolean.TRUE.getByte() == parameter.getIsEnable().getByte());
        SafetyBoolean isResult = null;
        mCallback = callback;
        
        if( isEnable )
        {
            enableFlightMode(mCallback); 
        }
        else
        {
            //reset Comms
            Debug.printD(TAG, "[SetFlightMode]: Power on comms ");
            IUICommandDispatcher uiCommandDispatcher = CustJavaFrameworkManager.getUICommandDispatcher(mContext);
            try
            {
                uiCommandDispatcher.resetComms();
                isResult = SafetyBoolean.TRUE;
            }
            catch (RemoteException e)
            {
                isResult = SafetyBoolean.FALSE;
            }
            finally
            {
                returnResult(isResult);
            }

        }
        
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
