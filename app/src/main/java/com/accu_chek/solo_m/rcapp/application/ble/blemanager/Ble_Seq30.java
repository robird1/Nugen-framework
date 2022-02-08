/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.Ble_Seq30
 * Brief: This class handles the register of Service changed status indication and returns result via callback. 
 *
 * Create Date: 2015/8/18
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: Ble_Seq30.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseProcess;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class Ble_Seq30 implements IBLERequest
{
    private static final String TAG = "Ble_Seq30";
     
    /**
     * The class instance  
     */
    private static volatile Ble_Seq30 mInstance = null;
    /**
     * The application context
     */
    private Context mContext = null;
    /**
     * The callback function
     */
    private ResponseCallback mCallback = null;
    
    /**
     *  The AttrNotification response
     */
    private AttrNotification mAttrNotification = new AttrNotification();
    
    /**
     * Get the one and only instance of the class Ble_Seq30.
     * 
     * @param context :
     *         Range: a valid object of Context
     *         Unit: Context
     *         Scaling: 1          
     * 
     * @return Ble_Seq30 : the one and only instance of the class Ble_Seq30
     *         Range: a valid object of Ble_Seq30
     *         Unit: Ble_Seq30
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class Ble_Seq30
     */
    public static Ble_Seq30 getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new Ble_Seq30(context);
        }

        return mInstance;
    }

    /**
     * The class constructor
     * 
     * @param context: 
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *     
     * @return void [out]             
     *            
     * @see mContext           
     */
    private Ble_Seq30(Context context)
    {
        mContext = context;
    }

    /**
     * Register the Service changed indication listener.
     * 
     * @param callback: a callback function
     *            Range: a valid object of ResponseCallback  
     *            Unit: ResponseCallback
     *            Scaling: 1
     *
     * @return void [out] 
     *            
     * @see mCallback
     * @see mContext
     */  
    public void registerServiceChangedIndicationListener(ResponseCallback callback)
    {
        Debug.printD(TAG, "Register ServiceChangedIndicationListener.");
        mCallback = callback;
        BLEResponseReceiver.getInstance(mContext.getApplicationContext())
        .registerReceiver(ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND, mAttrNotification);
    
   
    }
     
    class AttrNotification implements ResponseProcess
    {

    /**
         * This method gets the E2E-Result of the indication 
         * to check if the SetDateTimeCP Opcode is successful or not.
         * It returns the result of response and response pack via callback function.
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
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
            Debug.printD(TAG, " [Ble_Seq30]: AttrNotification ");

            // UUIDCode
            int iUUID = 0; 
        
            // Response Pack
            ResponsePack attPack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

            // Response
            AttributeChangeNotification response = (AttributeChangeNotification) attPack.getResponse();
        
            iUUID = response.getAttribHandle().get();

            Debug.printD(TAG, " iUUID : " + iUUID);
            
            // Operation corresponded with OpCode
            switch(iUUID)
            {
       
            case UUID.UUID16.SERVICE_CHANGE:
                BLEResponseReceiver.getInstance(context).unregisterReceiver();
        mCallback.onRequestCompleted(SafetyBoolean.TRUE);
        
                break;
                
            default:
                
                // Apply to the coding standard
                
                break;
            }
            
                       
        }
        
    }
    
//    public void doProcess(Context context, Intent intent)
//    {
//        Debug.printD(TAG, "[Ble_Seq30]: Response enter ");   
//        
//        // UUIDCode
//        int iUUID = 0; 
//         
//        // Response Pack
//        ResponsePack attPack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
//        
//        // Response
//        AttributeChangeNotification response = (AttributeChangeNotification) attPack.getResponse();
//        
//        iUUID = response.getAttribHandle().get();
//
//        Debug.printD(TAG, " iUUID : " + iUUID);
//        
//        // Operation corresponded with OpCode
//        switch(iUUID)
//        {
//
//        case UUID.UUID16.SERVICE_CHANGE:
//            BLEResponseReceiver.getInstance(context).unregisterReceiver();
//            mCallback.onRequestCompleted(SafetyBoolean.TRUE);
//            
//            break;
//            
//        default:
//            
//            // Apply to the coding standard
//            
//            break;
//        }
//        
//                   
//    }
    /**
     * No functionality
     * 
     * @param parameter [in] BLERequestParameter
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] ResponseCallback.     
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1 
     *                
     * @return void [out] 
     * 
     * @see mContext
     */
    
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        // No functionality   
    }

    @Override
    public void doProcess(Context context, Intent intent)
    {
        // TODO Auto-generated method stub
        
    }

}

/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
