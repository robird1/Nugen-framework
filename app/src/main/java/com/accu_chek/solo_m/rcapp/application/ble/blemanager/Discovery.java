/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.Discovery
 * Brief: This class handles the Discovery process and its responses sequence.
 *
 * Create Date: 2015/7/22
 * $Revision: 24910 $
 * $Author: IvanHuang $
 * $Id: Discovery.java 24910 2015-11-26 06:35:34Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID.UUID128;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID.UUID16;
import com.accu_chek.solo_m.rcapp.application.ble.request.ServiceDiscoveryRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.ServiceDiscoveryIndication;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * This class handles the Discovery and its responses sequence.
 */
public class Discovery implements IBLERequest
{
    private static final String TAG = "Discovery";
    
    /**
     * The instance of Discovery class
     */
    private static volatile Discovery mInstance = null;
    
    /**
     * The application context
     */
    private Context mContext = null;
    
    /**
     * The response callback
     */
    private static ResponseCallback mCallback = null;
    
    /**
     * The UUID16 service list
     */
    private static List<Integer> mServiceUUID16List = new LinkedList<Integer>();
    
    /**
     * The UUID128 service list
     */
    private static List<byte[]> mServiceUUID128List = new LinkedList<byte[]>();
    
    /**
     * UUID128 element length (+ GAP)
     */
    private static final int UUID128_ELEMENT_LENGTH = 18;
    
    /**
     * UUID16 element length (+ GAP)
     */
    private static final int UUID16_ELEMENT_LENGTH = 4;
    
    /**
     * The discovery handler map
     */
    private static Map<Integer, IDiscoveryHandler> mHandlerMap = 
            new HashMap<Integer, IDiscoveryHandler>();
    /**
     * 
     * The interface of discovery handler
     *
     */
    interface IDiscoveryHandler
    {
        void handle(ServiceDiscoveryIndication response);
    }

    static
    {
        mHandlerMap.put(UUID16_ELEMENT_LENGTH, new UUID16ServiceHandler());
        mHandlerMap.put(UUID128_ELEMENT_LENGTH, new UUID128ServiceHandler());
        mHandlerMap.put(0, new DiscoveryDoneHandler());        
    }
     
    /**
     * This method gets the one and only instance of the class Discovery.
     * 
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     *            
     * @return mInstance : the one and only instance of the class Disconnect
     *         Range: A valid object of Disconnect
     *         Unit: Disconnect
     *         Scaling: 1            
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class Discovery
     */
    public static Discovery getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new Discovery(context);
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
     * @see mContext   
     *            
     */
    protected Discovery(Context context)
    {
        mContext = context;
    }
    
    
    /**
     * This method dispatches the response to Discovery handler. It is called after receiving the 
     * certain broadcast.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: a valid object of context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: a valid object of Intent
     *            Unit: Intent
     *            Scaling: 1
     *            
     * @return void [out]   
     * 
     * @see mHandlerMap
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
          
        Debug.printD(TAG,"[Discovery] : Response enter");
        
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
        
        CommonUtils.objectCheck(pack);
        
        ServiceDiscoveryIndication response = 
                (ServiceDiscoveryIndication) pack.getResponse();
        
        CommonUtils.objectCheck(response);
        
        int cause = response.getCause().get();
        int elLength = response.getElementLength().get();
        int command = response.getCommand().get();
        int type = response.getDiscoveryType();
        int subCause = response.getSubcause().get();
        int elCount = response.getElementCount().get();           
        int elGap = response.getGap().get();
        
        Debug.printD(TAG, "Discovery Command = " + command);
        Debug.printD(TAG, "Discovery Type = " + type);
        Debug.printD(TAG, "Discovery Cause = " + cause);
        Debug.printD(TAG, "Discovery SubCause = " + subCause);
        Debug.printD(TAG, "Discovery Element Count = " + elCount);
        Debug.printD(TAG, "Discovery Element Length = " + elLength);
        Debug.printD(TAG, "Discovery Element Gap = " + elGap);
        
        if (0 == elLength)
        {
            BLEResponseReceiver.getInstance(context).unregisterReceiver();
        }
        
        mHandlerMap.get(elLength).handle(response);
    }
    
    
    /**
     * This function fills the UUIDs of services that is requested by the MPR process.
     * 
     * @param N/A
     *
     * @return void [out] 
     * 
     * @see mServiceUUID16List
     * @see mServiceUUID128List
     */
    protected void fillUUID()
    {

        boolean isBeforeBonding = (SafetyBoolean.FALSE.getByte() == BLEController.getInstance(mContext).isBonded().getByte());
        
        mServiceUUID16List.clear();
        mServiceUUID128List.clear();
        
        if (isBeforeBonding)
        {
            mServiceUUID16List.add(UUID16.SERVICE_CHANGE); // 0x2a05
            mServiceUUID16List.add(UUID16.IAS_BEEP_MP);  //0x2a06
            mServiceUUID16List.add(UUID16.DIS_SYSTEM_ID); //0x2a23
            mServiceUUID16List.add(UUID16.DIS_MANUFACTURER_NAME); //0x2a29

            mServiceUUID128List.add(UUID128.KEY_EXCHANGE_CP);
        }
        else
        {   
            mServiceUUID16List.add(0x2A00);
            mServiceUUID16List.add(0x2A01);
//          mServiceUUID16List.add(0x2A02);
            
            mServiceUUID16List.add(UUID16.SERVICE_CHANGE);        // 0x2a05
            mServiceUUID16List.add(UUID16.IAS_BEEP_MP);           //0x2a06
            mServiceUUID16List.add(UUID16.DIS_MANUFACTURER_NAME); //0x2a29
            mServiceUUID16List.add(UUID16.RECORD_ACCESS_CP);      //0x2a52
            
            mServiceUUID16List.add(UUID16.IDD_STATUS_CHANGED);   // 0x2b01
            mServiceUUID16List.add(UUID16.IDD_DEVICE_STATUS);    // 0x2b02
            mServiceUUID16List.add(UUID16.IDD_STATUS_READER_CP); // 0x2b04
            mServiceUUID16List.add(UUID16.IDD_FEATURES);         // 0x2b03
            mServiceUUID16List.add(UUID16.IDD_COMMAND_CP);       // 0x2b05
            mServiceUUID16List.add(UUID16.IDD_COMMAND_DATA);     // 0x2b06
            mServiceUUID16List.add(UUID16.IDD_ANNUNCIATION_STATUS); //0x2b07
            mServiceUUID16List.add(UUID16.IDD_HISTORY_DATA);     //0x2b08
            
            mServiceUUID128List.add(UUID128.SOLOM_STATUS_CHANGED);
            mServiceUUID128List.add(UUID128.SOLOM_CP);
        }

    }
    
    
    static class UUID16ServiceHandler implements IDiscoveryHandler
    {
        /**
         *  This method checks if the Discovery Information is in the UUID16 Service
         *  list or not. If it is yes, this method removes the Service UUID from the list.
         *  
         * @param response [in] the response of discovery 
         *            Range: a valid object of ServiceDiscoveryIndication
         *            Unit: ServiceDiscoveryIndication
         *            Scaling: 1
         *              
         * @return void [out] 
         * 
         * @see mServiceUUID16List
         */
        @Override
        public void handle(ServiceDiscoveryIndication response)
        {
            byte MASK = (byte) 0xFF;
            byte SHIFT8BIT = 8;
            int elCount = response.getElementCount().get(); 
            int elLength = response.getElementLength().get();
            byte[] data = response.getData();
            int byteArraySize = data.length;
            byte bHighByte = 0;
            byte bLowByte = 0;
            char d = 0;

            Debug.printD(TAG, "[Enter] checkUUID16Char");
            Debug.printD(TAG, "====================");
            for (int i = 0; i < elCount; i++)
            {
                bHighByte = data[byteArraySize - i * elLength - 1];
                bLowByte = data[byteArraySize - i * elLength - 2];

                d = (char) ((char) (bHighByte << SHIFT8BIT) | (char) (bLowByte & MASK));

                Debug.printD(TAG, String.format("0x%x%n", (int) d));
                
                mServiceUUID16List.remove(Integer.valueOf(d));
            }
            Debug.printD(TAG, "====================");
            
        }
        
    }
    

    static class UUID128ServiceHandler implements IDiscoveryHandler
    {
        /**
         *  This method checks if the Discovery Information is in the UUID128 Service
         *  list or not. this method removes the Service UUID from the list.
         *  
         * @param response [in] the response of discovery 
         *            Range: a valid object of ServiceDiscoveryIndication
         *            Unit: ServiceDiscoveryIndication
         *            Scaling: 1
         *              
         * @return void [out] 
         * 
         * @see mServiceUUID128List
         */
        @Override
        public void handle(ServiceDiscoveryIndication response)
        {
            byte[] uuid128 = new byte[BlueConstant.UUID128_LEN];
            byte[] match = null;
            ByteBuffer data = ByteBuffer.wrap(response.getData());
            
            data.position(2);
            data.get(uuid128);
            Debug.printD(TAG, "[Enter] checkUUID128Char");
            
            uuid128 = BLEController.makeLittleEndian(uuid128);
            
            for (byte[] each : mServiceUUID128List)
            {
                boolean isEqual = Arrays.equals(uuid128, each);
                
                if(isEqual)
                {
                    match = each;                 
                }
                else
                {
                    // Apply to the coding standard
                }
                
            }
            mServiceUUID128List.remove(match);
        }
        
    }
    
    
    /*
     * This class is used to check if all the discovering service are found. 
     */
    static class DiscoveryDoneHandler implements IDiscoveryHandler
    {

        /**
         *  The method checks if all discovering Services are found or not.
         *  It returns the result via callback function. 
         *  
         * @param response [in] the response of discovery 
         *            Range: a valid object of ServiceDiscoveryIndication
         *            Unit: ServiceDiscoveryIndication
         *            Scaling: 1
         *              
         * @return void [out] 
         * 
         * @see mServiceUUID128List
         * @see mServiceUUID16List
         * @see mCallback
         * 
         */
        @Override
        public void handle(ServiceDiscoveryIndication response)
        {
            SafetyBoolean isResult = SafetyBoolean.FALSE;
            boolean isServiceCheckDone = ( mServiceUUID16List.isEmpty() 
                    && mServiceUUID128List.isEmpty()) ;
            
            for (int each : mServiceUUID16List)
            {
                Debug.printI(TAG, "UUID16: " + each);
            }
            
            for (byte[] each : mServiceUUID128List)
            {
                Debug.printI(TAG, "UUID128:========== start " );
                Debug.dumpPayload(TAG, each);
                Debug.printI(TAG, "UUID128:========== end " );
            }
                    
            Debug.printI(TAG, "isCheckDone: " + isServiceCheckDone);
            
            if( isServiceCheckDone )
            {       
                isResult = SafetyBoolean.TRUE;                                
            }
            else
            {
                mServiceUUID16List.clear();
                mServiceUUID128List.clear();
            }
            returnResult(isResult);
        }        
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
    protected static void returnResult(SafetyBoolean isResult)
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
     * This method sets and submits the following requests to discover the service of
     * Ble-devices.
     * 
     * This method handles the Discovery request. 
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the callback function     
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1    
     *                     
     * @return void [out] 
     * 
     * @see mContext
     * @see mCallback
     */

    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[discovery] request enter");

        ServiceDiscoveryRequest request = (ServiceDiscoveryRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_DISCOVERY_SRV);       
        
        SafetyByteArray sfBdAdress = GlobalTools.MPR.getMpAddress();
        
        mCallback = callback;
        
        if ( null != request )
        {
            fillUUID();
            
            request.setDiscoveryType(new SafetyNumber<Integer>((int) BlueConstant.DiscoveryType.CHARC_DESC, 
                    -BlueConstant.DiscoveryType.CHARC_DESC));
            
            request.setRemoteBD(sfBdAdress);

            request.setStartHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MIN_HANDLE, -BlueConstant.HANDLE.MIN_HANDLE));

            request.setEndHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.MAX_HANDLE, -BlueConstant.HANDLE.MAX_HANDLE));

            request.setUUID16(new SafetyNumber<Integer>(UUID.UUID16.BLANK,
                    -UUID.UUID16.BLANK));

            request.setUUID128(new SafetyByteArray(UUID.UUID128.BLANK, CRCTool
                    .generateCRC16(UUID.UUID128.BLANK)));
            
            BLEResponseReceiver.getInstance(mContext.getApplicationContext()).registerReceiver(
                    ResponseAction.CommandResponse.BT_DISCOVERY_INFO, this);
            
            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            returnResult(SafetyBoolean.FALSE);
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
